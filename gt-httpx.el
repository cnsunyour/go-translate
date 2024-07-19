;;; gt-httpx.el --- Http Client Components -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>
;; Author: lorniu <lorniu@gmail.com>
;; Package-Requires: ((emacs "28.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Http client components used by the framework.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(declare-function gt-log "ext:gt-core")
(declare-function gt-single "ext:gt-core")


;;; Http Client

(defclass gt-http-client (gt-single)
  ((user-agent :initarg :user-agent :initform nil :type (or string null)))
  "Used to send a request and get the response."
  :abstract t)

(defvar gt-http-client-max-retry 3)

(defvar gt-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36")

(defvar-local gt-http-client-stream-abort-flag nil
  "Non-nil means to ignore following stream progress in callback of http filter.")

(cl-defgeneric gt-request (http-client &key url filter done fail data headers cache retry)
  "Send HTTP request using the given HTTP-CLIENT.

Optional keyword arguments:
  - URL: The URL to send the request to.
  - FILTER: A function to be called every time when some data returned.
  - DONE: A function to be called when the request succeeds.
  - FAIL: A function to be called when the request fails.
  - DATA: The data to include in the request.
  - HEADERS: Additional headers to include in the request.
  - CACHE: enable get and set results in cache for current request.
  - RETRY: how many times it can retry for timeout

It should return the process of this request."
  (:method :around ((client gt-http-client) &key url filter done fail data headers retry)
           (cl-assert (and url (or filter done)))
           (let* ((tag (eieio-object-class client))
                  (failfn (lambda (status)
                            ;; timeout and retry
                            (unless retry (setq retry gt-http-client-max-retry))
                            (if (and (string-match-p "Operation timeout" (format "%s" status)) (cl-plusp retry))
                                (progn (gt-log tag (format "request timeout, retrying (remains %d times)..." retry))
                                       (gt-request client
                                                   :url url :filter filter :done done :fail fail
                                                   :data data :headers headers :retry (1- retry)))
                              ;; failed case
                              (gt-log tag (format "Request FAIL: (%s) %s" url status))
                              (if fail (funcall fail status)
                                (signal (car status) (cdr status))))))
                  (filterfn (when filter
                              (lambda ()
                                ;; abort action and error case
                                (unless gt-http-client-stream-abort-flag
                                  (condition-case err
                                      (funcall filter)
                                    (error
                                     (setq gt-http-client-stream-abort-flag t)
                                     (gt-log tag (format "Error in filter: (%s) %s" url err))
                                     (funcall failfn err)))))))
                  (donefn (lambda (raw)
                            (gt-log tag (format "✓ %s" url))
                            (when done (funcall done raw)))))
             (gt-log tag
               (format "> %s\n> %s" client url)
               (if headers (format "> HEADER: %s" headers))
               (if data (format "> DATA:   %s" data)))
             (cl-call-next-method client :url url :headers headers :data data :filter filterfn :done donefn :fail failfn))))


;; http client implemented using `url.el'

(require 'url)

(defclass gt-url-http-client (gt-http-client)
  ((proxy-services
    :initarg :proxies
    :initform nil
    :type (or list null)
    :documentation "Proxy services passed to `url.el', see `url-proxy-services' for details."))
  :documentation "Http Client implemented using `url.el'.")

(defvar url-http-end-of-headers)

(defvar url-http-transfer-encoding)

(defvar gt-url-extra-filter nil)

(defun gt-url-http-extra-filter (beg end len)
  (when (and gt-url-extra-filter (bound-and-true-p url-http-end-of-headers)
             (if (equal url-http-transfer-encoding "chunked") (= beg end) ; when delete
               (= len 0))) ; when insert
    (save-excursion
      (save-restriction
        (narrow-to-region url-http-end-of-headers (point-max))
        (funcall gt-url-extra-filter)))))

(cl-defmethod gt-request ((client gt-url-http-client) &key url filter done fail data headers retry)
  (ignore retry)
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url-user-agent (or (oref client user-agent) gt-user-agent))
         (url-proxy-services (or (oref client proxy-services) url-proxy-services))
         (url-request-extra-headers headers)
         (url-request-method (if data "POST" "GET"))
         (url-request-data data)
         (url-mime-encoding-string "identity")
         (buf (url-retrieve
               url (lambda (status)
                     (let ((cb (current-buffer)))
                       (set-buffer-multibyte t)
                       (remove-hook 'after-change-functions #'gt-url-http-extra-filter t)
                       (unwind-protect
                           (if-let (err (or (cdr-safe (plist-get status :error))
                                            (when (or (null url-http-end-of-headers) (= 1 (point-max)))
                                              "Nothing responsed from server")))
                               (if fail (funcall fail err)
                                 (signal 'user-error err))
                             (when done
                               (funcall done (buffer-substring-no-properties url-http-end-of-headers (point-max)))))
                         (kill-buffer cb))))
               nil t)))
    (when (and filter (buffer-live-p buf))
      (with-current-buffer buf
        (setq-local gt-url-extra-filter filter)
        (add-hook 'after-change-functions #'gt-url-http-extra-filter nil t)))
    (get-buffer-process buf)))


;;; request with curl implements via package `plz'

;; you should install `plz' before use this

(defclass gt-plz-http-client (gt-http-client)
  ((extra-args
    :initarg :args
    :type list
    :documentation "Extra arguments passed to curl programe.")))

(defvar plz-curl-program)
(defvar plz-curl-default-args)
(defvar plz-http-end-of-headers-regexp)

(declare-function plz "ext:plz.el" t t)
(declare-function plz-error-message "ext:plz.el" t t)
(declare-function plz-error-curl-error "ext:plz.el" t t)
(declare-function plz-error-response "ext:plz.el" t t)
(declare-function plz-response-status "ext:plz.el" t t)
(declare-function plz-response-body "ext:plz.el" t t)

(defvar gt-plz-initialize-error-message
  "\n\nTry to install curl and specify the program like this to solve the problem:\n
  (setq plz-curl-program \"c:/msys64/usr/bin/curl.exe\")\n
Or switch http client to `gt-url-http-client' instead:\n
  (setq gt-default-http-client (gt-url-http-client))")

(cl-defmethod gt-request :before ((_ gt-plz-http-client) &rest _)
  (unless (and (require 'plz nil t) (executable-find plz-curl-program))
    (error "You should have `plz.el' and `curl' installed before using `gt-plz-http-client'")))

(cl-defmethod gt-request ((client gt-plz-http-client) &key url filter done fail data headers retry)
  (ignore retry)
  (let ((plz-curl-default-args
         (if (slot-boundp client 'extra-args)
             (append (oref client extra-args) plz-curl-default-args)
           plz-curl-default-args)))
    (plz (if data 'post 'get) url
      :headers (cons `("User-Agent" . ,(or (oref client user-agent) gt-user-agent)) headers)
      :body data
      :as 'string
      :filter (when filter
                (lambda (proc string)
                  (with-current-buffer (process-buffer proc)
                    (save-excursion
                      (goto-char (point-max))
                      (insert string)
                      (goto-char (point-min))
                      (when (re-search-forward plz-http-end-of-headers-regexp nil t)
                        (save-restriction
                          (narrow-to-region (point) (point-max))
                          (funcall filter)))))))
      :then (lambda (raw)
              (when done (funcall done raw)))
      :else (lambda (err)
              (let ((ret ;; try to compat with error object of url.el, see `url-retrieve' for details
                     (or (plz-error-message err)
                         (when-let (r (plz-error-curl-error err))
                           (list 'curl-error
                                 (concat (format "%s" (or (cdr r) (car r)))
                                         (pcase (car r)
                                           (2 (when (memq system-type '(cygwin windows-nt ms-dos))
                                                gt-plz-initialize-error-message))))))
                         (when-let (r (plz-error-response err))
                           (list 'http (plz-response-status r) (plz-response-body r))))))
                (if fail (funcall fail ret)
                  (signal 'user-error ret)))))))

(provide 'gt-httpx)

;;; gt-httpx.el ends here
