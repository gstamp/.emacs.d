;;; restclient.el --- An interactive HTTP client for Emacs

;; Public domain.

;; Author: Pavel Kurnosov <pashky@gmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 01 Apr 2012
;; Keywords: http

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

(require 'url)
(require 'cl)
(require 'json-reformat)

(defcustom restclient-same-buffer-response t
  "Re-use same buffer for responses or create a new one each time")

(defcustom restclient-same-buffer-response-name "*HTTP Response*"
  "Name for response buffer")

(defvar restclient-within-call nil)

; The following disables the interactive request for user name and
; password should an API call encounter a permission-denied response.
; This API is meant to be usable without constant asking for username
; and password.
(defadvice url-http-handle-authentication (around restclient-fix)
  (if restclient-within-call
	  (setq success t)
	ad-do-it)
  (setq restclient-within-call nil))
(ad-activate 'url-http-handle-authentication)

(defadvice url-cache-extract (around restclient-fix-2)
  (if restclient-within-call
	  (setq success t)
	ad-do-it)
  (setq restclient-within-call nil))
(ad-activate 'url-cache-extract)

(defun restclient-http-do (method url headers entity raw)
  "Send ARGS to URL as a POST request."
  (let* ((url-request-method method)
		(url-request-extra-headers headers)
		(url-request-data entity))
	(setq restclient-within-call t)
	(url-retrieve url 'restclient-http-handle-response
				  (list (if restclient-same-buffer-response
							restclient-same-buffer-response-name
						  (format "*HTTP %s %s*" method url)) raw)
				  )))

(defun restclient-prettify-response ()
  (save-excursion
	(let ((start (point)) (guessed-mode))
	  (while (not (looking-at "^\\s-*$"))
		(when (looking-at "^Content-[Tt]ype: \\([^; \n]+\\).*$")
		  (setq guessed-mode
				(cdr (assoc-string
					  (buffer-substring-no-properties (match-beginning 1) (match-end 1))
					  '(("text/xml" . xml-mode)
						("application/xml" . xml-mode)
						("application/json" . js-mode)
						("image/png" . image-mode)
						("image/jpeg" . image-mode)
						("image/gif" . image-mode)
						("text/html" . html-mode))))))
		(forward-line))
	  (let ((headers (buffer-substring-no-properties start (point))))
		(forward-line)
		(when guessed-mode
		  (delete-region start (point))
		  (unless (eq guessed-mode 'image-mode)
			(apply guessed-mode '())
			(font-lock-fontify-buffer))

		  (cond
		   ((eq guessed-mode 'xml-mode)
			(goto-char (point-min))
			(while (search-forward-regexp "\>[ \\t]*\<" nil t)
			  (backward-char) (insert "\n"))
			(indent-region (point-min) (point-max)))

		   ((eq guessed-mode 'image-mode)
			(let* ((img (buffer-string)))
			  (delete-region (point-min) (point-max))
			  (fundamental-mode)
			  (insert-image (create-image img nil t))

			  ))

		   ((eq guessed-mode 'js-mode)
			(json-reformat-region (point-min) (point-max))))

		  (goto-char (point-max))
		  (let ((hstart (point)))
			(insert "\n" headers)
			(unless (eq guessed-mode 'image-mode)
			  (comment-region hstart (point))
			  (indent-region hstart (point)))))))))

(defun restclient-http-handle-response (status bufname raw)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (if restclient-same-buffer-response
	  (if (get-buffer restclient-same-buffer-response-name)
		  (kill-buffer restclient-same-buffer-response-name)))
  (rename-buffer (generate-new-buffer-name bufname))
  (switch-to-buffer-other-window (current-buffer))

  (unless raw
	(restclient-prettify-response))
  (buffer-enable-undo))


(defconst restclient-method-url-regexp
  "^\\(GET\\|POST\\|DELETE\\|PUT\\|HEAD\\|OPTIONS\\) \\(.*\\)$")

(defconst restclient-header-regexp
  "^\\([^ :]+\\): \\(.*\\)$")

(defun restclient-current-min ()
  (save-excursion
	(beginning-of-line)
	(if (looking-at "^#")
		(if (re-search-forward "^[^#]" (point-max) t)
			(point-at-bol))
	  (if (re-search-backward "^#" (point-min) t)
		  (point-at-bol 2)
		(point-min)))))

(defun restclient-current-max ()
  (save-excursion
	(if (re-search-forward "^#" (point-max) t)
		(point-at-bol)
	  (point-max))))

(defconst restclient-params-regexp
  "^\\(PARAMS\\)$")

(defconst restclient-param-val-regexp
  "^\\([^ \\=]+\\) *= *\\(.*\\)$")

(defun restclient-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun restclient-find-params ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward restclient-params-regexp nil 't)
        (progn
          (forward-line)
          (let ((properties (make-hash-table)))
            (while (string-match ".*=.*" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (let ((key-val (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "=")))
                (puthash (restclient-trim-string (car key-val))
                         (restclient-trim-string (cadr key-val))
                         properties))
              (forward-line))
            properties))
      (make-hash-table))))

(defun restclient-string-replace (this withthat in)
  "replace THIS with WITHTHAT' in the string IN"
  (with-temp-buffer
    (insert in)
    (goto-char (point-min))
    (while (search-forward this nil t)
      (replace-match withthat nil t))
    (buffer-substring (point-min) (point-max))))

(defun restclient-replace-params (params str)
  (lexical-let ((temp-str str))
    (maphash (lambda (key value)
               (setq temp-str
                     (restclient-string-replace (concat "{{" key "}}")
                                                value
                                                temp-str
                                                )))
             params)
    temp-str))

;;;###autoload
(defun restclient-http-send-current (&optional raw)
  (interactive)
  (goto-char (restclient-current-min))
  (save-excursion
    (when (re-search-forward restclient-method-url-regexp (point-max) t)
      (let* ((begin (match-beginning 1))
             (end (match-end 1))
             (begin2 (match-beginning 2))
             (end2 (match-end 2))
             (params (restclient-find-params))
             (method (buffer-substring-no-properties begin end))
             (url (restclient-replace-params params (buffer-substring-no-properties begin2 end2)))
             (headers '()))
        (forward-line)
        (while (re-search-forward restclient-header-regexp (point-at-eol) t)
          (setq headers (cons
                         (cons (restclient-replace-params params (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                               (restclient-replace-params params (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
                              headers))
          (forward-line))
        (when (looking-at "^\\s-*$")
          (forward-line))
        (let ((entity (buffer-substring (point) (restclient-current-max))))
          (message "HTTP %s %s Headers:[%s] Body:[%s]" method url headers entity)
          (restclient-http-do method url headers entity raw))))))

;;;###autoload
(defun restclient-http-send-current-raw ()
  (interactive)
  (restclient-http-send-current t))

(setq restclient-mode-keywords
      (list
       (list restclient-method-url-regexp '(1 font-lock-keyword-face) '(2 font-lock-function-name-face))
       (list restclient-header-regexp '(1 font-lock-variable-name-face) '(2 font-lock-string-face))
       (list restclient-params-regexp '(1 font-lock-keyword-face))
       (list restclient-param-val-regexp '(1 font-lock-variable-name-face) '(2 font-lock-string-face))
       ))

(defvar restclient-mode-syntax-table
  (let ((table (make-syntax-table)))
	(modify-syntax-entry ?\# "<" table)
	(modify-syntax-entry ?\n ">#" table)
	table))

;;;###autoload
(define-derived-mode restclient-mode fundamental-mode "REST Client"

  (local-set-key "\C-c\C-c" 'restclient-http-send-current)
  (local-set-key "\C-c\C-r" 'restclient-http-send-current-raw)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#\\W*")
  (set (make-local-variable 'comment-column) 48)

  (set (make-local-variable 'font-lock-defaults) '(restclient-mode-keywords)))


(provide 'restclient)
;;; restclient.el ends here
