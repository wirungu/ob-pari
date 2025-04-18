;;; ob-pari.el --- org-babel functions for PARI/GP evaluation

;; Keywords: literate programming, reproducible research, pari, gp
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

(require 'org-babel)
(require 'comint)

;; Add GP file extension for tangling
(add-to-list 'org-babel-tangle-lang-exts '("gp" . "gp"))

;; Default header arguments for PARI/GP
(defvar org-babel-default-header-args:pari
  '((:session . "none")
    (:results . "output"))
  "Default arguments for evaluating PARI/GP blocks.")

(defconst org-babel-pari-eoe "org_babel_pari_eoe"
  "String to signal end of PARI/GP output.")

(defun org-babel-pari-table-or-string (results)
  "If RESULTS look like a table, turn it into a Lisp table, otherwise return as string."
  (let ((lines (split-string results "\n" t)))
    (if (cl-every (lambda (line)
                    (string-match-p "\`[0-9 	]+\'" line))
                  lines)
        (mapcar (lambda (line)
                  (mapcar #'string-to-number (split-string line)))
                lines)
      results)))

(defun org-babel-pari-initiate-session (&optional session)
  "Initiate or reuse a PARI/GP session named SESSION.
If SESSION is "none", do not start any process."
  (unless (string= session "none")
    (let* ((name (if (stringp session) session "default"))
           (buffer (get-buffer (format "*org-babel-pari-%s*" name))))
      (unless (comint-check-proc buffer)
        (apply #'make-comint-in-buffer name buffer "gp" nil '("-q" "-f")))
      buffer)))

(defun org-babel-expand-body:pari (body params)
  "Expand BODY according to PARAMS."
  ;; No variable substitution for now
  body)

(defun org-babel-execute:pari (body params)
  "Execute a block of PARI/GP code with Babel."
  (let* ((session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
         (buffer (org-babel-pari-initiate-session session))
         (full-body (org-babel-expand-body:pari body params))
         (eoe org-babel-pari-eoe)
         (comint-output-filter-functions nil)
         (results nil))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert full-body)
      (comint-send-input)
      (insert (format "print(\"%s\")\n" eoe))
      (comint-send-input)
      (while (not (save-excursion
                    (goto-char comint-last-input-end)
                    (re-search-forward eoe nil t)))
        (accept-process-output (get-buffer-process buffer) 0.1))
      (setq results
            (buffer-substring-no-properties
             comint-last-input-end
             (progn (goto-char comint-last-input-end)
                    (re-search-forward eoe nil t)
                    (match-beginning 0)))))
    (org-babel-pari-table-or-string results)))

(provide 'ob-pari)
;;; ob-pari.el ends here
