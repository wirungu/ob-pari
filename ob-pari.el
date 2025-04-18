;;; ob-pari.el --- org-babel functions for PARI/GP code evaluation -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-Babel support for evaluating PARI/GP code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'ob-comint)
(require 'comint)

(defvar org-babel-header-args:pari '())

(defcustom org-babel-pari-command "gp"
  "Name of command to use for executing PARI/GP code."
  :group 'org-babel
  :type 'string)

(defun org-babel-pari-initiate-session (&optional session _params)
  "Start a GP session if one doesn't already exist."
  (let ((session-name (or session "*PARI*")))
    (unless (org-babel-comint-buffer-livep session-name)
      (save-window-excursion
        (apply 'make-comint-in-buffer "PARI" session-name org-babel-pari-command nil '("-q"))))
    session-name))

(defun org-babel-pari-format-val (val)
  "Format VAL as a string of GP code."
  (cond
   ((listp val) (concat "[" (mapconcat #'org-babel-pari-format-val val ", ") "]"))
   ((stringp val) (format "%S" val))
   (t (format "%s" val))))

(defun org-babel-variable-assignments:pari (params)
  "Return list of GP code assigning the block's variables."
  (mapcar (lambda (pair)
            (format "%s = %s;" (car pair) (org-babel-pari-format-val (cdr pair))))
          (org-babel--get-vars params)))

(defun org-babel-expand-body:pari (body params)
  "Expand BODY with variable assignments, prologue and epilogue."
  (mapconcat #'identity
             (append
              (org-babel-variable-assignments:pari params)
              (when-let ((pro (cdr (assq :prologue params)))) (list pro))
              (list body)
              (when-let ((epi (cdr (assq :epilogue params)))) (list epi)))
             "\n"))

(defun org-babel-pari-clean-output (output)
  "Clean OUTPUT from GP prompt and result prefix."
  (replace-regexp-in-string "^%[0-9]+ = " "" (org-babel-chomp output)))

(defun org-babel-pari-evaluate-session (session body result-type _result-params)
  "Evaluate BODY in SESSION. Return result based on RESULT-TYPE."
  (let* ((eoe "org_babel_pari_eoe")
         (full-body (concat body "\nprint(\"" eoe "\")\n"))
         (results (org-babel-comint-with-output (session eoe)
                    (insert full-body)
                    (comint-send-input))))
    (org-babel-pari-clean-output
     (mapconcat #'identity (butlast results) "\n"))))

(defun org-babel-pari-evaluate-external-process (body _result-type _result-params)
  "Evaluate BODY in external GP process. Return the output as string."
  (let ((script-file (org-babel-temp-file "pari-" ".gp")))
    (with-temp-file script-file
      (insert body))
    (org-babel-eval (concat org-babel-pari-command " -q " script-file) "")))

;;;###autoload
(defun org-babel-execute:pari (body params)
  "Execute a block of PARI/GP code with org-babel."
  (let* ((session (cdr (assq :session params)))
         (result-type (cdr (assq :result-type params)))
         (result-params (cdr (assq :result-params params)))
         (full-body (org-babel-expand-body:pari body params)))
    (if (or (not session) (string= session "none"))
        (org-babel-pari-evaluate-external-process full-body result-type result-params)
      (org-babel-pari-evaluate-session
       (org-babel-pari-initiate-session session params)
       full-body result-type result-params))))

(provide 'ob-pari)

;;; ob-pari.el ends here
