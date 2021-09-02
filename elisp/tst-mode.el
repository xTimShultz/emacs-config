;;; begin tst-mode.el 

(require 'generic) 

(defun tst-mode-setup () 
  (modify-syntax-entry ?_ "w") 
  (setq imenu-generic-expression '((nil 
				    "^\\([A-Za-z0-9$_]+\\)\\s-*:\\s-*\\(\\(ACCESS\\|LOCAL\\|ROOT\\)\\b\\s-*\\)?PROCEDURE\\b" 
				    1)))) 

;;;###autoload 
(define-generic-mode 
    'tst-mode 
  '(("/*" . "*/"))
  '("IF" "THEN" "ELSE" "WHILE" 
    "DO" "END" 
    "CALL" "RETURN" 
    "PROCEDURE" "DECLARE" "STRUCTURE" 
    "LITERALLY" "BASED" "AT" 
    "AND" "OR" "NOT" 
    "TRUE" "FALSE" 
    "AS" "STRING"
    "DATA" "LOCAL" "PUBLIC") 
  '(("[^_]\\(INTEGER\\|WORD\\|DWORD\\|REAL\\|BYTE\\|BOOLEAN\\|POINTER\\)[^(_]" 
     .. (1 font-lock-type-face)) 
    ("^\\$\\sw+" . font-lock-preprocessor-face) 
    ("^\\([A-Za-z0-9$_]+\\)\\s-*:.*SUB.*;\\s-*$" . (1 
						    font-lock-function-name-face)) 
    ("^END\\s-+\\([A-Za-z0-9$_]+\\)\\s-*;\\s-*$" . (1 
						    font-lock-function-name-face))
("%.*%" . font-lock-constant-face)) 
    '("\\.tst\\'" "\\.TST\\'" "\\.ext\\'" "\\.EXT\\'")
  '(tst-mode-setup) 
  "Mode for editing PL/M source" 
  )


(provide 'tst-mode) 

;;; end tst-mode.el