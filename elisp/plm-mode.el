;;; begin plm-mode.el 

(require 'generic) 

(defun plm-mode-setup () 
  (modify-syntax-entry ?_ "w") 
  (setq imenu-generic-expression '((nil 
				    "^\\([A-Za-z0-9$_]+\\)\\s-*:\\s-*\\(\\(ACCESS\\|LOCAL\\|ROOT\\)\\b\\s-*\\)?PROCEDURE\\b" 
				    1)))) 

;;;###autoload 
(define-generic-mode 
    'plm-mode 
  '(("/*" . "*/")) 
  '("IF" "THEN" "ELSE" "WHILE" 
    "DO" "END" 
    "CALL" "RETURN" 
    "PROCEDURE" "DECLARE" "STRUCTURE" 
    "LITERALLY" "BASED" "AT" 
    "AND" "OR" "NOT" 
    "TRUE" "FALSE" 
    "DATA" "LOCAL" "PUBLIC") 
  '(("[^_]\\(INTEGER\\|WORD\\|DWORD\\|REAL\\|BYTE\\|BOOLEAN\\|POINTER\\)[^(_]" 
     .. (1 font-lock-type-face)) 
    ("^\\$\\sw+" . font-lock-preprocessor-face) 
    ("^\\([A-Za-z0-9$_]+\\)\\s-*:.*PROCEDURE.*;\\s-*$" . (1 
							  font-lock-function-name-face)) 
    ("^END\\s-+\\([A-Za-z0-9$_]+\\)\\s-*;\\s-*$" . (1 
						    font-lock-function-name-face)) 
    ("'.*'" . font-lock-constant-face)) 
  '("\\.plm\\'" "\\.PLM\\'" "\\.ext\\'" "\\.EXT\\'") 
  '(plm-mode-setup) 
  "Mode for editing PL/M source" 
  ) 


(provide 'plm-mode) 

;;; end plm-mode.el