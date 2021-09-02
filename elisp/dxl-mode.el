;;; begin dxl-mode.el 

(require 'generic) 

(defun dxl-mode-setup () 
  (modify-syntax-entry ?_ "w") 
  (setq imenu-generic-expression '((nil 
				    "^\\([a-za-z0-9$_]+\\)\\s-*:\\s-*\\(\\(access\\|local\\|root\\)\\b\\s-*\\)?procedure\\b" 
				    1)))) 

;;;###autoload 
(define-generic-mode 
    'dxl-mode 
  '(("/*" . "*/") ("//")) 
  '("if" "then" "else" "while" 
    "do" "end" "pragma" "Item" "#include"
    "return" "runLim" "stack"
    "Baseline" "Module" "break"
    "and" "or" "not" 
    "true" "false" 
    "data" "local" "public") 
  '(("[^_]\\(int\\|real\\|void\\|bool\\|char\\|string\\)[^(_]" 
     .. (1 font-lock-type-face)) 
    ("^\\$\\sw+" . font-lock-preprocessor-face) 
    ("^\\([a-za-z0-9$_]+\\)\\s-*:.*procedure.*;\\s-*$" . (1 
							  font-lock-function-name-face)) 
    ("^end\\s-+\\([a-za-z0-9$_]+\\)\\s-*;\\s-*$" . (1 
						    font-lock-function-name-face)) 
    ("'.*'" . font-lock-constant-face)) 
  '("\\.dxl\\'" "\\.dxl\\'" "\\.ext\\'" "\\.ext\\'") 
  '(dxl-mode-setup) 
  "mode for editing Doors DXL source" 
  ) 


(provide 'dxl-mode) 

;;; end dxl-mode.el