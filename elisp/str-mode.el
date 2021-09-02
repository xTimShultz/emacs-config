;;; begin str-mode.el 

(require 'generic) 

(defun str-mode-setup () 
  (modify-syntax-entry ?_ "w") 
  (setq imenu-generic-expression '((nil 
				    "^\\([A-Za-z0-9$_]+\\)\\s-*:\\s-*\\(\\(ACCESS\\|LOCAL\\|ROOT\\)\\b\\s-*\\)?PROCEDURE\\b" 
				    1)))) 

;;;###autoload 
(define-generic-mode 
    'str-mode 
  '(("***" . "***")) 
  '("Filechk" "Filetest" "Traceability" "Data Flow" "tts"
    "Control Flow" "" 
    "Input/Output Analysis" "Operational Characteristics" "Input/Output"
    "Partitioning Analysis" "System Testing" "STRUCTURE" 
    "filechk" "filetest" "clp" "CLP" "SM" "MON" "mon" "VDP"
    "sm" "vdp" "checklist" "Checklist" "SDD" "HCCI")
  '(("[^_]\\(INTEGER\\|WORD\\|DWORD\\|REAL\\|BYTE\\|BOOLEAN\\|POINTER\\)[^(_]" 
     .. (1 font-lock-type-face)) 
    ("<-" . font-lock-function-name-face) 
    ("->" . font-lock-function-name-face) 
    ("\\([A-Za-z0-9_]+\\.[A-Za-z][A-Za-z]?[A-Za-z]?,? \\)" . (1 font-lock-function-name-face)) 
    ("\\(Test #[0-9]+\\)" . (1 font-lock-variable-name-face)) 
    ("^\\([0-9]\)\\)" . (1 font-lock-function-name-face)) 
    ("^END\\s-+\\([A-Za-z0-9$_]+\\.[A-Za-z]+\\)\\s-*;\\s-*$" . (1 font-lock-function-name-face)) 
    ("'.*'" . font-lock-constant-face)) 
  '("\\.str\\'" "\\.STR\\'" "\\.ext\\'" "\\.EXT\\'") 
  '(str-mode-setup) 
  "Mode for editing STR's" 
) 


(provide 'str-mode) 

;;; end str-mode.el