;;; begin basicscript-mode.el 

(require 'generic) 

;;;###autoload 
(define-generic-mode 
    'basicscript-mode 
  '(("'") ("/*" . "*/"))
  '("Do" "While" "Loop" "If" "then" "End If" "Exit" "pSet" "pRamp" "sleep" "End" "Sub"
    "Private" "Public" "Main" "Call" "As" "Integer" "String" "+eblf" "msgbox" "_"
    "False" "True" "Or" "And" "+" "Boolean")
  '(("=" . 'font_lock_operator))
  '("\\.tst\\'" "\\.TST\\'")
  'nil
  "Mode for editing Basic Script source" 
  )

(provide 'basicscript-mode) 

;;; end basicscript-mode.el