;;; begin basicscript-mode.el 

(require 'generic) 

;;;###autoload 
(define-generic-mode 
    'basicscript-mode 
  '(("'")) 
  '("DO" "WHILE" "LOOP")
  '(("=" . 'font_lock_operator))
  '("\\.tst\\'" "\\.TST\\'")
  'nil
  "Mode for editing Basic Script source" 
  ) 

(provide 'basicscript-mode) 

;;; end basicscript-mode.el