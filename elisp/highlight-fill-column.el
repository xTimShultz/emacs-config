(defvar highlight-fill-column-string "\u2502")

(defvar highlight-fill-column-face 'highlight)

(defun highlight-fill-column (&rest _)
  (remove-overlays nil nil 'highlight-fill-column t)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((rest-width (- fill-column
                           (save-excursion
                             (end-of-line)
                             (current-column)))))
        (let* ((ov (make-overlay
                    (line-end-position) (1+ (line-end-position)) nil t))
               (ov2 (copy-overlay ov))
               (string (concat
                        (when (> rest-width 0)
                          (propertize (string 32)
                                      'display `(space-width ,rest-width)
                                      'cursor t))
                        (when (>= rest-width 0)
                          (propertize highlight-fill-column-string
                                      'cursor (= rest-width 0))))))
          (overlay-put ov 'before-string string)
          (overlay-put ov2 'face highlight-fill-column-face)
          (overlay-put ov 'priority 1)
          (overlay-put ov2 'priority 2)
          (overlay-put ov 'highlight-fill-column t)
          (overlay-put ov2 'highlight-fill-column t)))
      (next-line))))

(define-minor-mode highlight-fill-column-mode
  nil nil nil nil nil
  (if highlight-fill-column-mode
      (progn
        (add-hook 'after-change-functions 'highlight-fill-column nil t)
        (highlight-fill-column))
    (remove-overlays nil nil 'highlight-fill-column t)
    (remove-hook 'after-change-functions 'highlight-fill-column  t)))

(provide 'highlight-fill-column)
