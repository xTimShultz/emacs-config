;;; yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid
;; 
;; Author: pluskid <pluskid@gmail.com>
;; Version: 0.3.1
;; X-URL: http://code.google.com/p/yasnippet/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Basic steps to setup:
;;   1. Place `yasnippet.el' in your `load-path'.
;;   2. In your .emacs file:
;;        (require 'yasnippet)
;;   3. Place the `snippets' directory somewhere. E.g: ~/.emacs.d/snippets
;;   4. In your .emacs file
;;        (yas/initialize)
;;        (yas/load-directory "~/.emacs.d/snippets")
;;
;; For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/key-syntaxes (list "w" "w_" "w_." "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/root-directory nil
  "The root directory that stores the snippets for each major modes.")

(defvar yas/indent-line t
  "Each (except the 1st) line of the snippet template is indented to
current column if this variable is non-`nil'.")
(make-variable-buffer-local 'yas/indent-line)

(defvar yas/trigger-key (kbd "TAB")
  "The key to bind as a trigger of snippet.")
(defvar yas/next-field-key (kbd "TAB")
  "The key to navigate to next field.")

(defvar yas/keymap (make-sparse-keymap)
  "The keymap of snippet.")
(define-key yas/keymap yas/next-field-key 'yas/next-field-group)
(define-key yas/keymap (kbd "S-TAB") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-iso-lefttab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-tab>") 'yas/prev-field-group)

(defvar yas/show-all-modes-in-menu nil
  "Currently yasnippet only all \"real modes\" to menubar. For
example, you define snippets for \"cc-mode\" and make it the
parent of `c-mode', `c++-mode' and `java-mode'. There's really
no such mode like \"cc-mode\". So we don't show it in the yasnippet
menu to avoid the menu becoming too big with strange modes. The
snippets defined for \"cc-mode\" can still be accessed from
menu-bar->c-mode->parent (or c++-mode, java-mode, all are ok).
However, if you really like to show all modes in the menu, set
this variable to t.")
(defvar yas/use-menu t
  "If this is set to `t', all snippet template of the current
mode will be listed under the menu \"yasnippet\".")
(defvar yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger.")

(defface yas/field-highlight-face
  '((((class color) (background light)) (:background "DarkSeaGreen2"))
    (t (:background "DimGrey")))
  "The face used to highlight a field of snippet.")
(defface yas/mirror-highlight-face
  '((((class color) (background light)) (:background "LightYellow2"))
    (t (:background "gray22")))
  "The face used to highlight mirror fields of a snippet.")

(defvar yas/window-system-popup-function #'yas/x-popup-menu-for-template
  "When there's multiple candidate for a snippet key. This function
is called to let user select one of them. `yas/text-popup-function'
is used instead when not in a window system.")
(defvar yas/text-popup-function #'yas/text-popup-for-template
  "When there's multiple candidate for a snippet key. If not in a
window system, this function is called to let user select one of
them. `yas/window-system-popup-function' is used instead when in
a window system.")

(defvar yas/extra-mode-hooks
  '(ruby-mode-hook)
  "A list of mode-hook that should be hooked to enable yas/minor-mode.
Most modes need no special consideration. Some mode (like ruby-mode)
doesn't call `after-change-major-mode-hook' need to be hooked explicitly.")

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.
The hooks will be run in an environment where some variables bound to 
proper values:
 * yas/snippet-beg : The beginning of the region of the snippet.
 * yas/snippet-end : Similar to beg.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/version "0.3.1")

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major-mode.")
(defvar yas/menu-table (make-hash-table)
  "A hash table of menus of corresponding major-mode.")
(defvar yas/menu-keymap (make-sparse-keymap "YASnippet"))
;; empty menu will cause problems, so we insert some items
(define-key yas/menu-keymap [yas/about]
  '(menu-item "About" yas/about))
(define-key yas/menu-keymap [yas/reload]
  '(menu-item "Reload all snippets" yas/reload-all))
(define-key yas/menu-keymap [yas/separator]
  '(menu-item "--"))

(defvar yas/known-modes
  '(ruby-mode rst-mode)
  "A list of mode which is well known but not part of emacs.")
(defconst yas/escape-backslash
  (concat "YASESCAPE" "BACKSLASH" "PROTECTGUARD"))
(defconst yas/escape-dollar
  (concat "YASESCAPE" "DOLLAR" "PROTECTGUARD"))
(defconst yas/escape-backquote
  (concat "YASESCAPE" "BACKQUOTE" "PROTECTGUARD"))

(defconst yas/field-regexp
  (concat "$\\([0-9]+\\)" "\\|"
	  "${\\(?:\\([0-9]+\\):\\)?\\([^}]*\\)}"))

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet")
(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))

(defvar yas/overlay-modification-hooks
  (list 'yas/overlay-modification-hook)
  "The list of hooks to the overlay modification event.")
(defvar yas/overlay-insert-in-front-hooks
  (list 'yas/overlay-insert-in-front-hook)
  "The list of hooks of the overlay inserted in front event.")
(defvar yas/keymap-overlay-modification-hooks
  (list 'yas/overlay-maybe-insert-behind-hook)
  "The list of hooks of the big keymap overlay modification event.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.
With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When YASnippet mode is enabled, the TAB key
expands snippets of code depending on the mode.

You can customize the key through `yas/trigger-key'."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " yas"
  ;; The minor mode bindings.
  `((,yas/trigger-key . yas/expand))
  :group 'editing)
(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode."
  (interactive)
  (yas/minor-mode 1))
(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (yas/template (:constructor yas/make-template (content name)))
  "A template for a snippet."
  content
  name)
(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet."
  (groups nil)
  (exit-marker nil)
  (id (yas/snippet-next-id) :read-only t)
  (overlay nil))
(defstruct (yas/group (:constructor yas/make-group (primary-field snippet)))
  "A group contains a list of field with the same number."
  primary-field
  (fields (list primary-field))
  (next nil)
  (prev nil)
  snippet)
(defstruct (yas/field 
	    (:constructor yas/make-field (overlay number value transform)))
  "A field in a snippet."
  overlay
  number
  transform
  value)
(defstruct (yas/snippet-table (:constructor yas/make-snippet-table ()))
  "A table to store snippets for a perticular mode."
  (hash (make-hash-table :test 'equal))
  (parent nil))

(defun yas/snippet-add-field (snippet field)
  "Add FIELD to SNIPPET."
  (let ((group (find field
		     (yas/snippet-groups snippet)
		     :test
		     '(lambda (field group)
			(and (not (null (yas/field-number field)))
			     (not (null (yas/group-number group)))
			     (= (yas/field-number field)
				(yas/group-number group)))))))
    (if group
	(yas/group-add-field group field)
      (push (yas/make-group field snippet)
	    (yas/snippet-groups snippet)))))

(defun yas/group-value (group)
  "Get the default value of the field group."
  (or (yas/field-value
       (yas/group-primary-field group))
      ""))
(defun yas/group-number (group)
  "Get the number of the field group."
  (yas/field-number
   (yas/group-primary-field group)))
(defun yas/group-add-field (group field)
  "Add a field to the field group. If the value of the primary 
field is nil and that of the field is not nil, the field is set
as the primary field of the group."
  (push field (yas/group-fields group))
  (when (and (null (yas/field-value (yas/group-primary-field group)))
	     (yas/field-value field))
    (setf (yas/group-primary-field group) field)))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the start point of the overlay."
  (let ((n1 (yas/field-number field1))
	(n2 (yas/field-number field2)))
    (if n1
	(if n2
	    (< n1 n2)
	  t)
      (if n2
	  nil
	(< (overlay-start (yas/field-overlay field1))
	   (overlay-start (yas/field-overlay field2)))))))

(defun yas/snippet-table-fetch (table key)
  "Fetch a snippet binding to KEY from TABLE. If not found,
fetch from parent if any."
  (let ((templates (gethash key (yas/snippet-table-hash table))))
    (when (and (null templates)
	       (not (null (yas/snippet-table-parent table))))
      (setq templates (yas/snippet-table-fetch
		       (yas/snippet-table-parent table)
		       key)))
    templates))
(defun yas/snippet-table-store (table full-key key template)
  "Store a snippet template in the table."
  (puthash key
	   (yas/modify-alist (gethash key
				      (yas/snippet-table-hash table))
			     full-key
			     template)
	   (yas/snippet-table-hash table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/real-mode? (mode)
  "Try to find out if MODE is a real mode. The MODE bound to
a function (like `c-mode') is considered real mode. Other well
known mode like `ruby-mode' which is not part of Emacs might
not bound to a function until it is loaded. So yasnippet keeps
a list of modes like this to help the judgement."
  (or (fboundp mode)
      (find mode yas/known-modes)))

(defun yas/eval-string (string)
  "Evaluate STRING and convert the result to string."
  (condition-case err
      (save-excursion
	(save-restriction
	  (save-match-data
	    (format "%s" (eval (read string))))))
    (error (format "(error in elisp evaluation: %s)" 
		   (error-message-string err)))))
(defun yas/calculate-field-value (field value)
  "Calculate the value of the field. If there's a transform
for this field, apply it. Otherwise, the value is returned
unmodified."
  (let ((text value)
	(transform (yas/field-transform field)))
    (if transform
	(yas/eval-string transform)
      text)))
(defsubst yas/replace-all (from to)
  "Replace all occurance from FROM to TO."
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t t)))

(defun yas/snippet-table (mode)
  "Get the snippet table corresponding to MODE."
  (let ((table (gethash mode yas/snippet-tables)))
    (unless table
      (setq table (yas/make-snippet-table))
      (puthash mode table yas/snippet-tables))
    table))
(defsubst yas/current-snippet-table ()
  "Get the snippet table for current major-mode."
  (yas/snippet-table major-mode))

(defun yas/menu-keymap-for-mode (mode)
  "Get the menu keymap correspondong to MODE."
  (let ((keymap (gethash mode yas/menu-table)))
    (unless keymap
      (setq keymap (make-sparse-keymap))
      (puthash mode keymap yas/menu-table))
    keymap))

(defun yas/current-key ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
	(end (point))
	(syntaxes yas/key-syntaxes)
	syntax done)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
	(skip-syntax-backward syntax)
	(when (yas/snippet-table-fetch
	       (yas/current-snippet-table)
	       (buffer-substring-no-properties (point) end))
	  (setq done t)
	  (setq start (point)))))
    (list (buffer-substring-no-properties start end)
	  start
	  end)))

(defun yas/synchronize-fields (field-group)
  "Update all fields' text according to the primary field."
  (save-excursion
    (let* ((inhibit-modification-hooks t)
	   (primary (yas/group-primary-field field-group))
	   (primary-overlay (yas/field-overlay primary))
	   (text (buffer-substring-no-properties (overlay-start primary-overlay)
						 (overlay-end primary-overlay))))
      (dolist (field (yas/group-fields field-group))
	(let* ((field-overlay (yas/field-overlay field))
	       (original-length (- (overlay-end field-overlay)
				   (overlay-start field-overlay))))
	  (unless (eq field-overlay primary-overlay)
	    (goto-char (overlay-start field-overlay))
	    (insert (yas/calculate-field-value field text))
	    (if (= (overlay-start field-overlay)
		   (overlay-end field-overlay))
		(move-overlay field-overlay
			      (overlay-start field-overlay)
			      (point))
	      (delete-char original-length))))))))
  
(defun yas/overlay-modification-hook (overlay after? beg end &optional length)
  "Modification hook for snippet field overlay."
  (when (and after? (not undo-in-progress))
    (yas/synchronize-fields (overlay-get overlay 'yas/group))))
(defun yas/overlay-insert-in-front-hook (overlay after? beg end &optional length)
  "Hook for snippet overlay when text is inserted in front of a snippet field."
  (when after?
    (let ((field-group (overlay-get overlay 'yas/group))
	  (inhibit-modification-hooks t))
      (when (not (overlay-get overlay 'yas/modified?))
	(overlay-put overlay 'yas/modified? t)
	(when (> (overlay-end overlay) end)
	  (save-excursion
	    (goto-char end)
	    (delete-char (- (overlay-end overlay) end)))))
     (yas/synchronize-fields field-group))))
(defun yas/overlay-maybe-insert-behind-hook (overlay after? beg end &optional length)
  "Insert behind hook sometimes doesn't get called. I don't know why.
So I add modification hook in the big overlay and try to detect `insert-behind'
event manually."
  (when (and after?
	     (= length 0)
	     (> end beg)
	     (null (yas/current-snippet-overlay beg))
	     (not (bobp)))
    (let ((field-overlay (yas/current-snippet-overlay (1- beg))))
      (if field-overlay
	  (when (= beg (overlay-end field-overlay))
	    (move-overlay field-overlay
			  (overlay-start field-overlay)
			  end)
	    (yas/synchronize-fields (overlay-get field-overlay 'yas/group)))
	(let ((snippet (yas/snippet-of-current-keymap))
	      (done nil))
	  (if snippet
	      (do* ((groups (yas/snippet-groups snippet) (cdr groups))
		    (group (car groups) (car groups)))
		  ((or (null groups)
		       done))
		(setq field-overlay (yas/field-overlay 
				     (yas/group-primary-field group)))
		(when (and (= (overlay-start field-overlay)
			      (overlay-end field-overlay))
			   (= beg
			      (overlay-start field-overlay)))
		  (move-overlay field-overlay beg end)
		  (yas/synchronize-fields group)
		  (setq done t)))))))))

(defun yas/undo-expand-snippet (start end key snippet)
  "Undo a snippet expansion. Delete the overlays. This undo can't be
redo-ed."
  (let ((undo (car buffer-undo-list)))
    (while (null undo)
      (setq buffer-undo-list (cdr buffer-undo-list))
      (setq undo (car buffer-undo-list)))
    ;; Remove this undo operation record
    (setq buffer-undo-list (cdr buffer-undo-list))
  (let ((inhibit-modification-hooks t)
	(buffer-undo-list t))
    (yas/exit-snippet snippet)
    (goto-char start)
    (delete-char (- end start))
    (insert key))))

(defun yas/expand-snippet (start end template)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (goto-char start)

  (let ((key (buffer-substring-no-properties start end))
	(original-undo-list buffer-undo-list)
	(inhibit-modification-hooks t)
	(length (- end start))
	(column (current-column)))
    (save-restriction
      (narrow-to-region start start)

      (setq buffer-undo-list t)
      (insert template)

      ;; Step 1: do necessary indent
      (when yas/indent-line
	(let* ((indent (if indent-tabs-mode
			   (concat (make-string (/ column tab-width) ?\t)
				   (make-string (% column tab-width) ?\ ))
			 (make-string column ?\ ))))
	  (goto-char (point-min))
	  (while (and (zerop (forward-line))
		      (= (current-column) 0))
	    (insert indent))))

      ;; Step 2: protect backslash and backquote
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)

      ;; Step 3: evaluate all backquotes
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`]*\\)`" nil t)
	(replace-match (yas/eval-string (match-string-no-properties 1))
		       t t))

      ;; Step 4: protect all escapes, including backslash and backquot
      ;; which may be produced in Step 3
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)
      (yas/replace-all "\\$" yas/escape-dollar)

      (let ((snippet (yas/make-snippet)))
	;; Step 5: Create fields
	(goto-char (point-min))
	(while (re-search-forward yas/field-regexp nil t)
	  (let ((number (or (match-string-no-properties 1)
			    (match-string-no-properties 2)))
		(transform nil)
		(value (match-string-no-properties 3)))
	    (when (eq (elt value 0) ?\$)
	      (setq transform (substring value 1))
	      (setq value nil))
	    (if (and number
		     (string= "0" number))
		(progn
		  (replace-match "")
		  (setf (yas/snippet-exit-marker snippet)
			(copy-marker (point) t)))
	      (yas/snippet-add-field
	       snippet
	       (yas/make-field
		(make-overlay (match-beginning 0) (match-end 0))
		(and number (string-to-number number))
		value
		transform)))))

	;; Step 6: Sort and link each field group
	(setf (yas/snippet-groups snippet)
	      (sort (yas/snippet-groups snippet)
		    '(lambda (group1 group2)
		       (yas/snippet-field-compare
			(yas/group-primary-field group1)
			(yas/group-primary-field group2)))))
	(let ((prev nil))
	  (dolist (group (yas/snippet-groups snippet))
	    (setf (yas/group-prev group) prev)
	    (when prev
	      (setf (yas/group-next prev) group))
	    (setq prev group)))

	;; Step 7: Create keymap overlay for snippet
	(let ((overlay (make-overlay (point-min)
				     (point-max)
				     nil
				     nil
				     t)))
	  (overlay-put overlay 
		       'modification-hooks
		       yas/keymap-overlay-modification-hooks)
	  (overlay-put overlay 
		       'insert-behind-hooks
		       yas/keymap-overlay-modification-hooks)
	  (overlay-put overlay 'keymap yas/keymap)
	  (overlay-put overlay 'yas/snippet-reference snippet)
	  (setf (yas/snippet-overlay snippet) overlay))
	
	;; Step 8: Replace fields with default values
	(dolist (group (yas/snippet-groups snippet))
	  (let ((value (yas/group-value group)))
	    (dolist (field (yas/group-fields group))
	      (let* ((overlay (yas/field-overlay field))
		     (start (overlay-start overlay))
		     (end (overlay-end overlay))
		     (length (- end start)))
		(goto-char start)
		(insert (yas/calculate-field-value field value))
		(delete-char length)))))

	;; Step 9: restore all escape characters
	(yas/replace-all yas/escape-dollar "$")
	(yas/replace-all yas/escape-backquote "`")
	(yas/replace-all yas/escape-backslash "\\")

	;; Step 10: Set up properties of overlays
	(dolist (group (yas/snippet-groups snippet))
	  (let ((overlay (yas/field-overlay
			  (yas/group-primary-field group))))
	    (overlay-put overlay 'yas/snippet snippet)
	    (overlay-put overlay 'yas/group group)
	    (overlay-put overlay 'yas/modified? nil)
	    (overlay-put overlay 'modification-hooks yas/overlay-modification-hooks)
	    (overlay-put overlay 'insert-in-front-hooks yas/overlay-insert-in-front-hooks)
	    (overlay-put overlay 'face 'yas/field-highlight-face)
	    (dolist (field (yas/group-fields group))
	      (unless (equal overlay (yas/field-overlay field))
		(overlay-put (yas/field-overlay field)
			     'face 
			     'yas/mirror-highlight-face)))))

	;; Step 11: move to end and make sure exit-marker exist
	(goto-char (point-max))
	(unless (yas/snippet-exit-marker snippet)
	  (setf (yas/snippet-exit-marker snippet) (copy-marker (point) t)))

	;; Step 12: Construct undo information
	(unless (eq original-undo-list t)
	  (add-to-list 'original-undo-list
		       `(apply yas/undo-expand-snippet
			       ,(point-min)
			       ,(point-max)
			       ,key
			       ,snippet)))

	;; Step 13: remove the trigger key
	(widen)
	(delete-char length)

	(setq buffer-undo-list original-undo-list)

	;; Step 14: place the cursor at a proper place
	(let ((groups (yas/snippet-groups snippet))
	      (exit-marker (yas/snippet-exit-marker snippet)))
	  (if groups
	      (goto-char (overlay-start 
			  (yas/field-overlay
			   (yas/group-primary-field
			    (car groups)))))
	    ;; no need to call exit-snippet, since no overlay created.
	    (yas/exit-snippet snippet)))))))

(defun yas/current-snippet-overlay (&optional point)
  "Get the most proper overlay which is belongs to a snippet."
  (let ((point (or point (point)))
	(snippet-overlay nil))
    (dolist (overlay (overlays-at point))
      (when (overlay-get overlay 'yas/snippet)
	(if (null snippet-overlay)
	    (setq snippet-overlay overlay)
	  (when (> (yas/snippet-id (overlay-get overlay 'yas/snippet))
		   (yas/snippet-id (overlay-get snippet-overlay 'yas/snippet)))
	    (setq snippet-overlay overlay)))))
    snippet-overlay))

(defun yas/snippet-of-current-keymap (&optional point)
  "Get the snippet holding the snippet keymap under POINT."
  (let ((point (or point (point)))
	(keymap-snippet nil)
	(snippet nil))
    (dolist (overlay (overlays-at point))
      (setq snippet (overlay-get overlay 'yas/snippet-reference))
      (when snippet
	(if (null keymap-snippet)
	    (setq keymap-snippet snippet)
	  (when (> (yas/snippet-id snippet)
		   (yas/snippet-id keymap-snippet))
	    (setq keymap-snippet snippet)))))
    keymap-snippet))

(defun yas/current-overlay-for-navigation ()
  "Get current overlay for navigation. Might be overlay at current or previous point."
  (let ((overlay1 (yas/current-snippet-overlay))
	(overlay2 (if (bobp)
		      nil
		    (yas/current-snippet-overlay (- (point) 1)))))
    (if (null overlay1)
	overlay2
      (if (or (null overlay2)
	      (eq (overlay-get overlay1 'yas/snippet) 
		  (overlay-get overlay2 'yas/snippet)))
	  overlay1
	(if (> (yas/snippet-id (overlay-get overlay2 'yas/snippet))
	       (yas/snippet-id (overlay-get overlay1 'yas/snippet)))
	    overlay2
	  overlay1)))))

(defun yas/navigate-group (group next?)
  "Go to next of previous field group. Exit snippet if none."
  (let ((target (if next?
		    (yas/group-next group)
		  (yas/group-prev group))))
    (if target
	(goto-char (overlay-start
		    (yas/field-overlay
		     (yas/group-primary-field target))))
      (yas/exit-snippet (yas/group-snippet group)))))

(defun yas/parse-template ()
  "Parse the template in the current buffer.
If the buffer contains a line of \"# --\" then the contents
above this line are ignored. Variables can be set above this
line through the syntax:

#name : value

Currently only the \"name\" variable is recognized. Here's 
an example:

#name: #include \"...\"
# --
#include \"$1\""
  (goto-char (point-min))
  (let (template name bound)
    (if (re-search-forward "^# --\n" nil t)
	(progn (setq template 
		     (buffer-substring-no-properties (point) 
						     (point-max)))
	       (setq bound (point))
	       (goto-char (point-min))
	       (while (re-search-forward "^#\\([^ ]+\\) *: *\\(.*\\)$" bound t)
		 (when (string= "name" (match-string-no-properties 1))
		   (setq name (match-string-no-properties 2)))))
      (setq template
	    (buffer-substring-no-properties (point-min) (point-max))))
    (list template name)))

(defun yas/directory-files (directory file?)
  "Return directory files or subdirectories in full path."
  (remove-if (lambda (file)
	       (or (string-match "^\\."
				 (file-name-nondirectory file))
		   (if file?
		       (file-directory-p file)
		     (not (file-directory-p file)))))
	     (directory-files directory t)))

(defun yas/make-menu-binding (template)
  (lexical-let ((template template))
    (lambda ()
      (interactive)
      (yas/expand-snippet (point) 
			  (point)
			  template))))

(defun yas/modify-alist (alist key value)
  "Modify ALIST to map KEY to VALUE. return the new alist."
  (let ((pair (assoc key alist)))
    (if (null pair)
	(cons (cons key value)
	      alist)
      (setcdr pair value)
      alist)))

(defun yas/fake-keymap-for-popup (templates)
  "Create a fake keymap for popup menu usage."
  (cons 'keymap 
	(mapcar (lambda (pair)
		  (let* ((template (cdr pair))
			 (name (yas/template-name template))
			 (content (yas/template-content template)))
		    (list content 'menu-item name t)))
		templates)))

(defun yas/point-to-coord (&optional point)
  "Get the xoffset/yoffset information of POINT.
If POINT is not given, default is to current point.
If `posn-at-point' is not available (like in Emacs 21.3),
t is returned simply."
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (or point (point))))))
	(list (list (+ (car x-y) 10)
		    (+ (cdr x-y) 20))
	      (selected-window)))
    t))
 
(defun yas/x-popup-menu-for-template (templates)
  "Show a popup menu listing templates to let the user select one."
  (car (x-popup-menu (yas/point-to-coord)
		     (yas/fake-keymap-for-popup templates))))
(defun yas/text-popup-for-template (templates)
  "Can't display popup menu in text mode. Just select the first one."
  (yas/template-content (cdar templates)))
(defun yas/dropdown-list-popup-for-template (templates)
  "Use dropdown-list.el to popup for templates. Better than the 
default \"select first\" behavior of `yas/text-popup-for-template'.
You can also use this in window-system.

NOTE: You need to download and install dropdown-list.el to use this."
  (if (fboundp 'dropdown-list)
      (let ((n (dropdown-list (mapcar (lambda (i)
					(yas/template-name
					 (cdr i)))
				      templates))))
	(if n
	    (yas/template-content
	     (cdr (nth n templates)))
	  nil))
    (error "Please download and install dropdown-list.el to use this")))

(defun yas/popup-for-template (templates)

  (if window-system
      (funcall yas/window-system-popup-function templates)
    (funcall yas/text-popup-function templates)))

(defun yas/load-directory-1 (directory &optional parent)
  "Really do the job of loading snippets from a directory 
hierarchy."
  (let ((mode-sym (intern (file-name-nondirectory directory)))
	(snippets nil))
    (with-temp-buffer
      (dolist (file (yas/directory-files directory t))
	(when (file-readable-p file)
	  (insert-file-contents file nil nil nil t)
	  (push (cons (file-name-nondirectory file)
		      (yas/parse-template))
		snippets))))
    (yas/define-snippets mode-sym
			 snippets
			 parent)
    (dolist (subdir (yas/directory-files directory nil))
      (yas/load-directory-1 subdir mode-sym))))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
	  (replace-regexp-in-string "[\\\"]"
				    "\\\\\\&"
				    string
				    t)
	  "\""))

(defun yas/compile-bundle (yasnippet yasnippet-bundle snippet-roots)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.
SNIPPET-ROOTS is a list of root directories that contains the snippets
definition. YASNIPPET is the yasnippet.el file path. YASNIPPET-BUNDLE
is the output file of the compile result. Here's an example:

 (yas/compile-bundle \"~/.emacs.d/plugins/yasnippet/yasnippet.el\"
                     \"~/.emacs.d/plugins/yasnippet-bundle.el\"
                     '(\"~/.emacs.d/plugins/yasnippet/snippets\"))"
  (let ((dirs (or (and (listp snippet-roots) snippet-roots)
		  (list snippet-roots)))
	(bundle-buffer nil))
    (with-temp-buffer
      (setq bundle-buffer (current-buffer))
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert "(yas/initialize)\n")
      (flet ((yas/define-snippets 
	      (mode snippets &optional parent)
	      (with-current-buffer bundle-buffer
		(insert ";;; snippets for " (symbol-name mode) "\n")
		(insert "(yas/define-snippets '" (symbol-name mode) "\n")
		(insert "'(\n")
		(dolist (snippet snippets)
		  (insert "  (" 
			  (yas/quote-string (car snippet))
			  (yas/quote-string (cadr snippet))
			  (if (caddr snippet)
			      (yas/quote-string (caddr snippet))
			    "nil")
			  ")\n"))
		(insert "  )\n")
		(insert (if parent
			    (concat "'" (symbol-name parent))
			  "nil")
			")\n\n"))))
	    (dolist (dir dirs)
	      (dolist (subdir (yas/directory-files dir nil))
		(yas/load-directory-1 subdir nil))))
      (insert "(provide '"
	      (file-name-nondirectory
	       (file-name-sans-extension
		yasnippet-bundle))
	      ")\n")
      (setq buffer-file-name yasnippet-bundle)
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
		   yas/version
		   ") -- pluskid <pluskid@gmail.com>")))
(defun yas/reload-all ()
  "Reload all snippets."
  (interactive)
  (if yas/root-directory
      (yas/load-directory-1 yas/root-directory)
    (call-interactively 'yas/load-directory))
  (message "done."))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.
Below the top-level directory, each directory is a mode
name. And under each subdirectory, each file is a definition
of a snippet. The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (unless yas/root-directory
    (setq yas/root-directory directory))
  (dolist (dir (yas/directory-files directory nil))
    (yas/load-directory-1 dir))
  (when (interactive-p)
    (message "done.")))

(defun yas/initialize ()
  "Do necessary initialization."
  (add-hook 'after-change-major-mode-hook
	    'yas/minor-mode-on)
  (dolist (hook yas/extra-mode-hooks)
    (add-hook hook
	      'yas/minor-mode-on))
  (when yas/use-menu
    (define-key-after 
      (lookup-key global-map [menu-bar])
      [yasnippet]
      (cons "YASnippet" yas/menu-keymap)
      'buffer)))

(defun yas/define-snippets (mode snippets &optional parent-mode)
  "Define snippets for MODE. SNIPPETS is a list of
snippet definition, of the following form:
 (KEY TEMPLATE NAME)
or the NAME may be omitted. The optional 3rd parameter
can be used to specify the parent mode of MODE. That is,
when looking a snippet in MODE failed, it can refer to
its parent mode. The PARENT-MODE may not need to be a 
real mode."
  (let ((snippet-table (yas/snippet-table mode))
	(parent-table (if parent-mode
			  (yas/snippet-table parent-mode)
			nil))
	(keymap (if yas/use-menu
		    (yas/menu-keymap-for-mode mode)
		  nil)))
    (when parent-table
      (setf (yas/snippet-table-parent snippet-table)
	    parent-table)
      (when yas/use-menu
	(define-key keymap (vector 'parent-mode)
	  `(menu-item "parent mode"
		      ,(yas/menu-keymap-for-mode parent-mode)))))
    (when (and yas/use-menu
	       (yas/real-mode? mode))
      (define-key yas/menu-keymap (vector mode)
	`(menu-item ,(symbol-name mode) ,keymap)))
    (dolist (snippet snippets)
      (let* ((full-key (car snippet))
	     (key (file-name-sans-extension full-key))
	     (name (caddr snippet))
	     (template (yas/make-template (cadr snippet)
					  (or name key))))
	(yas/snippet-table-store snippet-table
				 full-key
				 key
				 template)
	(when yas/use-menu
	  (define-key keymap (vector (make-symbol full-key))
	    `(menu-item ,(yas/template-name template)
			,(yas/make-menu-binding (yas/template-content template))
			:keys ,(concat key yas/trigger-symbol))))))))

(defun yas/set-mode-parent (mode parent)
  "Set parent mode of MODE to PARENT."
  (setf (yas/snippet-table-parent
	 (yas/snippet-table mode))
	(yas/snippet-table parent))
  (when yas/use-menu
    (define-key (yas/menu-keymap-for-mode mode) (vector 'parent-mode)
      `(menu-item "parent mode"
		  ,(yas/menu-keymap-for-mode parent)))))

(defun yas/define (mode key template &optional name)
  "Define a snippet. Expanding KEY into TEMPLATE.
NAME is a description to this template. Also update
the menu if `yas/use-menu' is `t'."
  (yas/define-snippets mode
		       (list (list key template name))))
    

(defun yas/expand ()
  "Expand a snippet."
  (interactive)
  (multiple-value-bind (key start end) (yas/current-key)
    (let ((templates (yas/snippet-table-fetch (yas/current-snippet-table)
					      key)))
      (if templates
	  (let ((template (if (null (cdr templates)) ; only 1 template
			      (yas/template-content (cdar templates))
			    (yas/popup-for-template templates))))
	    (when template
	      (yas/expand-snippet start end template)))
	(let* ((yas/minor-mode nil)
	       (command (key-binding yas/trigger-key)))
	  (when (commandp command)
	    (call-interactively command)))))))

(defun yas/next-field-group ()
  "Navigate to next field group. If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-overlay-for-navigation)))
    (if overlay
	(yas/navigate-group (overlay-get overlay 'yas/group) t)
      (let ((snippet (yas/snippet-of-current-keymap))
	    (done nil))
	(if snippet
	    (do* ((groups (yas/snippet-groups snippet) (cdr groups))
		  (group (car groups) (car groups)))
		((or (null groups)
		     done)
		 (unless done 
		   (let* ((overlay (yas/snippet-overlay snippet))
			  (keymap (overlay-get overlay 'keymap))
			  (command nil))
		     (overlay-put overlay 'keymap nil)
		     (setq command (key-binding yas/next-field-key))
		     (when (commandp command)
		       (call-interactively command))
		     (overlay-put overlay 'keymap keymap))))
	      (when (= (point)
		       (overlay-start
			(yas/field-overlay
			 (yas/group-primary-field group))))
		(setq done t)
		(yas/navigate-group group t))))))))

(defun yas/prev-field-group ()
  "Navigate to prev field group. If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-overlay-for-navigation)))
    (if overlay
	(yas/navigate-group (overlay-get overlay 'yas/group) nil)
      (let ((snippet (yas/snippet-of-current-keymap))
	    (done nil))
	(if snippet
	  (do* ((groups (yas/snippet-groups snippet) (cdr groups))
		(group (car groups) (car groups)))
	      ((or (null groups)
		   done)
	       (unless done (message "Not in a snippet field.")))
	    (when (= (point)
		     (overlay-start
		      (yas/field-overlay
		       (yas/group-primary-field group))))
	      (setq done t)
	      (yas/navigate-group group nil)))
	  (message "Not in a snippet field."))))))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET and delete the snippet."
  (interactive)
  (let ((overlay (yas/snippet-overlay snippet)))
    (let ((yas/snippet-beg (overlay-start overlay))
	  (yas/snippet-end (overlay-end overlay)))
      (goto-char (yas/snippet-exit-marker snippet))
      (delete-overlay overlay)
      (dolist (group (yas/snippet-groups snippet))
	(dolist (field (yas/group-fields group))
	  (delete-overlay (yas/field-overlay field))))

      (run-hooks 'yas/after-exit-snippet-hook))))

(provide 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey patching for other functions that's causing
;; problems to yasnippet. For details on why I patch
;; those functions, refer to
;;   http://code.google.com/p/yasnippet/wiki/MonkeyPatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-neutralize-syntax-in-CPP
  (around yas-mp/c-neutralize-syntax-in-CPP activate)
  "Adviced `c-neutralize-syntax-in-CPP' to properly 
handle the end-of-buffer error fired in it by calling
`forward-char' at the end of buffer."
  (condition-case err
      ad-do-it
    (error (message (error-message-string err)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas/initialize)
;;; snippets for text-mode
(yas/define-snippets 'text-mode
'(
  ("time""`(current-time-string)`""(current time)")
  ("email""`user-mail-address`""(user's email)")
  )
nil)

;;; snippets for cc-mode
(yas/define-snippets 'cc-mode
'(
  ("struct""struct ${1:name}
{
    $0
};""struct ... { ... }")
  ("once""#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
#define $1

$0

#endif /* $1 */""#ifndef XXX; #define XXX; #endif")
  ("main""int main(int argc, char const *argv)
{
    $0
    return 0;
}""int main(argc, argv) { ... }")
  ("inc.1""#include <$1>
""#include <...>")
  ("inc""#include \"$1\"
""#include \"...\"")
  ("if""if (${1:condition})
{
    $0
}""if (...) { ... }")
  ("for""for (${1:int i = 0}; ${2:i < N}; ${3:++i})
{
    $0
}""for (...; ...; ...) { ... }")
  ("do""do
{
    $0
} while (${1:condition});""do { ... } while (...)")
  )
'text-mode)

;;; snippets for c++-mode
(yas/define-snippets 'c++-mode
'(
  ("using""using namespace ${std};
$0""using namespace ... ")
  ("class""class ${1:Name}
{
public:
    $1($2);
    virtual ~$1();
};""class ... { ... }")
  ("beginend""${1:v}.begin(), $1.end""v.begin(), v.end()")
  )
'cc-mode)

;;; snippets for c-mode
(yas/define-snippets 'c-mode
'(
  ("fopen""FILE *${fp} = fopen(${\"file\"}, \"${r}\");
""FILE *fp = fopen(..., ...);")
  )
'cc-mode)

;;; snippets for css-mode
(yas/define-snippets 'css-mode
'(
  ("border""border: ${1:1px} ${2:solid} #${3:999};""border size style color")
  ("background.1""background-image: url($1);""background-image: ...")
  ("background""background-color: #${1:DDD};""background-color: ...")
  )
'text-mode)

;;; snippets for html-mode
(yas/define-snippets 'html-mode
'(
  ("doctype.xhtml1_transitional""<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">""DocType XHTML 1.0 Transitional")
  ("doctype.xhtml1_strict""<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">""DocType XHTML 1.0 Strict")
  ("doctype.xhtml1_1""<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">""DocType XHTML 1.1")
  ("doctype.xhml1""<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">""DocType XHTML 1.0 frameset")
  ("doctype""<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">""Doctype HTML 4.01 Strict")
  ("div""<div$1>$0</div>""<div...>...</div>")
  )
'text-mode)

;;; snippets for objc-mode
(yas/define-snippets 'objc-mode
'(
  ("prop""- (${1:id})${2:foo}
{
    return $2;
}

- (void)set${2:$(capitalize text)}:($1)aValue
{
    [$2 autorelease];
    $2 = [aValue retain];
}
$0""foo { ... } ; setFoo { ... }")
  )
'text-mode)

;;; snippets for perl-mode
(yas/define-snippets 'perl-mode
'(
  ("xwhile""${1:expression} while ${2:condition};""... while ...")
  ("xunless""${1:expression} unless ${2:condition}""... unless ...")
  ("xif""${1:expression} if ${2:condition}""... if ...")
  ("xfore""${1:expression} foreach @${2:array};""... foreach ...")
  ("while""while ($1) {
    $0
}""while (...) { ... }")
  ("unless""unless ($1) {
    $0
}""unless (...) { ... }")
  ("sub""sub ${1:function_name} {
    $0
}""sub ... { ... }")
  ("ifee""if ($1) {
	${2:# body...}
} elsif ($3) {
	${4:# elsif...}
} else {
	${5:# else...}
}""if, elsif, else ...")
  ("ife""if ($1) {
    $2
} else {
    $3
}""if (...) { ... } else { ... }")
  ("if""if ($1) {
    $0
}""if (...) { ... }")
  ("fore""foreach my \\$${1:x} (@${2:array}) {
    ${3:# body...}
}""foreach ... { ... }")
  ("for""for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {
    ${3:# body...}
}""for (...) { ... }")
  ("eval""eval {
    ${1:# do something risky...}
};
if (\\$@) {
    ${2:# handle failure...}
}""eval { ... } if ($@) { ... }")
  )
'text-mode)

;;; snippets for cperl-mode
(yas/define-snippets 'cperl-mode
'(
  )
'perl-mode)

;;; snippets for python-mode
(yas/define-snippets 'python-mode
'(
  ("while""while ${condition}:
    $0""while ... : ...")
  ("ifmain""if __name__ == '__main__':
    $0""if __name__ == '__main__': ...")
  ("for""for ${var} in ${collection}:
    $0""for ... in ... : ...")
  ("def""def ${1:fname}(${self}):
    \"docstring for $1\"
    ${pass}$0""def ... :")
  ("class""class ${1:cname}(${2:object}):
    \"docstring for $1\"
    def __init__(self, ${3:arg}):
        self.$3 = $3
        $0""class ... :")
  ("__""__${init}__""__...__")
  )
'text-mode)

;;; snippets for rst-mode
(yas/define-snippets 'rst-mode
'(
  ("title""${1:$(make-string (string-width text) ?\\=)}
${1:Title}
${1:$(make-string (string-width text) ?\\=)}

$0""Document title")
  ("section""${1:Section}
${1:$(make-string (string-width text) ?\\-)}

$0""Section title")
  ("chapter""${1:Chapter}
${1:$(make-string (string-width text) ?\\=)}

$0""Chapter title")
  )
'text-mode)

;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
'(
  ("zip""zip(${enums}) { |${row}| $0 }""zip(...) { |...| ... }")
  ("y"":yields: $0"":yields: arguments (rdoc)")
  ("w""attr_writer :${attr_names}""attr_writer ...")
  ("select""select { |${1:element}| $0 }""select { |...| ... }")
  ("rw""attr_accessor :{attr_names}""attr_accessor ...")
  ("rreq""require File.join(File.dirname(__FILE__), $0)""require File.join(File.dirname(__FILE__), ...)")
  ("req""require \"$0\"""require \"...\"")
  ("reject""reject { |${1:element}| $0 }""reject { |...| ... }")
  ("rb""#!/usr/bin/ruby -wKU
""/usr/bin/ruby -wKU")
  ("r""attr_reader :${attr_names}""attr_reader ...")
  ("mm""def method_missing(method, *args)
  $0
end""def method_missing ... end")
  ("inject""inject(${1:0}) { |${2:injection}, ${3:element}| $0 }""inject(...) { |...| ... }")
  ("ife""if ${1:condition}
  $2
else
  $3
end""if ... else ... end")
  ("if""if ${1:condition}
  $0
end""if ... end")
  ("forin""for ${1:element} in ${2:collection}
  $0
end""for ... in ...; ... end")
  ("eawi""each_with_index { |${e}, ${i}| $0 }""each_with_index { |e, i| ... }")
  ("eav""each_value { |${val}| $0 }""each_value { |val| ... }")
  ("eai""each_index { |${i}| $0 }""each_index { |i| ... }")
  ("eac""each_cons(${1:2}) { |${group}| $0 }""each_cons(...) { |...| ... }")
  ("ea""each { |${e}| $0 }""each { |...| ... }")
  ("det""detect { |${e}| $0 }""detect { |...| ... }")
  ("deli""delete_if { |${e} $0 }""delete_if { |...| ... }")
  ("dee""Marshal.load(Marshal.dump($0))""deep_copy(...)")
  ("collect""collect { |${e}| $0 }""collect { |...| ... }")
  ("cls""class ${Name}
  $0
end""class ... end")
  ("classify""classify { |${e}| $0 }""classify { |...| ... }")
  ("cla""class << ${self}
  $0
end""class << self ... end")
  ("case""case ${1:object}
when ${2:condition}
  $0
end""case ... end")
  ("bm""Benchmark.bmbm(${1:10}) do |x|
  $0
end""Benchmark.bmbm(...) do ... end")
  ("app""if __FILE__ == $PROGRAM_NAME
  $0
end""if __FILE__ == $PROGRAM_NAME ... end")
  ("any""any? { |${e}| $0 }""any? { |...| ... }")
  ("am""alias_method :${new_name}, :${old_name}""alias_method new, old")
  ("all""all? { |${e}| $0 }""all? { |...| ... }")
  ("Comp""include Comparable

def <=> other
  $0
end""include Comparable; def <=> ... end")
  ("=b""=begin rdoc
  $0
=end"nil)
  ("#""# => ""# =>")
  )
'text-mode)

;;; snippets for str-mode
(yas/define-snippets 'str-mode
'(
  ("str""*** Development Notes - Travis Shultz - ${Date} ***
$0
*** ***

*** begin Software Impact Analysis - Travis Shultz - ${Date} ***
(Performed at level appropriate for software change)

1) Traceability:

2) Data Flow: 

3) Control Flow: 

4) Input/Output Analysis: 

5) Operational Characteristics:

6) Partitioning Analysis: 

(copy to Verification Requirements field)
7) Suggested System Testing based on SW changes and potential for unintentional downstream effects: 

*** end Software Impact Analysis ***

*** Development Notes - Travis Shultz - ${Date} ***
CLP SDD
*** ***

*** Development Notes - Travis Shultz - ${Date} ***
SM SDD
*** ***

*** begin Source Code Peer Review - date - name ***
This is the source code peer review checklist format.
This format is followed below for each source code file.

Module Name/PVCS Revision/Processor(s) -
1) Complies with Requirements?
2) Complies with Software Architecture?
3) Verifiable?
4) Conforms to standards?
5) Traceable?
6) Accurate and Consistent?
7) Pass?

filename/rev/processor -
1) Yes/No/NA
2) Yes/No/NA
3) Yes/No/NA
4) Yes/No/NA
5) Yes/No/NA
6) Yes/No/NA
7) Yes/No/NA


Lead implementation practice preferences 
1) const is used for all externally instantiated constants (and all variables possible)
2) READ_ONLY should be used only and for all locally instantiated constant declarations
3) STATIC is obsolete and should be phased out, it should always be static

Before selecting PASSED, ensure the SDDs are Verified 
Make sure the checklist is completed: N:\nt4k\software\reviews\checklists 

*** end Source Code Peer Review ***

=== Hints and tips for Change Impact Analysis ===
Only modified baseline code needs analysis. New code implementing new requirements requires analysis only where we get it slipped in and its impact on the baseline.

1) Traceability
The main point of this analysis is to determine if the code we changed impacts any requirements that have have not otherwise been considered. We must discuss if the changed functions trace from requirements that were not modified.
To do this, we look at the CLP<-In or SM<-In or VDP<-In trace trees in Requisite. My personal shorthand goes something like this (yours does not have to look like this, but it needs this information in some form)
Begin with a statement like:
Trace is accurate and complete (or corrected, or updated)
SRS### -> CLP###/SM### (if CLP and SM have the same function/file) OR
SRS### 
-> CLP##1-CLP#33 (if there's a whole mess of consecutively numbered tags) OR
SRS### 
-> CLP###
-> CLP###
-> CLP###
-> CLP###

CLP### 
^- SRS###
^- SRS###
^- SRS###
^- SRS###

The trace down from requirements ensures we have implemented per requirement changes.
The trace up to requirements ensures we have not unintentionally impacted something else.

I might discuss additional tracing is new functions have been added, or just include them above.
It is also important to note if code has been deleted and all requirements it traced to before deleting.
We must discuss if the changed functions trace from requirements that were not modified.

2) Data Flow:
The main point of this section is to determine if we modified any global or passed data that is referenced by functions that implement requirements not otherwise considered. grep is our friend. I use CodeWright.
- Use you software development tools to locate all references to global variables whose calculations have been modified.
- Find invocations of functions whose return/passed data has been modified.
- Find the referencing functions in the SDDs and figure out where they trace from.
- itemize these requirements.

3) Control Flow:
? have I added or deleted any function to the call tree on the processor
? have i altered something like BIT sequencing or the power-up sequence
One of the things we're looking for is whether changes to the call tree affect the sequence of calculations. to quote the SVP:

Inputs are still available to the function(s) in the current cycle. For example, the function was not moved to a point in the program where it is called before the inputs to it are set.

Outputs are still available to the users of the data in the current cycle. For example, the function was not moved to a point in the program where it is called after another function which uses its data.

4) Input/Output
? modified any data input/output to/from the HC
? deleted any data input/output to/from the HC
? references to modified/deleted data to/from the HC

5) Operational Characteristics 
Quoting from the SVP is sufficient:
evaluation of changes to gains, filters, limits, data validation, interrupt and exception handling, and fault mitigation to assure that there are no adverse effects. Assure filters, timers, initialization, etc. are not impacted. For example, a function with a filter was previously called every cycle and now is called only in Primary mode. 

6) Partitioning Analysis
Quoting from the SVP is sufficient:
Verify that data protection and communication mechanisms are implemented correctly or are not bypassed. For example, SRAM transfer, NVRAM transfer, additions to the display list, etc. 

7) Suggested System Testing
The main point of this section is to highlight items from the analysis that may be overlooked by systems analysis of the requirements.
""*** Development Notes ...")
  ("peer""*** begin Source Code Peer Review - Travis Shultz - ${Date} ***
This is the source code peer review checklist format.
This format is followed below for each source code file.

Module Name/PVCS Revision/Processor(s) -
1) Complies with Requirements?
2) Complies with Software Architecture?
3) Verifiable?
4) Conforms to standards?
5) Traceable?
6) Accurate and Consistent?
7) Pass?

filename/rev/processor -
1) Yes/No/NA
2) Yes/No/NA
3) Yes/No/NA
4) Yes/No/NA
5) Yes/No/NA
6) Yes/No/NA
7) Yes/No/NA


Lead implementation practice preferences 
1) const is used for all externally instantiated constants (and all variables possible)
2) READ_ONLY should be used only and for all locally instantiated constant declarations
3) STATIC is obsolete and should be phased out, it should always be static

Before selecting PASSED, ensure the SDDs are Verified 
Make sure the checklist is completed: N:\\nt4k Software\\reviews\\checklists 

$0 (/ r)
1)
2)
3)
4)
5)
6)
7)

*** end Source Code Peer Review ***
""*** Peer Review ...")
  )
'text-mode)


(provide 'yasnippet-bundle)
