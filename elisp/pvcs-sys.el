;;; pvcs.el -- simple pvcs minor mode 
;;; Copyright (c) 1997,1998 by Stefan Braun <brau...@ibm.net> 
;;; This file is NOT part of GNU Emacs. 
;;; Version: $Id: pvcs.el,v 1.19 1998/05/30 16:05:58 SB Exp $   
;;; This program is based on the following package : 
;; ---------------------------------------------------------- 
;; cvs.el --- Light cvs support for emacs (ediff + mode line) 
;; 
;; Copyright (C) 1995, Frederic Lepied <f...@sugix.frmug.fr.net> 
;; 
;; Author: Frederic Lepied <f...@sugix.frmug.fr.net> 
;; Version: Id: cvs.el,v 1.9 1995/09/11 20:59:32 fred Exp 
;; Keywords: cvs ediff mode-line 
;; 
;; LCD Archive Entry: 
;; cvs|Frederic Lepied|f...@sugix.frmug.fr.net| 
;; Light cvs support for emacs (ediff + mode line).| 
;; 11-Sep-1995|1.9|~/modes/cvs.el.gz| 
;; ---------------------------------------------------------- 
;; Purpose of this package: 
;;   1. Display PVCS revision in mode line. 
;;   2. Compare file changes between PVCS revision using ediff. 
;;   3. Some keystrokes and menu entries to execute get, commit, 
;;      unlock, log and other commands. 
;;   4. Simple interface to PVCS commit/put command. 
;; 
;; Known deficiencies: 
;;   1. The actual revision of a file is extracted out of the file itself. 
;;      So there has to be the Revision keyword of PVCS in every file. 
;;      I don't know another way to get the revision of a file. 
;;      Most commands will work w/o the current revision too. pvcs-diff and 
;;      pvcs-unlock need it. 
;;   2. The ediff commands need to check out a revision in a file other 
;;      than the default workname. This is accomplished with the -XO option. 
;;      This option isn't available in newer PVCS versions (>= 5.0) anymore. 
;; 
;; Installation: 
;;   Put pvcs.el in a directory in your load-path and byte compile it. 
;;   then put (require 'pvcs) in your .emacs or in site-start.el 
;; 
;;========================================================================= ==== 
;; dependencies 
;;========================================================================= ==== 
;;(require 'string)  ; from elib, need this for string-split 
(require 'easymenu) 
;;========================================================================= ==== 
(defvar pvcs:version "$Id: pvcs.el,v 1.19 1998/05/30 16:05:58 SB Exp $" 
  "Version number of pvcs.el. To communicate with bug report") 
;;========================================================================= ==== 
(defvar pvcs:current-revision  nil 
  "Stores the PVCS revision number of the file") 
(make-variable-buffer-local 'pvcs:current-revision) 
;;========================================================================= ==== 
(defvar pvcs-temp-dir (or (getenv "TMPDIR") 
                          (getenv "TMP") 
                          (getenv "TEMP")) 
  "* if non nil, pvcs-temp-dir is the directory where to extract 
versions.") 
;;========================================================================= ==== 
(defvar pvcs-mode-hooks nil 
  "* Hooks run when pvcs-mode is initialized") 
;;========================================================================= ==== 
(defvar pvcs-commit-hooks nil 
  "* Hooks run entering commit buffer") 
;;========================================================================= ==== 
(defvar pvcs-before-commit-hooks nil 
  "* Hooks run before commiting") 
;;========================================================================= ==== 
(defvar pvcs-after-commit-hooks nil 
  "* Hooks run after commiting") 
;;========================================================================= ==== 
(defvar pvcs-add-hooks nil 
  "* Hooks run after adding a file into PVCS with pvcs-add") 
;;========================================================================= ==== 
(defvar pvcs-regex-rev "^.*Revision:[ \t]+\\([0-9]+.[0-9]+[0-9\\.]*\\)[ \t]+" 
  "* Regular expression used for the extraction of the revision number.") 
;;========================================================================= ==== 
(defvar pvcs-file-option "-M@" 
  "PVCS option to read log from a file.") 
;;========================================================================= ==== 
(defvar pvcs-command-path nil 
  "* Path to pvcs commands if needed (with trailing '/').") 
;;========================================================================= ==== 
(defvar pvcs-old-command-path "" 
  "* Path to pre 5 version of pvcs commands if needed (with trailing '/').") 
;;========================================================================= ==== 
(defvar pvcs-vcs-cfg-option "-C" 
  "* PVCS option to use a special vcs.cfg file.") 
;;========================================================================= ==== 
(defvar pvcs-vcs-cfg nil 
  "* special pvcs configuration file instead of vcs.cfg.") 
;;========================================================================= ==== 
(defvar pvcs-commit-command (concat pvcs-command-path "put") 
  "Command to commit a file") 
;;========================================================================= ==== 
(defvar pvcs-commit-options (list "-U" "-Y") 
  "* options used with pvcs-commit-command.") 
;;========================================================================= ==== 
(defvar pvcs-log-command (concat pvcs-command-path "vlog") 
  "Command to get log info of the actual file") 
;;========================================================================= ==== 
(defvar pvcs-checkout-command (concat pvcs-command-path "get") 
  "Command to check a file out of the archive") 
;;========================================================================= ==== 
(defvar pvcs-checkout-options (list "-L" "-Y") 
  "* options used when checking out a file.") 
;;========================================================================= ==== 
(defvar pvcs-diff-command (concat pvcs-command-path "vdiff") 
  "Command to compare two revisions of a file.") 
;;========================================================================= ==== 
(defvar pvcs-diff-options (list "-A") 
  "* options used when comparing two revisions.") 
;;========================================================================= ==== 
(defvar pvcs-vcs-command (concat pvcs-command-path "vcs") 
  "Central command for misc administrative tasks.") 
;;========================================================================= ==== 
(defvar pvcs-locks-command (concat pvcs-command-path "vlog") 
  "Command to query locked files.") 
;;========================================================================= ==== 
(defvar pvcs-locks-options (list "-BL") 
  "* options used to query locked files.") 
;;========================================================================= ==== 
;; minor mode status variable (buffer local). 
;;========================================================================= ==== 
(defvar pvcs-mode nil 
  "Mode variable for pvcs minor mode.") 
(make-variable-buffer-local 'pvcs-mode) 
;;========================================================================= ==== 
(defvar pvcs:mark nil 
  "Status variable to say if a file will be commited in the next commit command.") 
(make-variable-buffer-local 'pvcs:mark) 
(put 'pvcs:mark 'permanent-local t) 
;;========================================================================= ==== 
(defvar pvcs:commit-list nil 
  "List of files uppon which to perform pvcs commit") 
;;========================================================================= ==== 
;; minor mode entry point. 
;;========================================================================= ==== 
(defun pvcs-mode (&optional arg) 
  "Help to admin PVCS controlled files : 
        \\[pvcs-get]    checkout a revision. 
        \\[pvcs-unlock] release/unlock a revision. 
        \\[pvcs-log]    display pvcs log output. 
        \\[pvcs-locks]  display locked files. 
        \\[pvcs-ediff-internal] run ediff on current file and a revision. 
        \\[pvcs-ediff]  run ediff on two revisions of the file. 
        \\[pvcs-diff]   display diff between current file and a revision. 
        \\[pvcs-mark]   add a file to the commit list. 
        \\[pvcs-list]   show commit list. 
        \\[pvcs-flush]  flush the commit list. 
        \\[pvcs-commit] perform the pvcs put command on the commit list. 
  The global key \\[pvcs-add] puts a new file under version control." 
  (interactive "P") 
  (setq pvcs-mode 
        (if (null arg) 
            (not pvcs-mode) 
          (> (prefix-numeric-value arg) 0))) 
  (if pvcs-mode 
      (is-under-pvcs)) 
  (run-hooks 'pvcs-mode-hooks)) 
;;========================================================================= ==== 
;; register pvcs minor mode keymap and mode line display. 
;;========================================================================= ==== 
(defvar pvcs:map (make-sparse-keymap) 
  "PVCS minor mode keymap") 
(defvar pvcs:commit-map (make-sparse-keymap) 
  "PVCS commit edition buffer keymap") 
(define-key pvcs:map "\C-cvo" 'pvcs-log) 
(define-key pvcs:map "\C-cvg" 'pvcs-get) 
(define-key pvcs:map "\C-cvr" 'pvcs-unlock) 
;(define-key pvcs:map "\C-cvx" 'pvcs-locks) 
(define-key pvcs:map "\C-cvd" 'pvcs-ediff-internal) 
(define-key pvcs:map "\C-cvi" 'pvcs-diff) 
(define-key pvcs:map "\C-cv\C-d" 'pvcs-ediff) 
(define-key pvcs:map "\C-cvm" 'pvcs-mark) 
(define-key pvcs:map "\C-cvc" 'pvcs-commit) 
(define-key pvcs:map "\C-cvl" 'pvcs-list) 
(define-key pvcs:map "\C-cvf" 'pvcs-flush) 
;(define-key pvcs:map "\C-cvu" 'pvcs-update-file) 
;(define-key pvcs:map "\C-cve" 'pvcs-merge-backup) 
;(define-key pvcs:map "\C-cvr" 'pvcs-revert) 
(define-key pvcs:commit-map "\C-c\C-c" 'pvcs-do-commit) 
(define-key pvcs:commit-map "\C-cvl" 'pvcs-list) 
(easy-menu-define 
 pvcs:menu 
 pvcs:map 
 "PVCS minor mode keymap" 
 '("PVCS" 
   ["Log" pvcs-log t] 
   ["Check out" pvcs-get t] 
   ["Release/Unlock" pvcs-unlock t] 
   ["Query locked files" pvcs-locks t] 
 ;  ["Update" pvcs-update-file t] 
 ;  ["Revert" pvcs-revert t] 
 ;  ["Merge backup" pvcs-merge-backup t] 
   ["EDiff" pvcs-ediff-internal t] 
    ["Diff" pvcs-diff t] 
   ["EDiff two revs" pvcs-ediff t] 
   ["--------" nil t] 
   ["Mark to commit" pvcs-mark t] 
   ["Commit files" pvcs-commit t] 
   ["List files" pvcs-list t] 
   ["Flush List" pvcs-flush t] 
   )) 
;;========================================================================= ==== 
(defconst pvcs:entry 
  (list 'pvcs-mode (cons "" '(" PVCS:" pvcs:current-revision))) 
  "Entry to display PVCS revision number in mode line") 
; append mode to minor-mode-alist 
(or (assq 'pvcs-mode minor-mode-alist) 
    (setq minor-mode-alist (cons pvcs:entry minor-mode-alist))) 
(or (assq 'pvcs-mode minor-mode-map-alist) 
    (setq minor-mode-map-alist (cons (cons 'pvcs-mode pvcs:map) 
                                     minor-mode-map-alist))) 
;;========================================================================= ==== 
(defconst pvcs:mark-entry 
  (list 'pvcs:mark " commit") 
  "* Entry to display PVCS revision number in mode line") 
(or (assq 'pvcs:mark minor-mode-alist) 
    (setq minor-mode-alist (cons pvcs:mark-entry minor-mode-alist))) 
;;========================================================================= ==== 
(defun is-under-pvcs () 
  "Test if the file in the current buffer is under PVCS. 
If so, set the variables pvcs:current-revision and pvcs-mode." 
  (interactive) 
  (let ((result nil) 
        current-revision) 
    (if (and buffer-file-name 
             (not (string-match "^/[^/:]*[^/:]:" buffer-file-name))) ; reject remote files 
        (progn 
          (save-excursion 
            (let ((filename buffer-file-name) 
                  (buffer (current-buffer)) 
                  (config-filename (concat (file-name-directory 
                                             buffer-file-name) 
                                            "vcs.cfg"))) 
              (if (file-exists-p config-filename)   ; if there is a PVCS configuration file -> look for revision in buffer 
                  (progn 
                    (goto-char 1) 
                    (setq result t) 
                    (if (re-search-forward pvcs-regex-rev 
                                           nil t) 
                        (progn 
                          (setq current-revision (buffer-substring 
                                                  (match-beginning 1) 
                                                  (match-end 1))))))))) 
          (if result 
              (setq pvcs:current-revision current-revision 
                    pvcs-mode t)))) 
    result)) 
;;========================================================================= ==== 
(defun pvcs-ediff (old-rev new-rev) 
  "Run Ediff between versions old-rev and new-rev of the current buffer." 
  (interactive "sFirst version to visit (default is latest version): 
sSecond version to visit (default is latest version): ") 
  (let ((old-vers (pvcs-version-other-window old-rev))) 
    (other-window 1) 
    (pvcs-version-other-window new-rev) 
    ;; current-buffer is now supposed to contain the old version 
    ;; in another window 
    ;; We delete the temp file that was created by vc.el for the old 
    ;; version 
    (ediff-buffers old-vers (current-buffer) 
                   (list (` (lambda () (delete-file (, (buffer-file-name)))))) 
                   'ediff-revision) 
    )) 
;;========================================================================= ==== 
(defun pvcs-ediff-internal (rev) 
  "Run Ediff on version REV of the current buffer in another window. 
If the current buffer is named `F', the version is named `F.~REV~'. 
If `F.~REV~' already exists, it is used instead of being re-created." 
  (interactive "sVersion to visit (default is latest version): ") 
  (let ((newvers (current-buffer))) 
    (pvcs-version-other-window rev) 
    ;; current-buffer is now supposed to contain the old version 
    ;; in another window 
    ;; We delete the temp file that was created by vc.el for the old 
    ;; version 
    (ediff-buffers (current-buffer) newvers 
                   (list (` (lambda () (delete-file (, (buffer-file-name)))))) 
                   'ediff-revision) 
    )) 
;;========================================================================= ==== 
(defun pvcs-version-other-window (rev) 
  "Visit version REV of the current buffer in another window. 
If the current buffer is named `F', the version is named `F.~REV~'. 
If `F.~REV~' already exists, it is used instead of being re-created." 
  (interactive "sVersion to visit (default is latest version): ") 
  (if buffer-file-name 
      (let* ((version (if (string= rev "") 
                         "tip" 
                       rev)) 
            (filename (if pvcs-temp-dir 
                          (concat (file-name-as-directory pvcs-temp-dir) 
                                  (file-name-nondirectory buffer-file-name)))) 
            (extension (copy-sequence version))) 
        (setq extension (replace-char-in-string extension ?. ?_)) 
        (if (> (length extension) 3) 
            (setq extension (substring extension 0 3))) 
        (message "extension = %s, rev = %s, version = %s" extension rev version) 
        (setq filename (concat (file-name-sans-extension filename) "." extension)) 
        (if (or (file-exists-p filename) 
                (pvcs:checkout (file-name-nondirectory buffer-file-name) rev filename)) 
            (find-file-other-window filename))))) 
;;========================================================================= ==== 
(defun pvcs:checkout (filename rev output-name) 
  "checkout filename with revision rev to output-name" 
  ;; (let ((command-list (list "-Q" (concat "-XO" output-name) filename)) 
  (let ((command-list (list (concat "-XO" output-name) filename)) 
        (command pvcs-checkout-command) 
        (buf)) 
    (if (string= rev "") 
        () 
      (setq command-list (append (list (concat "-R" rev)) command-list))) 
    (if pvcs-vcs-cfg 
        (setq command-list (append (list pvcs-vcs-cfg-option pvcs-vcs-cfg) command-list))) 
    (setq command (concat pvcs-old-command-path (file-name-nondirectory command))) 
    (message "Retrieving version with: %s  %S" command command-list) 
    (if (/= (apply 'call-process command nil t t 
                   command-list) 
            0) 
        (progn 
          (setq buf (get-buffer-create "*PVCS checkout*")) 
          (pvcs:display-temp-buffer buf "checkout") 
          (error "Error while retrieving %s version of %s into %s" 
                 (if (string= "" rev) "last" rev) filename output-name) 
          ) 
      output-name))) 
;;========================================================================= ==== 
(defun pvcs-get (rev) 
  "Run pvcs get command for  the current buffer." 
  (interactive "sVersion to visit (default is latest version): ") 
  (let ((command-list)) 
    (setq command-list (append (if (string= "" rev) 
                                   () 
                                 (list (concat "-r" rev))) 
                               pvcs-checkout-options 
                               (list (file-name-nondirectory (buffer-file-name))))) 
    (message "Calling %s %S" pvcs-checkout-command command-list) 
    (save-excursion 
      (pvcs:call-command pvcs-checkout-command "*PVCS get*" "get" nil 
                         command-list)) 
    (revert-buffer t t))) 
;;========================================================================= ==== 
(defun pvcs-log () 
  "Show the PVCS log for the current buffer's file." 
  (interactive) 
  (pvcs:call-command pvcs-log-command "*PVCS Log*" "vlog" t 
                    (list (file-name-nondirectory (buffer-file-name))))) 
;;========================================================================= ==== 
(defun pvcs-locks (dir) 
  "Query locked files." 
  (interactive "sDirectory to check (default is current): ") 
  (let ((command-list) 
        (path dir)) 
       (setq command-list (append pvcs-locks-options 
                                  (list (concat path "*.*v*")))) 
       (pvcs:call-command pvcs-locks-command "*PVCS Locks*" "locks" nil 
                          command-list))) 
;;========================================================================= ==== 
(defun pvcs-diff (rev) 
  "Run pvcs diff with version REV of the current buffer." 
  (interactive "sVersion to visit (default is latest version): ") 
  (let ((command-list)) 
    (setq command-list (append (if (string= "" rev) 
                                   (list (concat "-r" pvcs:current-revision)) 
                                 (list (concat "-r" rev))) 
                               pvcs-diff-options 
                               (list (file-name-nondirectory (buffer-file-name)) 
                                     (file-name-nondirectory (buffer-file-name))))) 
    (message "VDiff with: %S" command-list) 
    (pvcs:call-command pvcs-diff-command "*PVCS Diff*" "diff" t 
                       command-list))) 
;;========================================================================= ==== 
(defun is-in-commit-list-p (file) 
  " Return t if file is in pvcs:commit-list." 
  (let ((found nil) 
        (clist pvcs:commit-list)) 
    (save-excursion 
      (while (and clist (not found)) 
        (setq found (string= file (car (car clist)))) 
        (setq clist (cdr clist)))) 
    found)) 
;;========================================================================= ==== 
(defun pvcs-mark () 
  "Mark the current file to be committed in the commit command. 
If the file is already in the list, delete it and reset pvcs:mark." 
  (interactive) 
  (if (not (is-in-commit-list-p buffer-file-name)) 
      (progn 
        (setq pvcs:commit-list (cons (list buffer-file-name pvcs:current-revision) pvcs:commit-list)) 
        (setq pvcs:mark t)) 
    (setq pvcs:commit-list (delete (list buffer-file-name pvcs:current-revision) pvcs:commit-list)) 
    (setq pvcs:mark nil)) 
 (force-mode-line-update)) 
;;========================================================================= ==== 
(defun pvcs-flush () 
  "Flush the list of files to be committed" 
  (interactive) 
  (mapcar (function (lambda (c) 
                      (if (get-file-buffer (car c)) 
                          (save-excursion 
                            (set-buffer (get-file-buffer (car c))) 
                            (setq pvcs:mark nil))))) 
          pvcs:commit-list) 
  (setq pvcs:commit-list nil) 
  (force-mode-line-update t)) 
;;========================================================================= ==== 
(defun pvcs-commit () 
  "Setup a buffer to enter comment associated with the commit process. 
Use " 
  (interactive) 
  (save-some-buffers nil) 
  (if (null pvcs:commit-list) 
      (setq pvcs:commit-list (list (list buffer-file-name pvcs:current-revision)))) 
  (let ((dir default-directory)) 
    (switch-to-buffer-other-window (get-buffer-create "*PVCS Commit*")) 
    (setq default-directory dir) 
    (erase-buffer) 
    (insert "\n") 
    (insert "PVCS: ----------------------------------------------------------------------\n") 
    (insert "PVCS: Enter Log.  Lines beginning with `PVCS: ' are removed automatically\n") 
    (insert "PVCS: committing files:\n") 
    (mapcar (function (lambda(c) 
                        (insert "PVCS: " (car c) "\n"))) 
                pvcs:commit-list) 
    (insert "PVCS: Type C-c C-c when done.\n") 
    (insert "PVCS: ----------------------------------------------------------------------\n") 
    (use-local-map pvcs:commit-map) 
    (goto-char 0) 
    (run-hooks 'pvcs-commit-hooks) 
    (message "Type C-c C-c when done."))) 
;;========================================================================= ==== 
(defun pvcs-do-commit () 
  "Commit the list of files pvcs:commit-list" 
  (interactive) 
  (goto-char 0) 
  ; create temporary file with log info 
  (flush-lines "^PVCS: .*$") 
  (run-hooks 'pvcs-before-commit-hooks) 
  (let* ((filename (replace-char-in-string (make-temp-name (concat (or (and pvcs-temp-dir 
                                                    (file-name-as-directory pvcs-temp-dir)) 
                                               "/tmp/") 
                                           "vc")) ?/ ?\\))  ; file PVCS<unique> in TMP directory (8 chars only!) 
         (command-list (if pvcs-vcs-cfg 
                           (list pvcs-vcs-cfg-option pvcs-vcs-cfg (concat pvcs-file-option filename)) 
                         (list (concat pvcs-file-option filename)))) 
         (tmpBuf (current-buffer)) 
         (tmp-command-list nil)) 
    (setq command-list (append command-list pvcs-commit-options)) 
    (message "pvcs/commit: command-list = %S" command-list) 
    (write-file filename) 
    (bury-buffer) 
    (other-window -1) 
    (let ((buf (set-buffer (get-buffer-create "*PVCS Commit output*")))) 
      (setq buffer-read-only nil) 
      (goto-char (point-max)) 
      ; commit all files in pvcs:commit-list 
      (mapcar 
       (lambda(elt) 
         (setq default-directory (car elt)) 
         (message "Committing in %s..." (car elt)) 
         (let ((l (cdr elt)) 
               (file nil) 
               (rev nil)) 
               (while l 
                 (setq tmp-command-list command-list) 
                 (setq file (car (car l))) 
                 (setq rev (car (cdr (car l)))) 
                 (if (null rev) 
                     (setq tmp-command-list (append tmp-command-list (list (concat "-T@" filename))))) 
                 (if (/= (apply 'call-process pvcs-commit-command nil t t 
                                (append tmp-command-list (list file))) 
                         0) 
                     (progn 
                       (pvcs:display-temp-buffer buf "commit") 
                       (error "Error while committing %s in %s" file (car elt)))) 
                 (setq l (cdr l))) 
               (message "Committing in %s...done" (car elt)))) 
       (pvcs:files-to-alist pvcs:commit-list)) 
      (insert "-------------------------------------------------------------------------- ----\n") 
      (pvcs:display-temp-buffer buf "commit") 
      (delete-file filename) 
      ; finally revert all commited buffers 
      (save-excursion 
        (mapcar 
         (lambda(el) 
           (let ((buf (get-file-buffer (car el)))) 
             (if buf 
                 (progn 
                   (set-buffer buf) 
                   (revert-buffer t t))))) 
         pvcs:commit-list))) 
    (kill-buffer tmpBuf)) 
  (pvcs-flush) 
  (run-hooks 'after-commit-hooks)) 
;;========================================================================= ==== 
(defun pvcs:files-to-alist(l) 
  "Sort a list of files to an alist containing the directory as the key and 
the list of file names without directory as the value" 
  (let ((alist nil) 
        (elt nil) 
        (tmp nil) 
        (file nil) 
        (rev nil)) 
    (while l 
      (setq tmp (car l)) 
      (setq file (car tmp)) 
      (setq rev (car (cdr tmp))) 
      (setq elt (assoc (file-name-directory file) alist)) 
      (if elt 
          (setcdr elt 
                   (nconc (cdr elt) (list (list (file-name-nondirectory file) rev)))) 
        (setq alist (cons 
                     (cons (file-name-directory file) 
                           (list (list (file-name-nondirectory file) rev))) 
                     alist))) 
      (setq l (cdr l))) 
    alist)) 
;;========================================================================= ==== 
(defun pvcs-list () 
  "List the files to commit pvcs:commit-list in a buffer" 
  (interactive) 
  (let ((dir default-directory)) 
    (set-buffer (get-buffer-create "* PVCS List of files to commit *")) 
    (setq buffer-read-only nil) 
    (setq default-directory dir) 
    (erase-buffer) 
    (goto-char 0) 
    (if pvcs:commit-list 
        (mapcar (function (lambda(c) 
                            (insert (car c)) (insert "\t") 
                            (if (car (cdr c)) 
                                (insert (car (cdr c))) 
                              (insert "new")) 
                            (insert "\n"))) 
                pvcs:commit-list) 
      (insert "No file to commit\n")) 
    (set-buffer-modified-p nil) 
    (pvcs:display-temp-buffer (current-buffer) "list"))) 
;;========================================================================= ==== 
(defun pvcs:call-command (program bufname name erase &optional args) 
  "Call PROGRAM synchronously in a separate process. 
Input comes from /dev/null. 
Output goes to a buffer named BUFNAME, which is created or emptied first, and 
displayed afterward (if non-empty). 
NAME is appended to the mode entry in the mode line. 
If ERASE is t the buffer will be erased, else the new output is appended. 
Optional fifth arg ARGS is a list of arguments to pass to PROGRAM." 
  (let ((dir default-directory)) 
    (set-buffer (get-buffer-create bufname)) 
    (setq default-directory dir) 
    (setq buffer-read-only nil) 
    (goto-char (point-max)) 
    (if erase 
        (erase-buffer)) 
    (apply 'call-process program nil t nil 
           (if pvcs-vcs-cfg 
               (append (list (concat pvcs-vcs-cfg-option pvcs-vcs-cfg)) args) 
             args)) 
    (if erase 
        (goto-char 0) 
      (goto-char (point-max)) 
      (insert "-------------------------------------------------------------\n")) 
    (pvcs:display-temp-buffer (current-buffer) name))) 
;;========================================================================= ==== 
(defun pvcs:display-temp-buffer (buf name) 
  "Display buffer setting it read only, unmodified and binding key q 
to bury-buffer" 
  (save-excursion 
    (set-buffer buf) 
    (pvcs-info-mode name) 
    (set-buffer-modified-p nil) 
    (setq buffer-read-only t)) 
  (display-buffer buf)) 
;;========================================================================= ==== 
(defun pvcs-unlock () 
  " Remove lock of actual file." 
  (interactive) 
  (let* ((filename (file-name-nondirectory (buffer-file-name (current-buffer)))) 
         (command-list (list "-U" (concat "-R" pvcs:current-revision) filename))) 
  (message "Calling: %s %S" pvcs-vcs-command command-list) 
  (save-excursion 
    (pvcs:call-command pvcs-vcs-command "*PVCS vcs*" "vcs" t 
                       command-list)) 
  (set-file-modes filename 292) ;; set to r/o 
  (revert-buffer t t))) 
;;========================================================================= ==== 
(defun pvcs-add () 
  " Activate version control for actual file. " 
  (interactive) 
  (is-under-pvcs) 
  (run-hooks 'pvcs-add-hooks)) 
;;========================================================================= ==== 
;; major mode stuff to display PVCS info buffers 
;;========================================================================= ==== 
(defvar pvcs-info-mode-hooks nil 
  "Hooks run when entering pvcs-info-mode") 
(defvar pvcs-info-mode-map nil 
  "key map used by pvcs-info-mode") 
(if pvcs-info-mode-map 
    () 
  (setq pvcs-info-mode-map (make-keymap)) 
  (define-key pvcs-info-mode-map "q" 'bury-buffer) 
  (define-key pvcs-info-mode-map " " 'scroll-up) 
  (define-key pvcs-info-mode-map "\177" 'scroll-down) 
) 
;;========================================================================= ==== 
(defun pvcs-info-mode (name) 
  "Major mode to display PVCS information buffers. 
Special commands: 
\\{pvcs-info-mode-map} 
Turning on pvcs-info-mode runs the hooks `pvcs-info-mode-hooks'." 
  (interactive "s") 
  (use-local-map pvcs-info-mode-map) 
  (setq mode-name (concat "PVCS " name)) 
  (setq major-mode 'pvcs-info-mode) 
  (run-hooks 'pvcs-info-mode-hooks) 
) 
;;========================================================================= ==== 
(defun pvcs:hook () 
  "Find-file and revert-buffer hooks to position the ediff variable 
ediff-version-control to process diff between PVCS revisions" 
  (message "loading pvcs") 
  ;(is-under-pvcs)) 
  (if (is-under-pvcs) 
      (progn 
        (if (not (boundp 'ediff-version-control-package)) 
            (setq ediff-version-control-package 'vc)) 
        (make-local-variable 'ediff-version-control-package) 
        (setq ediff-version-control-package 'pvcs)))) 
;;========================================================================= ==== 
;; This is a copy from: 
;;; emx-patch.el --- override parts of files.el etc. for emx. 
;; Copyright (C) 1992-1996 Eberhard Mattes 
;; Author: Eberhard Mattes <mat...@azu.informatik.uni-stuttgart.de> 
(defun replace-char-in-string (str c1 c2) 
  "Replace in string STR character C1 with character C2 and return STR. 
This function does *not* copy the string." 
  (let ((indx 0) (len (length str)) chr) 
    (while (< indx len) 
      (setq chr (aref str indx)) 
      (if (eq chr c1) 
          (aset str indx c2)) 
      (setq indx (1+ indx))) 
    str)) 
(global-set-key "\C-cva" 'pvcs-add)  ;; activate version control for actual buffer 
(add-hook 'find-file-hooks (function pvcs:hook)) 
(add-hook 'after-revert-hook (function pvcs:hook)) 
(run-hooks 'pvcs-mode-hooks) 
;;========================================================================= ==== 
(provide 'pvcs) 
;;; $Log: pvcs.el,v $ 
; Revision 1.19  1998/05/30  16:05:58  SB 
; new command pvcs-locks 
; 
; Revision 1.18  1998/05/30  12:29:58  SB 
; In pvcs:call-command goto end of buffer before call-process 
; 
; Revision 1.17  1998/05/27  20:46:15  SB 
; fixed menu entry pvcs-unlock 
; calling pvcs-after-commit-hooks and pvcs-add-hooks 
; 
; Revision 1.16  1998/05/26  22:12:46  SB 
; use old command path for checkout 
; 
; Revision 1.15  1998/05/26  21:55:55  SB 
; bug fix in pvcs-do-commit: committing more than one file in a directory 
; is ok now. 
; 
; Revision 1.14  1998/05/20  23:47:48  SB 
; Minor fixes in the documentation. 
; 
; Revision 1.13  1998/05/20  15:56:59  SB 
; Deficiencies documented 
; 
; Revision 1.12  1998/05/20  15:43:10  SB 
; Package docs and installation hints. 
; 
; Revision 1.11  1998/05/20  15:30:58  SB 
; New command pvcs-unlock releases a locked revision. 
; 
; Revision 1.10  1998/05/19  23:04:06  SB 
; Append to checkout buffer. 
; 
; Revision 1.9  1998/05/19  22:18:00  SB 
; pvcs:commit-list contains the file's revision now 
; 
; Revision 1.8  1998/05/18  23:20:22  SB 
; new pvcs-get command 
; 
; Revision 1.7  1998/05/18  22:10:25  SB 
; changed -Q in pvcs options to -Q0 (even quieter) 
; 
; Revision 1.6  1998/05/18  18:43:43  SB 
; display state of pvcs:mark in the status line. 
; 
; Revision 1.5  1998/05/18  16:49:25  SB 
; new commands: diff and ediff, committing, view/flush commit list 
; 
; Revision 1.4  1998/05/16  22:40:35  SB 
; pvcs is a minor mode now. 
; 
; Revision 1.3  1998/05/10  20:51:44  SB 
; Using vc-command for general code sequence. 
; 
; Revision 1.2  1997/08/16  23:54:59  SB 
; Inserted Log keyword. 
; 
;;;; end of file 