;;; inf-haskell.el --- Interaction with an inferior Haskell process -*- lexical-binding: t -*-

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.
;; Copyright (C) 2017 Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: languages, Haskell

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for the buffer that holds the inferior process

;; Todo:

;; - Check out Shim for ideas.
;; - i-h-load-buffer and i-h-send-region.

;;; Code:

(require 'comint)
(require 'shell)             ; For directory tracking.
(require 'etags)
(require 'compile)
(require 'haskell-decl-scan)
(require 'haskell-cabal)
(require 'haskell-customize)
(require 'cl-lib)
(require 'haskell-string)

(defgroup inf-haskell nil
  "Settings for REPL interaction via `inf-haskell-mode'."
  :link '(custom-manual "(haskell-mode)inf-haskell-mode")
  :prefix "inf-haskell-"
  :prefix "haskell-"
  :group 'haskell)

(defcustom inf-haskell-hook nil
  "The hook that is called after starting inf-haskell."
  :type 'hook)

(defvar inf-haskell-set+c-p nil
  "Set `:set +c` in `inf-haskell-init' if non-nil.")

(defun haskell-program-name-with-args ()
  "Return the command with the arguments to start the repl based on the
directory structure."
  (cl-ecase (haskell-process-type)
    (ghci       (cond ((eq system-type 'cygwin) `("ghcii.sh" ,@haskell-process-args-ghci))
                      (t (append
                          (if (listp haskell-process-path-ghci)
                              haskell-process-path-ghci
                            (list haskell-process-path-ghci))
                          haskell-process-args-ghci))))
    (cabal-repl `(,haskell-process-path-cabal "repl" ,@haskell-process-args-cabal-repl))
    (stack-ghci `(,haskell-process-path-stack "ghci" ,@haskell-process-args-stack-ghci))))

(defconst inf-haskell-info-xref-re
  "-- Defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?$")

(defconst inf-haskell-module-re
  "-- Defined in \\(.+\\)$"
  "Regular expression for matching module names in :info.")

(defvar inf-haskell-multiline-prompt-re
  "^\\*?[[:upper:]][\\._[:alnum:]]*\\(?: \\*?[[:upper:]][\\._[:alnum:]]*\\)*| "
  "Regular expression for matching multiline prompt.
the one inside :{ ... :} blocks.")

(defconst inf-haskell-error-regexp-alist
  `(;; Format of error messages used by GHCi.
	;; <file path>:<line>:<col>: warning
	;; <file path>:<line>:<col start>:<col end>: warning
    (,(rx bol
		  (group "/" (+? (not whitespace))) ":"	; src file path
		  (group (+ num)) ":"					; line num
		  (group (or (+ num)
					 (and (+ num) "-" (+ num)))) ; col num or col range
		  ":" (+ " ")							 ; separator
		  (? (group (and (any "Ww") "arning")))) ; warning
	 ;; "^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\([Ww]arning\\)?"
     1 2 3 (4) nil (4 '(face nil font-lock-multiline t)))
	;; <file path>:(<beg line>,<beg col>)-(<end line>,<end col>): warning
	(,(rx bol
		  (group "/" (+? (not whitespace))) ":"		   ; src file path
		  "(" (group (+ num)) "," (group (+ num)) ")-" ; start pos
		  "(" (group (+ num)) "," (group (+ num)) "):" ; end pos
		  (+ " ")									   ; separator
		  (? (group (and (any "Ww") "arning"))))	   ; warning
	 ;; "^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\([Ww]arning\\)?"
     1 (2 . 4) (3 . 5) (6) nil (6 '(face nil font-lock-multiline t)))
    ;; Runtime exceptions, from ghci.
	;; <file path>:<line>:<col>
	(,(rx (group "/" (+? (not whitespace))) ":"	; src file path
		  (group (+ num)) ":"					; start pos
		  (group (+ num))						; end pos
		  )
	 1 2 3
	 2									; 2 = real error, 1 = warning
	 )
    ("^\\*\\*\\* Exception: \\(.+?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\)): .*"
     1 ,@(if (fboundp 'compilation-fake-loc) '((2 . 4) (3 . 5))
		   '(2 3)))
    ;; GHCi uses two different forms for line/col ranges, depending on
    ;; whether it's all on the same line or not :-( In Emacs-23, I
    ;; could use explicitly numbered subgroups to merge the two
    ;; patterns.
    ("^\\*\\*\\* Exception: \\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\): .*"
     1 2 ,(if (fboundp 'compilation-fake-loc) '(3 . 4) 3))
    ;; Info messages.  Not errors per se.
    ,@(when (fboundp 'compilation-fake-loc)
        `(;; Other GHCi patterns used in type errors.
          ("^[ \t]+at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
           1 2 (3 . 4) 0)
          ;; Foo.hs:318:80:
          ;;     Ambiguous occurrence `Bar'
          ;;     It could refer to either `Bar', defined at Zork.hs:311:5
          ;;                  or `Bar', imported from Bars at Frob.hs:32:0-16
          ;;                       (defined at Location.hs:97:5)
          ("[ (]defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3 0)
          ("imported from .* at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
           1 2 (3 . 4) 0)
          ;; Info xrefs.
          (,inf-haskell-info-xref-re 1 2 (3 . 4) 0))))
  "Regexps for error messages generated by inferior Haskell processes.
The format should be the same as for `compilation-error-regexp-alist'.")

(defconst haskell-prompt-regexp
  ;; "^[[:alnum:].*_() |λ]*> "
  "^ghci> "
  "Ignore everything before the first '> '.
This allows us to
correctly interpret multi-line input even when modules are imported.")

;;; TODO
;;; -> Make font lock work for strings, directories, hyperlinks
;;; -> Make font lock work for key words???

(defvar inf-haskell-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-z" 'run-haskell)
    (define-key map "\C-c\C-l" 'inf-haskell-load-file)
    (define-key map "\C-c\C-r" 'inf-haskell-reload)
    map))

(defun inferior-haskell-type (sym)
  "Find the type of SYM with `:type' ghci feature."
  (inferior-haskell-get-result (format ":type (%s)" sym)))

(defun inf-haskell-eldoc-at-point (callback &rest _ignored)
  "Use CALLBACK on haskell type of symbol at point.
Intended for `eldoc-documentation-functions' (which see)."
  (when-let* ((sym (thing-at-point 'symbol))
			  (docstring (inferior-haskell-type sym)))
    (funcall callback docstring
             ;; :thing sym
             :face 'font-lock-variable-name-face)))

;; (haskell-doc-mode)
(define-minor-mode inf-haskell-minor-mode
  "Minor mode for interacting with inf-haskell from Haskell source file."
  :group 'inf-haskell
  :lighter "Inf-Haskell"
  :keymap inf-haskell-minor-mode-map
  (if inf-haskell-minor-mode
	  (progn
		;; (setq-local eldoc-documentation-function
		;; 			(lambda ()
		;; 			  (inf-haskell-eldoc-at-point (eldoc--make-callback :eager))))
		(add-hook 'completion-at-point-functions
				  'inf-haskell-completion-at-point nil 'local))
	(remove-hook 'eldoc-documentation-functions
				 'inf-haskell-eldoc-at-point
				 'local)
	;; (setq-local eldoc-documentation-function 'inf-haskell-eldoc-at-point)
	(remove-hook 'completion-at-point-functions
				 'inf-haskell-completion-at-point 'local)))

(defvar inf-haskell-mode-map
  (let ((map (make-sparse-keymap)))
	;; (set-keymap-parent map comint-mode-map)
    (define-key map "\C-c\C-d" 'comint-kill-subjob)
    map))

(define-derived-mode inf-haskell-mode comint-mode "Inf-Haskell"
  "Major mode for interacting with an inferior Haskell process."
  :group 'inf-haskell

  (setq-local paragraph-start haskell-prompt-regexp)

  (setq-local comint-input-autoexpand nil
			  comint-prompt-read-only t
			  comint-prompt-regexp haskell-prompt-regexp
			  comint-output-filter-functions
			  '(comint-truncate-buffer
				ansi-color-process-output
				comint-postoutput-scroll-to-bottom
				comint-watch-for-password-prompt))

  ;; Setup directory tracking.
  (setq-local shell-cd-regexp ":cd")
  (condition-case nil
      (shell-dirtrack-mode 1)
    (error ;The minor mode function may not exist or not accept an arg.
     (setq-local shell-dirtrackp t)
     (add-hook 'comint-input-filter-functions 'shell-directory-tracker
               nil 'local)))

  ;; Setup `compile' support so you can just use C-x ` and friends.
  (setq-local compilation-error-regexp-alist inf-haskell-error-regexp-alist
			  compilation-first-column 1) ;GHCI counts from 1
  (if (and (not (boundp 'minor-mode-overriding-map-alist))
           (fboundp 'compilation-shell-minor-mode))
      ;; If we can't remove compilation-minor-mode bindings, at least
      ;; try to use compilation-shell-minor-mode, so there are fewer
      ;; annoying bindings.
      (compilation-shell-minor-mode 1)
    ;; Else just use compilation-minor-mode but without its bindings
    ;; because things like mouse-2 are simply too annoying.
    (compilation-minor-mode 1)
    (let ((map (make-sparse-keymap)))
      (dolist (keys '([menu-bar] [follow-link]))
        ;; Preserve some of the bindings.
        (define-key map keys (lookup-key compilation-minor-mode-map keys)))
      (add-to-list 'minor-mode-overriding-map-alist
                   (cons 'compilation-minor-mode map))))
  (add-hook 'inf-haskell-hook 'inf-haskell-init))

(defvar inf-haskell-buffer nil
  "The buffer in which the inferior process is running.")

(defun inf-haskell-start-process ()
  "Start an inferior haskell process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `haskell-program-name-with-args'.
It runs the hook `inf-haskell-hook' after starting the process and
setting up the inf-haskell buffer."
  (let ((command (haskell-program-name-with-args)))
    (when (bound-and-true-p inf-haskell-root-dir)
      (setq default-directory inf-haskell-root-dir))
	;; (if (buffer-live-p inf-haskell-buffer)
	;; 	(comint-exec inf-haskell-buffer
	;; 				 "haskell"
	;; 				 (car command)
	;; 				 nil
	;; 				 (cdr command)))
	(setq inf-haskell-buffer
		  (apply 'make-comint "haskell" (car command) nil (cdr command)))
    (with-current-buffer inf-haskell-buffer
      (inf-haskell-mode)
      (run-hooks 'inf-haskell-hook))))

(defun inf-haskell-proc-alive-p (proc)
  "PROC for (process-live-p)."
  (not (eq 'stop (process-status proc))))

(defun inf-haskell-try-start-process (ntries)
  (cond ((zerop ntries)
		 (user-error "Inf-haskell cannot start %s"
					 (haskell-program-name-with-args)))
		((and (buffer-live-p inf-haskell-buffer)
              (comint-check-proc inf-haskell-buffer)
			  (inf-haskell-proc-alive-p
			   (get-buffer-process inf-haskell-buffer)))
		 (get-buffer-process inf-haskell-buffer))
        (t (inf-haskell-start-process)
           (inf-haskell-try-start-process (- ntries 1)))))

;; (process-status (inf-haskell-process))
(defun inf-haskell-process ()
  "Restart if not present."
  (inf-haskell-try-start-process 3))

;;;###autoload
(defun run-haskell ()
  "Show the inf-haskell buffer.  Start the process if needed."
  (interactive)
  (let ((proc (inf-haskell-process)))
    (pop-to-buffer (process-buffer proc))))

(defun inf-haskell-init ()
  (when inf-haskell-set+c-p
	(inf-haskell-get-result ":set -v1")
    (inf-haskell-get-result ":set +c")
	;; TODO: implement inf-haskell-echo
	;; (when haskell-process-show-debug-tips
;;       (inf-haskell-echo "
;; If I break, you can:
;;   1. Restart:           M-x haskell-process-restart
;;   2. Configure logging: C-h v haskell-process-log (useful for debugging)
;;   3. General config:    M-x customize-mode
;;   4. Hide these tips:   C-h v haskell-process-show-debug-tips"))
	))

(defvar inf-haskell-last-result nil)

(defvar haskell--next-input ""
  "Used to store the intermediate results temporarily while
`accecpt-process-output' with `inf-haskell-extract-exp'")

(defalias 'inf-haskell-get-result 'inf-haskell-send-string-no-output)
(defalias 'inferior-haskell-get-result 'inf-haskell-get-result)

(defun inf-haskell-send-string (string &optional process)
  "Send STRING to inferior Haskell PROCESS."
  (let ((process
		 (or process (inf-haskell-process))))
    (comint-send-string process string)
    (comint-send-string process "\n")))

(defun inf-haskell-load-file (file-name &optional process)
  "Load file FILE-NAME to the shell.  Use PROCESS."
  (interactive (list (or (buffer-file-name)
						 (read-file-name "File to load: "))))
  (with-current-buffer inf-haskell-buffer
	(goto-char (cdr comint-last-prompt))
	(insert "\n"))
  (inf-haskell-send-string (format ":load %s" file-name) process))

(defun inf-haskell-reload (&optional process)
  "Send :reload to haskell PROCESS."
  (interactive)
  (inf-haskell-send-string ":reload" process))

(defun inf-haskell-reload-with-fbytecode (process module-buffer)
  "Query a PROCESS to reload MODULE-BUFFER with -fbyte-code set.
Restores -fobject-code after reload finished.
MODULE-BUFFER is the actual Emacs buffer of the module being loaded."
  (inf-haskell-send-string-no-output process ":set -fbyte-code")
  ;; We prefix the module's filename with a "*", which asks ghci to
  ;; ignore any existing object file and interpret the module.
  ;; Dependencies will still use their object files as usual.
  (inf-haskell-send-string-no-output
   process
   (format ":load \"*%s\""
           (replace-regexp-in-string
            "\""
            "\\\\\""
            (buffer-file-name module-buffer))))
  (inf-haskell-send-string-no-output process ":set -fobject-code"))

;;; completions

(defun inf-haskell--last-prompt ()
  "Return last prompt start and end."
  comint-last-prompt)

(defun inf-haskell-get-completions (process input)
  "Do completion at point using PROCESS for IMPORT or INPUT.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  (message "getting %s" input)
  (let ((rawstr
         (s-trim
          (inf-haskell-get-result
           (format ":complete repl 5 \"%s\"" input) process))))
    (when rawstr
      ;; parse REPL response if any
      (let* ((s1 (split-string rawstr "\r?\n" t))
             (cs (remove
				  nil
				  (mapcar #'haskell-string-literal-decode
						  (cdr s1))))
             (h0 (car s1))) ;; "<limit count> <all count> <unused string>"
        (save-match-data
		  (unless (string-match
                  (rx (group (+ digit))
					  whitespace
					  (group (+ digit))
					  whitespace
					  (group ?" (* nonl) ?"))
                  h0)
           (error "Invalid `:complete' response '%s'" h0))
          (let ((cnt1 (match-string 1 h0))
				(h1 (haskell-string-literal-decode (match-string 3 h0))))
			(unless (= (string-to-number cnt1) (length cs))
              (error "Lengths inconsistent in `:complete' response"))
			(cons h1 cs)))))))

(defun inf-haskell-completion-at-point (&optional process)
  "Function for `completion-at-point-functions' in `haskell-comint-mode'.
Optional argument PROCESS forces completions to be retrieved
using that one instead of current buffer's process."
  (interactive)
  (setq process (or process (get-buffer-process inf-haskell-buffer)))
  (let ((line-start (if (derived-mode-p 'haskell-comint-mode)
                        ;; Working on a shell buffer: use prompt end.
                        (cdr (inf-haskell--last-prompt))
                      (line-beginning-position)))
        (start nil)
        (end nil))
	(cond ((string-match-p
			(rx (* space) word-start "import" word-end space)
			(buffer-substring-no-properties line-start (point)))
		   ;; Import statement
		   (setq start line-start
				 end (point)))
		  ((pcase-let ((`(,start% . ,end%)
						(bounds-of-thing-at-point 'symbol)))
			 (setq start start%
				   end end%)
			 t)))
    (and start
		 end
		 (list start end
			   (inf-haskell-get-completions
				process (buffer-substring start end))
			   ;; (completion-table-dynamic
			   ;;  (apply-partially #'inf-haskell-get-completions process))
			   ))))

;;; Get results

(defvar-local inf-haskell-output-filter-in-progress nil)
(defvar-local inf-haskell-filtered-output nil)

(defun inf-haskell-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (let ((start
         (string-match
          ;; XXX: It seems on macOS an extra carriage return is
          ;; attached at the end of output, this handles that too.
          (concat
           "\r?\n?"
           ;; Remove initial caret from calculated regexp
           (replace-regexp-in-string
            (rx string-start ?^) ""
			haskell-prompt-regexp)
           (rx eos))
          output)))
    (and start (substring output start))))

(defun inf-haskell-output-filter (buf)
  "Filter used in `haskell-shell-send-string-no-output' to grab output in BUF.
string is the output received to this point from the process.
This filter saves received output from the process in
`haskell-shell-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (lambda (string)
	(with-current-buffer buf
	  (setq ;; string (ansi-color-filter-apply string)
	   inf-haskell-filtered-output
	   (concat inf-haskell-filtered-output string))
	  (let ((res (inf-haskell-comint-end-of-output-p
				  inf-haskell-filtered-output)))
		(when res
		  ;; Output ends when `haskell-shell-output-filter-buffer'
		  ;; contains the prompt attached at the end of it.
		  (setq inf-haskell-output-filter-in-progress nil
				inf-haskell-filtered-output
				(substring inf-haskell-filtered-output
						   0 (match-beginning 0)))))
	  "")))

(defun inf-haskell-send-string-no-output (string &optional process)
  "Send STRING to PROCESS and inhibit output.
Return the output."
  (let* ((process (or process (inf-haskell-process)))
		 (buf (process-buffer process))
		 (comint-preoutput-filter-functions
		  (list (inf-haskell-output-filter buf))))
	(with-current-buffer buf
	  (let ((inf-haskell-output-filter-in-progress t)
			(inhibit-quit t)
			(inf-haskell-filtered-output nil))
		(or
		 (with-local-quit
		   (inf-haskell-send-string string process)
		   (while inf-haskell-output-filter-in-progress
			 ;; `haskell-shell-output-filter' takes care of setting
			 ;; `haskell-shell-output-filter-in-progress' to NIL after
			 ;; it detects end of output.
			 (accept-process-output process 0.2))
		   (prog1 (s-trim-left inf-haskell-filtered-output)
			 (setq inf-haskell-filtered-output nil)))
		 (with-current-buffer (process-buffer process)
		   (comint-interrupt-subjob)))))))

(provide 'inf-haskell)

;;; inf-haskell.el ends here
