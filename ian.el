;(setq visible-bell nil)
;(hl-line-mode -1)
;(global-auto-revert-mode t)
;(if (eq window-system 'x)
;    (set-default-font "Inconsolata-10")) ;or perhaps dejavu sans
(require 'tramp)
; ctags settings from http://www.emacswiki.org/emacs/BuildTags
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file"
  (interactive "Directory: ")
  (shell-command (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name dir-name)))
(add-to-list 'default-frame-alist '(font . "Inconsolata-10"))
(delete-selection-mode t)
(menu-bar-mode t)
(column-number-mode t)
(add-to-list 'load-path "~/src/emacs/")
(require 'revbufs)

(require 'color-theme)
(require 'color-theme-my-forest)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-my-forest)))

(setq c-default-style "linux")
; maybe I should use "linux" style
(add-hook 'c-mode-hook 'run-coding-hook)
(add-hook 'c-mode-hook '(lambda ()
                          (local-set-key (kbd "DEL") 'paredit-backward-delete)
                          (local-set-key (kbd "<delete>") 'paredit-forward-delete)
                          (local-set-key (kbd "\"") 'paredit-doublequote)
                          (local-set-key (kbd "(") 'paredit-open-parenthesis)
                          (local-set-key (kbd ")") 'paredit-close-parenthesis)
                           (local-set-key (kbd "[") 'paredit-open-bracket)
                           (local-set-key (kbd "]") 'paredit-close-bracket)
                          ;;(paredit-mode +1)
                          ;;(local-unset-key (kbd "M-;"))
                          ;;(local-set-key (kbd "M-;") 'comment-dwim)
                          (local-set-key (kbd "{") 'paredit-open-curly)
                          (local-set-key (kbd "}") 'paredit-close-curly)))
;; need that hook in java too
(add-hook 'scheme-mode 'run-coding-hook)
;; random
(global-set-key (kbd "C-x M-d") 'fixup-whitespace)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(require 'mwe-log-commands)

;; mode-compile
(require 'mode-compile)
(autoload 'mode-compile "mode-compile")
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile")
(global-set-key "\C-ck" 'mode-compile-kill)

;(require 'el-expectations)

(add-to-list 'load-path "~/src/emacs/rspec-mode")
(require 'rspec-mode)
;(require 'rspec-expectations)

(add-to-list 'auto-mode-alist '("\\.sls$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sps$" . scheme-mode))

;; geiser
;(load-file "~/src/emacs/geiser/elisp/geiser.el")
(load "/home/ian/src/emacs/geiser/build/elisp/geiser-load")
(setq geiser-active-implementations '(guile racket)); forget about racket for now :)
(add-hook 'geiser-repl-mode-hook (lambda () (paredit-mode +1)))
(setq geiser-guile-load-init-file-p t)
(eval-after-load "geiser-impl"
'(add-to-list 'geiser-implementations-alist
             '((regexp ".") guile))) ; use guile otherwise

;; quack
(require 'quack)
(setq quack-default-program "ikarus")
(add-hook 'inferior-scheme-mode-hook (lambda () (paredit-mode +1)))

;; scheme-complete
(autoload 'scheme-complete-or-indent "scheme-complete" nil t)
 (eval-after-load 'scheme
   '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

;; web stuff
(require 'haml-mode)
(require 'sass-mode)
;;
;; Comment out nxhtml stuff while using multi web mode
;;
;; (load "/home/ian/src/emacs/nxhtml/autostart.el")
;; (add-to-list 'load-path "~/src/emacs/rinari")
;; (require 'rinari)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
;; (setq
;;  nxhtml-global-minor-mode t
;;  mumamo-chunk-coloring 'submode-colored
;;  nxhtml-skip-welcome t
;;  indent-region-mode t
;;  rng-nxml-auto-validate-flag nil
;;  nxml-degraded t)
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))
;;
;(eval-after-load 'js2-mode '(add-hook 'js2-mode-hook 'moz-minor-mode))

(setq auto-mode-alist
      (append '(("\\.js$" . espresso-mode)("\\.json$" . espresso-mode))
              auto-mode-alist))
(eval-after-load 'espresso-mode '(add-hook 'esspresso-mode-hook
'moz-minor-mode))
; From http://emacs-fu.blogspot.com/2009/11/making-buffer-names-unique.html
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-strip-common-suffix nil)

; From http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;(global-font-lock-mode 1)

;; javascript
;(autoload 'js2-mode "js2" nil t)
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;(setq js2-highlight-level 3)


;; Rainbow Mode
(add-to-list 'load-path "~/src/emacs/jd-el")
(require 'rainbow-mode)
(autoload 'rainbow-mode "rainbow-mode")
(add-hook 'css-mode-hook 'rainbow-mode)
(require 'pabbrev)
;; Taken from http://www.reddit.com/r/emacs/comments/af2sg/bnfmode_for_emacs/
(define-generic-mode 'bnf-mode
 () ;; comment char: inapplicable because # must be at start of line
 nil ;; keywords
 '(
   ("^#.*" . 'font-lock-comment-face) ;; comments at start of line
   ("^<[^ \t\n]*?>" . 'font-lock-function-name-face) ;; LHS nonterminals
   ("<[^ \t\n]*?>" . 'font-lock-builtin-face) ;; other nonterminals
   ("::=" . 'font-lock-const-face) ;; "goes-to" symbol
   ("\|" . 'font-lock-warning-face) ;; "OR" symbol
   )
 '("\\.bnf\\'") ;; filename suffixes
 nil ;; extra function hooks
 "Major mode for BNF highlighting.")

;(add-to-list 'load-path "~/src/emacs/ocaml-emacs")
;(load "~/src/emacs/ocaml-emacs/ocaml.emacs")
(add-to-list 'load-path  "~/src/emacs/tuareg-2.0.1")
(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml Debugger" t)

(defun split-window-horizontally-and-switch ()
  "Splits the window in 2 Horizontally. Then switches to it"
  (interactive)
  (split-window-horizontally)
  (other-window 1))
(defun split-window-vertically-and-switch ()
  "Splits the window in 2 Vertically. Then switches to it"
  (interactive)
  (split-window-vertically)
  (other-window 1))

;;; Commented out because I never use them
;;(global-set-key "\C-x5" 'split-window-horizontally-and-switch)
;;(global-set-key "\C-x4" 'split-window-vertically-and-switch)


;split-window-vertically2
;split-window-horizontally3
;other-window
;     (global-set-key "\C-cw" 'compare-windows) ;there is also a global-unset-key


;; Sick fed up of the git trying to use less
;; needs to be _BEFORE_ the first call to shell
(setenv "PAGER" "cat")

(setenv "NODE_NO_READLINE" "1")

;; better M-x compile
;; from emacswiki http://www.emacswiki.org/emacs/CompileCommand
 (require 'compile)
 (add-hook 'c-mode-hook
           (lambda ()
	     (unless (file-exists-p "Makefile")
	       (set (make-local-variable 'compile-command)
                    ;; emulate make's .c.o implicit pattern rule, but with
                    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                    ;; variables:
                    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		    (let ((file (file-name-nondirectory buffer-file-name)))
                      (format "%s -c -o %s.o %s %s %s"
                              (or (getenv "CC") "gcc")
                              (file-name-sans-extension file)
                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
                              (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			      file))))))


(global-set-key (kbd "\C-c +") 'hs-toggle-hiding)
;; I wish there was a programming "super mode" i could hook into
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'scheme-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook       'hs-minor-mode)
;; from http://www.emacswiki.org/emacs/HideShow
(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))
(add-hook 'ruby-mode-hook         'hs-minor-mode)


;; No longer use it
;; (add-to-list 'load-path "~/src/emacs/google-weather-el")
;; (require 'google-weather)
;; (require 'org-google-weather)


(global-set-key (kbd "\C-c ;") 'comment-or-uncomment-region)

(split-window-vertically)
(other-window 1)
(eshell) ;; nice idea steve yegge
(other-window 1)
;; also from Steve Yegge, see
;; http://sites.google.com/site/steveyegge2/my-dot-emacs-file

;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

;;
;; Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	n(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
	(progn (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

;;
;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))


(add-to-list 'load-path "/home/ian/src/emacs/ioccur/")
(require 'ioccur)


;; vala
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
;(add-to-list 'file-coding-system-alist ("\\.vala$" . utf-8))
;(add-to-list 'file-coding-system-alist ("\\.vapi$" . utf-8))


(define-skeleton ruby-class
  "Ruby class skeleton"
  "Enter the name of the class: "
  "class " str \n \n
  ("Enter the name of a method:" "def " str \n  -2 "end" \n \n)
  -2 "end" \n)


;(setq initial-scratch-message nil)
(defun scheme-eval-last-sexp-in-current-buffer ()
  "Evals the previous sexp in a scheme buffer, and returns the result to the current buffer. Does not return printed output."
  (interactive)
  (comint-redirect-send-command-to-process
   (format "%S" (preceding-sexp))
    (current-buffer)
    (scheme-get-process)
    t))
(add-hook 'scheme-mode-hook
          (lambda () (local-set-key (kbd "\C-c C-n") 'scheme-eval-last-sexp-in-current-buffer)))

(require 'pretty-mode)
(global-pretty-mode 1)
; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode

;; sql stuff
(require 'sql-indent)
(require 'sql-transform)
;(require 'plsql)

(defun scheme-library-name ()
  "Determines the scheme library name based on the buffer name, otherwise empty string"
  (interactive)
  (let ((buffer-name (buffer-file-name))
        (name-regex ".*/\\(.*?\\)\\..*"))
    (if (string-match name-regex buffer-name)
        (match-string 1 buffer-name)
      "")))
(define-skeleton let-ans
  "A skeleton for wrapping a lisp expression in a let statement"
  nil
  "(let ((" (read-string "What name are you binding to? " nil nil "ans") " " _ "))" \n
  - ")")

(define-skeleton scheme-library
  "A skeleton for creating Scheme-libraries"
  nil
  "#!r6rs\n(library "
  ; library name
  "(" (read-string "Enter a library name: " nil nil (scheme-library-name)) ")"
  "\n"
  ; export list
  "(export " ("Enter an export: " str \n) ")" \n
  ; import list
  "(import " ("Enter an import: " "(" str ")" \n) ")" \n \n
  ; definitions
  ("Enter a function name: "
   "(define (" str ("Enter an argument name: " " " str) ")" \n "#f)" \n \n)
  ")")
(define-skeleton html-tag-skel
  ;; should store previous tag as default
  "A skeleton for creating html tags"
  "Type of tag:"
  "<" str
  ("Attribute: " " " str "=\"" (read-string (concat str "= ")) "\"")
  ">" _ "</" str ">")
;; Need to add it to the html one only
;(global-set-key (kbd "C-c t") 'html-tag-skel)
;; (setq skeleton-pair t)
;; (local-set-key "<" 'skeleton-pair-insert-maybe)
;;
;; (push '(?< (setq v1 (skeleton-read "Type of tag:"))
;;            ("Attribute: " " " str "=\"" (read-string (concat str "= ")) "\"")
;;            ">" _ "</" v1 ">") skeleton-pair-alist)



;; (setq default-abbrev-mode t)
;; (add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))


;; Haskell Mode
(load "~/src/emacs/haskell-mode-2.8.0/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;(add-hook 'haskell-mode-hook 'font-lock-mode) ; Needed?

(defvar normal-guilep t)
(defun toggle-guile-binary ()
  (interactive)
  "Toggles the guile binary for geiser between \"guile\" and \"guile-ncurses-shell\""
  (if normal-guilep
      (setq geiser-guile-binary "guile-ncurses-shell")
    (setq geiser-guile-binary "guile")))


;; from http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(defun make-buffer-executable-except-r6rs-libs ()
  ;; *cough* hack *cough*
  (unless (string-match "\\.sls$" (buffer-name))
    (executable-make-buffer-file-executable-if-script-p)))

;; More effort than it was worth at the moment

;; (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; (add-hook 'after-save-hook
;;           'make-buffer-executable-except-r6rs-libs)
;; (remove-hook 'after-save-hook
;;              'make-buffer-executable-except-r6rs-libs
;;              nil)

;; ELPA
;; from http://sachachua.com/blog/2011/01/emacs-24-package-manager/

;; Add the original Emacs Lisp Package Archive
;(add-to-list 'package-archives
;             '("elpa" . "http://tromey.com/elpa/"))
; ^^ already in mine
;
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'load-path "/home/ian/src/emacs/company")
(autoload 'company-mode "company" nil t)

(add-hook 'scheme-mode-hook (lambda () (company-mode +1)))
(add-hook 'geiser-repl-mode-hook (lambda () (company-mode +1)))


;; Commented out, until I can figure out how to turn this off for
;; certain git repos
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Erlang mode was installed from distribution
(setq erlang-root-dir "/usr/lib/erlang")

;; Erlang  + Distel
(add-to-list 'load-path "~/src/emacs/distel/elisp")
(require 'distel)
(distel-setup)

;;; recommended by
;;; http://alexott.net/en/writings/emacs-devenv/EmacsErlang.html
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
                                        (lambda ()
                                                ;; add some Distel bindings to the Erlang shell
                                                (dolist (spec distel-shell-keys)
                                                        (define-key erlang-shell-mode-map (car spec) (cadr spec)))))


;; yasnippet
(add-to-list 'load-path "~/src/emacs/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/src/emacs/yasnippet-0.6.1c/snippets/")


;; New Python mode
(add-to-list 'load-path "~/src/emacs/python.el/")
(require 'python)


;; Browse
(global-set-key (kbd "M-#") 'browse-url)
(global-set-key (kbd "C-'") 'browse-url-at-point)


;;;; Check these are installed
;; (require 'xml-rpc)
;; (require 'lisppaste)
;; (require 'android-mode)


; (server-start) ;; not necessary If I'm using --alternate-editor="" option



;; Abbrevs
;; Misspellings file taken from
;; https://secure.wikimedia.org/wikipedia/en/wiki/Wikipedia:Lists_of_common_misspellings/For_machines
;; and the processed with
;; sed -e 's/\(.*\)->\(.*\),.*/\1->\2/' -e "/.*[-'\";][^>].*->.*/d" -e
;; 's/\(.*\)\->\(.*\)/(define-global-abbrev "\1" "\2")/'
;; .emacs.d/misspelling_abbrevs.bak > .emacs.d/misspelling_abbrevs
(read-abbrev-file "~/.emacs.d/misspelling_abbrevs")
;; other misspellings of mine
(define-global-abbrev "lsit" "list")    ; far too common for me being
                                        ; a lisper :P
(define-global-abbrev "hygeinic" "hygienic") ; and for being a schemer


;; Recreate Scratch Buffer
;; from http://www.emacswiki.org/emacs/RecreateScratchBuffer
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))


(global-set-key (kbd "H-<left>") 'windmove-left)
(global-set-key (kbd "H-<right>") 'windmove-right)
(global-set-key (kbd "H-<down>") 'windmove-down)
(global-set-key (kbd "H-<up>") 'windmove-up)



;;; Scheme indentation
(put 'with-test-prefix 'scheme-indent-function 1)
(put 'pass-if 'scheme-indent-function 1);; Guile Testing
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'with-syntax* 'scheme-indent-function 1)
(put 'with-code-coverage 'scheme-indent-function 1)
(put 'with-implicit 'scheme-indent-function 1)
(put 'cases 'scheme-indent-function 2)
(put 'when-let 'scheme-indent-function 1)
(put 'syntax-parameterize 'scheme-indent-function 1)
;; vv I don't actually use those two, but I'd hate if I accidentally
;; reindented someone elses code
(put 'while 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
;; I wonder if I can write general with- and call-with- rules

(put 'test-case 'scheme-indent-function 2)

(put 'syntax-parse 'scheme-indent-function 1)

(put 'catch 'scheme-indent-function 1)
(put 'call-with-prompt 'scheme-indent-function 1)

;(put 'mlet 'scheme-indent-function 1)

(put 'stream-case 'scheme-indent-function 1)

;;; Diminish
(require 'diminish)
(eval-after-load 'paredit
'(diminish 'paredit-mode "Ped"))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq mode-name "elisp")))

;; idea from
;; http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(global-set-key (kbd "C-.") 'flyspell-auto-correct-word)


;; I'm a moron, the following is done by emacs if you use
;; auto-insert-mode and aren't stupid enough to accidentally override
;; its default auto-insert-alist
;;
;; (defun headerify-name (name)
;;   (concat "__" (replace-regexp-in-string "\\." "_"  (upcase (file-name-nondirectory name)))))
;;
;; (define-skeleton c-header-guard
;;   "boilerplate for ensuring header included only once"
;;   nil
;;   (let ((name (headerify-name (buffer-file-name))))
;;     (list nil 
;;      "#ifndef " name '\n
;;      "#define " name '\n
;;      '_
;;      "\n#endif /* " name " */" '\n)))

;; Auto inserts
;; Doing it this way sucks, next time use define-auto-insert

(setq auto-insert-query nil)              ; stop asking already, jeez
(setq auto-insert-directory (concat dotfiles-dir "inserts"))
(auto-insert-mode 1)
(setq auto-insert-alist
      (append '(("\\.sls$" . scheme-library)
                ;;("\\*scratch\\*" . "test.autoinsert")
                ;; ( scratch . fortune)
                ;; i should have scratch auto insert fortunes
                )
              auto-insert-alist))
;; gnus
(setq gnus-select-method '(nntp "news.btinternet.com"))
;; news.btopenworld.com was also suggested, but this seems to work fine


;; From
;; http://atomized.org/2011/01/toggle-between-root-non-root-in-emacs-with-tramp/
;; He uses C-c C-x C-q and C-c x f but I'm unsure at the moment
(defun find-file-as-root ()
  "Find a file as root."
  (interactive)
  (let* ((parsed (when (tramp-tramp-file-p default-directory)
                   (coerce (tramp-dissect-file-name default-directory)
                           'list)))
         (default-directory
           (if parsed
               (apply 'tramp-make-tramp-file-name
                      (append '("sudo" "root") (cddr parsed)))
             (tramp-make-tramp-file-name "sudo" "root" "localhost"
                                         default-directory))))
    (call-interactively 'find-file)))

(defun toggle-alternate-file-as-root (&optional filename)
  "Toggle between the current file as the default user and as root."
  (interactive)
  (let* ((filename (or filename (buffer-file-name)))
         (parsed (when (tramp-tramp-file-p filename)
                   (coerce (tramp-dissect-file-name filename)
                           'list))))
    (unless filename
      (error "No file in this buffer."))

    (find-alternate-file
     (if (equal '("sudo" "root") (butlast parsed 2))
         ;; As non-root
         (if (or
              (string= "localhost" (nth 2 parsed))
              (string= (system-name) (nth 2 parsed)))
             (car (last parsed))
           (apply 'tramp-make-tramp-file-name
                  (append (list tramp-default-method nil) (cddr parsed))))

       ;; As root
       (if parsed
           (apply 'tramp-make-tramp-file-name
                  (append '("sudo" "root") (cddr parsed)))
         (tramp-make-tramp-file-name "sudo" nil nil filename))))))



(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Lusty explorer is worth exploring
(defvar lusty-on t)
(require 'lusty-explorer)
(defun toggle-lusty ()
  (interactive)
  (if lusty-on
      (progn
        (setq lusty-on nil)
        (global-set-key (kbd "C-x C-f") 'ido-find-file)
        (global-set-key (kbd "C-x b") 'ido-switch-buffer)
        (message "ido"))
    (progn
      (setq lusty-on t)
      (global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
      (global-set-key (kbd "C-x b") 'lusty-buffer-explorer)
      (message "lusty"))))


;; Tea Time
(add-to-list 'load-path "~/src/emacs/tea-time")
(require 'tea-time)
; drip-sounce /usr/share/sounds/gnome/default/alerts/drip.ogg 
; freedesktop complete /usr/share/sounds/freedesktop/stereo/complete.oga
(setq tea-time-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")

(define-key global-map "\C-ct" 'tea-time)



(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
(global-set-key (kbd "C-c h") 'hide-lines)


(global-set-key (kbd "C-x 9") 'kill-buffer-and-window)

(global-set-key (kbd "C-x TAB") 'indent-rigidly)
;; should be necessary, but starter kit overrides it
(global-set-key (kbd "C-c TAB") 'ido-imenu)


;; I found this yow.lines at
;; https://code.google.com/p/a2bot/source/browse/trunk/data/yow.lines?r=2
(setq yow-file "~/.emacs.d/yow.lines")

(global-set-key (kbd "C-c q") 'refill-mode)


;; for GDB
(setq gdb-many-windows t)
;; needs to toggle off when done
;; ask on #emacs
;; (add-hook 'gdb-mode-hook 'tool-bar-mode)
;; (setq 'gud-kill-buffer-hook nil)
;; (add-hook 'gud-kill-buffer-hook 'tool-bar-mode)
;; (turn-off-tool-bar)


(global-set-key "\C-xc" 'mode-compile)

;; put this file in the dot[e]macs register ;)
(set-register ?e `(file . ,(concat dotfiles-dir "ian.el")))

(require 'sr-speedbar)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)


;; Temporary until I see if dired-x or dired-aux provide this
;; functionality (dired-do-find-marked-files ?)
;; from http://stackoverflow.com/questions/1110118/in-emacs-dired-how-to-find-visit-multiple-files
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))
;;; Not necessary in Emacs 24 I think, since F = dired-do-find-marked-files


;; ASM
(add-hook 'asm-mode-hook
          (lambda ()
            (local-set-key (kbd "<RET>") 'newline)))


;; might be useful, means I can just use C-SPC after C-u C-SPACE,
;; rather than having to keep using a prefix
(setq set-mark-command-repeat-pop t)

(setq user-mail-address "ianprice90@googlemail.com")
(require 'legalese)
(setq legalese-default-license 'bsd)


(setq comment-style 'extra-line)
;; same as 'indent', but you get multiline quotes rather than single
;; quotes i.e.
;;
;; /* 
;;  * memset (&hints, 0, sizeof (struct addrinfo));
;;  * memset (&addr, 0, sizeof (struct in_addr));
;;  */
;;
;; instead of
;;
;; /* memset (&hints, 0, sizeof (struct addrinfo)); */
;; /* memset (&addr, 0, sizeof (struct in_addr)); */

;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'comment-add) 1)))
;; is recommended in legalese.el, but I haven't noticed a difference?
;; Maybe it's already fine?



;; Experimenting with different hiding things hideshow , which I bound
;; to C-c + is good because it lets you toggle, but it's a shitty
;; keybinding C-c C-h would be better I think,but I'm going to try out
;; hide-region for a while as it seems more flexible
(require 'hide-region)
(global-set-key (kbd "C-c C-h") 'hide-region-hide)
;; doesn't seem to play nicely with transient mark mode
(global-set-key (kbd "C-c M-h") 'hide-region-unhide)



;; could be useful
(require 'hidesearch)
(global-set-key (kbd "C-c C-s") 'hidesearch)
(global-set-key (kbd "C-c C-a") 'show-all-invisible); may keep this


;; highlight-tail is an interesting hack, but I'm not sure it adds
;; anything to the emacs experience

;; you'll want to turn off line highlightfirst ; hl-line-mode
;;(require 'highlight-tail)
;;(message "Highlight-tail loaded - now your Emacs will be even more sexy!")
;;(("#d8971d" . 0) ("#ddee1f" . 20))
;; steps 80
;; timer 0.04
;; posterior type t
;; (setq highlight-tail-colors '(("black" . 0)
;;                               ("#bc2525" . 25)
;;                               ("black" . 66)))
;; (setq highlight-tail-steps 14
;;       highlight-tail-timer 1)
;; (setq highlight-tail-posterior-type 'const)
;; (highlight-tail-mode)


;; hide copyleft licenses
(autoload 'hide-copyleft-region   "hide-copyleft" nil t)
(mapcar '(lambda (hook) (add-hook hook 'hide-copyleft-region))
        '(emacs-lisp-mode-hook lisp-mode-hook
          cperl-mode-hook perl-mode-hook c-mode-hook
          autolisp-mode-hook haskell-mode-hook scheme-mode-hook
          ruby-mode-hook python-mode-hook tuareg-mode-hook
          erlang-mode-hook))

(require 'c-eldoc)
;; Add in commonly used packages/include directorys
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ -I/usr/include `guile-config compile`") ; not sure if /usr/include is necessary but oh well
;; need to find out how to refresh when I add a new include
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


;;(global-whitespace-mode 1) ;; keep on till it gets annoying
;; interferes with erc highlighting WTF?

;; having a hell of a time with vvvv

;; (setq whitespace-style
;;       ;'(trailing lines space-before-tab indentation space-after-tab)
;;       (append whitespace-style ;;original
;;               '(newline newline-mark tabs tab-mark))
;;       ;; '(newline−mark tabs tab-mark newline
;;       ;; trailing lines space−before−tab indentation space−after−tab)
;;       ;; '(;spaces tabs newline
;;       ;;   ;;        space-mark tab-mark newline-marka
;;       ;;   trailing newline newline-mark; spaces; trailing
;;       ;;   )
;;       )
;; ;; using ¶ for newline and ▷ for tab was an idea from Xah Lee
;; ;; rest are as default
;; (setq whitespace-display-mappings
;;       '((newline−mark 10
;;                       [182 10])
;;         (tab−mark 9
;;                   [9655]
;;                   [187 9]
;;                   [92 9])
;;         (space−mark 32
;;                     [183]
;;                     [46])
;;         (space−mark 160
;;                     [164]
;;                     [95])
;;         (space−mark 2208
;;                     [2212]
;;                     [95])
;;         (space−mark 2336
;;                     [2340]
;;                     [95])
;;         (space−mark 3616
;;                     [3620]
;;                     [95])
;;         (space−mark 3872
;;                     [3876]
;;                     [95])))


;; not sure I really need quack, but then I'd need to do my
;;highlighting myself
;;http://community.schemewiki.org/?emacs-syntax-hilight



(load-file "~/src/emacs/graphviz-dot-mode.el") 


;; (add-to-list 'load-path "~/src/emacs/workgroups.el/")
;; (require 'workgroups)
;; (setq wg-prefix-key (kbd "C-c w")) ; C-z by default, maybe I should
;;                                    ; use that?
;; (workgroups-mode -1)
;; (wg-load "~/.emacs.d/wg/wg.bak")


(require 'rainbow-delimiters)
(mapcar (lambda (hook)
          (add-hook hook 'rainbow-delimiters-mode))
        '(emacs-mode-hook lisp-mode-hook scheme-mode-hook))

;;; ibuffer
;;; http://www.emacswiki.org/emacs/IbufferMode
(setq ibuffer-saved-filter-groups
      ;; or maybe just ibuffer-filter-groups?
      '(("default"
         ("dired" (mode . dired-mode))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (filename . "~/.emacs.d/ian.el")))
         ("erc" (mode . erc-mode))
         ("gnus" (or
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)
                  (name . "^\\.newsrc-dribble$")
                  (name . "^.bbdb$")
                  (name . "^\\*BBDB\\*$")))
         ("scheme" (or
                    (mode . scheme-mode)
                    (mode . geiser-repl-mode)
                    (name . "^.*[Gg]eiser.*$")
                    (name . "^\\*scheme\\*$"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
;; default filterings are as follows
;; predicate
;; content
;; size-lt
;; size-gt
;; filename
;; name
;; used-mode
;; mode

;; just follow them god damn it
(setq vc-follow-symlinks t)



;; bbdb
(add-to-list 'load-path "~/src/emacs/bbdb/lisp/")
;; add tex to tex search path
;; add info to info search path
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
;(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;;done automatically by above
(setq bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook)
(setq bbdb/news-auto-create-p 'bbdb-ignore-some-messages-hook)
(setq bbdb-ignore-some-messages-alist
      ;; doesn't work if they already have an entry?
      ;; I think I can hack around this by writing a function creating
      ;; a bbdb-always-add-addresses function that checks for invalid
      ;; names and returns nil on invalid, or 'ask if valid
      '(("From" . "INVALID\\|invalid\\|noreply\\|googlegroups\\|public.gmane.org")
        ("Newsgroups:" . "gmane.lisp.scheme.reports")))
;(setq bbdb/news-auto-create-p t)
(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-use-pop-up nil)
(setq bbdb-offer-save 'save-automagically)

(defun net-address-trash-p (net-address)
  (string-match "INVALID\\|invalid\\|noreply\\|googlegroups\\|public.gmane.org"
                net-address))


(defun my-always-add-addresses ()
  "Checks if net address is trash, if so don't add it to BBDB"
  (if (net-address-trash-p net)
      ;(progn (message "trash") nil)
      nil
    'ask))

(setq bbdb-always-add-addresses 'my-always-add-addresses)


;; M-y for yes-or-no-p
;; no idea how useful it is, because every example I'm getting is
;; y-or-no-p, not yes-or-no-p
(require 'quick-yes)

;; neil van dyke recommends these for use with quack, they should be
;; in a local keymap though

;; (global-set-key [(meta left)]      'backward-sexp)
;; (global-set-key [(meta right)]     'forward-sexp)
;; (global-set-key [(meta backspace)] 'backward-kill-sexp)
;; (global-set-key [(meta delete)]    'kill-sexp)
;; (global-set-key [(meta up)]        'backward-up-list)
;; (global-set-key [(meta down)]      'down-list)



;; whitespace (for whitespace programming language)
;; NB. Name conflicts with builtin whitespace-mode
;; (require 'whitespace-mode)
;; (add-to-list 'auto-mode-alist '("\\.ws$" . whitespace-mode))


;; I think I'm missing some stuff, but it's a small price to pay
;; considering I accidentally deleted my ian.el
;; thank god emacs made that back up the day before

(defun kill-buffer-and-file (arg)
  "Kills a buffer, and the file the BUFFER is visiting, if it is visiting one.
If called with a prefix argument, kills the current buffer.
If called without a prefix argument, reads a buffer name using ido-read-buffer.
If buffer doesn't exist, does nothing."
  (interactive "P")
  (let ((buffer (get-buffer (if arg
                                (current-buffer)
                              (ido-read-buffer "Kill buffer: ")))))
    (when buffer
      (let ((file-name (buffer-file-name buffer)))
        (kill-buffer buffer)
        (when file-name 
          (delete-file file-name))))))
(global-set-key (kbd "C-x M-k") 'kill-buffer-and-file)



(autoload 'j-mode "j-mode.el" "Major mode for J." t)
(autoload 'j-shell "j-mode.el" "Run J from emacs." t)
(setq auto-mode-alist
      (cons '("\\.ij[rstp]" . j-mode) auto-mode-alist))
(setq j-path "/home/ian/j701/bin")


;; Taken from
;; http://www.emacswiki.org/cgi-bin/wiki?BackToIndentationOrBeginning
;; I wonder why I never thought of this
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)


;; Magit
;; (add-to-list 'load-path "~/src/emacs/magit/")
(require 'magit)
(setq magit-save-some-buffers nil)
;; maybe try out `(require 'magit-topgit)` - integrates with topgit.
;(require 'magit-blame) ; not in package  I am using

;; Foreign word abbrevs
;; maybe not bother ??
;; see also:
;; https://secure.wikimedia.org/wikipedia/en/wiki/Category:French_loanwords
;; https://secure.wikimedia.org/wikipedia/en/wiki/Category:English_words_and_phrases_of_foreign_origin
(define-global-abbrev "facade" "façade")
(define-global-abbrev "naive" "naïve")
(define-global-abbrev "naivete" "naïveté")
(define-global-abbrev "touche" "touché")


;; systemd
(add-to-list 'auto-mode-alist '("\\.service$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.unit$" . conf-mode))
;; there are others, but this is fine for now


(require 'srfi)


;;; Emacs Database
;; (add-to-list 'load-path "~/src/elisp/edb-1.31/lisp/")
;; (require 'database)
;; (defun my-dired-edb-interact ()
;;   (interactive)
;;   (let ((filename (dired-get-filename)))
;;     (if (string-match "[.]edb$" filename)
;;         (edb-interact filename nil)
;;       (db-find-file filename))))



;; markerpen
(require 'markerpen)
;; Remember you can use prefix args to select pen
;; C-M-, markerpen-mark-region
;; C-M-' markerpen-clear-all-marks


(require 'ido-hacks) ;; OMFG
(ido-hacks-mode t)

;;; games
(require 'tetris)
(require 'gnugo)


(add-to-list 'auto-mode-alist '("\\rfc[0-9][0-9][0-9][0-9].txt$" . rfcview-mode))


(define-global-abbrev "blase" "blasé")


(require 'typing)


(put 'dired-find-alternate-file 'disabled nil)

;; gists
;; elpa version isn't working for me for whatever reason
;;(setq gist-use-curl t)
;;(load-file "~/src/emacs/gist.el/gist.el")
(add-to-list 'load-path "~/src/emacs/tabulated-list.el/")
(add-to-list 'load-path "~/src/emacs/logito/")
(add-to-list 'load-path "~/src/emacs/pcache/")
(add-to-list 'load-path "~/src/emacs/gh.el/")
(add-to-list 'load-path "~/src/emacs/gist.el/")
(require 'gist)


(setq flyspell-use-meta-tab nil);; isn't working
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mode-map (kbd "M-TAB") nil)))

(add-to-list 'load-path "/home/ian/src/emacs/hideshow-org/")
(require 'hideshow-org)
(global-set-key (kbd "C-c f") 'hs-org/minor-mode)
; hs-org/minor-mode


(add-hook 'java-mode-hook (lambda () (c-set-style "java")))
(add-hook 'ibuffer-hook (lambda () (local-unset-key "C-x C-f")))


;;; chess
(add-to-list 'load-path "~/src/emacs/emacs-chess/")
(require 'chess)
(setq chess-default-engine 'chess-gnuchess)
;; make it smaller by default
;; turn off sounds
;; open in same frame

(defun iota (n) (number-sequence 0 (- n 1)))


;; thanks pjb
(defun just-one-line ()
  (interactive)
  (flush-lines "^$"))
;; starter kit rebinds it
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)


;; no more annoying behavour in paredit after #
;; need to figure this out better for bytevectors etc.
;; may just set it to (lambda (a b) nil)
;;(setq paredit-space-for-delimiter-predicates nil) (not)
(setq paredit-space-for-delimiter-predicates
      (list (lambda (endp delimiter)
              ;; for some reason it is useing ( as the delimiter
              ;;(not (eql delimiter ?#))
              ;; 40 and 91 are ( and [
              (not (member delimiter '(40 91 ?#))))))
;; (eval-after-load "paredit"
;;   '(add-to-list 'paredit-space-for-delimiter-predicates
;;                 (lambda (endp delimiter)
;;                  (not (eql delimiter ?#)))))


;; I get errors for this, but I'm not sure what the deal is yet

;; (add-to-list 'load-path "~/src/emacs/ido-ubiquitous/")
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous t)

(ido-everywhere 1)

(add-to-list 'load-path "~/src/emacs/multi-web-mode/")
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)

(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))

(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


(setq prolog-program-name "gprolog")



(setq tramp-auto-save-directory "/home/ian/.emacs.d/trampdir/")

(setq ido-auto-merge-work-directories-length -1)

(setq sql-sqlite-program "sqlite3")




;;; fade-out-kill-buffer.el -- fade to black when killing buffer
;; Ryan Yeske 20051013
;; modified by tali713 http://paste.lisp.org/display/131107
(defvar fade-out-tick .05)
(defun fade-out-kill-buffer (buffer)
  (interactive `(,(ido-completing-read "Fade kill buffer: "
                                       (remove-if (lambda (name)
                                                    (string-match "^ " name))
                                                  (map-buffer (buffer-name)
                                                    (buffer-list))))))
  (with-current-buffer buffer
    (let ((str (buffer-string)))
      (when (kill-buffer buffer)
        (with-temp-buffer
          (insert str)
          (switch-to-buffer (current-buffer))
          (goto-char (point-min))
          (setq cursor-type nil)
          (dotimes (i 20)
            (put-text-property (point-min) (point-max)
                               'face (list :foreground 
                                           (format "gray%d"
                                                  ;(* 5  (1+ i))))) ; for white background
                                                   (- 100 (* 5 (1+ i))))))
            (sit-for 0)
            (sleep-for fade-out-tick)))))))
;;(global-set-key (kbd "C-x k") 'fade-out-kill-buffer)
;;(global-set-key (kbd "C-c k") 'fade-out-kill-buffer)


(setq ispell-dictionary "english")


(require 'htmlfontify)
(defun html-entity-encode-region (start end)
  ;; Thanks to fledermaus for pointing out the functions below, so I
  ;; could write this one
  (interactive "r")
  (narrow-to-region start end)
  (hfy-html-enkludge-buffer)
  (hfy-html-dekludge-buffer)
  (widen))


;;(require 'ess)
;; not sure if necessary -- maybe reinstall ess from elpa


;; stolen from forcer
(defun google (what)
  "Use google to search for WHAT."
  (interactive "sSearch: ")
  (browse-url (format "http://www.google.co.uk/search?q=%s" what)))


(define-global-abbrev "unhygeinic" "unhygienic")
(define-global-abbrev "hygeinic" "hygienic")
(define-global-abbrev "sequiter" "sequitur") ; bah, that - gets in the
                                        ; way of the abbrevs working right

;; Try and fix my C-x RET

;; <legumbre> ,dk C-x RET f
;; <fsbot> set-buffer-file-coding-system is an interactive compiled Lisp function
;; <fsbot> in `mule.el'.
;; <fsbot> It is bound to C-x RET f, <menu-bar> <options> <mule>

;;; Clippy
;; (add-to-list 'load-path "/home/ian/src/emacs/clippy.el/")
;; (require 'clippy)


;;;; Org Mode
(setq load-path (cons "~/src/emacs/org-mode/lisp/" load-path))
;(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
(require 'org-install)
;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(setq org-log-done t)
(setq org-agenda-files (list "~/org/notes.org"
                             "~/org/appointments.org"
                             "~/org/habits.org"
                             "~/org/coursera.org"
                             ))
(setq org-habit-show-habits-only-for-today nil ) ; did I turn habit on
                                        ; with customize?
;; I think either org mode or emacs starter kit changes this setting :(
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
        ;; bugs
        (sequence "FOUND" "REPORTED" "|" "FIXED" "ACCEPTED" )
        ))
(setq org-default-notes-file "~/org/notes.org")

;; org contrib
;; if this doesn't work out try deft (see bucket.org)
(setq load-path (cons "~/src/emacs/org-mode/contrib/lisp/" load-path))
(require 'org-velocity)
(setq org-velocity-bucket "~/org/bucket.org")
(global-set-key (kbd "M-N") 'org-velocity-read)
;; site recommend C-c v, use that?

(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)
   (emacs-lisp . t)
   (ruby . t)
   (python . t)))


(add-hook 'message-mode 'turn-on-orgstruct)
(add-hook 'message-mode 'turn-on-orgstruct++)

(setq org-src-window-setup 'other-window)
(setq org-src-fontify-natively t)

;; see (info "(org) Breaking down tasks")
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;;;; Erc

(setq erc-nick "ijp")
(setq erc-save-buffer-on-part t)
(add-hook 'erc-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-l")
                           'erc-save-buffer-in-logs)))
(add-hook 'erc-mode-hook 'abbrev-mode)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#scheme" "#guile"
         "#haskell" "#haskell.jp" "#racket"
         )
        ("irc2.2ch.net" "#japanese") ; server uses the  ISO-2022-JP encoding
        ;- except for the #japanese channel which uses utf-8. I need
        ;to make the erc-server-coding-system local with make-local-variable
        ("irc.rizon.net" "#ajatt")
        ))


(defvar my-irc-servers
  '("irc.freenode.net"
    "irc.rizon.net"
    "irc2.2ch.net"))

(defun my-erc-start ()
  (interactive)
  (save-current-buffer
    (erc :server "irc.freenode.net" :port "6667" :nick "ijp")
    (erc :server "irc.rizon.net" :port "6667" :nick "ijp")
    (erc :server "irc2.2ch.net" :port "6667" :nick "ijp")
    ;;(message "TODO: setup for irc.2ch.net")
    (set-buffer "irc2.2ch.net:6667")
    (set (make-local-variable 'erc-server-coding-system) '(iso-2022-jp . undecided))))

(defun my-erc-quit-server ()
  (interactive)
  (save-current-buffer
    (dolist (server my-irc-servers)
      (set-buffer (concat server ":6667"))
      (erc-quit-server nil))))

;; Note, this needs color.el which was actually added in emacs 24
(add-to-list 'load-path "/home/ian/src/emacs/erc-hl-nicks/")
(require 'erc-hl-nicks)

(setq erc-quit-reason 'erc-quit-reason-normal)

(setq erc-kill-buffer-on-part t)
(defun my-erc-quit (s)
  (or s "The garbage collector got me"))
(defun my-erc-part (s)
  (or s "(prompt (begin (control f (f 0) (f 0)) (control f (f 0) (f 0))))"))
(setq erc-part-reason 'my-erc-part)
(setq erc-quit-reason 'my-erc-quit)

(erc-keep-place-mode 1)

(defun erc-cmd-IGNORE (&optional user duration)
  ;; Duration is in minutes.
  "Ignore USER.  This should be a regexp matching nick!user@host.
If no USER argument is specified, list the contents of `erc-ignore-list'."
  (if user
      (let ((quoted (regexp-quote user)))
	(when (and (not (string= user quoted))
		   (y-or-n-p (format "Use regexp-quoted form (%s) instead? "
				     quoted)))
	  (setq user quoted))
	(erc-display-line
	 (erc-make-notice (format "Now ignoring %s" user))
	 'active)
	(erc-with-server-buffer (add-to-list 'erc-ignore-list user))
        (if duration
            (run-at-time (* 60 (timer-duration duration)) nil 'my-unignore user)))
    (if (null (erc-with-server-buffer erc-ignore-list))
	(erc-display-line (erc-make-notice "Ignore list is empty") 'active)
      (erc-display-line (erc-make-notice "Ignore list:") 'active)
      (mapc #'(lambda (item)
		(erc-display-line (erc-make-notice item)
				  'active))
	    (erc-with-server-buffer erc-ignore-list))))
  t)

(defun my-unignore (user)
  (interactive)
  (and (erc-cmd-UNIGNORE user)
       (message (format "Now unignoring %s"))))

(setq erc-join-buffer 'bury)
(setq erc-track-shorten-aggressively 'max)
;; ^^ problematic when there is only one channel, hmm


;; ta offby1
;; modified from something from bpt
(defun erc-rank-stalkers ()
  "See who is in the same IRC channels as me."
  (interactive)
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (buf (erc-channel-list nil))
      (message "%s..." (buffer-name buf))
      (dolist (nick (mapcar (lambda (thing) (aref (car thing) 1))
                            (with-current-buffer buf (erc-get-channel-user-list))))
        (when (not (equal nick erc-nick))
          (puthash nick (cons (buffer-name buf)
                              (gethash nick tbl '()))
                   tbl))))
    (with-output-to-temp-buffer
        "*IRC stalkers*"
      (let ((list '()))
        (maphash (lambda (k v) (push (cons k v) list)) tbl)

        ;; Sort the alist by number of channels.  Be nice if I could
        ;; break ties by noting how recently someone has spoken --
        ;; more recent would bring them to the front of the list.
        (setq list (sort* list '> :key (lambda (thing)
                                         (length (cdr thing)))))

        (princ (format "%-15s %s %s\n" "Nick" "channels in common with" erc-nick))
        (princ "----------------------------------\n")
        (let ((count 1))
          (while (and (not (null list)) (< count 20))
            (princ (let ((x (pop list)))
                     (format "%-15s %d %s\n"
                             (car x)
                             (length (cdr x))
                             (sort (cdr x) 'string-lessp))))
            (incf count)))))))

(add-to-list 'load-path "/home/ian/src/emacs/erc-shoot")
(require 'erc-shoot)


;; Newticker
(defun read-sexp-from-file (filename)
  "reads one sexp from a file"
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

(require 'newsticker)
(setq newsticker-frontend 'newsticker-plainview)
(setq newsticker-retrieval-interval (* 2 60 60))
(setq newsticker-url-list-defaults nil)
(setq newsticker-url-list (read-sexp-from-file (concat user-emacs-directory "feeds")))
(global-set-key (kbd "C-c C-r") 'newsticker-treeview)
(setq newsticker-automatically-mark-items-as-old nil)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
