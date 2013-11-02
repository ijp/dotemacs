;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;;; init.el ends here
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-to-list 'default-frame-alist '(font . "Inconsolata-10"))
(delete-selection-mode t)
(menu-bar-mode t)
(column-number-mode t)
(add-to-list 'load-path "~/src/emacs/")

(load-theme 'monokai t)

(defun read-sexp-from-file (filename)
  "reads one sexp from a file"
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

;; honestly, when was the last time anyone ever turned on overwrite
;; mode _on purpose_.
(global-set-key (kbd "<insert>") 'ignore)

(require 'mwe-log-commands)

(require 'misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(set-input-method "TeX")

;; Thanks forcer.
(global-set-key (kbd "C-x 8 p") 'fc/unicode-info-at-point)
(defun fc/unicode-info-at-point (&optional do-kill)
  "Display the unicode name of the character at point."
  (interactive "P")
  (let ((char-code (elt (thing-at-point 'char) 0))
        name)
    (setq name (get-char-code-property char-code 'name))
    (when (or (not name)
              (= ?< (elt name 0)))
      (setq name (get-char-code-property char-code 'old-name)))
    (when do-kill
      (kill-new name))
    (message "%s" name)))

;; geiser
;(load-file "~/src/emacs/geiser/elisp/geiser.el")
(load "/home/ian/src/emacs/geiser/build/elisp/geiser-load")
(setq geiser-active-implementations '(guile racket)); forget about racket for now :)
(add-hook 'geiser-repl-mode-hook #'turn-on-paredit)
(setq geiser-guile-load-init-file-p t)
(eval-after-load "geiser-impl"
'(add-to-list 'geiser-implementations-alist
             '((regexp ".") guile))) ; use guile otherwise

;; web stuff
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

(require 'pabbrev)

(global-set-key (kbd "\C-c +") 'hs-toggle-hiding)
(add-hook 'prog-mode-hook 'hs-minor-mode)
;; I wish there was a programming "super mode" i could hook into
;; from http://www.emacswiki.org/emacs/HideShow
(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

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

(add-to-list 'load-path "/home/ian/src/emacs/company")
(autoload 'company-mode "company" nil t)

(defun turn-on-company-mode ()
  (company-mode +1))
(add-hook 'scheme-mode-hook #'turn-on-company-mode)
(add-hook 'geiser-repl-mode-hook #'turn-on-company-mode)


;; Commented out, until I can figure out how to turn this off for
;; certain git repos
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)


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

;;; Diminish
(require 'diminish)
(eval-after-load 'paredit
'(diminish 'paredit-mode "Ped"))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq mode-name "elisp")))

;; Auto inserts
;; Doing it this way sucks, next time use define-auto-insert

(setq auto-insert-query nil)              ; stop asking already, jeez
(setq auto-insert-directory (concat dotfiles-dir "inserts"))
(auto-insert-mode 1)
(setq auto-insert-alist (cons '("\\.sls$" . scheme-library) auto-insert-alist))

;; Tea Time
(add-to-list 'load-path "~/src/emacs/tea-time")
(require 'tea-time)
; drip-sounce /usr/share/sounds/gnome/default/alerts/drip.ogg 
; freedesktop complete /usr/share/sounds/freedesktop/stereo/complete.oga
(setq tea-time-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")

(define-key global-map (kbd "C-c t") 'tea-time)

(global-set-key (kbd "C-x 9") 'kill-buffer-and-window)

(global-set-key (kbd "C-x TAB") 'indent-rigidly)

(global-set-key (kbd "C-c q") 'refill-mode)

(require 'dired-x)

(require 'c-eldoc)
;; Add in commonly used packages/include directorys
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ -I/usr/include `guile-config compile`") ; not sure if /usr/include is necessary but oh well
;; need to find out how to refresh when I add a new include
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; ----- NEVER FORGET -----
;; I think I'm missing some stuff, but it's a small price to pay
;; considering I accidentally deleted my ian.el
;; thank god emacs made that back up the day before
;; ------------------------

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

(add-to-list 'load-path "/home/ian/src/emacs/hideshow-org/")
(require 'hideshow-org)
(global-set-key (kbd "C-c f") 'hs-org/minor-mode)
; hs-org/minor-mode

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

(add-to-list 'load-path "~/src/emacs/multi-web-mode/")
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)

(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))

(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


;;;; Haskell
(load "~/src/emacs/haskell-mode-2.8.0/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;;; Tuareg
(add-to-list 'load-path  "~/src/emacs/tuareg-2.0.1")
(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml Debugger" t)

;;;; Functions
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
	(progn (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

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

;; Taken from
;; http://www.emacswiki.org/cgi-bin/wiki?BackToIndentationOrBeginning
;; I wonder why I never thought of this
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)

(require 'htmlfontify)
(defun html-entity-encode-region (start end)
  ;; Thanks to fledermaus for pointing out the functions below, so I
  ;; could write this one
  (interactive "r")
  (narrow-to-region start end)
  (hfy-html-enkludge-buffer)
  (hfy-html-dekludge-buffer)
  (widen))

;;;; Misc
(setenv "PAGER" "cat")
(setenv "NODE_NO_READLINE" "1")

(set-register ?e `(file . ,(concat dotfiles-dir "init.el")))
;; might be useful, means I can just use C-SPC after C-u C-SPACE,
;; rather than having to keep using a prefix
(setq user-mail-address "ianprice90@googlemail.com")
(setq set-mark-command-repeat-pop t)
(setq prolog-program-name "gprolog")
(setq sql-sqlite-program "sqlite3")
(setq vc-follow-symlinks t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq c-default-style "linux")
(add-hook 'java-mode-hook (lambda () (c-set-style "java")))
(put 'dired-find-alternate-file 'disabled nil)
(setq yank-pop-change-selection t) ; suggest adding to better-defaults.el
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-strip-common-suffix nil)

(require 'legalese)
(setq legalese-default-license 'bsd)
(autoload 'hide-copyleft-region   "hide-copyleft" nil t)
(add-hook 'prog-mode-hook 'hide-copyleft-region)

(require 'pretty-mode)
(global-pretty-mode 1)

(add-to-list 'load-path "/home/ian/src/emacs/ioccur/")
(require 'ioccur)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(add-to-list 'load-path "~/src/emacs/jd-el")
(require 'rainbow-mode)
(autoload 'rainbow-mode "rainbow-mode")
(add-hook 'css-mode-hook 'rainbow-mode)

(setq auto-mode-alist
      (append
       '(("\\.sls$" . scheme-mode)
         ("\\.sps$" . scheme-mode)
         ("\\.service$" . conf-mode)
         ("\\.unit$" . conf-mode)
         ("\\rfc[0-9][0-9][0-9][0-9].txt$" . rfcview-mode) ; in emacs-goodies
         ("\\.m$" . octave-mode))
       auto-mode-alist))

(global-set-key (kbd "C-x M-d") 'fixup-whitespace)

;;;; Ibuffer
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

(add-hook 'ibuffer-mode-hook (lambda () (local-unset-key (kbd "C-x C-f"))))

;;;; Tramp
(require 'tramp)
(setq tramp-auto-save-directory "/home/ian/.emacs.d/trampdir/")

;;;; Ido
(global-set-key (kbd "C-c TAB") 'ido-imenu)
(require 'ido-hacks) ;; OMFG
(ido-hacks-mode t)
(setq ido-auto-merge-work-directories-length -1)

;;;; Spellcheck
;; Why "british" is not working, I have no idea, probably the
;; dictionary files changed how they do naming
(add-to-list 'ispell-dictionary-alist
             '("en_GB" "[A-Za-z]" "[^A-Za-z]" "[']" nil
               ("-B")
               nil iso-8859-1))
(setq ispell-dictionary "en_GB")
(setq flyspell-use-meta-tab nil)
;; idea from http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(global-set-key (kbd "C-.") 'flyspell-auto-correct-word)

;;;; Magit
(add-to-list 'load-path "~/src/emacs/magit/")
(require 'magit)
(setq magit-save-some-buffers nil)
(require 'magit-blame)

;;;; Scheme
(defun my-scheme-setup-indents (list)
  (mapc (lambda (p)
          (let ((level (car p))
                (vars  (cdr p)))
            (mapc (lambda (var)
                    (put var 'scheme-indent-function level))
                  vars)))
        list))

(my-scheme-setup-indents
 '((1 with-test-prefix pass-if call-with-input-string with-syntax*
      with-code-coverage with-implicit catch call-with-prompt stream-case
      when-let syntax-parameterize syntax-parse while until)
   (2 cases test-case)))

(defun scheme-library-name ()
  "Determines the scheme library name based on the buffer name, otherwise empty string"
  (interactive)
  (let ((buffer-name (buffer-file-name))
        (name-regex ".*/\\(.*?\\)\\..*"))
    (if (string-match name-regex buffer-name)
        (match-string 1 buffer-name)
      "")))

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

(require 'srfi)

;;;; Gnus
(setq gnus-select-method '(nntp "news.btinternet.com"))
;; news.btopenworld.com was also suggested, but this seems to work fine

;;;; BBDB
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

;;;; Fun stuff
(require 'tetris)
(require 'gnugo)
(require 'typing)

(setq yow-file "~/lib/homie-yow/homie.lines")

;;;; Abbrevs

;; Misspellings file taken from
;; https://secure.wikimedia.org/wikipedia/en/wiki/Wikipedia:Lists_of_common_misspellings/For_machines
;; and the processed with
;; sed -e 's/\(.*\)->\(.*\),.*/\1->\2/' -e "/.*[-'\";][^>].*->.*/d" -e
;; 's/\(.*\)\->\(.*\)/(define-global-abbrev "\1" "\2")/'
;; .emacs.d/misspelling_abbrevs.bak > .emacs.d/misspelling_abbrevs
(read-abbrev-file "~/.emacs.d/misspelling_abbrevs")
;; other misspellings of mine
(define-global-abbrev "lsit" "list")
(define-global-abbrev "hygeinic" "hygienic")
(define-global-abbrev "unhygeinic" "unhygienic")
;; can't have "non-sequiter" because - doesn't play nicely with abbrevs
(define-global-abbrev "sequiter" "sequitur")

;; Foreign word abbrevs
(define-global-abbrev "facade" "façade")
(define-global-abbrev "naive" "naïve")
(define-global-abbrev "naivete" "naïveté")
(define-global-abbrev "touche" "touché")
(define-global-abbrev "blase" "blasé")
(define-global-abbrev "detre" "d'être")

;;;; Org Mode
(setq load-path (cons "~/src/emacs/org-mode/lisp/" load-path))
;(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
(require 'org-install)
;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-log-done 'time)
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

(erc-autojoin-mode 1)

(defvar rizon-server "irc.x2x.cc"
  "Which rizon server to connect to.
irc.rizon.net operates a round robin, where it will connect you
to one of a bunch of other servers. This is a pain with autojoin,
so just always connect to one specifically. See
http://wiki.rizon.net/index.php?title=Servers for a list.")

(setq erc-autojoin-channels-alist
      `(("freenode.net" "#emacs" "#scheme" "#guile"
         "#haskell" "#haskell.jp" "#racket" "##juggling"
         "#coq"
         )
        ("irc2.2ch.net" "#japanese" "#おもしろネタ速報" "#漫画雑談")
        (,rizon-server "#ajatt")
        ))

(defun my-erc-coding-hook (server nick)
  (when (string-match "2ch\\.net" server)
    (save-current-buffer ;; necessary?
      (set-buffer (concat server ":6667"))
      (set (make-local-variable 'erc-server-coding-system)
           '(iso-2022-jp . undecided)))))

;; This must come _before_ autojoin in the hook, so pushing it on after.
(add-hook 'erc-after-connect 'my-erc-coding-hook)

(defvar my-irc-servers
  `("irc.freenode.net"
    ,rizon-server
    "irc2.2ch.net"))

(defun my-erc-start ()
  (interactive)
  (save-current-buffer
    (dolist (server my-irc-servers)
      (erc :server server :port "6667" :nick erc-nick))))

(defun my-erc-quit-server ()
  (interactive)
  (save-current-buffer
    (dolist (server my-irc-servers)
      (set-buffer (concat server ":6667"))
      (erc-quit-server nil))))

;; Note, this needs color.el which was actually added in emacs 24
(add-to-list 'load-path "/home/ian/src/emacs/erc-hl-nicks/")
(require 'erc-hl-nicks)

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
            (run-at-time (* 60 (timer-duration duration)) nil 'erc-cmd-UNIGNORE user)))
    (if (null (erc-with-server-buffer erc-ignore-list))
	(erc-display-line (erc-make-notice "Ignore list is empty") 'active)
      (erc-display-line (erc-make-notice "Ignore list:") 'active)
      (mapc #'(lambda (item)
		(erc-display-line (erc-make-notice item)
				  'active))
	    (erc-with-server-buffer erc-ignore-list))))
  t)

(setq erc-join-buffer 'bury)
(setq erc-track-shorten-aggressively 'max)
(setq erc-track-shorten-start 2) ;; fixes the one channel issue

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

;; erc-track-exclude is available as a NO-distraction alternative
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "333" "353")
      erc-track-priority-faces-only
      ;; They talk too damn much, and worst of all, it's on topic :/
      '("#haskell")
      )

;; Taken from jlf
(defun erc-set-topic (topic)
  "Prompt for a TOPIC for the current channel."
  (interactive
   (list
    (let* ((prompt (concat "Set topic of " (erc-default-target) ": "))
	   (cur-topic
	    (when erc-channel-topic
	      (let ((ss (split-string erc-channel-topic "\C-o")))
		(cons (apply 'concat (if (cdr ss) (butlast ss) ss)) 0))))
	   (max-len
	    (ignore-errors
	      (parse-integer
	       (cdr (assoc "TOPICLEN"
			   (erc-with-server-buffer erc-server-parameters))))))
	   (new-topic nil))
      (while (or (null new-topic)
		 (and max-len
		      (> (length new-topic) max-len)
		      (prog1
			  (message "New length of %s exceeds server's maximum of %s" (length new-topic) max-len)
			;; FIXME there has to be a better approach than sleep-for, but what is it?
			(sleep-for 2))))
	(setq new-topic (read-from-minibuffer prompt (or new-topic cur-topic))))
      new-topic)))
      (let ((topic-list (split-string topic "\C-o"))) ; strip off the topic setter
	(erc-cmd-TOPIC (concat (erc-default-target) " " (car topic-list)))))

(erc-match-mode 1)
(setq erc-fools (read-sexp-from-file (concat user-emacs-directory "fools")))
(set-face-attribute 'erc-fool-face nil :foreground "orange red")
(setq erc-fool-highlight-type 'all)

(defun erc-update-mode-line-buffer (buffer)
  "Update the mode line in a single ERC buffer BUFFER."
  (with-current-buffer buffer
    (let ((spec (format-spec-make
		 ?a (erc-format-away-status)
		 ?l (erc-format-lag-time)
		 ?m (erc-format-channel-modes)
		 ?n (or (erc-current-nick) "")
		 ?N (erc-format-network)
		 ?o (or (erc-controls-strip erc-channel-topic) "") ;; <- fix here
		 ?p (erc-port-to-string erc-session-port)
		 ?s (erc-format-target-and/or-server)
		 ?S (erc-format-target-and/or-network)
		 ?t (erc-format-target)))
	  (process-status (cond ((and (erc-server-process-alive)
				      (not erc-server-connected))
				 ":connecting")
				((erc-server-process-alive)
				 "")
				(t
				 ": CLOSED")))
	  (face (cond ((eq erc-header-line-face-method nil)
		       nil)
		      ((functionp erc-header-line-face-method)
		       (funcall erc-header-line-face-method))
		      (t
		       'erc-header-line))))
      (cond ((featurep 'xemacs)
	     (setq modeline-buffer-identification
		   (list (format-spec erc-mode-line-format spec)))
	     (setq modeline-process (list process-status)))
	    (t
	     (setq mode-line-buffer-identification
		   (list (format-spec erc-mode-line-format spec)))
	     (setq mode-line-process (list process-status))))
      (when (boundp 'header-line-format)
	(let ((header (if erc-header-line-format
			  (format-spec erc-header-line-format spec)
			nil)))
	  (cond (erc-header-line-uses-tabbar-p
		 (set (make-local-variable 'tabbar--local-hlf)
		      header-line-format)
		 (kill-local-variable 'header-line-format))
		((null header)
		 (setq header-line-format nil))
		(erc-header-line-uses-help-echo-p
		 (let ((help-echo (with-temp-buffer
				    (insert header)
				    (fill-region (point-min) (point-max))
				    (buffer-string))))
		   (setq header-line-format
			 (erc-replace-regexp-in-string
			  "%"
			  "%%"
			  (if face
			      (erc-propertize header 'help-echo help-echo
					      'face face)
			    (erc-propertize header 'help-echo help-echo))))))
		(t (setq header-line-format
			 (if face
			     (erc-propertize header 'face face)
			   header)))))))
    (if (featurep 'xemacs)
	(redraw-modeline)
      (force-mode-line-update))))

(erc-truncate-mode 1)

;;;; Newticker
(require 'newsticker)
(setq newsticker-frontend 'newsticker-plainview)
(setq newsticker-retrieval-interval (* 2 60 60))
(setq newsticker-url-list-defaults nil)
(setq newsticker-url-list (read-sexp-from-file (concat user-emacs-directory "feeds")))
(global-set-key (kbd "C-c C-r") 'newsticker-treeview)
(setq newsticker-automatically-mark-items-as-old nil)

;; TZ
;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq display-time-world-list
      '(("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Europe/Istanbul" "Istanbul")
        ("Asia/Calcutta" "Bangalore")
        ("Asia/Tokyo" "Tokyo")
        ("Pacific/Auckland" "Auckland")))
;; Νøöβ§ ¢αn'τ ウИï©Øδε
;; <ijp> fledermaus: actually, it could be a fun idea for an input method
;; <fledermaus> ijp - what, random unil33t char for every key pressed?

;; proof general
(load-file "~/src/emacs/ProofGeneral-4.2/generic/proof-site.el")

;; Japanese Keyboard experiments
(global-set-key (kbd "<henkan>") (kbd "<backspace>"))
(global-set-key (kbd "<muhenkan>") 'ido-switch-buffer)
