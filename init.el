(add-to-list 'load-path "~/src/emacs/use-package")
(require 'use-package)
(require 'bind-key)

;; Load up ELPA, the package manager


(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(setq autoload-file (concat user-emacs-directory "loaddefs.el"))
(setq package-user-dir (concat user-emacs-directory "elpa"))
(setq custom-file (concat user-emacs-directory "custom.el"))

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'ffap)
(require 'ansi-color)
(require 'recentf)

;; Load up starter kit customizations
(add-to-list 'load-path (concat user-emacs-directory "esk-remnants"))
(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-lisp)

(load custom-file 'noerror)

;;; init.el ends here
(setq disabled-command-function nil) ; handle all disabled commands -  thanks YoungFrog

(add-to-list 'default-frame-alist '(font . "Inconsolata-10"))
(delete-selection-mode t)
(menu-bar-mode t)
(column-number-mode t)
(setq sentence-end-double-space nil)
(add-to-list 'load-path "~/src/emacs/")

(let ((warning-minimum-level :error))
  (load-theme 'monokai t))

(defun read-sexp-from-file (filename)
  "reads one sexp from a file"
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

;; honestly, when was the last time anyone ever turned on overwrite
;; mode _on purpose_.
(bind-key "<insert>" 'ignore)

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(set-input-method "TeX")

;; Thanks forcer.
(bind-key "C-x 8 p" 'fc/unicode-info-at-point)
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

;; less clutter on startup
;; see http://bzg.fr/emacs-strip-tease.html
(setq initial-scratch-message "")
(setq inhibit-startup-echo-area-message "ian")
;(eval '(setq inhibit-startup-echo-area-message "ian")) ; if byte-compiling
; (toggle-frame-fullscreen) ; emacs > 24.4 , I think
; (menu-bar-mode 1) ; undecided
;; TODO: add his hidden mode-line hack

;; geiser
(use-package geiser
  :config
  (setq geiser-active-implementations '(guile)
        geiser-guile-load-init-file-p t
        geiser-implementations-alist '(((regexp "\\.ss$")  racket)
                                       ((regexp "\\.rkt$") racket)
                                       ((regexp ".") guile)))
  (add-hook 'geiser-repl-mode-hook #'turn-on-paredit)
  (add-hook 'geiser-repl-mode-hook #'turn-on-company-mode))

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

(bind-key "\C-c +" 'hs-toggle-hiding)
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

(use-package company
  :commands company-mode
  :init
  (defun turn-on-company-mode ()
    (company-mode +1)))

(add-hook 'scheme-mode-hook #'turn-on-company-mode)


;; Commented out, until I can figure out how to turn this off for
;; certain git repos
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; eshell
(use-package eshell
  :commands eshell
  :config
  (setq eshell-cmpl-cycle-completions t ; nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

  (defun eshell/clear ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (defun eshell/cds ()
    "Change directory to the project's root."
    (eshell/cd (locate-dominating-file default-directory "src")))

  (defun eshell/find (dir &rest opts)
    (find-dired dir (mapconcat 'identity opts " "))))

(use-package esh-module
  :config
  (add-to-list 'eshell-modules-list 'eshell-smart))

(use-package em-term
  :config
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "tail"))

(use-package em-cmpl
  :config
  (add-to-list 'eshell-command-completions-alist
               '("gunzip" . "gz\\'"))
  (add-to-list 'eshell-command-completions-alist
               '("tar" . "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'")))

;; yasnippet
(yas-global-mode 1)
(bind-key "C-c y" 'yas/expand)

;; Browse
(bind-key "C-c o" 'browse-url)

;;; Diminish
(require 'diminish)

(use-package lisp-mode
  :init
  (defun my-rename-elisp-mode ()
    (setq mode-name "elisp"))
  (add-hook 'emacs-lisp-mode-hook 'my-rename-elisp-mode))

;; Auto inserts
;; Doing it this way sucks, next time use define-auto-insert

(setq auto-insert-query nil)              ; stop asking already, jeez
(setq auto-insert-directory (concat user-emacs-directory "inserts"))
(auto-insert-mode 1)

;; Tea Time
(use-package tea-time
  :load-path "~/src/emacs/tea-time"
  :bind ("C-c t" . tea-time)
  :config
  (setq tea-time-sound "/usr/share/sounds/freedesktop/stereo/complete.oga"))

(bind-key "C-x 9" 'kill-buffer-and-window)

(bind-key "C-x TAB" 'indent-rigidly)

(bind-key "C-c q" 'refill-mode)

(use-package dired
  :commands dired-mode
  :config
  (use-package dired-x)
  (use-package dired-aux))

(use-package c-eldoc
  :commands c-turn-on-eldoc-mode
  :init (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  :config
  (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ -I/usr/include `guile-config compile`"))

;; ----- NEVER FORGET -----
;; I think I'm missing some stuff, but it's a small price to pay
;; considering I accidentally deleted my ian.el
;; thank god emacs made that back up the day before
;; ------------------------

(use-package hideshow-org
  :load-path "~/src/emacs/hideshow-org/"
  :bind ("C-c f" . hs-org/minor-mode))

(use-package paredit
  :diminish (paredit-mode . "Ped"))


;;;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

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
(bind-key "C-x M-k" 'kill-buffer-and-file)

;; Taken from
;; http://www.emacswiki.org/cgi-bin/wiki?BackToIndentationOrBeginning
;; I wonder why I never thought of this
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(bind-key "<home>" 'back-to-indentation-or-beginning)

(use-package htmlfontify
  :commands hfy-html-enkludge-buffer
  :init
  (defun html-entity-encode-region (start end)
    ;; Thanks to fledermaus for pointing out the functions below, so I
    ;; could write this one
    (interactive "r")
    (narrow-to-region start end)
    (hfy-html-enkludge-buffer)
    (hfy-html-dekludge-buffer)
    (widen)))

;;;; Misc
(setenv "PAGER" "cat")
(setenv "NODE_NO_READLINE" "1")

(set-register ?g `(file . ,(concat user-emacs-directory "fools")))
(set-register ?e `(file . ,(concat user-emacs-directory "init.el")))
(set-register ?b '(file . "~/org/2015-books.org"))
;; might be useful, means I can just use C-SPC after C-u C-SPACE,
;; rather than having to keep using a prefix
(setq user-mail-address "ianprice90@gmail.com")
(setq set-mark-command-repeat-pop t)
(setq prolog-program-name "gprolog")
(setq sql-sqlite-program "sqlite3")
(setq vc-follow-symlinks t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq c-default-style "linux")
(add-hook 'java-mode-hook (lambda () (c-set-style "java")))
(setq yank-pop-change-selection t) ; suggest adding to better-defaults.el
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-strip-common-suffix nil)
(setq auto-save-include-big-deletions t)

(use-package legalese
  :commands legalese
  :config
  (setq legalese-default-license 'bsd))

(use-package hide-copyleft
  :commands hide-copyleft-region
  :init
  (add-hook 'prog-mode-hook 'hide-copyleft-region))

(require 'pretty-mode)
(global-pretty-mode 1)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(add-hook 'css-mode-hook 'rainbow-mode)

(setq auto-mode-alist
      (append
       '(("\\.sls$" . scheme-mode)
         ("\\.sps$" . scheme-mode)
         ("\\.service$" . conf-mode)
         ("\\.unit$" . conf-mode)
         ("\\.toml$" . conf-mode)
         ("\\rfc[0-9][0-9][0-9][0-9].txt$" . rfcview-mode) ; in emacs-goodies
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.m$" . octave-mode)
         ("\\.css$" . css-mode)
         ("\\.ya?ml$" . yaml-mode)
         ("\\.rb$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("\\.js\\(on\\)?$" . js2-mode)
         ("\\.xml$" . nxml-mode))
       auto-mode-alist))

(bind-key "C-x M-d" 'fixup-whitespace)

;;;; Ibuffer
(use-package ibuffer
  :init
  (defun my-ibuffer-switch-to-default ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-switch-to-default)
  :config
  (setq ibuffer-saved-filter-groups
        ;; or maybe just ibuffer-filter-groups?
        '(("default"
           ("dired" (mode . dired-mode))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (filename . "~/.emacs.d/init.el")))
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
  (unbind-key "C-x C-f" ibuffer-mode-map))

;;;; Tramp
(use-package tramp
  :init (setq tramp-ssh-controlmaster-options nil) ;; FIXES hanging tramp
  :config
  (setq tramp-auto-save-directory "/home/ian/.emacs.d/trampdir/"))

;;;; Ido
(require 'ido-hacks) ;; OMFG
(ido-hacks-mode t)
(setq ido-auto-merge-work-directories-length -1)

;;;; Spellcheck
(setq ispell-dictionary "british")
(add-hook 'text-mode ' flyspell-mode)
;; TODO: erc spelling
(setq flyspell-use-meta-tab nil)
;; idea from http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(bind-key "C-." 'flyspell-auto-correct-word)

;;;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-save-some-buffers nil)

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
(setq yow-file "~/lib/homie-yow/homie.lines")

;;;; Abbrevs
(read-abbrev-file "~/.emacs.d/misspelling_abbrevs")

;;;; Org Mode
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-default-notes-file (concat user-emacs-directory "capture.org")
        org-log-done 'time
        org-agenda-files (list "~/org/appointments.org"
                               "~/org/university.org")
        org-habit-show-habits-only-for-today nil
        org-todo-keywords '((sequence "TODO" "|" "DONE")
                            ;; bugs
                            (sequence "FOUND" "REPORTED" "|" "FIXED" "ACCEPTED" ))
        org-src-window-setup 'other-window
        org-src-fontify-natively t
        org-completion-use-ido t
        org-catch-invisible-edits 'smart)

  ;; see (info "(org) Breaking down tasks")
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; I think either org mode or emacs starter kit changes this setting :(
  (defun turn-off-truncate-lines ()
    (toggle-truncate-lines -1))
  (add-hook 'org-mode-hook 'turn-off-truncate-lines)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (emacs-lisp . t)
     (ruby . t)
     (python . t)
     (sh . t)))

  (use-package org-velocity
    :load-path "~/src/emacs/org-velocity/"
    :bind ("M-N" . org-velocity-read)
    :config
    (setq org-velocity-bucket "~/org/bucket.org")
    (setq org-velocity-edit-entry t)))

;;;; Erc
(use-package erc
  :defer t
  :init
  (add-hook 'erc-mode-hook 'abbrev-mode)

  (setq erc-nick "ijp")

  (defvar rizon-server "irc.x2x.cc"
    "Which rizon server to connect to.
irc.rizon.net operates a round robin, where it will connect you
to one of a bunch of other servers. This is a pain with autojoin,
so just always connect to one specifically. See
http://wiki.rizon.net/index.php?title=Servers for a list.")

  (defvar my-irc-servers `("irc.freenode.net" ,rizon-server "irc.juggler.jp"))

  (defun my-erc-start ()
    (interactive)
    (save-current-buffer
      (dolist (server my-irc-servers)
        (erc :server server :port "6667" :nick erc-nick))))

  :config
  (bind-key "C-c C-l" 'erc-save-buffer-in-logs erc-mode-map)

  (setq erc-autojoin-channels-alist
        `(("freenode.net" "#emacs" "#scheme" "#guile" "#racket"
           "##juggling" "#coq" "#guix")
          ("juggler.jp" "#japanese" "#おもしろネタ速報" "#漫画雑談")
          (,rizon-server "#ajatt"))
        erc-fool-highlight-type 'all
        erc-fools (read-sexp-from-file (concat user-emacs-directory "fools"))
        erc-join-buffer 'bury
        erc-kill-buffer-on-part t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-part-reason 'my-erc-quit
        erc-quit-reason 'my-erc-quit
        erc-save-buffer-on-part t
        ;; erc-track-exclude is available as a NO-distraction alternative
        erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "333" "353"
                                  "MODE" "324" "328" "329" "901" "332" "366"
                                  "333" "353" "477")
        erc-track-faces-priority-list '(erc-current-nick-face erc-keyword-face)
        erc-track-priority-faces-only '("#haskell") ; They talk too damn much, and worst of all, it's on topic :/
        erc-track-shorten-aggressively 'max
        erc-track-shorten-start 2       ; fixes the one channel issue
        erc-user-full-name "Ian Price"
        my-erc-quit-reasons (read-sexp-from-file (concat user-emacs-directory "quit_messages"))
        pcomplete-cycle-cutoff-length nil)

  (setq-default erc-ignore-list ;; TODO: consider using erc-ignore-reply-list
                '("jdoles" ;; congrats to jdoles on being the first to make this list
                  "[gG]uest[^!]*!.*" "average"))

  (erc-autojoin-mode 1)
  (erc-keep-place-mode 1)
  (erc-truncate-mode 1)

  (defun my-erc-coding-hook (server nick)
    (when (string-match "juggler.jp" server)
      (set (make-local-variable 'erc-server-coding-system)
           '(iso-2022-jp . undecided))))

  ;; This must come _before_ autojoin in the hook, so pushing it on after.
  (add-hook 'erc-after-connect 'my-erc-coding-hook)
  (add-to-list 'erc-complete-functions 'erc-pcomplete)

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

  (defun erc-ctcp-query-VERSION (proc nick login host to msg)
    "Respond to a CTCP VERSION query."
    (unless erc-disable-ctcp-replies
      (erc-send-ctcp-notice nick "VERSION \C-btelnet\C-b"))
    nil)

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
                (read
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

  (defun my-erc-quit (s)
    (or s (concat "brb " (aref my-erc-quit-reasons (random (length my-erc-quit-reasons))))))

  (defun my-erc-quit-server ()
    (interactive)
    (save-current-buffer
      (dolist (server my-irc-servers)
        (set-buffer (concat server ":6667"))
        (erc-quit-server nil))))

  (use-package erc-hl-nicks
    :config
    ;; works around erc-hl-nicks for erc-track-faces-priority-list
    (add-to-list 'erc-hl-nicks-skip-faces "erc-current-nick-face"))

  (use-package erc-shoot
    :load-path "/home/ian/src/emacs/erc-shoot")

  ;; match-mode is delayed to here until I figure out the interaction
  ;; with erc-hl-nicks
  (erc-match-mode 1)
  (set-face-attribute 'erc-fool-face nil :foreground "orange red"))

;;;; Elfeed
(use-package elfeed
  :bind ("C-c w" . elfeed)
  :init
  (defvar my-feeds-file (concat user-emacs-directory "feeds"))
  (set-register ?f (cons 'file my-feeds-file))
  :config
  (setq elfeed-feeds (read-sexp-from-file my-feeds-file)))

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
;; cpdt
(setq coq-prog-args
      '("-emacs" ; needed?
        "-I" "/home/ian/lib/cpdt/src"
        "-R" "/home/ian/src/coq/ynot/src/coq/" "Ynot"))

;; Ace Jump Mode
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;; More key crap
(bind-key "C-c i" 'imenu)
(bind-key "C-c d" 'diff-buffer-with-file)
(bind-key "C-x 8 \" RET" (lambda () (interactive) (insert "̈")))

;; Color Identifiers
(use-package color-identifiers-mode
  :load-path "~/src/emacs/color-identifiers-mode/"
  :commands color-identifiers-mode
  :init
  (defun turn-on-color-identifiers ()
    (color-identifiers-mode 1))
  (add-hook 'prog-mode-hook 'turn-on-color-identifiers))

;; snakehump
(use-package snakehump
  :bind (("C-}" . snakehump-next-at-point)
         ("C-{" . snakehump-prev-at-point)))

;; indirect region
;; https://emacs.stackexchange.com/questions/10104/narrow-to-previous-restriction
(defun indirect-region (beg end name)
  "Open a new named indirect buffer of the current buffer,
narrowed to region [BEG, END]."
  (interactive "r\nsname of narrowed buffer: ")
  (let ((new-buff
         (make-indirect-buffer (current-buffer)
                               (generate-new-buffer-name name)
                               t)))
    (switch-to-buffer new-buff nil t)
    (narrow-to-region beg end)))

;; ws-butler
(use-package ws-butler
  :load-path "~/src/emacs/ws-butler/"
  :commands ws-butler-mode
  :init (add-hook 'prog-mode-hook 'ws-butler-mode))

;; scpaste
(use-package scpaste
  :commands scpaste
  :init
  (setq scpaste-http-destination "http://shift-reset.com/pastes"
        scpaste-scp-destination "ec2-user@shift-reset.com:pastes"
        scpaste-user-name "ijp"
        scpaste-user-address "http://shift-reset.com/"))
