;; ----- NEVER FORGET -----
;; I think I'm missing some stuff, but it's a small price to pay
;; considering I accidentally deleted my ian.el
;; thank god emacs made that back up the day before
;; ------------------------
(add-to-list 'load-path "~/src/emacs/use-package")
(require 'use-package)
(require 'bind-key)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'ffap)
(require 'ansi-color)
(require 'recentf)

(add-to-list 'load-path (concat user-emacs-directory "esk-remnants"))
(require 'starter-kit-defuns)
(require 'starter-kit-misc)

(add-to-list 'load-path "~/src/emacs/")
(require 'diminish)

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

(add-hook 'prog-mode-hook 'hs-minor-mode)
;; I wish there was a programming "super mode" i could hook into
;; from http://www.emacswiki.org/emacs/HideShow
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

;;;; Use-Packages

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package bbdb-com
  :load-path "~/src/emacs/bbdb/lisp/"
  :commands (bbdb bbdb-create)
  :config
  (bbdb-initialize 'gnus 'message) ; init?

  (defun net-address-trash-p (net-address)
    (string-match "INVALID\\|invalid\\|noreply\\|googlegroups\\|public.gmane.org"
                  net-address))

  (defun my-always-add-addresses ()
    "Checks if net address is trash, if so don't add it to BBDB"
    (if (net-address-trash-p net)
        nil
      'ask))

  (setq bbdb-always-add-addresses 'my-always-add-addresses
        bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
        bbdb/news-auto-create-p 'bbdb-ignore-some-messages-hook
        bbdb-north-american-phone-numbers-p nil
        bbdb-offer-save 'save-automagically
        bbdb-use-pop-up nil
        bbdb-ignore-some-messages-alist
        ;; doesn't work if they already have an entry?
        ;; I think I can hack around this by writing a function creating
        ;; a bbdb-always-add-addresses function that checks for invalid
        ;; names and returns nil on invalid, or 'ask if valid
        '(("From" . "INVALID\\|invalid\\|noreply\\|googlegroups\\|public.gmane.org")
          ("Newsgroups:" . "gmane.lisp.scheme.reports"))))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package c-eldoc
  :commands c-turn-on-eldoc-mode
  :init (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  :config
  (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ -I/usr/include `guile-config compile`"))

(use-package color-identifiers-mode
  :load-path "~/src/emacs/color-identifiers-mode/"
  :commands color-identifiers-mode
  :init
  (defun turn-on-color-identifiers ()
    (color-identifiers-mode 1))
  (add-hook 'prog-mode-hook 'turn-on-color-identifiers))

(use-package company
  :commands company-mode
  :init
  (defun turn-on-company-mode ()
    (company-mode +1)))

(use-package conf-mode
  :mode (("\\.service\\'" . conf-mode)
         ("\\.unit\\'" . conf-mode)
         ("\\.toml\\'" . conf-mode)))

(use-package dired
  :commands dired-mode
  :config
  (use-package dired-x)
  (use-package dired-aux))

(use-package elfeed
  :bind ("C-c w" . elfeed)
  :init
  (defvar my-feeds-file (concat user-emacs-directory "feeds"))
  (set-register ?f (cons 'file my-feeds-file))
  :config
  (setq elfeed-feeds (read-sexp-from-file my-feeds-file)))

(use-package em-cmpl
  :config
  (add-to-list 'eshell-command-completions-alist
               '("gunzip" . "gz\\'"))
  (add-to-list 'eshell-command-completions-alist
               '("tar" . "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'")))

(use-package em-term
  :config
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "tail"))

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

(use-package esh-module
  :config
  (add-to-list 'eshell-modules-list 'eshell-smart))

(use-package eshell
  :commands eshell
  :bind ("C-x m" . eshell)
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

(use-package flyspell-mode
  :bind ("C-." . flyspell-auto-correct-word)
  :config
  (setq flyspell-use-meta-tab nil
        ispell-dictionary "british")
  (add-hook 'text-mode 'flyspell-mode))

(use-package geiser
  :config
  (setq geiser-active-implementations '(guile)
        geiser-guile-load-init-file-p t
        geiser-implementations-alist '(((regexp "\\.ss$")  racket)
                                       ((regexp "\\.rkt$") racket)
                                       ((regexp ".") guile)))
  (add-hook 'geiser-repl-mode-hook #'turn-on-paredit)
  (add-hook 'geiser-repl-mode-hook #'turn-on-company-mode))

(use-package gnus
  :commands gnus
  :config
  (use-package smtpmail)
  (setq user-mail-address "ianprice90@gmail.com"
        mail-host-address "gmail.com"

        gnus-buttonized-mime-types '("multipart/alternative"
                                     "multipart/signed")
        gnus-message-archive-group "archive"
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
        gnus-select-method '(nnimap "gmail"
                                    (nnimap-address "imap.gmail.com")
                                    (nnimap-server-port "imaps")
                                    (nnimap-stream ssl))
        ;; strongly consider using message-signature-file instead
        message-signature
        "Ian Price -- shift-reset.com

\"Programming is like pinball. The reward for doing it well is
the opportunity to do it again\" - from \"The Wizardy Compiled\""
        message-subscribed-addresses '("bug-guile@gnu.org"
                                       "guile-devel@gnu.org"
                                       "guile-user@gnu.org")
        message-use-followup-to 'use
        ;; Discarding text/html and show the text/plain version. cheers twb
        mm-discouraged-alternatives '("text/html" "multipart/related")

        smtpmail-smtp-service 587
        starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        )

  (add-to-list 'gnus-secondary-select-methods
               '(nntp "news.gmane.org")))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index))

(use-package hide-copyleft
  :commands hide-copyleft-region
  :init
  (add-hook 'prog-mode-hook 'hide-copyleft-region))

(use-package hideshow-org
  :load-path "~/src/emacs/hideshow-org/"
  :bind ("C-c f" . hs-org/minor-mode))

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

(use-package ibuffer
  :init
  (defun my-ibuffer-switch-to-default ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-switch-to-default)
  :config
  (setq ibuffer-saved-filter-groups
        ;; or maybe just ibuffer-filter-groups?
        `(("default"
           ("dired" (mode . dired-mode))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (filename . ,user-init-file)))
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

(use-package ido-hacks
  :config
  (setq ido-auto-merge-work-directories-length -1)
  (ido-hacks-mode t))

(use-package js2-mode
  :mode "\\.js\\(on\\)?\\'")

(use-package legalese
  :commands legalese
  :config
  (setq legalese-default-license 'bsd))

(use-package lisp-mode
  :config
  (defun my-rename-elisp-mode ()
    (setq mode-name "elisp"))

  (defun esk-remove-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (if (file-exists-p (concat buffer-file-name "c"))
                    (delete-file (concat buffer-file-name "c"))))))

  (add-hook 'emacs-lisp-mode-hook 'my-rename-elisp-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

  (bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)
  (bind-key "TAB" 'lisp-complete-symbol read-expression-map)
  (bind-keys :map lisp-mode-shared-map
             ("RET" . reindent-then-newline-and-indent)
             ("C-\\" . lisp-complete-symbol)
             ("C-c v" . eval-buffer)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-save-some-buffers nil))

(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package nxml-mode
  :mode "\\.xml\\'")

(use-package octave
  :mode ("\\.m\\'" . octave-mode))

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

(use-package paredit
  :diminish (paredit-mode . "Ped")
  :commands paredit-mode
  :init
  (defun turn-on-paredit ()
    (paredit-mode t))
  (add-hook 'scheme-mode-hook 'turn-on-paredit)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
  (add-hook 'lisp-mode-hook 'turn-on-paredit)
  :config
  (bind-key "M-)" 'paredit-forward-slurp-sexp paredit-mode-map))

(use-package pretty-mode
  :commands turn-on-pretty-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-pretty-mode))

(use-package proof-site
  :load-path "~/src/emacs/ProofGeneral-4.2/generic/"
  :mode ("\\.v\\'" . coq-mode)
  :config
  (setq coq-prog-args
        '("-emacs"                      ; needed?
          "-I" "/home/ian/lib/cpdt/src"
          "-R" "/home/ian/src/coq/ynot/src/coq/" "Ynot")))

(use-package rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package rfcview
  :mode ("\\rfc[0-9][0-9][0-9][0-9].txt\\'" . rfcview-mode))

(use-package scheme
  :mode (("\\.sls\\'" . scheme-mode)
         ("\\.sps\\'" . scheme-mode))
  :config
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

  (add-hook 'scheme-mode-hook #'turn-on-company-mode))

(use-package snakehump
  :bind (("C-}" . snakehump-next-at-point)
         ("C-{" . snakehump-prev-at-point)))

(use-package scpaste
  :commands scpaste
  :init
  (setq scpaste-http-destination "http://shift-reset.com/pastes"
        scpaste-scp-destination "ec2-user@shift-reset.com:pastes"
        scpaste-user-name "ijp"
        scpaste-user-address "http://shift-reset.com/"))

(use-package tea-time
  :load-path "~/src/emacs/tea-time"
  :bind ("C-c t" . tea-time)
  :config
  (setq tea-time-sound "/usr/share/sounds/freedesktop/stereo/complete.oga"))

(use-package time
  :commands display-time-world
  :config
  ;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  (setq display-time-world-list
        '(("America/Los_Angeles" "Seattle")
          ("America/New_York" "New York")
          ("Europe/London" "London")
          ("Europe/Paris" "Paris")
          ("Europe/Istanbul" "Istanbul")
          ("Asia/Calcutta" "Bangalore")
          ("Asia/Tokyo" "Tokyo")
          ("Pacific/Auckland" "Auckland"))))

(use-package tramp
  :init (setq tramp-ssh-controlmaster-options nil) ;; FIXES hanging tramp
  :config
  (setq tramp-auto-save-directory (locate-user-emacs-file "trampdir")))

(use-package ws-butler
  :load-path "~/src/emacs/ws-butler/"
  :commands ws-butler-mode
  :init (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package yasnippet
  :bind ("C-c y" . yas-expand)
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;;;; Functions

;; http://www.emacswiki.org/cgi-bin/wiki?BackToIndentationOrBeginning
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

;; Thanks forcer.
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

(defun read-sexp-from-file (filename)
  "reads one sexp from a file"
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

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

;;;; Bindings

;; honestly, when was the last time anyone ever turned on overwrite
;; mode _on purpose_.
(bind-key "<insert>" 'ignore)
(bind-key "<home>" 'back-to-indentation-or-beginning)
(bind-key "C-c d" 'diff-buffer-with-file)
(bind-key "C-c e" 'eval-and-replace)
(bind-key "C-c i" 'imenu)
(bind-key "C-c n" 'cleanup-buffer)
(bind-key "C-c o" 'browse-url)
(bind-key "C-c q" 'refill-mode)
(bind-key "C-c r" 'revert-buffer)
(bind-key "C-c +" 'hs-toggle-hiding)
(bind-key "C-h a" 'apropos)
(bind-key "C-x \\" 'align-regexp)
(bind-key "C-x f" 'recentf-ido-find-file)
(bind-key "C-x TAB" 'indent-rigidly)
(bind-key "C-x M-d" 'fixup-whitespace)
(bind-key "C-x M-k" 'kill-buffer-and-file)
(bind-key "C-x 8 p" 'fc/unicode-info-at-point)
(bind-key "C-x 8 \" RET" (lambda () (interactive) (insert "̈")))
(bind-key "C-x 9" 'kill-buffer-and-window)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)
(bind-key "C-M-h" 'backward-kill-word)

(bind-key "C-o" 'isearch-occur isearch-mode-map)
;; FIXME: should be in use-package but no isearch feature, and
;; can't coax use-package to not require it

(set-register ?b '(file . "~/org/2015-books.org"))
(set-register ?e `(file . ,user-init-file))
(set-register ?g `(file . ,(concat user-emacs-directory "fools")))

;;;; Misc Settings

(setq auto-save-include-big-deletions t
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux"))
      cookie-file "~/lib/homie-yow/homie.lines"
      custom-file (concat user-emacs-directory "custom.el")
      disabled-command-function nil ; handle all disabled commands -  thanks YoungFrog
      inhibit-startup-echo-area-message "ian"
      initial-scratch-message ""
      sentence-end-double-space nil
      set-mark-command-repeat-pop t
      uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      uniquify-strip-common-suffix nil
      user-mail-address "ianprice90@gmail.com"
      vc-follow-symlinks t
      yank-pop-change-selection t ; suggest adding to better-defaults.el
      )

(setenv "PAGER" "cat")
(setenv "NODE_NO_READLINE" "1")

(delete-selection-mode t)
(menu-bar-mode t)
(column-number-mode t)

(add-to-list 'default-frame-alist '(font . "Inconsolata-10"))

(let ((warning-minimum-level :error))
  (load-theme 'monokai t))

(load custom-file 'noerror)

(set-input-method "TeX")

(read-abbrev-file (locate-user-emacs-file "misspelling_abbrevs"))
