(setq user-mail-address "ianprice90@googlemail.com")

;; set this to t, if I want to use message-signature-file instead
;; (setq message-signature
;;       "Ian Price

;; \"There are only two hard problems in Computer Science: cache invalidation
;; and naming things.\" - Phil Karlton")
(setq message-signature
      "Ian Price -- shift-reset.com

\"Programming is like pinball. The reward for doing it well is
the opportunity to do it again\" - from \"The Wizardy Compiled\"")

;; In the beginning was the lambda. God saw it, realized he didn't need anything else, and stopped there. -- andy wingo
;; http://wingolog.org/archives/2011/07/12/static-single-assignment-for-functional-programmers

;; It is easier to write an incorrect program than understand a correct one.  -- Alan Perlis

;; Anyone caught breaking the law will be made to mend it. -- Loonie
;; party 2005 manifesto

;; How about "Free as in "COOL FREE RINGTONES"" for a sig?
;; Free as in Beard?

;; thanks http://ghislain.vieilledent.free.fr/?p=237
(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments nil)
;; Thanks http://www.emacswiki.org/emacs/GnusGmail
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "ianprice90@googlemail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smptmail-debug-info t)
(require 'smtpmail)


(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.gmane.org"))

;; grc
(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.grc.com"))


;; archive all outgoing messages in ~/Mail/archive/
(setq gnus-message-archive-group "archive")


(setq message-subscribed-addresses
      '("bug-guile@gnu.org"
        "guile-devel@gnu.org"
        "guile-user@gnu.org"))

(setq message-use-followup-to 'use)
