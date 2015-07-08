(require 'smtpmail)

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
             '(nntp "news.gmane.org"))
