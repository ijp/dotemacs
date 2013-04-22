#!/usr/local/bin/guile -s
!#
;;; As far as I am concerned, newsticker is not a good replacement for
;;; Google Reader, but since that is no more, it will have to do for
;;; now. :(
(use-modules (sxml simple)
             (sxml xpath)
             (sxml match)
             (ice-9 match)
             (ice-9 pretty-print))

(define get-feeds
  (sxpath '(// (outline (@ title) (@ xmlUrl)))))

(define (munge-subscriptions sxml)
  (map (lambda (feed)
         (sxml-match feed
           [(outline (@ (title ,t) (xmlUrl ,url)))
            (list t url)]
           [,else
            (throw 'bad-feed feed)]))
       (get-feeds sxml)))

(define (main)
  (match (command-line)
    ((_ file)
     (call-with-input-file file
       (lambda (in)
         (set-port-encoding! in "UTF-8")
         (pretty-print (munge-subscriptions (xml->sxml in)))
         (newline))))
    ((script . rest)
     (format (current-error-port) "Usage: ~s XML-FILE\n" script)
     (exit 1))))

(setlocale LC_ALL "")
(main)
