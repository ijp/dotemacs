# Dot Emacs
My Dot Emacs originally started off as technomancy's starter kit a
few years back. But as any good Emacs user knows, starter kits just
end up causing you trouble.

Over time I've ripped bits out and replaced others, but you can still
tell that's what it was. Over time, I'm trying to flush out the last
bits of utility I got from it.

# installation

To install my .emacs.d (based on technomancys)
I need to install color-theme, w3m, and erlang-mode from my
distributions package manager; the inconsolata font from my package
manager; and I had to rename a bunch of 'Ian's to 'ian's.
Also hunspell-en-GB

gnutls-utils is needed for gnus

sudo yum install emacs-color-theme emacs-color-theme-el emacs-w3m emacs-w3m-el emacs-erlang emacs-erlang-el emacs-el aspell aspell-en gnutls-utils
 sudo yum install levien-inconsolata-fonts

symlink mygnus.el to ~/.gnus.el
