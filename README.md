Description
===========

An [Oplop](http://code.google.com/p/oplop/) implementation for Emacs.

Installation Instructions
=========================

Add oplop.el to your load path and require it. For example:

    (add-to-list 'load-path "<directory containing oplop.el>")
    (require 'oplop)

Usage
=====

Use `execute-extended-command`:

`M-x oplop`

This will prompt you for your nickname and master password. The
account password will be copied to the Emacs clipboard.
