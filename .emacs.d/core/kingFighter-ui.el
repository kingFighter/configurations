;;; kingFighter-ui.el --- Emacs KingFighter: UI optimizations and tweaks.
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; We dispense with most of the point and click UI, reduce the startup noise,
;; configure smooth scolling and a nice theme that's easy on the eyes (zenburn).

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)


;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)
;; Don't use messages that you don't read
;; (setq initial-scratch-message "")


;; nice scrolling
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)
(scroll-bar-mode 0)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " KingFighter - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; use zenburn as the default theme
(load-theme 'solarized-dark t)
;; (load-theme 'zenburn t)
;;(require 'color-theme)
;;(color-theme-initialize)
;; (color-theme-gnome2)
;;(color-theme-arjen)  
;;(color-theme-molokai)                   
;; (require 'color-theme-solarized)
;; (color-theme-solarized-dark)

(provide 'kingFighter-ui)
;;; kingFighter-ui.el ends here
