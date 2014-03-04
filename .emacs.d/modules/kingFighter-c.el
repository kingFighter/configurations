;;; kingFighter-c.el --- Emacs KingFighter: cc-mode configuration.
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for cc-mode and the modes derived from it.

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

(require 'kingFighter-programming)

(defun kingFighter-c-mode-common-defaults ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(setq kingFighter-c-mode-common-hook 'kingFighter-c-mode-common-defaults)

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks 'kingFighter-c-mode-common-hook)))

(defun kingFighter-makefile-mode-defaults ()
  (setq indent-tabs-mode t))

(setq kingFighter-makefile-mode-hook 'kingFighter-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'kingFighter-makefile-mode-hook)))
(message "kingFighter-c is loading")
(provide 'kingFighter-c)

;;; kingFighter-c.el ends here
