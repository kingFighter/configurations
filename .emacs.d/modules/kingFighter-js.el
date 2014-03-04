;;; kingFighter-js.el --- Emacs KingFighter: js-mode configuration.
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for js-mode.

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

(eval-after-load 'js-mode
  '(progn
     (defun kingFighter-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with js-mode
       (electric-layout-mode -1))

     (setq kingFighter-js-mode-hook 'kingFighter-js-mode-defaults)

     (add-hook 'js-mode-hook (lambda () (run-hooks 'kingFighter-js-mode-hook)))))

(provide 'kingFighter-js)

;;; kingFighter-js.el ends here
