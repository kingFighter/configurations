;;; kingFighter-python.el --- Emacs KingFighter: python.el configuration.
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

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

(defun kingFighter-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (electric-indent-mode -1))

(setq kingFighter-python-mode-hook 'kingFighter-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'kingFighter-python-mode-hook)))
(provide 'kingFighter-python)

;;; kingFighter-python.el ends here
