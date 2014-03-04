;;; kingFighter-mode.el --- Emacs KingFighter: minor mode
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minor mode defining a local keymap, plus a menu.

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
(require 'easymenu)

(defvar kingFighter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'kingFighter-open-with)
    (define-key map (kbd "C-c g") 'kingFighter-google)
    (define-key map (kbd "C-c G") 'kingFighter-github)
    (define-key map (kbd "C-c y") 'kingFighter-youtube)
    (define-key map (kbd "C-c U") 'kingFighter-duckduckgo)
    ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
    (define-key map [(shift return)] 'kingFighter-smart-open-line)
    (define-key map (kbd "M-o") 'kingFighter-smart-open-line)
    (define-key map [(control shift return)] 'kingFighter-smart-open-line-above)
    (define-key map [(control shift up)]  'move-text-up)
    (define-key map [(control shift down)]  'move-text-down)
    (define-key map [(meta shift up)]  'move-text-up)
    (define-key map [(meta shift down)]  'move-text-down)
    (define-key map (kbd "C-c n") 'kingFighter-cleanup-buffer)
    (define-key map (kbd "C-c f")  'kingFighter-recentf-ido-find-file)
    (define-key map (kbd "C-M-\\") 'kingFighter-indent-region-or-buffer)
    (define-key map (kbd "C-M-z") 'kingFighter-indent-defun)
    (define-key map (kbd "C-c u") 'kingFighter-view-url)
    (define-key map (kbd "C-c e") 'kingFighter-eval-and-replace)
    (define-key map (kbd "C-c s") 'kingFighter-swap-windows)
    (define-key map (kbd "C-c D") 'kingFighter-delete-file-and-buffer)
    (define-key map (kbd "C-c d") 'kingFighter-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'kingFighter-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'kingFighter-rename-file-and-buffer)
    (define-key map (kbd "C-c t") 'kingFighter-visit-term-buffer)
    (define-key map (kbd "C-c k") 'kingFighter-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'kingFighter-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c h") 'helm-kingFighter)
    (define-key map (kbd "C-c +") 'kingFighter-increment-integer-at-point)
    (define-key map (kbd "C-c -") 'kingFighter-decrement-integer-at-point)
    (define-key map (kbd "C-c I") 'kingFighter-find-user-init-file)
    (define-key map (kbd "C-c S") 'kingFighter-find-shell-init-file)
    ;; make some use of the Super key
    (define-key map [?\s-d] 'projectile-find-dir)
    (define-key map [?\s-p] 'projectile-switch-project)
    (define-key map [?\s-f] 'projectile-find-file)
    (define-key map [?\s-g] 'projectile-grep)

    (define-key map (kbd "s-r") 'kingFighter-recentf-ido-find-file)
    (define-key map (kbd "s-j") 'kingFighter-top-join-line)
    (define-key map (kbd "s-k") 'kingFighter-kill-whole-line)
    (define-key map (kbd "s-m m") 'magit-status)
    (define-key map (kbd "s-m l") 'magit-log)
    (define-key map (kbd "s-m f") 'magit-file-log)
    (define-key map (kbd "s-m b") 'magit-blame-mode)
    (define-key map (kbd "s-o") 'kingFighter-smart-open-line-above)

    map)
  "Keymap for KingFighter mode.")

(defun kingFighter-mode-add-menu ()
  "Add a menu entry for `kingFighter-mode' under Tools."
  (easy-menu-add-item nil '("Tools")
                      '("KingFighter"
                        ("Files"
                         ["Open with..." kingFighter-open-with]
                         ["Delete file and buffer" kingFighter-delete-file-and-buffer]
                         ["Rename file and buffer" kingFighter-rename-file-and-buffer]
                         ["Copy file name to clipboard" kingFighter-copy-file-name-to-clipboard])

                        ("Buffers"
                         ["Clean up buffer" kingFighter-cleanup-buffer]
                         ["Kill other buffers" kingFighter-kill-other-buffers])

                        ("Editing"
                         ["Insert empty line" kingFighter-insert-empty-line]
                         ["Move line up" kingFighter-move-line-up]
                         ["Move line down" kingFighter-move-line-down]
                         ["Indent buffer" kingFighter-indent-buffer]
                         ["Indent buffer or region" kingFighter-indent-buffer-or-region]
                         ["Duplicate line or region" kingFighter-duplicate-current-line-or-region]
                         ["Indent rigidly and copy to clipboard" kingFighter-indent-rigidly-and-copy-to-clipboard]
                         ["Insert date" kingFighter-insert-date]
                         ["Eval and replace" kingFighter-eval-and-replace]
                         ["Increment integer at point" kingFighter-increment-integer-at-point]
                         ["Decrement integer at point" kingFighter-decrement-integer-at-point])

                        ("Navigation"
                         ["Helm" helm-kingFighter])

                        ("Windows"
                         ["Swap windows" kingFighter-swap-windows])

                        ("General"
                         ["Visit term buffer" kingFighter-visit-term-buffer]
                         ["Search in Google" kingFighter-google]
                         ["View URL" kingFighter-view-url]))
                      "Search Files (Grep)...")

  (easy-menu-add-item nil '("Tools") '("--") "Search Files (Grep)..."))

(defun kingFighter-mode-remove-menu ()
  "Remove `kingFighter-mode' menu entry."
  (easy-menu-remove-item nil '("Tools") "KingFighter")
  (easy-menu-remove-item nil '("Tools") "--"))

;; define minor mode
(define-minor-mode kingFighter-mode
  "Minor mode to consolidate Emacs KingFighter extensions.

\\{kingFighter-mode-map}"
  :lighter " Kin"
  :keymap kingFighter-mode-map
  (if kingFighter-mode
      ;; on start
      (kingFighter-mode-add-menu)
    ;; on stop
    (kingFighter-mode-remove-menu)))

(define-globalized-minor-mode kingFighter-global-mode kingFighter-mode kingFighter-on)

(defun kingFighter-on ()
  "Turn on `kingFighter-mode'."
  (kingFighter-mode +1))

(defun kingFighter-off ()
  "Turn off `kingFighter-mode'."
  (kingFighter-mode -1))

(provide 'kingFighter-mode)
;;; kingFighter-mode.el ends here
