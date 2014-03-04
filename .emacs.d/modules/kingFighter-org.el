;;; kingFighter-org.el --- Emacs KingFighter: org-mode configuration.
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

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

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done t)

(setq org-agenda-files (list "~/todos/work.org"
                             "~/todos/projects.org"
                             "~/todos/home.org"
                             ))

(setq org-capture-templates

      `(("t" "Task" entfry (file+headline ,"~/todos/todo.org" "Task")
          "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ))

(defun kingFighter-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'kingFighter-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(kingFighter-mode . ,newmap) minor-mode-overriding-map-alist))
)

(setq kingFighter-org-mode-hook 'kingFighter-org-mode-defaults)

(add-hook 'org-mode-hook (lambda () (run-hooks 'kingFighter-org-mode-hook)))

(message "kingFighter-org is loaded")
(provide 'kingFighter-org)

;;; kingFighter-org.el ends here
