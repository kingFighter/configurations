;;; kingFighter-packages.el --- Emacs KingFighter: default package selection.
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs KingFighter.

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
(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; set package-user-dir to be relative to KingFighter install path
(setq package-user-dir (expand-file-name "elpa" kingFighter-dir))
(package-initialize)

(defvar kingFighter-packages
  '(ace-jump-mode ack-and-a-half anzu
    browse-kill-ring
    dash diminish elisp-slime-nav
    epl expand-region flycheck gist
    gitconfig-mode gitignore-mode grizzl
    guru-mode projectile
    magit move-text rainbow-mode
    smartparens undo-tree
    volatile-highlights solarized-theme htmlize nyan-mode)
  "A list of packages to ensure are installed at launch.")

(defun kingFighter-packages-installed-p ()
  "Check if all packages in `kingFighter-packages' are installed."
  (every #'package-installed-p kingFighter-packages))

(defun kingFighter-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package kingFighter-packages)
    (add-to-list 'kingFighter-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun kingFighter-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'kingFighter-require-package packages))

(define-obsolete-function-alias 'kingFighter-ensure-module-deps 'kingFighter-require-packages)

(defun kingFighter-install-packages ()
  "Install all packages listed in `kingFighter-packages'."
  (unless (kingFighter-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs KingFighter is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (kingFighter-require-packages kingFighter-packages)))

;; run package installation
(kingFighter-install-packages)

(defun kingFighter-list-foreign-packages ()
  "Browse third-party packages not bundled with KingFighter.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `kingFighter-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list kingFighter-packages)))

(defmacro kingFighter-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar kingFighter-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (kingFighter-auto-install extension package mode))))
 kingFighter-auto-install-alist)

(provide 'kingFighter-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; kingFighter-packages.el ends here
