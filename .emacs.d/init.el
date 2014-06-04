;;; init.el --- KingFighter's configuration entry point.
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs KingFighter.

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
(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "KingFighter is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.1")
  (error "KingFighter requires at least GNU Emacs 24.1"))

(defvar kingFighter-dir (file-name-directory load-file-name)
  "The root dir of the Emacs KingFighter distribution.")
(defvar kingFighter-core-dir (expand-file-name "core" kingFighter-dir)
  "The home of KingFighter's core functionality.")
(defvar kingFighter-modules-dir (expand-file-name  "modules" kingFighter-dir)
  "This directory houses all of the built-in KingFighter modules.")
(defvar kingFighter-personal-dir (expand-file-name "personal" kingFighter-dir)
  "This directory is for your personal configuration.

Users of Emacs KingFighter are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by KingFighter.")
(defvar kingFighter-personal-preload-dir (expand-file-name "preload" kingFighter-personal-dir)
  "This directory is for your personal configuration, that you want loaded before KingFighter.")
(defvar kingFighter-vendor-dir (expand-file-name "vendor" kingFighter-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar kingFighter-savefile-dir (expand-file-name "savefile" kingFighter-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar kingFighter-modules-file (expand-file-name "kingFighter-modules.el" kingFighter-dir)
  "This files contains a list of modules that will be loaded by KingFighter.")

(unless (file-exists-p kingFighter-savefile-dir)
  (make-directory kingFighter-savefile-dir))

(defun kingFighter-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (equal f ".."))
                (not (equal f ".")))
       (add-to-list 'load-path name)
       (kingFighter-add-subfolders-to-load-path name)))))

;; similar to kingFighter-add-subfolders-to-load-path
;; (add-to-list 'load-path "~/.emacs.d/site-lisp")
;; (progn (cd "~/.emacs.d/site-lisp")
;;        (normal-top-level-add-subdirs-to-load-path))

;; add KingFighter's directories to Emacs's `load-path'
(add-to-list 'load-path kingFighter-core-dir)
(add-to-list 'load-path kingFighter-modules-dir)
(add-to-list 'load-path kingFighter-vendor-dir)
(kingFighter-add-subfolders-to-load-path kingFighter-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; preload the personal settings from `kingFighter-personal-preload-dir'
(when (file-exists-p kingFighter-personal-preload-dir)
  (message "Loading personal configuration files in %s..." kingFighter-personal-preload-dir)
  (mapc 'load (directory-files kingFighter-personal-preload-dir 't "^[^#].*el$")))

(message "Loading KingFighter's core...")

;; the core stuff
(require 'kingFighter-packages)
(require 'kingFighter-ui)
(require 'kingFighter-core)
(require 'kingFighter-mode)
(require 'kingFighter-editor)
(require 'kingFighter-global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'kingFighter-osx))

(message "Loading KingFighter's modules...")

;; the modules
(when (file-exists-p kingFighter-modules-file)
  (load kingFighter-modules-file))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" kingFighter-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p kingFighter-personal-dir)
  (message "Loading personal configuration files in %s..." kingFighter-personal-dir)
  (mapc 'load (directory-files kingFighter-personal-dir 't "^[^#].*el$")))

(message "KingFighter is ready to do thy bidding, Master %s!" current-user)

(kingFighter-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'kingFighter-tip-of-the-day))

;;; init.el ends here
