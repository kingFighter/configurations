;;; kingFighter/-core.el --- Emacs KingFighter: Core KingFighter functions.
;;
;; Copyright (c) 2014 Kaiyang Lv(Kevin Lui)
;;
;; Author: Kaiyang Lv(Kevin Lui) <kevinlui598@gmail.com>
;; URL: http://kingFighter.github.io
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by KingFighter.

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

(require 'thingatpt)
(require 'dash)

(defun kingFighter-open-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (start-process "kingFighter-open-with-process"
                   "*kingFighter-open-with-output*"
                   (cond
                    ((and (not arg) (eq system-type 'darwin)) "open")
                    ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                    (t (read-shell-command "Open current file with: ")))
                   (shell-quote-argument buffer-file-name))))

(defun kingFighter-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun kingFighter-visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (kingFighter-start-or-switch-to (lambda ()
                                (ansi-term (getenv "SHELL")))
                              "*ansi-term*"))

(defun kingFighter-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro kingFighter-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "kingFighter-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (kingFighter-search ,search-engine-url ,search-engine-prompt)))

(kingFighter-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(kingFighter-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(kingFighter-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(kingFighter-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(defun kingFighter-indent-rigidly-and-copy-to-clipboard (begin end arg)
  "Indent region between BEGIN and END by ARG columns and copy to clipboard."
  (interactive "r\nP")
  (let ((arg (or arg 4))
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) arg)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun kingFighter-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun kingFighter-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (kingFighter-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun kingFighter-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun kingFighter-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(defun kingFighter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'kingFighter-move-beginning-of-line)


(defun kingFighter-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun kingFighter-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (kingFighter-indent-buffer)
        (message "Indented buffer.")))))

(defun kingFighter-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun kingFighter-annotate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

(defun kingFighter-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun kingFighter-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (-dotimes arg
                (lambda (n)
                  (goto-char end)
                  (newline)
                  (insert region)
                  (setq end (point))))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; TODO: Remove code duplication by extracting something more generic
(defun kingFighter-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (comment-or-uncomment-region beg end)
      (setq end (line-end-position))
      (-dotimes arg
                (lambda (n)
                  (goto-char end)
                  (newline)
                  (insert region)
                  (setq end (point))))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun kingFighter-rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun kingFighter-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun kingFighter-view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (nxml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun kingFighter-untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun kingFighter-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (kingFighter-indent-buffer)
  (kingFighter-untabify-buffer)
  (whitespace-cleanup))

(defun kingFighter-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%s" value))))

(defun kingFighter-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory kingFighter-dir 0))

(defun kingFighter-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (or (equal major-mode 'dired-mode)
              (and (buffer-file-name)
                   (not (file-exists-p (file-name-directory (buffer-file-name)))))
              (and (buffer-file-name)
                   (file-writable-p buffer-file-name)))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun kingFighter-start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun kingFighter-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun kingFighter-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file file))))

(defun kingFighter-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun kingFighter-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun kingFighter-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
   (->> (buffer-list)
     (-filter #'buffer-file-name)
     (--remove (eql (current-buffer) it)))
   #'kill-buffer))

(defun kingFighter-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*scratch*")))
    (emacs-lisp-mode)))

(defvar kingFighter-tips
  '("Press <C-c o> to open a file with external program."
    "Press <C-c p f> or <s-f> to navigate a project's files with ido."
    "Press <C-c p g> or <s-g> to run grep on a project."
    "Press <C-c p s> or <s-p> to switch between projects."
    "Press <C-=> or <s-x> to expand the selected region."
    "Press <jj> quickly to jump to the beginning of a visible word."
    "Press <jk> quickly to jump to a visible character."
    "Press <jl> quickly to jump to a visible line."
    "Press <C-c g> to search in Google."
    "Press <C-c G> to search in GitHub."
    "Press <C-c y> to search in YouTube."
    "Press <C-c U> to search in DuckDuckGo."
    "Press <C-c r> to rename the current buffer and file it's visiting."
    "Press <C-c t> to open a terminal in Emacs."
    "Press <C-c k> to kill all the buffers, but the active one."
    "Press <C-x g> or <s-m> to run magit-status."
    "Press <C-c D> to delete the current file and buffer."
    "Press <C-c s> to swap two windows."
    "Press <S-RET> or <M-o> to open a new beneath the current one."
    "Press <s-o> to open a line above the current one."
    "Press <C-c C-z> in a Elisp buffer to launch an interactive Elisp shell."
    "Press <C-Backspace> to kill a line backwards."
    "Press <C-S-Backspace> or <s-k> to kill the whole line."
    "Press <f11> to toggle fullscreen mode."
    "Press <f12> to toggle the menu bar."
    "Explore the Tools->KingFighter menu to find out about some of KingFighter extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."
    "Visit the EmacsWiki at http://emacswiki.org to find out even more about Emacs."))

(defun kingFighter-tip-of-the-day ()
  "Display a random entry from `kingFighter-tips'."
  (interactive)
  (unless (window-minibuffer-p)
    ;; pick a new random seed
    (random t)
    (message
     (concat "KingFighter tip: " (nth (random (length kingFighter-tips)) kingFighter-tips)))))

(defun kingFighter-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun kingFighter-exchange-point-and-mark ()
  "Identical to `exchange-point-and-mark' but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(require 'epl)

(defun kingFighter-update ()
  "Update KingFighter to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update KingFighter? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Updating KingFighter...")
    (cd kingFighter-dir)
    (shell-command "git pull")
    (kingFighter-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun kingFighter-update-packages (&optional arg)
  "Update KingFighter's packages.
This includes package installed via `kingFighter-require-package'.

With a prefix ARG updates all installed packages."
  (interactive "P")
  (when (y-or-n-p "Do you want to update KingFighter's packages? ")
    (if arg
        (epl-upgrade)
      (epl-upgrade (-filter (lambda (p) (memq (epl-package-name p) kingFighter-packages))
                            (epl-installed-packages))))
    (message "Update finished. Restart Emacs to complete the process.")))

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
      (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun kingFighter-increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun kingFighter-decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (kingFighter-increment-integer-at-point (- (or dec 1))))

;;; Emacs in OSX already has fullscreen support
;;; Emacs has a similar built-in command in 24.4
(defun kingFighter-fullscreen ()
  "Make Emacs window fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(defun kingFighter-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun kingFighter-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (s-split "/" (getenv "SHELL")))))
         (shell-init-file (cond
                           ((s-equals? "zsh" shell) ".zshrc")
                           ((s-equals? "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))

(defun kingFighter-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

;; Enhance the comment
(defun kingFighter-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(provide 'kingFighter-core)
;;; kingFighter-core.el ends here
