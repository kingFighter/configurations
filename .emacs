(setenv "HOME" "E:/Others/Emacs/emacs-24.1")
(setenv "PATH" "E:/Others/Emacs/emacs-24.1")
 ;;set the default file path
(setq default-directory "~/")
;;

(global-linum-mode t)
;;show line number

(load "~/.emacs.d/init.el")

;; (setq completion-ignore-case t           ;; ignore case when completing...
;;       read-file-name-completion-ignore-case t) ;; ...filenames too


;;set word wrap at windows edge on
(global-visual-line-mode 0);


;; fontify code in code blocks
(setq org-src-fontify-natively t)
;;org octopress 配置还是有些问题的
(require 'org-publish)
(setq org-publish-project-alist
      '(("blog-org"
         :base-directory "E:/Others/octopress/org/";;先凑合着用吧
         :publishing-directory "E:/Others/octopress/source/"
         :base-extension "org"
         :recursive t
         :publishing-function org-publish-org-to-html
	 :headline-levels 4
	 :html-extension "html"
	 :table-of-contents t
	 :author nil
	 :creator-info nil
	 :export-with-tags nil
	 :section-numbers t
	 ;;:sub-superscript nil
	 :todo-keywords nil
	 :html-postamble nil
	 :body-only t ;; Only export section between <body> </body>
     )

        ("blog-static"
         :base-directory  "E:/Others/octopress/org/"
         :publishing-directory  "E:/Others/octopress/source/"
         :recursive t
         :base-extension "css\\|js\\|bmp\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el\\|pl\\|mht\\|log\\|bin_\\|bat\\|tst\\|doc\\|docx\\|gz"
         :publishing-function org-publish-attachment
     )
        ("blog"
         :components ("blog-org" "blog-static")
         :author "Kevin Lui"
         )))


;;最基本的就是记录当一个Item被标记为DONE时的时间戳了，另外，在一个Item完成后，可能会想要写点心得或者备注什么的，可以用如下配置实现：
(setq org-log-done 'time)
;;(setq org-log-done 'note)
;;org-mode自动换行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))


;;Below is cedet
;; buildin cedet
(when (and (fboundp 'semantic-mode)
           (not (locate-library "semantic-ctxt"))) ; can't found offical cedet
  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    global-semantic-idle-summary-mode
                                    global-semantic-mru-bookmark-mode
				    global-semantic-load-enable-code-helpers
				    global-semantic-load-enable-excessive-code-helpers
				    global-semantic-idle-completions-mode))
  (semantic-mode 1)
  (global-semantic-decoration-mode 1)
  (require 'semantic/decorate/include nil 'noerror)
  (semantic-toggle-decoration-style "semantic-tag-boundary" -1)
  (global-semantic-highlight-edits-mode (if window-system 1 -1))
  (global-semantic-show-unmatched-syntax-mode 1)
  (global-semantic-show-parser-state-mode 1)
  (global-ede-mode 1)
  (when (executable-find "global")
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)
    (setq ede-locate-setup-options '(ede-locate-global ede-locate-base)))
  ;; (setq semantic-c-obey-conditional-section-parsing-flag nil) ; ignore #if

  ;; (defun my-semantic-inhibit-func ()
  ;;   (cond
  ;;    ((member major-mode '(Info-mode))
  ;;     ;; to disable semantic, return non-nil.
  ;;     t)
  ;;    (t nil)))
  ;; (add-to-list 'semantic-inhibit-functions 'my-semantic-inhibit-func)

;;  (when (executable-find "gcc")
;;    (require 'semantic/bovine/c nil 'noerror)
;;    (and (eq system-type 'windows-nt)
;;         (semantic-gcc-setup)))


  (defconst cedet-win32-include-dirs
    (list "E:/Development Tools/VS2010/VC/include/"
  	 ))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        cedet-win32-include-dirs)


 ;; (dolist (file c-preprocessor-symbol-files)
 ;;   (when (file-exists-p file)
 ;;     (setq semantic-lex-c-preprocessor-symbol-file
 ;;           (append semantic-lex-c-preprocessor-symbol-file (list file)))))

  (require 'semantic/bovine/el nil 'noerror)

  (require 'semantic/analyze/refs)      ; for semantic-ia-fast-jump
  (defadvice push-mark (around semantic-mru-bookmark activate)
    "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
    (semantic-mrub-push semantic-mru-bookmark-ring
                        (point)
                        'mark)
    ad-do-it)
  (defun semantic-ia-fast-jump-back ()
    (interactive)
    (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
        (error "Semantic Bookmark ring is currently empty"))
    (let* ((ring (oref semantic-mru-bookmark-ring ring))
           (alist (semantic-mrub-ring-to-assoc-list ring))
           (first (cdr (car alist))))
      ;; (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
      ;;     (setq first (cdr (car (cdr alist)))))
      (semantic-mrub-visit first)
      (ring-remove ring 0)))
  (defun semantic-ia-fast-jump-or-back (&optional back)
    (interactive "P")
    (if back
        (semantic-ia-fast-jump-back)
      (semantic-ia-fast-jump (point))))
  (defun semantic-ia-fast-jump-mouse (ev)
    "semantic-ia-fast-jump with a mouse click. EV is the mouse event."
    (interactive "e")
    (mouse-set-point ev)
    (semantic-ia-fast-jump (point)))
  (define-key semantic-mode-map [f12] 'semantic-ia-fast-jump-or-back)
  (define-key semantic-mode-map [C-f12] 'semantic-ia-fast-jump-or-back)
  (define-key semantic-mode-map [S-f12] 'semantic-ia-fast-jump-back)
  ;; (define-key semantic-mode-map (kbd "ESC ESC <f12>")
  ;;   'semantic-ia-fast-jump-back)
  ;; (define-key semantic-mode-map [S-f12] 'pop-global-mark)
  (global-set-key [mouse-2] 'semantic-ia-fast-jump-mouse)
  ;; (define-key semantic-mode-map [mouse-2] 'semantic-ia-fast-jump-mouse)
  (define-key semantic-mode-map [S-mouse-2] 'semantic-ia-fast-jump-back)
  (define-key semantic-mode-map [double-mouse-2] 'semantic-ia-fast-jump-back)
  (define-key semantic-mode-map [M-S-f12] 'semantic-analyze-proto-impl-toggle)
  (define-key semantic-mode-map (kbd "C-c , ,") 'semantic-force-refresh)

  (autoload 'pulse-momentary-highlight-one-line "pulse" "" nil)
  (autoload 'pulse-line-hook-function "pulse" "" nil)
  (setq pulse-command-advice-flag t)    ; (if window-system 1 nil)
  (defadvice goto-line (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice exchange-point-and-mark (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p)
               (> (abs (- (point) (mark))) 400))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice find-tag (after pulse-advice activate)
    "After going to a tag, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice tags-search (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
         (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice tags-loop-continue (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice pop-tag-mark (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice imenu-default-goto-function (after pulse-advice activate)
    "After going to a tag, pulse the line the cursor lands on."
    (when pulse-command-advice-flag
      (pulse-momentary-highlight-one-line (point))))
  (defadvice cua-exchange-point-and-mark (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p)
               (> (abs (- (point) (mark))) 400))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice switch-to-buffer (after pulse-advice activate)
    "After switch-to-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice previous-buffer (after pulse-advice activate)
    "After previous-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice next-buffer (after pulse-advice activate)
    "After next-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice ido-switch-buffer (after pulse-advice activate)
    "After ido-switch-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice beginning-of-buffer (after pulse-advice activate)
    "After beginning-of-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (add-hook 'next-error-hook 'pulse-line-hook-function))

;;ecb
(setq stack-trace-on-error t)

(require 'semantic/analyze)
(provide 'semantic-analyze)
(provide 'semantic-ctxt)
(provide 'semanticdb)
(provide 'semanticdb-find)
(provide 'semanticdb-mode)
(provide 'semantic-load)

(require 'ecb)
;;(require 'ecb-autoloads)
(autoload 'html-fold-mode "html-fold" "Minor mode for hiding and revealing elements." t)

;; backup policies
(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 10)
(setq kept-new-versions 10)
(setq delete-old-versions t)
(setq backup-directory-alist '(("" . "~/.backups")))

;;Real auto saving for emacs
(require 'real-auto-save)
(add-hook 'text-mode-hook 'turn-on-real-auto-save)
(add-hook 'muse-mode-hook 'turn-on-real-auto-save)
;;Auto save interval is 10 seconds by default. You can change it:
(setq real-auto-save-interval 5) ;; in seconds


(show-paren-mode 1) ; turn on paren match highlighting
(setq show-paren-style 'expression) ; highlight entire bracket
                                    ; expression

;; always end a file with a newline
;; (setq require-final-newline 'query)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "~/.emacs.d/vendor/yasnippet/snippets"        ;; the default collection
        ))
(yas-global-mode 1)  
;; (require 'org-notify) 

(setq column-number-mode t)
(set-face-foreground 'minibuffer-prompt "white")

