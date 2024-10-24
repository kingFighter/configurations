;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'tsdh-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '(
          ;; If set to true, code completion will include index symbols that are not defined in the scopes
          ;; (e.g. namespaces) visible from the code completion point. Such completions can insert scope qualifiers
          "--all-scopes-completion"
          ;; Index project code in the background and persist index on disk.
          "--background-index"
          ;; Enable clang-tidy diagnostics
          "--clang-tidy"
          ;; Whether the clang-parser is used for code-completion
          ;;   Use text-based completion if the parser is not ready (auto)
          "--completion-parse=auto"
          ;; Granularity of code completion suggestions
          ;;   One completion item for each semantically distinct completion, with full type information (detailed)
          "--completion-style=detailed"
          ;; clang-format style to apply by default when no .clang-format file is found
          "--fallback-style=Google"
          ;; When disabled, completions contain only parentheses for function calls.
          ;; When enabled, completions also contain placeholders for method parameters
          "--function-arg-placeholders"
          ;; Add #include directives when accepting code completions
          ;;   Include what you use. Insert the owning header for top-level symbols, unless the
          ;;   header is already directly included or the symbol is forward-declared
          "--header-insertion=never"
          ;; Prepend a circular dot or space before the completion label, depending on whether an include line will be inserted or not
          "--header-insertion-decorators=0"
          ;; Enable index-based features. By default, clangd maintains an index built from symbols in opened files.
          ;; Global index support needs to enabled separatedly
          "--index"
          ;; Attempts to fix diagnostic errors caused by missing includes using index
          "--suggest-missing-includes"
          ;; Number of async workers used by clangd. Background index also uses this many workers.
          "-j=4"))
  (set-lsp-priority! 'clangd 2))

;; Always keep 10 lines of buffer above/below the cursor
(setq scroll-margin 10)

;; Auto save buffers on focus lost
(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))
;; Exit insert mode on focus loss
(add-function :after after-focus-change-function (lambda () (evil-normal-state)))
