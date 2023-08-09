(setq vc-follow-symlinks t)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package org)

(use-package evil
  :config
  (evil-mode 1))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind (
	 ("s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map)))

(use-package magit)

(use-package clojure-mode)

(use-package cider)

(use-package sly)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package flycheck
  :init (global-flycheck-mode))
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
)

(use-package marginalia
  :init
  (marginalia-mode))

(use-package vertico
  :init
  (vertico-mode))


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-echo-documentation 0.2)

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
		("M-SPC" . corfu-insert-separator)
		("RET" . nil)
		("S-<return>" . corfu-insert)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  ;; (corfu-echo-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (global-corfu-mode))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package which-key
  :init
  (setq which-key-idle-delay 0)
  (which-key-mode)
  :diminish which-key-mode)

(use-package sly)

;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun open-config ()
  "Opens ny config."
  (interactive) (find-file "~/modmacs/init.el"))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(setq modus-themes-org-blocks 'gray-background
      modus-themes-mixed-fonts t)
(setq modus-themes-common-palette-overrides
      '(
	;; Make the active mode line a fine shade of lavender
	;; (purple) and tone down the gray of the inactive mode
	;; lines.
	(bg-mode-line-active bg-lavender)
	(border-mode-line-active bg-lavender)
	(bg-mode-line-inactive bg-dim)
	(border-mode-line-inactive bg-inactive)
	;; Hide fringe
	(fringe unspecified)
	;; Make line numbers less intense and add a shade of cyan
	;; for the current line number.
	(fg-line-number-inactive "gray50")
	(fg-line-number-active cyan-cooler)
	(bg-line-number-inactive unspecified)
	(bg-line-number-active unspecified)
	;; Remove the border
	(border-mode-line-active unspecified)
	(border-mode-line-inactive unspecified)
	))

(use-package solar
  :ensure nil
  :config
  (setq calendar-latitude 59.33
	calendar-longitude 18.06))

(use-package circadian
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
			   (:sunset  . modus-vivendi-tinted)))
  (circadian-setup))

;;;;;;;;;
;; Org ;;
;;;;;;;;;

;; Org-tempo is a package that allows for '<s' followed by TAB to expand to a begin_src tag.  Other expansions available include:

;;| Typing the below + TAB | Expands to ...                          |
;;|------------------------+-----------------------------------------|
;;| <a                   | '#+BEGIN_EXPORT ascii' … '#+END_EXPORT  |
;;| <c                     | '#+BEGIN_CENTER' … '#+END_CENTER'       |
;;| <C                     | '#+BEGIN_COMMENT' … '#+END_COMMENT'     |
;;| <e                     | '#+BEGIN_EXAMPLE' … '#+END_EXAMPLE'     |
;;| <E                     | '#+BEGIN_EXPORT' … '#+END_EXPORT'       |
;;| <h                     | '#+BEGIN_EXPORT html' … '#+END_EXPORT'  |
;;| <l                     | '#+BEGIN_EXPORT latex' … '#+END_EXPORT' |
;;| <q                     | '#+BEGIN_QUOTE' … '#+END_QUOTE'         |
;;| <s                     | '#+BEGIN_SRC' … '#+END_SRC'             |
;;| <v                     | '#+BEGIN_VERSE' … '#+END_VERSE'         |


;; Make TAB work as expected in org-mode
(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "TAB" 'org-cycle)

(use-package org-tempo
  :ensure nil) ;; tell use-package not to try to install org-tempo since it's already there.

;; We want the same syntax highlighting in source blocks as in the native language files.
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; Enable evaluating code blocks
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

;;;;;;;;;;;;;;;;;;;
;; Misc settings ;;
;;;;;;;;;;;;;;;;;;;

;;Tab width
(setq tab-width 4
      evil-shift-width 4
      ;;Help find tree-sitter grammars
      treesit-extra-load-path '("/User/mans/.emacs.d/tree-sitter")
      ;;Bind fn to super
      ns-function-modifier 'super
      ;;Refresh dired when files change
      global-auto-revert-non-file-buffers t
      ;;Start with a scratch buffer
      inhibit-startup-message t)

;;Font size
(set-face-attribute 'default nil
		    :height 150)

;;Disable Menubar, Toolbars and Scrollbars
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
(scroll-bar-mode -1)

;;Highlight active line
(hl-line-mode t)

;;Show line numbers
(global-display-line-numbers-mode 1)

;;Refresh buffer if the underlying file changes
(global-auto-revert-mode 1)


;;Enable recent files
(recentf-mode 1)

;;Restore last cursor location in previously opened files
(save-place-mode 1)

;;Start in fullscreen
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


(display-time-mode 1)
(format-time-string "%H:%M")

(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(fset 'yes-or-no-p 'y-or-n-p) ; y-or-n-p makes answering questions faster

(show-paren-mode 1)
(setq linum-format "%4d ")
