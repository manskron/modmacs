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

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(hl-line-mode t)

(global-display-line-numbers-mode -1)

(global-auto-revert-mode 1)

(recentf-mode 1)

(save-place-mode 1)

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(display-time-mode 1)
(format-time-string "%H:%M")

(show-paren-mode 1)

(fset 'yes-or-no-p 'y-or-n-p) ; y-or-n-p makes answering questions faster

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

;; Custom functions 
(defun open-config ()
  "Opens my config."
  (interactive) (find-file "~/modmacs/init.el"))

(defun dom-indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


;; Theme Setup
(use-package modus-themes)

(use-package solar 
  :ensure nil
  :config
  (setq calendar-latitude 59.32
        calendar-longitude 18.06))

(use-package circadian
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-vivendi-tinted)))
  (circadian-setup))

;; Packages 
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-ts-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config (
           setq lsp-ui-doc-show-with-cursor t)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package nvm)

(use-package evil-leader
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; Project
    "pf" 'project-find-file
    "pp" 'project-switch-project
    "ps" 'consult-ripgrep
    ;;Files
    "fr" 'recentf
    "fs" 'save-buffer
    "fc" 'open-config
    ;; Buffers
    "<SPC>" 'switch-to-buffer
    "bd" 'kill-buffer
    "be" 'eval-buffer
    "bi" 'ibuffer
    "cf" 'dom-indent-buffer
    ;; Window Management
    "w/" 'split-window-right
    "w-" 'split-window-below
    "wd" 'delete-window
    "wr" 'restart-emacs
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    "wt" 'vterm-other-window
    ;; Git 
    "gs" 'magit
    ;; Search
    "ss" 'avy-goto-char-2
    )
  )

(use-package evil
  :after evil-leader
  :config
  (evil-set-initial-state 'eww-mode 'emacs)
  :init
  (global-evil-leader-mode)
  :hook
  ((prog-mode) . evil-mode)
  )

(use-package magit)

(use-package prettier-js)

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

(use-package which-key
  :init
  (setq which-key-idle-delay 0)
  (which-key-mode)
  :diminish which-key-mode)


;;Vars
(setq vc-follow-symlinks t)
(setq treesit-extra-load-path '("~/tree-sitter-module/dist"))
;;Tab width
(setq ;;Bind fn to super
 ns-function-modifier 'super
 ;;Refresh dired when files change
 global-auto-revert-non-file-buffers t
 ;;Start with a scratch buffer
 inhibit-startup-message t)
;;Font size
(set-face-attribute 'default nil
                    :height 150)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
