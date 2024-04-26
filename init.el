(setq
 modmacs/font-size 15
 modmacs/font-family "JetbrainsMono Nerd Font"
 )

(use-package emacs
  :init

  ;;Hide scroll bar, tool bar and menu bar. Don't do this if you are new to emacs.
  ;;It is a fantastic way to discover functionality and keybindings.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;;Start in fullscreen
  (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

  :custom

  (package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("elpa" . "https://elpa.gnu.org/packages/")))

  ;;Highlight the active line.
  (hl-line-mode t)

  ;;Hide line numbers. These are mostly only needed when pair programming.
  (global-display-line-numbers-mode -1)

  ;;Refresh buffer if the underlying file changes.
  (global-auto-revert-mode 1)

  ;;Enable recent files
  (recentf-mode 1)

  ;;Restore last cursor location in previously opened files
  (save-place-mode 1)

  ;;Display time the way I like it.
  (display-time-mode 1)
  (format-time-string "%H:%M")

  ;;Hightlight matching parens
  (show-paren-mode 1)

  ;;Make confirmation prompts comfortable
  (fset 'yes-or-no-p 'y-or-n-p) ; y-or-n-p makes answering questions faster

  ;; Follow symlinks
  (vc-follow-symlinks t)

  ;;Refresh dired when files change
  (global-auto-revert-non-file-buffers t)

  ;;Start with a scratch buffer
  (inhibit-startup-message t)

  ;; Send custom.el to oblivion
  (custom-file (make-temp-file "emacs-custom-"))

  ;;Don't create backups and lockfiles
  (make-backup-files nil)
  (backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
  (create-lockfiles nil)

  ;;Allow eldoc to resize
  (eldoc-echo-area-use-multiline-p t)

  )

(use-package modus-themes)
(use-package ef-themes)

(use-package solar 
  :ensure nil
  :config
  (setq calendar-latitude 59.32
        calendar-longitude 18.06))

(use-package circadian
  :after solar 
  :config
  (setq circadian-themes '((:sunrise . ef-summer)
                           (:sunset  . ef-bio)))
  (circadian-setup))

(set-face-attribute 'default nil
                    :height (* modmacs/font-size 10)
                    :family modmacs/font-family
                    )

(use-package general
  :config
  (general-define-key
   :states '(emacs insert normal)
   :prefix-map 'modmacs-prefix-map
   :global-prefix "C-c"
   :non-normal-prefix "M-SPC"
   :prefix "SPC")

  (general-create-definer modmacs-definer 
    :keymaps 'modmacs-prefix-map)

  (modmacs-definer
    ;;Common
    "SPC" 'execute-extended-command
    "/" 'consult-line
    ";" 'vterm-toggle

    ;;Config
    "," '("config" . (keymap))
    ",c" 'modmacs/open-config
    ",r" 'modmacs/reload-config
    ",t" 'modmacs/tangle-config

    ;;Buffer
    "b" '("buffer" . (keymap))
    "bb" 'consult-buffer
    "bd" 'kill-this-buffer
    "be" 'eval-buffer
    "bi" 'ibuffer

    ;;Code
    "c" '("code" . (keymap))
    "cd" 'lsp-ui-doc-show
    "cf" 'modmacs/indent-buffer
    "cx" 'consult-flymake

    ;;File
    "f" '("file" . (keymap))
    "fr" 'recentf
    "fs" 'save-buffer

    ;;Git
    "g" '("git" . (keymap))
    "gs" 'magit

    ;;Help
    "h" '("help" . (keymap))
    "hf" 'modmacs/what-face

    ;;Project
    "p" '("project" . (keymap))
    "pd" 'project-dired
    "pf" 'project-find-file
    "pp" 'project-switch-project
    "pb" 'project-list-buffers
    "ps" 'consult-ripgrep
    "p." 'project-async-shell-command

    ;;Search
    "s" '("search" . (keymap))
    "ss" 'avy-goto-char-2

    ;;Toggle
    "t" '("toggle" . (keymap))
    "tt" 'consult-theme

    ;;Window
    "w" '("window" . (keymap))
    "w/" 'split-window-right
    "w-" 'split-window-below
    "wd" 'delete-window
    "wr" 'restart-emacs
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    "wt" 'vterm-other-window
    ))

(use-package avy)

(use-package consult)

(use-package corfu
  :init 
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  )

(use-package dired
  :ensure nil
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  ;; Avoid having to q myself to death
  :config (setf dired-kill-when-opening-new-dired-buffer t)
  :general
  (:states 'normal
           :prefix ","
           "d" '(lambda() (interactive) (dired default-directory)))
  (:states 'normal
           :keymaps 'dired-mode-map
           "j" 'dired-next-line
           "k" 'dired-previous-line
           "h" 'dired-up-directory
           "l" 'dired-find-file
           "yy" 'dired-do-copy
           "yn" 'dired-copy-filename-as-kill
           "m" 'dired-mark
           "u" 'dired-unmark
           "t" 'dired-toggle-marks
           "v" 'dired-display-file
           "cw" 'dired-do-rename
           "r" 'revert-buffer
           "nd" 'dired-create-directory
           "nf" 'dired-create-empty-file
           "s" 'dired-do-async-shell-command
           ))

(use-package embark
  :ensure t

  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-:" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil
  :config
  (evil-set-initial-state 'eww-mode 'emacs)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  :init
  (setq evil-want-C-i-jump nil)
  :hook
  ((prog-mode) . evil-mode)
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.stories.tsx\\'" . typescript-ts-mode))
  :custom
  ;;Point to tree-sitter grammars
  (treesit-extra-load-path '("~/tree-sitter-module/dist"))

  :hook (
         (typescript-ts-base-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :config
  (setq lsp-lens-enable t)
  :commands lsp-ui-mode)

(use-package magit)

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(use-package vterm)

(use-package vterm-toggle)

(use-package nvm)

(use-package prettier-js)

(use-package which-key
  :init
  (setq which-key-idle-delay 0)
  (which-key-mode)
  :diminish which-key-mode)

;; Custom functions 
(defun modmacs/open-config ()
  "Opens my config."
  (interactive) (find-file "~/modmacs/modmacs.org"))

(defun modmacs/tangle-config ()
  "Tangles my config."
  (interactive)
  (org-babel-tangle "~/modmacs/modmacs.org"))

(defun modmacs/reload-config ()
  "Reloads my config."
  (interactive) (load-file "~/modmacs/init.el"))

(defun modmacs/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
