                                        ; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2019 Michael Lausch
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

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
;; You should have received a cfopy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(defvar use-helm-p nil)
(defvar use-vertico-p t)
(defvar use-consult-p t)
(defvar use-orderless-p t)
(defvar use-rtags-p nil)

(when (version< emacs-version "25.1")
  (error "Mla requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(message "Mla is powering up... Be patient, Master %s!" current-user)


(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(when (version< emacs-version "29.0")
  (defmacro keymap-global-set (key-chord action)
    `(global-set-key (kbd ,key-chord) ,action)))

(when (version< emacs-version "29.0")
  (defmacro keymap-set (key-map key-chord action)
    `(define-key ,key-map (kbd ,key-chord) ,action)))

(when (not (version< emacs-version "29.0"))
  (progn
    (pixel-scroll-precision-mode)))



(with-system darwin
  ;;; I prefer cmd key for meta
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar mla-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Mla distribution.")

(defvar mla-core-dir (expand-file-name "core" mla-dir)
  "The home of Mla's core functionality.")

(defvar mla-modules-dir (expand-file-name  "modules" mla-dir)
  "This directory houses all of the built-in Mla modules.")

(defvar mla-personal-dir (expand-file-name "personal" mla-dir)
  "This directory is for your personal configuration.")

(defvar mla-vendor-dir (expand-file-name "vendor" mla-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar mla-savefile-dir (expand-file-name "savefile" mla-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar mla-modules-file (expand-file-name "mla-modules.el" mla-personal-dir)
  "This file contains a list of modules that will be loaded by Mla.")

(unless (file-exists-p mla-savefile-dir)
  (make-directory mla-savefile-dir))

(unless (file-exists-p mla-personal-dir)
  (make-directory mla-personal-dir))


(defun mla-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (mla-add-subfolders-to-load-path name)))))

;; add Mla's directories to Emacs's `load-path'
(add-to-list 'load-path mla-core-dir)
(add-to-list 'load-path mla-modules-dir)
(add-to-list 'load-path mla-vendor-dir)
(mla-add-subfolders-to-load-path mla-vendor-dir)

(setq custom-file (expand-file-name "custom.el" mla-personal-dir))
(load custom-file)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(eval-after-load 'smerge-mode
  (lambda ()
    (keymap-set smerge-mode-map "C-c v" smerge-basic-map)
    (keymap-set smerge-mode-map "C-c C-v" smerge-basic-map)))

(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("org"          . 5)
        ("MELPA"        . 20)
        ))

(package-initialize)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; update the package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'bind-key)

                                        ; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(setq use-package-verbose t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(window-divider-mode t)

(require 'treesit)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

(use-package compat
	     :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)



(keymap-global-set "C-z" #'undo)
(keymap-global-set "C-x C-b" #'ibuffer)

;; align code in a pretty way
(keymap-global-set "C-x \\" #'align-regexp)

;; misc useful keybindings
(keymap-global-set "s-<" #'beginning-of-buffer)
(keymap-global-set "s->" #'end-of-buffer)
(keymap-global-set "s-q" #'fill-paragraph)
(keymap-global-set "s-x" #'execute-extended-command)

                                        ; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)


(recentf-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'rst-mode-hook 'display-line-numbers-mode)
(add-hook 'salt-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)



(use-package diminish
  :ensure t)

(use-package delight
  :ensure t)

(use-package company
  :diminish company-mode
  :ensure t
  :bind ("M-/" . company-complete-common)
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))


;;; built-in packages
(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package abbrev
  :delight abbrev-mode
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" mla-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" mla-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" mla-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package edit-server
  :ensure t
  :if window-system
  :config
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package which-key
  :ensure t
  :diminish
  :init (which-key-mode +1))

(use-package eldoc
  :ensure t
  :defer t
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package lisp-mode
  :after
  (rainbow-delimiter-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (keymap-set emacs-lisp-mode-map "C-c C-c" #'eval-defun)
  (keymap-set emacs-lisp-mode-map "C-c C-b" #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))


(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))


(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package smart-mode-line
  :ensure t
  :config
  (load-theme 'smart-mode-line-dark t))

(use-package magit
  :ensure t
  :bind (("s-g" . magit-status))
  :commands magit-status)

(use-package forge
  :ensure t
  :after magit)


(use-package salt-mode
  :ensure t
  :config (add-hook 'salt-mode-hook
                    (lambda ()
                      (flyspell-mode 1)))
  :mode (("\\.sls\\'" . salt-mode)))




(if use-vertico-p
    (progn
      ;; Workaround for problem with `tramp' hostname completions. This overrides
      ;; the completion style specifically for remote files! See
      ;; https://github.com/minad/vertico#tramp-hostname-completion
      (defun kb/basic-remote-try-completion (string table pred point)
        (and (vertico--remote-p string)
             (completion-basic-try-completion string table pred point)))
      (defun kb/basic-remote-all-completions (string table pred point)
        (and (vertico--remote-p string)
             (completion-basic-all-completions string table pred point)))
      (add-to-list 'completion-styles-alist
                   '(basic-remote           ; Name of `completion-style'
                     kb/basic-remote-try-completion kb/basic-remote-all-completions nil))


      (use-package vertico
        :demand t
        :ensure t
        :bind
        (:map vertico-map
                  ("<tab>" . #'vertico-insert) ; Set manually otherwise setting `vertico-quick-insert' overrides this
                  ("<escape>" . #'minibuffer-keyboard-quit)
                  ("?" . #'minibuffer-completion-help)
                  ("C-M-n" . #'vertico-next-group)
                  ("C-M-p" . #'vertico-previous-group)
                  ;; Multiform toggles
                  ("<backspace>" . #'vertico-directory-delete-char)
                  ("C-w" . #'vertico-directory-delete-word)
                  ("C-<backspace>" . #'vertico-directory-delete-word)
                  ("RET" . #'vertico-directory-enter)
                  ("C-i" . #'vertico-quick-insert)
                  ("C-o" . #'vertico-quick-exit)
                  ("M-o" . #'kb/vertico-quick-embark)
                  ("M-G" . #'vertico-multiform-grid)
                  ("M-F" . #'vertico-multiform-flat)
                  ("M-R" . #'vertico-multiform-reverse)
                  ("M-U" . #'vertico-multiform-unobtrusive)
                  ("C-l" . #'kb/vertico-multiform-flat-toggle)
                  )

        :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
               (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
               )
        :custom
        (vertico-count 13)
;;        (vertico-resize t)
        (vertico-cycle nil)
        ;; Extensions
        (vertico-grid-separator "       ")
        (vertico-grid-lookahead 50)
        (vertico-buffer-display-action '(display-buffer-reuse-window))
        (vertico-multiform-categories
         '((consult-grep buffer)
           (consult-location)
           (imenu buffer)
           ;(library reverse indexed)
           (library indexed)
           ;(org-roam-node reverse indexed)
           (org-roam-node indexed)
           ))
        (vertico-multiform-commands
         '(("flyspell-correct-*" grid reverse)
           (org-refile grid reverse indexed)
           (consult-yank-pop indexed)
           (consult-flycheck)
           (consult-lsp-diagnostics)
           ))
        :init
        (defun kb/vertico-multiform-flat-toggle ()
          "Toggle between flat and reverse."
          (interactive)
          (vertico-multiform--display-toggle 'vertico-flat-mode)
          (if vertico-flat-mode
              (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
            (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
        (defun kb/vertico-quick-embark (&optional arg)
          "Embark on candidate using quick keys."
          (interactive)
          (when (vertico-quick-jump)
            (embark-act arg)))

        :config
        (vertico-mode)
        ;; Extensions
        (vertico-multiform-mode)

        ;; Prefix the current candidate with “» ”. From
        ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
        (advice-add #'vertico--format-candidate :around
                    (lambda (orig cand prefix suffix index _start)
                      (setq cand (funcall orig cand prefix suffix index _start))
                      (concat
                       (if (= vertico--index index)
                           (propertize "» " 'face 'vertico-current)
                         "  ")
                       cand)))
        )

      (use-package marginalia
        :after vertico
        :ensure t
        :custom
        (marginalia-annotator '(marginalia-annotators-heavy marginalia-annotators-light nil))
        :init
        (marginalia-mode))

      (use-package all-the-icons-completion
        :ensure t
        :after (marginalia all-the-icons)
        :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
        :init
        (all-the-icons-completion-mode))

      (use-package emacs
        :init
        ;; Add prompt indicator to `completing-read-multiple'.
        ;; Alternatively try `consult-completing-read-multiple'.
        (defun crm-indicator (args)
          (cons (concat "[CRM] " (car args)) (cdr args)))
        (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
        ;; Do not allow the cursor in the minibuffer prompt
        (setq minibuffer-prompt-properties
              '(read-only t cursor-intangible t face minibuffer-prompt))
        (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
        ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
        ;; Vertico commands are hidden in normal buffers.
        (setq read-extended-command-predicate
              #'command-completion-default-include-p)
        ;; Enaable recursive minibuffers
        (setq enable-recursive-minibuffers 't)
        )))

(if use-consult-p
    (progn
      ;; Example configuration for Consult
      (use-package consult
        :ensure t
        ;; Replace bindings. Lazily loaded due by `use-package'.
        :bind (;; C-c bindings (mode-specific-map)
               ("C-c h" . consult-history)
               ("C-c m" . consult-mode-command)
               ("C-c b" . consult-bookmark)
               ("C-c k" . consult-kmacro)
               ;; C-x bindings (ctl-x-map)
               ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
               ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
               ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
               ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
               ;; Custom M-# bindings for fast register access
               ("M-#" . consult-register-load)
               ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
               ("C-M-#" . consult-register)
               ;; Other custom bindings
               ("M-y" . consult-yank-pop)                ;; orig. yank-pop
               ("<help> a" . consult-apropos)            ;; orig. apropos-command
               ;; M-g bindings (goto-map)
               ("M-g e" . consult-compile-error)
               ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
               ("M-g g" . consult-goto-line)             ;; orig. goto-line
               ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
               ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
               ("M-g m" . consult-mark)
               ("M-g k" . consult-global-mark)
               ("M-g i" . consult-imenu)
               ("M-g I" . consult-imenu-multi)
               ;; M-s bindings (search-map)
               ("M-s f" . consult-find)
               ("M-s F" . consult-locate)
               ("M-s g" . consult-grep)
               ("M-s G" . consult-git-grep)
               ("M-s r" . consult-ripgrep)
               ("M-s l" . consult-line)
               ("M-s L" . consult-line-multi)
               ("M-s m" . consult-multi-occur)
               ("M-s k" . consult-keep-lines)
               ("M-s u" . consult-focus-lines)
               ;; Isearch integration
               ("M-s e" . consult-isearch)
               :map isearch-mode-map
               ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
               ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
               ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
               ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

        ;; Enable automatic preview at point in the *Completions* buffer.
        ;; This is relevant when you use the default completion UI,
        ;; and not necessary for Vertico, Selectrum, etc.
        :hook (completion-list-mode . consult-preview-at-point-mode)

        ;; The :init configuration is always executed (Not lazy)
        :init

        ;; Optionally configure the register formatting. This improves the register
        ;; preview for `consult-register', `consult-register-load',
        ;; `consult-register-store' and the Emacs built-ins.
        (setq register-preview-delay 0.5
              register-preview-function #'consult-register-format)

        ;; Optionally tweak the register preview window.
        ;; This adds thin lines, sorting and hides the mode line of the window.
        (advice-add #'register-preview :override #'consult-register-window)

;        ;; Optionally replace `completing-read-multiple' with an enhanced version.
;        (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

        ;; Use Consult to select xref locations with preview
        (setq xref-show-xrefs-function #'consult-xref
              xref-show-definitions-function #'consult-xref)

        ;; Configure other variables and modes in the :config section,
        ;; after lazily loading the package.
        :config

        ;; Optionally configure preview. The default value
        ;; is 'any, such that any key triggers the preview.
        ;; (setq consult-preview-key 'any)
        ;; (setq consult-preview-key (kbd "M-."))
        ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
        ;; For some commands and buffer sources it is useful to configure the
        ;; :preview-key on a per-command basis using the `consult-customize' macro.
        (consult-customize
         consult-theme :preview-key '(:debounce 0.2 any)
         consult-ripgrep consult-git-grep consult-grep
         consult-bookmark consult-recent-file consult-xref
         consult--source-bookmark consult--source-file-register
         consult--source-recent-file consult--source-project-recent-file
         :preview-key '(:debounce 0.4 any)
         :preview-key "M-.")

        ;; Optionally configure the narrowing key.
        ;; Both < and C-+ work reasonably well.
        (setq consult-narrow-key "<") ;; (kbd "C-+")

        ;; Optionally make narrowing help available in the minibuffer.
        ;; You may want to use `embark-prefix-help-command' or which-key instead.
        ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

        ;; Optionally configure a function which returns the project root directory.
        ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
        ;;        (setq consult-project-root-function
        ;;            (lambda ()
        ;;            (when-let (project (project-current))
        ;;            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
        (autoload 'projectile-project-root "projectile")
        (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
        ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
        ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
        )
      (use-package embark
        :ensure t
        :after projectile
        :bind
        (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

        :init

        ;; Optionally replace the key help with a completing-read interface
        (setq prefix-help-command #'embark-prefix-help-command)

        :config

        ;; Hide the mode line of the Embark live/completions buffers
        (add-to-list 'display-buffer-projectile-alist
                     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                       nil
                       (window-parameters (mode-line-format . none)))))

      ;; Consult users will also want the embark-consult package.
      (use-package embark-consult
        :ensure t
        :after (embark consult)
        :demand t ; only necessary if you have the hook below
        ;; if you want to have consult previews as you move around an
        ;; auto-updating embark collect buffer
        :hook
        (embark-collect-mode . consult-preview-at-point-mode))

      ))

(if use-orderless-p
    (progn

      (defun flex-if-twiddle (pattern _index _total)
        (when (string-suffix-p "~" pattern)
          `(orderless-flex . ,(substring pattern 0 -1))))

      ;; Maybe use this:
      (defun mla/orderless-initialism-dispatcher (pattern _index _total)
        "Leading initialism  dispatcher using the comma suffix.
         It matches PATTERN _INDEX and _TOTAL according to how Orderless
         parses its input."
        (when (string-suffix-p "," pattern)
          `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))


      (defun first-initialism (pattern index _total)
        (if (= index 0) 'orderless-initialism))

      (defun without-if-bang (pattern _index _total)
        (cond
         ((equal "!" pattern)
          '(orderless-literal . ""))
         ((string-prefix-p "!" pattern)
          `(orderless-without-literal . ,(substring pattern 1)))))


      (defun mla/orderless--strict-*-initialism (component &optional anchored)
        "Match a COMPONENT as a strict initialism, optionally ANCHORED.
         The characters in COMPONENT must occur in the candidate in that
         order at the beginning of subsequent words comprised of letters.
         Only non-letters can be in between the words that start with the
         initials.

         If ANCHORED is `start' require that the first initial appear in
         the first word of the candidate.  If ANCHORED is `both' require
         that the first and last initials appear in the first and last
         words of the candidate, respectively."
        (orderless--separated-by
            '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
          (cl-loop for char across component collect `(seq word-start ,char))
          (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
          (when (eq anchored 'both)
            '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))


      (defun mla/orderless-strict-initialism (component)
        "Match a COMPONENT as a strict initialism.
         This means the characters in COMPONENT must occur in the
         candidate in that order at the beginning of subsequent words
         comprised of letters.  Only non-letters can be in between the
         words that start with the initials."
        (mla/orderless--strict-*-initialism component))


      (defun mla/orderless-literal-dispatcher (pattern _index _total)
        "Literal style dispatcher using the equals sign as a suffix.
         It matches PATTERN _INDEX and _TOTAL according to how Orderless
         parses its input."
        (when (string-suffix-p "=" pattern)
          `(orderless-literal . ,(substring pattern 0 -1))))

      (defun mla/orderless-strict-initialism-dispatcher (pattern _index _total)
        "Leading initialism  dispatcher using the comma suffix.
         It matches PATTERN _INDEX and _TOTAL according to how Orderless
         parses its input."
        (when (string-suffix-p "," pattern)
          `(mla/orderless-strict-initialism . ,(substring pattern 0 -1))))


      (defun mla/orderless-flex-dispatcher (pattern _index _total)
        "Flex  dispatcher using the tilde suffix.
         It matches PATTERN _INDEX and _TOTAL according to how Orderless
         parses its input."
        (when (string-suffix-p "." pattern)
          `(orderless-flex . ,(substring pattern 0 -1))))

      (use-package orderless
        :ensure t
        :custom
        completion-styles '(orderless)
        :init
        (setq completion-category-overrides '((file (styles basic-remote orderless)))
              orderless-matching-styles '(orderless-literal
                                          orderless-prefixes
                                          orderless-initialism
                                          orderless-regexp)
              orderless-style-dispatchers '(mla/orderless-literal-dispatcher
                                            mla/orderless-strict-initialism-dispatcher
                                            mla/orderless-flex-dispatcher)))))


(if use-helm-p
    (progn
      (/ 1 0)
      (use-package helm
        :ensure t
        :diminish helm-mode
        :init
        (require 'shell)
        :config
        (require 'helm-config)
        (setq projectile-complet\ion-system 'helm)
        (setq helm-split-window-inside-p t
              helm-buffers-fuzzy-matching t
              helm-move-to-line-cycle-in-source t
              helm-ff-search-library-in-sexp t
              helm-ff-file-name-history-use-recentf t
              helm-split-window-default-side 'other)

        (global-unset-key (kbd "C-x c"))
        (helm-mode 1)
        :hook
        (eshell-mode . (lambda ()
                         (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))
        :bind-keymap
        ("C-c h" . helm-command-prefix)
        :bind (
               (("M-x" . helm-M-x)
                ("M-y" . helm-show-kill-ring)
                ("C-x C-f" . helm-find-files)
                ("C-x f" . helm-recentf)
                ("C-c o" . helm-occur)
                ("C-x b" . helm-mini)
                ("C-x C-b" . helm-buffers-list)
                ("C-h f" . helm-apropos)
                ("C-h r" . helm-info-emacs)
                ("C-c C-l" . helm-locate-library))
               :map minibuffer-local-map
               ("C-c  C-l" . 'helm-minibuffer-history)
               :map isearch-mode-map
               ("C-o" . 'helm-occur-from-isearch)
               :map shell-mode-map
               ("C-c C-l" . 'helm-comint-input-ring)
               :map helm-map
               ("<tab>" . 'helm-execute-persistent-action)
               :map helm-map
               ("C-i" . 'helm-execute-persistent-action)
               ("C-z" . helm-select-action)))

      (use-package helm-swoop
        :ensure t
        :bind
        (("C-S-s" . helm-swoop-without-pre-input)
         ("S-s" . helm-swoop)))

      (use-package helm-descbinds
        :ensure t
        :init
        (helm-descbinds-mode))

      (use-package helm-git-grep
        :ensure t
        :bind
        (("C-c j" . helm-git-grep)
         ("C-c J" . helm-git-grep-at-point)))

      (use-package helm-ls-git
        :ensure t
        :bind
        (("C-c g" . helm-ls-git-ls)))

      (use-package helm-make
        :ensure t
        :bind
        (("C-c K" . helm-make)))

      (use-package helm-projectile
        :ensure t
        :config
        :after (helm projectile)
        (helm-projectile-on))

      (use-package helm-lsp
        :ensure t
        :after (helm
                :commands helm-lsp-workspace-symbol))

      (use-package flyspell-correct-helm
        :ensure t
        :demand t
        :bind ("C-M-;" . flyspell-correct-wrapper)
        :init (setq flyspell-correct-interface #'flyspell-correct-helm))

      (use-package helm-org
        :ensure t)

      ))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  :init
  (setq projectile-cache-file (expand-file-name
                               "bookmarks"  mla-savefile-dir))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map))


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :delight
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :delight whitespace-mode
  :init
  :config
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :preface
  (defun jekyll-insert-image-url ()
    (interactive)
    (let* ((files (directory-files "../assets/images"))
           (selected-file (completing-read "Select image: " files nil t)))
      (insert (format "![%s](/assets/images/%s)" selected-file selected-file))))

  (defun jekyll-insert-post-url ()
    (interactive)
    (let* ((files (remove "." (mapcar #'file-name-sans-extension (directory-files "."))))
           (selected-file (completing-read "Select article: " files nil t)))
      (insert (format "{%% post_url %s %%}" selected-file)))))

(use-package highlight-indentation
  :ensure t)

(use-package yaml-mode
  :ensure t
  :init
  (require 'yaml-ts-mode)
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  :bind
  (:map yaml-ts-mode-map
        ("\C-m" . newline-and-indent))
  (:map yaml-mode-map
        ("\C-m" . newline-and-indent)))


(if (executable-find "rg")
    (use-package rg
      :ensure t
      :bind
      (("C-c R" . rg))))


(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C/C++ Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package google-c-style
  :ensure t
  )

(c-add-style "google-c-style" google-c-style)
(setq-default c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c++-mode . "google-c-style")
                        (other . "google-c-style")))

(defun ws-no-tabs-highlight ()
  (interactive)
  (setq whitespace-style (remove 'tabs whitespace-style))
  (whitespace-mode 0)
  (whitespace-mode 1)
  )


(defun mla-c-mode-keys ()
  (local-set-key "\C-i" #'company-indent-or-complete-common)
  (local-set-key (kbd "<tab>") #'company-indent-or-complete-common)
  )

(defun mla-c-mode-minor-modes ()
  (rainbow-mode 0))

(add-hook 'c-mode-common-hook #'ws-no-tabs-highlight)
(add-hook 'c-mode-common-hook #'mla-c-mode-keys)
(add-hook 'c-mode-common-hook #'mla-c-mode-minor-modes)
(add-hook 'makefile-mode-hook #'ws-no-tabs-highlight)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c-mode-common-hook #'ws-no-tabs-highlight)

(if use-rtags-p
    (progn
      (use-package rtags
        :ensure t
        :init
        (add-hook 'c-mode-common-hook  #'rtags-start-process-unless-running)
        :demand
        :bind
        (:map c-mode-map
              ("M-." . rtags-find-symbol-at-point)
              ("S-." . rtags-find-references-at-point)
              ("M-," . rtags-location-stack-back))

        :config
        (progn
          (rtags-enable-standard-keybindings nil "C-c R")
          (setq rtags-autostart-diagnostics t)
          (rtags-diagnostics)
          (setq rtags-completions-enabled t)))


      (use-package company-rtags
        :config
        (push 'company-rtags company-backends))

      (if use-helm-p
          (use-package helm-rtags
            :ensure t
            :init
            (setq rtags-use-helm t)
            :config
            (setq rtags-display-result-backend 'helm)))

      (use-package flycheck-rtags
        :ensure t)
      ))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook
  (go-mode . ws-no-tabs-highlight)
  (go-mode . lsp-deferred)
  (go-mode . lsp-go-install-save-hooks)
  (go-mode . company-mode)
  (go-ts-mode . ws-no-tabs-highlight)
  (go-ts-mode . lsp-deferred)
  (go-ts-mode . lsp-go-install-save-hooks)
  (go-ts-mode . company-mode)
  :ensure t)

(use-package go-stacktracer
  :ensure t)

(use-package golint
  :ensure t)

(use-package flycheck-golangci-lint
  :after flycheck
  :hook (go-mode . flycheck-golangci-lint-setup)
  :ensure t)

(use-package flycheck
  :ensure t
  :diminish
  :init (global-flycheck-mode))



(use-package python
  :ensure t
  :config
  (add-hook 'python-mode-hook  #'ws-no-tabs-highlight)
  :interpreter ("python3" . python-mode)
  :mode (("\\.py\\'" . python-mode)))

(use-package pyvenv
  :ensure t
  :defer t
  :diminish
  :config
  (setenv "WORKON_HOME" "~/.virtualenvs")
  (add-hook 'pyython-mode-hook  #'pyvenv-tracking-mode)
  (add-hook 'python-mode-hook #'pyvenv-mode))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode))
  )

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf\\'" . terraform-mode))
  )


(use-package lsp-mode
  :ensure t
  ;;  :after (pyvenv)
  :config
  (message "lsp-mode loaded")
  (setq lsp-prefer-flymake nil
        lsp-log-io nil
        lsp-rust-analyzer-cargo-watch-command "clippy")
  :hook (
         (terraform-mode . lsp)
         (typescript-mode . lsp)
         (ruby-mode . lsp)
         (ruby-ts-mode . lsp)
         (js-mode . lsp)
         (javascript-mode . lsp)
         (yaml-mode . lsp-deferred)
         (yaml-ts-mode . lsp-deferred)
         (typescript-mode . lsp)
         (web-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (python-mode . (lambda ()
                          (require 'lsp-pylsp)
                          (lsp)))
         (python-ts-mode . (lambda ()
                          (require 'lsp-pylsp)
                          (lsp)))
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode)))



(use-package lsp-ui
  :ensure t
  :after flycheck
  :config
  (setq lsp-ui-doc-use-webkit nil
                                        ;        lsp-ui-flycheck-enable t
                                        ;        lsp-ui-flycheck-list-position 'bottom
                                        ;        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (require 'lsp-ui-peek)
  (keymap-set lsp-ui-mode-map "<remap> <xref-find-definitions>" #'lsp-ui-peek-find-definitions)
  (keymap-set lsp-ui-mode-map "s-." #'lsp-ui-peek-find-references)
  (require 'lsp-ui-doc)
  :commands lsp-ui-mode)



(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1)
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t
  :after 'lsp-mode
  :config
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-dlv-go)
  (require 'dap-hydra)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)

  (dap-register-debug-template
   "Rust::GDB Run Configuratrion"
   (list :type "gdb"
         :request "launch"
         :name "GDB::Run"
         :gdbpath "rust-gdb"
         :target nil
         :cwd (expand-file-name "~"))

  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
         :target nil
         :gdbpath "rust-lldb"
            ))
  :hook
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))))

(use-package gud
  :ensure t)

(use-package beacon
  :ensure t
  :diminish
  :config
  (setq beacon-blink-duration 1.0
        beacon-color "aquamarine")
  (beacon-mode 1))

(use-package company
  :hook (python-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

(use-package posframe
  :ensure t
  ;; grame is needed by dap-mode, must be installed manually
  )

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (keymap-set winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :disabled
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-perspective
  :after (treemacs perspective)
  :ensure t
  :init (treemacs-set-scope-type 'Frames))

(defun mla/download-bookmark+ ()
  "Download the bookmark+ source from the Emacs wiki."
  (interactive)
  (let ((gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
        (bookmarkplus-dir "~/.emacs.d/vendor/bookmark-plus/")
        (emacswiki-base "https://www.emacswiki.org/emacs/download/")
        (bookmark-files '("bookmark+.el" "bookmark+-mac.el" "bookmark+-bmu.el" "bookmark+-key.el" "bookmark+-lit.el" "bookmark+-1.el")))
    (require 'url)
    (add-to-list 'load-path bookmarkplus-dir)
    (make-directory bookmarkplus-dir t)
    (mapcar (lambda (arg)
              (let ((local-file (concat bookmarkplus-dir arg)))
                (unless (file-exists-p local-file)
                  (url-copy-file (concat emacswiki-base arg) local-file t))))
            bookmark-files)
    (byte-recompile-directory bookmarkplus-dir 0)))


(use-package bookmark+
  :config
  (setq bookmark-version-control t)
  (setq delete-old-versions t)
  :load-path "vendor/bookmark-plus")

(use-package ein
  :ensure t
  :commands (ein:notebooklist-open)
  :config
  (require 'ein-loaddefs)
  (require 'ein))




(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))


(use-package slime
  :config
  (progn
    (load (expand-file-name "~/quicklisp/slime-helper.el")
          (setq inferior-lisp-program "/usr/bin/sbcl")))
  :ensure t
  :mode (("\\.lsp\\'" . slime-mode)
         ("\\.lisp\\'" . slime-mode)))

(use-package slime-company
  :ensure t)

(use-package slime-docker
  :ensure t)

(use-package goggles
  :demand t
  :ensure t
  :diminish
  :config
  (goggles-mode)
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing


(use-package sqlite3
  :ensure t
  )

(use-package org-ref
  :ensure t
  )


(use-package protobuf-mode
  :init
  :ensure t)

(use-package org
  :ensure t
  :bind
  ("C-c a". org-agenda)
  :config
  (setq org-agenda-files (list
                          "~/Dropbox-Decrypted/org/agenda.org"
                          "~/Dropbox-Decrypted/org/home.org"
                          "~/Dropbox-Decrypted/org/kubernetes.org"
                          "~/Dropbox-Decrypted/org/omnia.org"
                          "~/Dropbox-Decrypted/org/personal-projects.org"
                          "~/Dropbox-Decrypted/org/technology.org"
                          "~/Dropbox-Decrypted/org/todo.org"
                          "~/Dropbox-Decrypted/org/work.org")))

(use-package org-pomodoro
  :ensure t)


(defun org-roam-node-insert-immediate (arg &rest args)
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append org-roam-capture-templates)
                                          `(:immediate-finish t))))
    (apply #'org-roam-node-insert args)))

(defun mla/org-roam-filter-by-tag (tag-name)
  (maopcar #'org-roam-node-file
           (seq-filter
            (mla/org-roam-filter-by-tag tag-name)
            (org-roam-list))))

(defun mla/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (require 'org-roam-protocol)
  (require 'org-protocol)
  (require 'org-roam-dailies)
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-directory "~/Dropbox-Decrypted/org-roam" "home of org roam")
  (org-roam-dailies-directory "~/Dropbox-Decrypted/org-roam/daily")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-ref-capture-templates
   '(("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :head "#+TITLE: ${title}\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unarrowed t))
   '(("r" "ref" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "websites/${slug}"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
- source :: ${ref}"
      :unnarrowedwed t)))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-db-autosync-mode))

(use-package consult-org-roam
  :ensure t
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-igrep)
  :bind
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n r" . consult-org-roam-search))

(use-package org-journal
  :ensure t
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/Dropbox-Decrypted/org-roam")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y"))

(setq org-journal-enable-agenda-integration t)


(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

(use-package spdx
  :ensure t
  :bind (:map prog-mode-map
              ("C-c i l" . spdx-insert-spdx))
  :custom
  (spdx-copyright-holder 'auto)
  (spdx-project-detection 'auto))

(use-package deft
  :ensure t
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recusive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Dropbox-Decrypted/org-roam/")
  )

;;;
;;; https://github.com/stardiviner/edebug-inline-result
;;; https://github.com/stardiviner/edebug-inline-result
;;; https://github.com/alpha-catharsis/sudo-utils
;;;

(put 'erase-buffer 'disabled nil)1

(use-package goggles
  :demand t
  :ensure t
  :config
  (goggles-mode)
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

(use-package monkeytype
  :demand t
  :ensure t)

;; rust development
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save nil
        rustic-format-trigger nil
        rustic-lsp-format t
        lsp-rust-analyzer-cargo-watch-command "clippy")
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package cargo-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

                                        ; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)
(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-s-p"))
  (persp-state-default-file (expand-file-name "perspectives.el" mla-personal-dir))
  :init (persp-mode)
  :hook (kill-emacs . persp-state-save))


(use-package smartparens
  :ensure t
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp )
              ("C-M-b" . sp-backward-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("M-s" . sp-split-sexp)
              ("M-j" . sp-join-sexp)
              ("C->" . sp-forward-slurp-sexp)
              ("C-<" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-slurp-sexp)
              ("C-}" . sp-backward-barf-sexp)
              ))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package adoc-mode
  :ensure t
  :mode (("\\.adoc\\'" . adoc-mode)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package bufler
  :ensure t
  :bind (("C-M-j" . bufler-switch-buffer)
         ("C-M-k" . bufler-workspace-frame-set))
  :config
  (setf bufler-groups
        (bufler-defgroups
          ;; Subgroup collecting all named workspaces.
          (group (auto-workspace))
          ;; Subgroup collecting buffers in a projectile project.
          (group (auto-projectile))
          ;; Grouping browser windows
          (group
           (group-or "Browsers"
                     (name-match "Vimb" (rx bos "vimb"))
                     (name-match "Qutebrowser" (rx bos "Qutebrowser"))
                     (name-match "Chromium" (rx bos "Chromium"))))
          (group
           (group-or "Chat"
                     (mode-match "Telega" (rx bos "telega-"))))
          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "Help/Info"
                     (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                     ;; (mode-match "*Helpful*" (rx bos "helpful-"))
                     (mode-match "*Info*" (rx bos "info-"))))
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (name-match "**Special**"
                                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*"))))
          ;; Group remaining buffers by major mode.
          (auto-mode))))

(use-package consult-projectile
  :ensure t)

(with-system gnu/linux
  (use-package vterm
    :ensure t))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'". web-mode)
         ("\\.jsx\\'". web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        ))


(use-package org-modern
  :ensure t
  :hook (org-mode-hook . org-modern-mode))

(use-package gh-md
  :ensure t
  )


(use-package codespaces
  :ensure t
  :config (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect))

(setq vc-handled-backends '(Git))


;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   ;; activate tree-sitter on any buffer containing code for which it has a parser available
;;   (global-tree-sitter-mode)
;;   ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
;;   ;; by switching on and off
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

;; (use-package tree-sitter-indent
;;   :ensure t
;;   :after tree-sitter)


(use-package exercism
  :ensure t)

(use-package gitlab-ci-mode
  :ensure t
  :after lsp-mode
  :init (progn
          (add-to-list 'lsp-language-id-configuration '(gitlab-ci-mode . "yaml"))
          (lsp-register-client
           (make-lsp-client :new-connection (lsp-tramp-connection
                                             (lambda () '("gh-solargraph" "stdio")))
                            :major-modes '(ruby-mode)
                            :language-id "ruby"
                            :priority -2
                            :server-id 'remote-solargraph-lsp
                            :remote? t
                            :after-open-fn (lambda ()
                                             (setq-local lsp-completion-filter-on-incomplete nil))))
          :commands (lsp lsp-deferred)

          (lsp-register-client
           (make-lsp-client :new-connection (lsp-stdio-connection
                                             (lambda ()
                                               `(,(or (executable-find (cl-first lsp-yaml-server-command))
                                                      (lsp-package-path 'yaml-language-server))
                                                 ,@(cl-rest lsp-yaml-server-command))))
                            :major-modes '(gitlab-ci-mode)
                            :priority 0
                            :server-id 'yamlci
                            :initialized-fn (lambda (workspace)
                                              (with-lsp-workspace workspace
                                                                  (lsp--set-configuration
                                                                   (lsp-configuration-section "yaml"))))
                            :download-server-fn (lambda (_client callback error-callback _update?)
                                                  (lsp-package-ensure 'yaml-language-server
                                                                      callback error-callback))))))


(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (setq yaml-ts-mode-hook yaml-mode-hook)
  (global-treesit-auto-mode))

(use-package meson-mode
  :ensure t
  :config (add-hook 'meson-mode-hook 'company-mode)
)


;; make sure '--stdio' is part of lsp-go-gopls-server-args
;; and return a modified list

(defun _fixup-gopls-server-args ()
  (if (not (member "--stdio" lsp-go-gopls-server-args))
      (append lsp-go-gopls-server-args '("--stdio"))))

(defun _fixup-gopls-server-args ()
  (if (not (member "--stdio" lsp-rust-gopls-server-args))
      (append lsp-go-gopls-server-args '("--stdio"))))
