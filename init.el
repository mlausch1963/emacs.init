;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2019 Michael Lausch
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)


;; Take a look at https://github.com/danielmartin/dotfiles/blob/master/.emacs.d/init.el
;;                https://ladicle.com/post/config/
;; for lsp hints i used: https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/

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

(with-system darwin
             (setenv "PATH" (concat "/usr/local/bin"
                                    ":"
                                    (getenv "GOPATH") "/bin"
                                    ":"
                                    (getenv "PATH")))
             (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2"))


;; Always load newest byte code
(setq load-prefer-newer t)

(defvar mla-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Mla distribution.")
(defvar mla-core-dir (expand-file-name "core" mla-dir)
  "The home of Mla's core functionality.")
(defvar mla-modules-dir (expand-file-name  "modules" mla-dir)
  "This directory houses all of the built-in Mla modules.")
(defvar mla-personal-dir (expand-file-name "personal" mla-dir)
  "This directory is for your personal configuration.

Users of Emacs Mla are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Mla.")
(defvar mla-personal-preload-dir (expand-file-name "preload" mla-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Mla.")
(defvar mla-vendor-dir (expand-file-name "vendor" mla-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar mla-savefile-dir (expand-file-name "savefile" mla-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar mla-modules-file (expand-file-name "mla-modules.el" mla-personal-dir)
  "This file contains a list of modules that will be loaded by Mla.")
(defvar mla-deprecated-modules-file
  (expand-file-name "mla-modules.el" mla-dir)
  (format "This file may contain a list of Mla modules.

This is DEPRECATED, use %s instead." mla-modules-file))

(unless (file-exists-p mla-savefile-dir)
  (make-directory mla-savefile-dir))

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

(require 'package)
(add-to-list 'package-archives '("gnu". "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))


(package-initialize)
;; update the package metadata is the local cache is missing

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'bind-key)
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



(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

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
  (defun mla-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'mla-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
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
  :after magit
  :config
  (add-to-list 'forge-alist '("gitlab.advertima.com:2222" "gitlab.advertima.com/api/v4" "gitlab.advertima.com" forge-gitlab-repository)))

(use-package salt-mode
  :ensure t
  :config (add-hook 'salt-mode-hook
                    (lambda ()
                      (flyspell-mode 1)))
  :mode (("\\.sls\\'" . salt-mode)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (require 'shell)
  :config
  (require 'helm-config)
  (setq projectile-completion-system       'helm)
  (setq helm-split-window-in-side-p         t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t
      helm-split-window-default-side        'other)

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



(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-cache-file (expand-file-name
                               "bookmarks"  mla-savefile-dir))
  :delight '(:eval (concat " " (projectile-project-name)))
  )

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

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
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  :bind
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

;; (use-package rtags
;;   :ensure t
;;   :init
;;   (add-hook 'c-mode-common-hook  #'rtags-start-process-unless-running)
;;   :demand
;;   :bind
;;   (:map c-mode-map
;;         ("M-." . rtags-find-symbol-at-point)
;;         ("S-." . rtags-find-references-at-point)
;;         ("M-," . rtags-location-stack-back))

;;   :config
;;   (progn
;;     (rtags-enable-standard-keybindings nil "C-c R")
;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq rtags-completions-enabled t)))


;; (use-package company-rtags
;;   :config
;;   (push 'company-rtags company-backends))

;; (use-package helm-rtags
;;   :ensure t
;;   :init
;;   (setq rtags-use-helm t)

;;   :config
;;   (setq rtags-display-result-backend 'helm))

;; (use-package flycheck-rtags)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook
  (go-mode . ws-no-tabs-highlight)
  (go-mode . lsp-deferred)
  (go-mode . lsp-go-install-save-hooks)
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
  :init (global-flycheck-mode))


(use-package flyspell-correct-helm
  :ensure t
  :demand t
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init (setq flyspell-correct-interface #'flyspell-correct-helm))

(use-package python
  :ensure t
  :interpreter ("python3" . python-mode)
  :mode (("\\.py\\'" . python-mode)))

(use-package pyvenv
  :ensure t
  :defer t
  :diminish
  :config
  (setenv "WORKON_HOME" "/home/mla/python-envs")
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


;;;(defun mla/get-pylint-venv-path ()
;  "Calculate the pylint exec path from active venv"
;  (when pyvenv-activate
;    (setq flycheck-python-pylint-executable
;          (concat (file-name-as-directory
;                   (concat (file-name-as-directory pyvenv-activate) "bin"))
;            "pylint"))))


;(defun mla/set-pylint-from-venv ()
;  "Set the pylint executable depending on the virtualenv setting."
;  (setq flycheck-python-pylint-executable (mla/get-pylint-venv-path)))

;(add-hook 'flycheck-before-syntax-check-hook
;          #'mla/set-pylint-from-venv 'local)

;:plugins.jedi_completion.enabled t
;; :plugins.jedi_definition.follow_imports t
;; :configurationSources ["pycodestyle"]
;; :plugins.jedi_completion.enabled t
;; :plugins.jedi_definition.enabled t
;; :plugins.jedi_definition.follow_imports nil
;; :plugins.jedi_definition.follow_builtin_imports nil
;; :plugins.jedi_hover.enabled t
;; :plugins.jedi_references.enabled t
;; :plugins.jedi_signature_help.enabled nil
;; :plugins.jedi_symbols.enabled nil
;; :plugins.jedi_symbols.all_scopes t
;; :plugins.mccabe.enabled nil
;; :plugins.mccabe.threshold 15
;; :plugins.preload.enabled t
;; :plugins.preload.modules nil
;; :plugins.pycodestyle.enabled t
;; :plugins.pycodestyle.exclude nil
;; :plugins.pycodestyle.filename nil
;; :plugins.pycodestyle.select nil
;; :plugins.pycodestyle.ignore nil
;; :plugins.pycodestyle.hangClosing nil
;; :plugins.pycodestyle.maxLineLength nil
;; :plugins.pydocstyle.enabled nil
;; :plugins.pydocstyle.convention nil
;; :plugins.pydocstyle.addIgnore nil
;; :plugins.pydocstyle.addSelect nil
;; :plugins.pydocstyle.ignore nil
;; :plugins.pydocstyle.select nil
;; :plugins.pydocstyle.match "(?!test_).*.py"
;; :plugins.pydocstyle.matchDir nil
;; :plugins.pyflakes.enabled t
;; :plugins.rope_completion.enabled t
;; :plugins.yapf.enabled t
;; :rope.extensionModules nil
;; :rope.ropeFolder nil

;(defun mla/set-pyls-config ()
;     (let ((lsp-cfg `(:pyls (:plugins.pylint nil))))
;       (lsp--set-configuration lsp-cfg)))



(use-package lsp-mode
  :ensure t
;;  :after (pyvenv)
  :config
  (message "lsp-mode loaded")
  (setq lsp-prefer-flymake nil
        lsp-pyls-plugins-flake8-enabled t
        lsp-log-io t)
  :hook (
         (terraform-mode . lsp)
         (typescript-mode . lsp)
         (python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))


(setq lsp-completion-provider :capf)

(defun mla/on-venv-change-restart-pyls ()
  (let ((venv (lsp-session-get-metadata :venv)))
    (message "pyvenv post hook: lsp env %s, my env %s" venv (getenv "VIRTUAL_ENV"))
    (if (not (eq (getenv "VIRTUAL_ENV") venv))
        (--when-let (pcase (lsp-workspaces)
                      (`nil (user-error "There are no active servers in the current buffer"))
                      (`(,workspace) workspace)
                      (workspaces (lsp--completing-read "VENV Changefd, restaret server: "
                                                        workspaces
                                                        'lsp--workspace-print nil t)))
          (message "pyvenv post hook: restarting pylsp")
          (lsp-workspace-restart it)))))

;(trace-function 'mla/on-venv-change-restart-pyls)

;(add-hook 'pyvenv-post-activate-hooks #'mla/on-venv-change-restart-pyls)

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
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "s-.") #'lsp-ui-peek-find-references)
  (require 'lsp-ui-doc)
  :commands lsp-ui-mode)


(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1)
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-go)
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
    "Rust::GDB Run Configuratrion"
    (list :type "gdb"
	  :request "launch"
	  :name "GDB::Run"
	  :gdbpath "rust-gdb"
	  :target nil
	  :cwd nil))
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))
  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-controls-mode 1)
    (dap-ui-mode 1)))


(use-package gud
  :ensure t)

(use-package go-dlv
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
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
    (treemacs-fringe-indicator-mode t)
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
  :load-path "vendor/bookark-plus")

(use-package ein
  :ensure t)

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
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-agenda-files (quote ("/home/mla/Dropbox-Decrypted/org")))
  )

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :config
      (define-key global-map (kbd "C-c n r") #'org-roam-buffer-toggle-display)
      (define-key global-map (kbd "C-c n i") #'org-roam-insert)
      (define-key global-map (kbd "C-c n /") #'org-roam-find-file)
      (define-key global-map (kbd "C-c n b") #'org-roam-switch-to-buffer)
      (define-key global-map (kbd "C-c n d") #'org-roam-find-directory)
      (define-key global-map (kbd "C-c n c") #'org-roam-capture)
      (define-key global-map (kbd "C-C n t") #'org-roam-dailies-capture-today)
      (define-key global-map (kbd "C-c n x") #'org-roam-dailies-find-date)
      (require 'org-roam-protocol)
      (require 'org-protocol)
      :custom
      (org-roam-directory "/home/mla/Dropbox-Decrypted/org-roam" "home of org roam")
      (org-roam-dailies-directory "/home/mla/Dropbox-Decrypted/org-roam/daily")
      (org-roam-ref-capture-templates
       '(("d" "default" plain (function org-roam--capture-get-point)
        "%?"
        :file-name "${slug}"
        :head "#+TITLE: ${title}\n"
        :unarrowed t)))
      (org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
- source :: ${ref}"
               :unnarrowed t)))
      (org-roam-capture-templates
       `(("d" "default" plain #'org-roam-capture--get-point
           :file-name "%<%Y-%m-%d>-${slug}"
           :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n#+created: %u\n#+last_modified: %U\n%?"
           :unnarrowed t
           :prepend t
           :jump-to-captured t)
          ("l" "clipboard" plain #'org-roam-capture--get-point "%i%a"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+ROAM_TAGS: %?"
           :unnarrowed: t
           :prepend: t
           :jump-to-captured: t
          )))

      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              ("C-c n i" . org-roam-insert)
              ("C-c n I" . org-roam-insert-immediate)))


(use-package org-journal
      :bind
      ("C-c n j" . org-journal-new-entry)
      :custom
      (org-journal-dir "~/Dropbox-Decrypted/org-roam")
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%A, %d %B %Y"))
    (setq org-journal-enable-agenda-integration t)

(use-package helm-org
  :ensure t)

(use-package editorconfig
  :ensure t
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
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

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
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
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
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :custom
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


;;(use-package lsp-jedi
;;  :ensure t
;;  :config
;;  (with-eval-after-load "lsp-mode"
;;    (add-to-list 'lsp-disabled-clients 'pyls)))
;;
;;    (add-to-list 'lsp-enabled-clients 'jedi)))
