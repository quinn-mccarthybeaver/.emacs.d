;; initial setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

;; Sane defaults
(line-number-mode t)
(setq initial-scratch-message "; scratch pad\n")
(windmove-default-keybindings)
(electric-pair-mode)
(show-paren-mode)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode)
(setq indent-tabs-mode nil)
(setq scroll-conservatively 5)
(setq-default major-mode 'prog-mode)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 4) ((meta)) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; remember mode
(global-set-key (kbd "C-c r") 'remember)

;; Org-mode defaults
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(defun my-org-newline-and-indent ()
  "makes new item, indents, and shifts the item head to the
left. Makes making indented lists nicer"
  (interactive)
  (org-insert-item)
  (org-indent-item)
  (org-shiftleft))
(eval-after-load 'org '(bind-key (kbd "C-c <C-return>") 'my-org-newline-and-indent org-mode-map))

;; Python
(setq python-shell-interpreter "/usr/bin/python")

;;; PACKAGES

;; Appearance
;; themes I like:
;; base16-gruvbox-dark-pale
;; doom-gruvbox
;; doom-acario-dark
;; doom-manegarm
;; doom-outrun-electric

(use-package base16-theme
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;;(load-them 'base16-gruvbox-dark-pale)
  (load-theme 'doom-outrun-electric t))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :after all-the-icons
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Editing
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global ";;" "\C-e;"))

(use-package company
  :ensure t
  :diminish
  :bind ("C-c TAB" . company-complete)
  :config
  (global-company-mode 1)
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq-default abbrev-mode nil))

(use-package yasnippet
  :ensure t
  :diminish
  :config
  (yas-global-mode 1))

;;; dependancies for pyls:
;;; Rope: completions and renaming (Downloaded)
;;; Pyflakes: error detection
;;; McCabe: complexity checking
;;; pycodestyle: checks codestyle
;;; YAPF: code formatting (Downloaded)
(use-package eglot
  :ensure t
  :diminish
  :hook
  ((python-mode c-mode) . eglot-ensure)
  :bind
  ("C-c e r" . 'eglot-rename)
  ("C-c e f" . 'eglot-format-buffer)
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-size 0))

(use-package flyspell-correct-ivy
  :ensure t
  :diminish
  :after ivy
  :config
  (bind-key (kbd "C-;") 'flyspell-correct-wrapper flyspell-mode-map))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<". mc/mark-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Search / Movement / Quality of life
(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure t
  :after (ivy swiper))

(use-package avy
  :ensure t
  :init (unbind-key (kbd "C-z"))
  :bind ("C-z" . avy-goto-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTOMATICALLY CONFIGURED DONT TOUCH ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-auto-complete-chars '(40 41 46))
 '(company-backends
   '(company-bbdb company-semantic company-clang company-xcode company-cmake company-capf company-files company-oddmuse company-dabbrev))
 '(custom-safe-themes
   '("1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" default))
 '(electric-pair-mode t)
 '(package-selected-packages
   '(doom-modeline doom-themes yasnippet eglot key-chord smart-mode-line flyspell-correct-ivy company counsel swiper avy ivy expand-region multiple-cursors base16-theme use-package diminish))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
