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

(use-package which-key
  :ensure t
  :config
  (which-key-add-key-based-replacements
    "C-c &" "Yasnippet"
    "C-c e" "Eglot"
    "C-c a" "Avy"
    "C-c a m" "Move"
    "C-c a c" "Copy"
    "C-c a k" "Kill")
  (which-key-mode 1))

;; Sane defaults
(setq initial-scratch-message "")
(windmove-default-keybindings)
(electric-pair-mode t)
(show-paren-mode t)
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

(defun beginning-of-indent-or-line ()
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(bind-key (kbd "C-a") 'beginning-of-indent-or-line)

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
  ;; the nil enables the theme immediatly
  (load-theme 'doom-outrun-electric t nil))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :after all-the-icons
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t))

;; Editing
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global ";;" "\C-e;"))

(use-package company
  :ensure t
  :diminish
  :bind
  ("C-;" . company-complete)
  (:map company-active-map
	("C-n" . 'company-select-next)
	("C-p" . 'company-select-previous))
  :custom
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-idle-delay 0)
  :config
  (global-company-mode 1))

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
  ("C-c e d" . 'eglot-find-declaration)
  ("C-c e e" . 'eglot-reconnect)
  ("C-c e r" . 'eglot-rename)
  ("C-c e f" . 'eglot-format-buffer)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0))
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))

(use-package flyspell-correct-ivy
  :ensure t
  :diminish
  :after ivy
  :config
  (bind-key (kbd "C-o s") 'flyspell-correct-wrapper flyspell-mode-map))

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
  :bind
  (("C-z" . avy-goto-char)
   ("C-c a c l" . avy-copy-line)
   ("C-c a c r" . avy-copy-region)
   ("C-c a m l" . avy-move-line)
   ("C-c a m r" . avy-move-region)
   ("C-c a k l" . avy-kill-line)
   ("C-c a k r" . avy-kill-region)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy counsel swiper expand-region multiple-cursors flyspell-correct-ivy eglot yasnippet company key-chord doom-modeline all-the-icons doom-themes base16-theme which-key diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
