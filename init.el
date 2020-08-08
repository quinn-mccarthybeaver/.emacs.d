;; Bootstraps straight process
;; possible source of slow start times
(defvar bootstrap-version)
(setq straight-fix-flycheck t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(setq straight-use-package-by-default t)

(use-package bind-key)

(use-package which-key
  :config
  (which-key-add-key-based-replacements
    "C-c &" "Yasnippet"
    "C-c e" "Eglot"
    "C-c a" "Avy"
    "C-c a m" "Move"
    "C-c a c" "Copy"
    "C-c a k" "Kill"
    "C-c m" "multiple cursors")
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
  "move to beginning of line text unless already there, then move
to beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'beginning-of-indent-or-line)

(defun open-previous-line ()
  "moves current line down one."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))
(global-set-key (kbd "C-o") 'open-previous-line)

(defun open-next-line ()
  "Opens and moves to next line, regardless of current column
position."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<C-return>") 'open-next-line)

(defun kill-and-join-forward ()
  "if at the end of line, brings below line up and deletes
  whitespace"
  (interactive)
  (if (and (eolp) (not (bolp)))      (delete-indentation t)
    (kill-line))
  (indent-for-tab-command))
(global-set-key (kbd "C-k") 'kill-and-join-forward)

;; ibuffer config
(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-auto-mode 1)
                    (ibuffer-switch-to-saved-filter-groups "Default")))
  :config
  (setq-default ibuffer-saved-filter-groups
              '(("Default"
                 ("Dired" (mode . dired-mode))
		 ("System" (name . "\*.*\*"))))))

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

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t)
  ;; the nil enables the theme immediatly
  ;;(load-theme 'doom-outrun-electric nil)
  )

(use-package all-the-icons)

(use-package doom-modeline
  :after all-the-icons
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t))

(use-package linum-relative
  :custom
  (linum-relative-backend 'display-line-numbers-mode)
  :config
  (linum-relative-global-mode))

;; Editing
(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global ";;" "\C-e;"))

(use-package company
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
  (eglot-events-buffer-size 0)
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  ;(add-to-list 'eglot-server-programs '(web-mode . ("javascript-typescript-stdio")))
  )

;;(use-package web-mode
;;  :custom
;;  (web-mode-content-types-alist '(("jsx" . "\\.jsx?\\'")
;;				  ("jsx" . "\\.html\\'")))
;;  :config
;;  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
;;  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

(use-package flyspell-correct-ivy
  :diminish
  :after ivy
  :config
  (bind-key (kbd "C-o s") 'flyspell-correct-wrapper flyspell-mode-map))

(use-package multiple-cursors
  :bind
  ("C->"     . mc/mark-next-like-this)
  ("C-<"     . mc/mark-previous-like-this)
  ("C-c m a" . mc/vertical-align-with-space))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Search / Movement / Quality of life


(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package swiper
  :after ivy
  :bind
  ("C-s" . swiper))

(use-package counsel
  :after (ivy swiper))

(use-package avy
  :init (unbind-key (kbd "C-z"))
  :bind
  (("C-z" . avy-goto-char)
   ("C-c a c l" . avy-copy-line)
   ("C-c a c r" . avy-copy-region)
   ("C-c a m l" . avy-move-line)
   ("C-c a m r" . avy-move-region)
   ("C-c a k l" . avy-kill-line)
   ("C-c a k r" . avy-kill-region)))

;; Machine generated, don't touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" default))
 '(package-selected-packages
   '(linum-relative avy counsel swiper expand-region multiple-cursors flyspell-correct-ivy yasnippet company key-chord doom-modeline all-the-icons doom-themes base16-theme which-key diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
