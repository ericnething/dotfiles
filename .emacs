;;; emacs configuration

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"          . "http://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Set up environment variables
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'default-frame-alist '(font . "Source Code Pro 15"))
;; (set-fontset-font t 'han (font-spec :name "Heiti SC"))
(add-to-list 'default-frame-alist '(height . 32))
(add-to-list 'default-frame-alist '(width . 80))

;; Show column numbers
(column-number-mode t)

;; Set default return key behavior
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Convert tabs to spaces
(setq-default indent-tabs-mode nil)

;; set tab width to 4 spaces
(setq tab-width 4)

;; Electric pair
(electric-pair-mode t)

;; Set up `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

;; Theme
(use-package zenburn-theme)

;; Helm
(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-list 150
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files))


;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Projectile
(use-package projectile
  :init (setq projectile-require-project-root nil)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :config
  (helm-projectile-on))

;; All The Icons
(use-package all-the-icons)

;; Doom modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Flycheck
(use-package flycheck
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :init

  ;; Enable flycheck globally
  (global-flycheck-mode)
  
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (haskell-mode-hook    . haskell-mode-map)
                   (js2-mode-hook        . js2-mode-map)
		   (elm-mode-hook        . elm-mode-map)
                   (c-mode-common-hook   . c-mode-base-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point)

  (defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
  This lets us fix any errors as quickly as possible, but in a
  clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)

  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)

  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))

  (defun flycheck-handle-idle-change ()
    "Handle an expired idle time since the last change.
  This is an overwritten version of the original
  flycheck-handle-idle-change, which removes the forced deferred.
  Timers should only trigger inbetween commands in a single
  threaded system and the forced deferred makes errors never show
  up before you execute another command."
    (flycheck-clear-idle-change-timer)
    (flycheck-buffer-automatically 'idle-change)))

(use-package flycheck-haskell
  :commands flycheck-haskell-setup)

(use-package flycheck-elm
  :config (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))


;;;;;;;;;;;;;;;;;;;;
;;; Language modes
;;;;;;;;;;;;;;;;;;;;


;; Haskell mode
(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :config
  (add-to-list 'flycheck-disabled-checkers #'haskell-stack-ghc))

;; Elm mode
(use-package elm-mode
  :mode ("\\.elm\\'")
  :config
  (setq elm-format-on-save t))

;; JavaScript mode
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1))

;; Typescript mode
(use-package typescript-mode
  :mode "\\.ts\\'")

;; JSON mode
(use-package json-mode
  :mode "\\.json\\'")

;; Web mode
(use-package web-mode
  :commands web-mode
  :config
  ;; adjust indents for web-mode to 2 spaces
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil))
  
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;; use web-mode for .jsx files
  (add-to-list 'auto-mode-alist '("\\.(t|j)sx$" . web-mode))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package magit)

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'turn-on-auto-fill))

;; Turn on auto-fill for literate text modes
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'rst-mode-hook 'turn-on-auto-fill)

;; Use Shift + arrow keys to switch windows
(windmove-default-keybindings)

