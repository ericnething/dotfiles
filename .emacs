(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; (setq exec-path (append "/usr/local/bin/" exec-path))
 (setenv "PATH"
     (concat (getenv "PATH")
             ":/usr/local/bin"))

 (setq exec-path (append exec-path
     '("/usr/local/bin")))

;; Enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Custom theme path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Return
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Default tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; C style
(setq c-default-style "k&r" c-basic-offset 8)

;; Font
(set-default-font "Source Code Pro 15")
;;(set-default-font "Fira Mono 16")
;;(set-default-font "Monaco 16")
;;(set-default-font "Menlo 16")
(set-fontset-font t 'han (font-spec :name "Heiti SC"))

;; Autopair
;; (add-to-list 'load-path "~/.emacs.d/settings")
;; (require 'autopair)
;; (autopair-global-mode)
(electric-pair-mode t)

;; Column numbers
(column-number-mode t)

;; Line wrapping for text modes
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'rst-mode-hook 'turn-on-auto-fill)

;; Haskell's shakespeare templates
(add-hook 'hamlet-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-to-list 'auto-mode-alist '("\\.cassius\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius\\'" . js2-mode))

;; Haskell Mode
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

;; Elm
;; (add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1)))
(setq elm-format-on-save t)
;; (setq elm-format-command "/usr/local/bin/elm-format")
;; (setq elm-indent-offset 4)

;; Javascript
(setq js-indent-level 4)
(setq js2-basic-offset 4)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; ASCII Doc
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))

;; Purescript
;;(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
;; (add-hook 'purescript-mode-hook 'turn-on-purescript-unicode-input-method)

;; Pollen (Racket)
;;(require 'pollen-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; recentf
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; toggle window split
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)
