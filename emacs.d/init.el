;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq ad-redefinition-action 'accept)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

;;; Set up package
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; flyCheck
(package-install 'flycheck)
(global-flycheck-mode)

;;; autocomplete
(package-install 'auto-complete)
(ac-config-default)

;;; js2-mode
(package-install 'auto-complete)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;;; tern
; NVM을 이용하기때문에 환경변수로 tern의 경로를 로드한다
(add-to-list 'load-path (concat (substring (getenv "NVM_BIN") 0 (- (length (getenv "NVM_BIN")) 3)) "lib/node_modules/tern/emacs/"))
(autoload 'tern-mode' "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
:
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;;; ace jump
(package-install 'ace-jump-mode)
(add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;; base16
(package-install 'base16-theme)
(load-theme 'base16-default-dark t)

;;; evil
(package-install 'evil)
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
;(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-r" 'search-backward)
(define-key evil-visual-state-map "\C-w" 'evil-delete)

;;; evil-escape
(package-install 'evil-escape)
(evil-escape-mode)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)

;;; ido
(require 'ido)
(ido-mode t)

;;; projectile
(package-install 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(provide 'init)
;;; init.el ends here
