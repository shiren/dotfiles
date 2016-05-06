;;; package --- Summary
;;; Commentary:
;;; Begin initialization
;;; Turn off mouse interface early in startup to avoid momentary display
;;; Code:
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq ad-redefinition-action 'accept)

(setq-default indent-tabs-mode nil)

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-q") 'toggle-input-method)

(setq x-select-enable-clipboard t)

;;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

;;; 라인넘버 보이도록
;(global-linum-mode t)

;;; mouse setup
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

;;; Set up package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;(package-refresh-contents)
(package-initialize)

;;; flyCheck
(package-install 'flycheck)
(global-flycheck-mode)

;;; autocomplete
(package-install 'auto-complete)
(ac-config-default)

;;; tern
; NVM을 이용하기때문에 환경변수로 tern의 경로를 로드한다
(add-to-list 'load-path (concat (substring (getenv "NVM_BIN") 0 (- (length (getenv "NVM_BIN")) 3)) "lib/node_modules/tern/emacs/"))
(autoload 'tern-mode' "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;;; ace jump
(package-install 'ace-jump-mode)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;; base16
(package-install 'base16-theme)
(load-theme 'base16-default-dark t t)

;;; helm
(package-install 'helm)
(require 'helm)
(helm-mode 1)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(global-set-key (kbd "M-x") 'helm-M-x)

;;; projectile
(package-install 'projectile)
(package-install 'helm-projectile)
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)

;;; org
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (emacs-lisp . nil)
   ))
(setq org-confirm-babel-evaluate nil)

;;; Magit
(package-install 'magit)
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       "~/.emacs.d/site-lisp/magit/Documentation/"))
(global-set-key (kbd "C-c g") 'magit-status)

;;;; VIM-emulation
;;; evil
(package-install 'evil)
(setq evil-want-C-i-jump nil)
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
(define-key evil-insert-state-map "\C-r" 'search-backward)
(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-visual-state-map "\C-w" 'evil-delete)

;;; key-chord
(package-install 'key-chord)
(require 'key-chord)
(key-chord-mode 1) ; turn on key-chord-mode
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;;; evil-leader
(package-install 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "n" 'dired
  "f" 'ace-jump-mode
  "p" 'projectile-find-file
  "s" 'save-buffer)

(provide 'init)
;;; init.el ends here
