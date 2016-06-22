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

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source code pro")
  (set-face-attribute 'default nil :height 140)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;;; GUI모드에서는 nvm이 제대로 안되서 node경로를 지정해줘야
(when window-system
  (setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.nvm/versions/node/v6.0.0/bin")))

;;; 라인넘버 보이도록
;;; (global-linum-mode t)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(unless window-system
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;;; mouse setup
(require 'mouse)
(xterm-mouse-mode t)
;(defun track-mouse (e))

;;; Set up package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;(package-refresh-contents)
(package-initialize)

;;; highlight parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;;; expand region
(package-install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-c C-v") 'er/expand-region)

;;; Web mode
(package-install 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;;; js2-mode
(package-install 'js2-mode)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-include-node-externs t)
(setq-default js2-basic-offset 4
              js1-bounce-indent-p nil)
(setq-default js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil)

;;; flyCheck
(package-install 'flycheck)
(global-flycheck-mode)

;;; autocomplete
(package-install 'auto-complete)
(ac-config-default)

;;; tern
(package-install 'tern)
(package-install 'tern-auto-complete)
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
;(load-theme 'base16-default-dark t)

(package-install 'cyberpunk-theme)
(load-theme 'cyberpunk t)

;;; helm
(package-install 'helm)
(require 'helm)
(helm-mode 1)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;;; projectile
(package-install 'projectile)
(package-install 'helm-projectile)
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)

;;; org
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-agenda-files (list "~/org"))
(setq org-default-notes-file (concat org-directory "~/org/notes.org"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (emacs-lisp . nil)
   (plantuml . t)
   ))
(setq org-confirm-babel-evaluate nil)
(eval-after-load "org"
  '(require 'ox-md nil t))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml/plantuml.jar"))

;; org에서 linewrap 되게
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;;; Magit
(package-install 'magit)
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/magit/Documentation/"))
(global-set-key (kbd "C-c g") 'magit-status)

;; VIM-emulation
;;; evil
;(package-install 'evil)
;(setq evil-want-C-i-jump nil)
;(setq evil-want-C-u-scroll t)
;(require 'evil)
;(evil-mode 1)
;(define-key evil-normal-state-map "\C-y" 'yank)
;(define-key evil-insert-state-map "\C-y" 'yank)
;(define-key evil-visual-state-map "\C-y" 'yank)
;(define-key evil-insert-state-map "\C-e" 'end-of-line)
;(define-key evil-insert-state-map "\C-r" 'search-backward)
;(define-key evil-insert-state-map "\C-[" 'evil-normal-state)

;(evil-set-initial-state 'dired-mode 'emacs)
;(evil-set-initial-state 'org-mode 'emacs)
;(evil-set-initial-state 'Info-mode 'emacs)
;(evil-set-initial-state 'help-mode 'emacs)
; I need copy words from eshell history
;(evil-set-initial-state 'eshell-mode 'emacs)
;(evil-set-initial-state 'shell-mode 'emacs)
;(evil-set-initial-state 'term-mode 'emacs)
;(evil-set-initial-state 'message-mode 'emacs)

;(defun set-control-w-shortcuts ()
  ;(define-prefix-command 'my-window-map)
  ;(global-set-key (kbd "C-w") 'my-window-map)
  ;(define-key my-window-map (kbd "h") 'windmove-left)
  ;(define-key my-window-map (kbd "j") 'windmove-down)
  ;(define-key my-window-map (kbd "k") 'windmove-up)
  ;(define-key my-window-map (kbd "l") 'windmove-right)
  ;(define-key my-window-map (kbd "v") 'split-window-right)
  ;(define-key my-window-map (kbd "b") 'split-window-below)
  ;(define-key my-window-map (kbd "x") 'delete-window)
  ;(define-key my-window-map (kbd "o") 'delete-other-windows))

;(set-control-w-shortcuts)

;(eval-after-load "evil-maps"
  ;'(progn
    ;(define-key evil-window-map "\C-w" 'nil)
    ;(set-control-w-shortcuts)))

;;; key-chord
;(package-install 'key-chord)
;(require 'key-chord)
;(key-chord-mode 1) ; turn on key-chord-mode
;(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;;; evil-nerd-commenter
;(package-install 'evil-nerd-commenter)

;;; evil-leader
;(package-install 'evil-leader)
;(global-evil-leader-mode)
;(evil-leader/set-leader "<SPC>")
;(evil-leader/set-key
    ;"n" 'dired
    ;"f" 'ace-jump-mode
    ;"gs" 'magit-status
    ;"p" 'projectile-find-file
    ;"e" 'projectile-switch-to-buffer
    ;"ci" 'evilnc-comment-or-uncomment-lines
    ;"cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    ;"ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    ;"cc" 'evilnc-copy-and-comment-lines
    ;"cp" 'evilnc-comment-or-uncomment-paragraphs
    ;"cr" 'comment-or-uncomment-region
    ;"cv" 'evilnc-toggle-invert-comment-line-by-line
    ;"s" 'save-buffer)

;;; evil-magit
;(package-install 'evil-magit)
;(require 'evil-magit)

;(add-hook 'org-mode-hook
    ;(lambda () (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))

(provide 'init)
;;; init.el ends here
