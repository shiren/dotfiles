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
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when (and window-system (eq system-type 'darwin))
  (set-face-attribute 'default nil :family "Source code pro")
  (set-face-attribute 'default nil :height 140)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(package-selected-packages
   (quote
    (goto-last-change git-timemachine git-gutter rainbow-delimiters eyebrowse eyebrowse-mode ox-reveal projectile helm exec-path-from-shell web-mode use-package tern-auto-complete ox-gfm multi-term markdown-mode magit js2-mode hydra helm-projectile flycheck expand-region cyberpunk-theme cider base16-theme ace-window ace-jump-mode))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

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
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Setup PATH environment
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; highlight parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; recent file list
(require 'recentf)
(recentf-mode t)

;;; Expand Region
(unless (package-installed-p 'expand-region)
  (package-install 'expand-region))
(require 'expand-region)
(global-set-key (kbd "C-c v") 'er/expand-region)

;;; Web mode
(unless (package-installed-p 'web-mode)
  (package-install 'web-mode))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;;; js2-mode
(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-include-node-externs t)
(setq-default js2-basic-offset 4
              js1-bounce-indent-p nil)
(setq-default js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil)

;;; Clojure setup
;; CIDER
(unless (package-installed-p 'cider)
  (package-install 'cider))
;; clojure-mode
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

;;; flyCheck
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                        '(javascript-jshint)))
(setq flycheck-checkers '(javascript-eslint))

;;; autocomplete
(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(ac-config-default)

;;; tern
(unless (package-installed-p 'tern)
  (package-install 'tern))
(unless (package-installed-p 'tern-auto-complete)
  (package-install 'tern-auto-complete))

(autoload 'tern-mode' "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;;; ace jump
(unless (package-installed-p 'ace-jump-mode)
  (package-install 'ace-jump-mode))
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;; base16
(unless (package-installed-p 'base16-theme)
  (package-install 'base16-theme))
;;(load-theme 'base16-tomorrow-dark t)

(unless (package-installed-p 'cyberpunk-theme)
  (package-install 'cyberpunk-theme))
(load-theme 'cyberpunk t)

;;; Eyebrowse
(unless (package-installed-p 'eyebrowse)
  (package-install 'eyebrowse))
(eyebrowse-mode t)

;;; helm
(unless (package-installed-p 'helm)
  (package-install 'helm))
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
(global-set-key (kbd "C-c e") 'helm-buffers-list)
(global-set-key (kbd "C-c r") 'helm-recentf)

;;; projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(unless (package-installed-p 'helm-projectile)
  (package-install 'helm-projectile))

(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)
;;; 아무데서나 프로젝타일을 사용하게하려면 주석해제
;(setq projectile-require-project-root nil)
(setq projectile-indexing-method 'native)

;;; org
(unless (package-installed-p 'org)
  (package-install 'org))
(unless (package-installed-p 'ox-gfm)
  (package-install 'ox-gfm))
(unless (package-installed-p 'ox-reveal)
  (package-install 'ox-reveal))

(require 'org)
(require 'ox-reveal)
(setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.3.0/")

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-agenda-files (list "~/org"))
(setq org-default-notes-file (concat org-directory "~/org/notes.org"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (emacs-lisp . nil)
   (plantuml . t)
   (clojure . t)
   (sh . t)
   ))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
;(setq org-src-tab-acts-natively t)
(eval-after-load "org"
  '(require 'ox-gfm nil t))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml/plantuml.jar"))

;; org에서 linewrap 되게
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;;; Magit
(unless (package-installed-p 'magit)
  (package-install 'magit))
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/magit/Documentation/"))
(global-set-key (kbd "C-c m") 'magit-status)

;;; markdown mode
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; multi term
(unless (package-installed-p 'multi-term)
  (package-install 'multi-term))
(require 'multi-term)
(setq multi-term-program "/usr/local/bin/zsh")

;;; ace window
(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))
(require 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(defvar aw-dispatch-alist
'((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows))
"List of actions for `aw-dispatch-default'.")

;;; hydra
(unless (package-installed-p 'hydra)
  (package-install 'hydra))
(defhydra hydra (global-map "C-;")
  "leader"
  ("g" git-timemachine-toggle "Git timemachine"))

;;; rainbow-delimiters
(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js2-mode-hook 'rainbow-delimiters-mode)

(unless (package-installed-p 'git-gutter)
  (package-install 'git-gutter))
(require 'git-gutter)
(global-git-gutter-mode +1)

(unless (package-installed-p 'git-timemachine)
  (package-install 'git-timemachine))

(unless (package-installed-p 'goto-last-change)
  (package-install 'goto-last-change))
(global-set-key (kbd "C-c gl") 'goto-last-change)

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
