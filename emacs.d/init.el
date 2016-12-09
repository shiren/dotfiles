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

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-conservatively 200) ;; 스크롤 도중에 센터로 커서 이동하지 않도록
(setq scroll-margin 3) ;; 스크롤시 남기는 여백

;;; mouse setup
(require 'mouse)
(xterm-mouse-mode t)
;(defun track-mouse (e))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

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
    (highlight-parentheses omnisharp csharp-mode yasnippet smooth-scroll org-tree-slide counsel projectile hydra prodigy autopair paredit iedit ace-window multi-term markdown-mode magit ox-reveal ox-gfm counsel-projectile swiper eyebrowse zenburn-theme cyberpunk-theme base16-theme tern-auto-complete tern auto-complete flycheck cider js-doc js2-mode web-mode goto-last-change git-timemachine git-gutter rainbow-delimiters expand-region use-package))))

;;; Set up package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 10)
                          (projects . 10))))

;; Setup PATH environment
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;;;; Themes
(use-package cyberpunk-theme
  :ensure t)

(use-package base16-theme
  :ensure t
  :init
  (load-theme 'base16-solarized-dark t))

;;; multi term
(use-package multi-term
  :ensure t
  :init
  (global-set-key (kbd "C-c t") 'multi-term)
  (setq multi-term-program "/usr/local/bin/zsh"))

(use-package paren
  :init
  (show-paren-mode 1)
  (setq show-paren-delay 0))

;; rainbow delimiters랑 같이 쓰면 엄청 정신 없다.
;; (use-package highlight-parentheses
;;   :ensure t
;;   :init
;;   (define-globalized-minor-mode global-highlight-parentheses-mode
;;     highlight-parentheses-mode
;;     (lambda ()
;;       (highlight-parentheses-mode t)))
;;   (global-highlight-parentheses-mode t))

;; hl line
(use-package hl-line
  :init
  (global-hl-line-mode +1))

(use-package highlight-thing
  :ensure t
  :init
  (setq highlight-thing-case-sensitive-p t)
  ;; (setq highlight-thing-what-thing 'word)
  (global-highlight-thing-mode))

;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(unless (package-installed-p 'git-timemachine)
  (package-install 'git-timemachine))

(use-package git-timemachine
  :ensure t)

;;; Eyebrowse
(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t)
  (global-set-key (kbd "C-c C-w C-w") 'eyebrowse-next-window-config))

;;; ace window
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?1 ?2 ?3 ?4 ?5))
  ;(setq aw-dispatch-always t)
  :bind ("C-x o" . ace-window))

;; swiper and ivy
(use-package swiper
  :ensure t
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c r") 'counsel-recentf)
  (global-set-key (kbd "C-c g") 'counsel-ag)
  (global-set-key (kbd "C-c e") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c 4 e") 'ivy-switch-buffer-other-window)
  (global-set-key (kbd "C-c o") 'counsel-imenu))

(use-package git-timemachine
  :ensure t)

(use-package goto-last-change
  :ensure t)

;;; autocomplete
(use-package auto-complete
  :ensure t
  :init
  (require 'auto-complete-config)
  (ac-config-default))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'js2-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/dotfiles/yaSnippets"))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<C-M-return>") 'yas-expand)
  (yas-reload-all))

;;; Iedit
(use-package iedit
  :ensure t)

;;; Auto pair
(use-package autopair
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'autopair-mode))

;;; Expand Region
(use-package expand-region
  :ensure t
  :init
  (global-set-key (kbd "C-c v") 'er/expand-region))

;; recent file list
(use-package recentf
  :init
  (recentf-mode t))

(use-package counsel
  :ensure t)

;;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching nil)
  ;;; 아무데서나 프로젝타일을 사용하게하려면 주석해제
  ;; (setq projectile-require-project-root nil)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-directories
        (append '(".DS_Store" ".git" ".svn" "out" "repl" "target" "dist" "lib" "node_modules" "libs")
                projectile-globally-ignored-directories))

  (setq projectile-globally-ignored-files
        (append '(".DS_Store" "*.tar.gz" "*.tgz" "*.zip" "*.png" "*.jpg" "*.gif")
                projectile-globally-ignored-files)))

;;; countsel-projectile
(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-on))

;;; Web mode
(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

;;; flyCheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (setq flycheck-checkers '(javascript-eslint)))

;;;; javascript
;; js2-mode
(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook
          '(lambda ()
             (js2-imenu-extras-mode)))
  :config
  (setq js2-include-node-externs t)
  (setq-default js2-basic-offset 4
                js1-bounce-indent-p nil)
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil))

;; tern
(use-package tern
  :ensure t
  :init
  (autoload 'tern-mode' "tern.el" nil t)
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package tern-auto-complete
  :ensure t
  :config
  (setq tern-ac-on-dot t)
  (tern-ac-setup))

;; jsdoc
(use-package js-doc
  :ensure t
  :init
  (add-hook 'js2-mode-hook
          '(lambda ()
             (define-key js2-mode-map "\C-cd" 'js-doc-insert-function-doc)
             (define-key js2-mode-map "@" 'js-doc-insert-tag)))
  :config
  (setq js-doc-mail-address "your email address"
      js-doc-author (format "your name <%s>" js-doc-mail-address)
      js-doc-url "url of your website"
      js-doc-license "MIT"))

;;; Clojure setup
;; CIDER
(use-package cider
  :ensure t)

;; clojure-mode
(use-package clojure-mode
  :ensure t)

;;; C# and Unity
(use-package csharp-mode
  :ensure t)

(use-package omnisharp
  :ensure t
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

;;; org
(unless (package-installed-p 'org)
  (package-install 'org))

(use-package org
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-agenda-files (list "~/org"))
  (setq org-default-notes-file (concat org-directory "~/org/notes.org"))
  (setq org-babel-clojure-backend 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (emacs-lisp . t)
     (clojure . t)
     (plantuml . t)
     (sh . t)
     ))
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (eval-after-load "org"
    '(require 'ox-gfm nil t))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)
  (setq org-plantuml-jar-path
        (expand-file-name "~/plantuml/plantuml.jar"))

  ;; org에서 linewrap 되게
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil))))

(use-package ox-gfm
  :ensure t)

(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.3.0/")
  )

(use-package org-tree-slide
  :ensure t)

;; (require 'org)
;; (require 'ox-reveal)
;; (require 'ob-clojure)

;;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; Magit
(use-package magit
  :ensure t
  :init
  (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/site-lisp/magit/Documentation/"))
  (global-set-key (kbd "C-c m") 'magit-status))

;;; prodigy
(use-package prodigy
  :ensure t
  :init
  (define-key global-map (kbd "C-c f") 'prodigy)
  (prodigy-define-service
    :name "Tui Chart server"
    :command "npm"
    :cwd "~/masterpiece/ws_nhn/fedev/tui.chart"
    :args '("run" "dev")
    :port 8080
    :tags '(webpack-server))

  (prodigy-define-service
    :name "Tui Chart test"
    :command "npm"
    :cwd "~/masterpiece/ws_nhn/fedev/tui.chart"
    :args '("run" "test")
    :tags '(karma))

  (prodigy-define-tag
    :name 'webpack-server
    :ready-message "Http://0.0.0.0:[0-9]+/webpack-dev-server/")

  (prodigy-define-tag
    :name 'karma
    :ready-message " Executed [0-9]+ of [0-9]+ SUCCESS"))

;;; hydra
(use-package hydra
  :ensure t
  :init
  ;; (defhydra hydra-jump (:hint nil)
  ;;   "MOVE"
  ;;   ("i" swiper "swiper!")
  ;;   ("j" avy-goto-char "to char")
  ;;   ("k" avy-goto-char-2 "to 2char")
  ;;   ("w" avy-goto-word-1 "to word")
  ;;   ("g" avy-goto-line "to line")
  ;;   ("l" goto-last-change "to last Change")
  ;;   ("t" git-timemachine-toggle "to timemachine"))

  ;; (define-key global-map (kbd "C-j") 'hydra-jump/body)
  )

(define-key global-map (kbd "C-j") nil)
(define-key global-map (kbd "C-j j") 'avy-goto-char-2)
(define-key global-map (kbd "C-j k") 'avy-goto-char)
(define-key global-map (kbd "C-j w") 'avy-goto-word-1)
(define-key global-map (kbd "C-j g") 'avy-goto-line)
(define-key global-map (kbd "C-j l") 'goto-last-change)
(define-key global-map (kbd "C-j t") 'git-timemachine-toggle)
(define-key global-map (kbd "C-j i") 'swiper)
(define-key global-map (kbd "C-j o") 'swiper-all)

(provide 'init)
;;; init.el ends here
(custom-set-faces
 '(show-paren-match ((t (:foreground nil :background "black" :weight ultra-bold))))
 '(iedit-occurrence ((t (:background nil :foreground "DeepPink3"))))
 '(iedit-read-only-occurrence ((t (:background nil :foreground "DeepPink2"))))
 '(hi-yellow ((t (:foreground nil :background nil :underline t))))
 '(ac-completion-face ((t (:background nil :foreground "dim gray" :weight extra-bold)))))
