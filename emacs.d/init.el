;;; package --- Summary
;;; Commentary:
;;; Begin initialization
;;; Turn off mouse interface early in startup to avoid momentary display
;;; -*- lexical-binding: t -*-
;;; Code:

;; 에러시 디버그모드
;; (setq debug-on-error t)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq user-full-name "Sungho Kim(shiren)"
      user-mail-address "shirenbeat@gmail.com")

(setq ad-redefinition-action 'accept) ;; 함수 redefine으로 인한 경고 생략

(set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(setq echo-keystrokes 0.001) ;; 키입력시 에코창에 표시되는 딜레이 타임, 거이 없게 설정

(setq tab-width 2)

;; 이맥스르 투명하게 하려면 숫자 조절
(set-frame-parameter nil 'alpha 1.0)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(set-variable 'cursor-type 'bar)

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)

;;; Paste setup
(defun copy-from-osx ()
  "Copy from osx."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(unless window-system
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;;; Scroll setup
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-conservatively 200) ;; 스크롤 도중에 센터로 커서 이동하지 않도록
(setq scroll-margin 3) ;; 스크롤시 남기는 여백

;; 백업들 끄기
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; No popup frame(새버퍼열때 현재 프레임에서 열기)
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

;; 소리 끄고 비쥬얼벨로
(setq visible-bell t)

;; 풀스크린키 변경
(define-key global-map (kbd "C-M-f") 'toggle-frame-fullscreen)

;; 저장키 변경
(define-key global-map (kbd "M-s") 'save-buffer)

;; split smart!
(defun shiren/split-smart ()
  (if (< (window-pixel-width) (window-pixel-height))
      (with-selected-window (selected-window)
        (split-window-vertically))
    (with-selected-window (selected-window)
      (split-window-horizontally))))

(defcustom split-window-preferred-function 'shiren/split-smart
  "Split smart."
  :type 'function
  :version "25.1"
  :group 'windows)

(when (and window-system (eq system-type 'darwin))
  ;; (set-face-attribute 'default nil :family "Source Code Pro" :height 140 :weight 'normal)
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 140 :weight 'normal)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  (setq-default line-spacing 0))



;;(add-to-list 'face-font-rescale-alist `("NanumGothicCoding" . 1.2307692307692308))
;;(add-to-list 'face-font-rescale-alist `("Apple SD Gothic Neo" . 1.5))

;;; default modes
;; unset some default keybinding for my custom key bindings
(define-key global-map (kbd "C-j") nil)

;; dired
(put 'dired-find-alternate-file 'disabled nil)

;; hippie-expand
(global-set-key "\M-n" 'hippie-expand)

;; delete highlighted region before yank
(delete-selection-mode 1)

;; 탭인덴트 생략
(setq-default indent-tabs-mode nil)

(add-hook 'focus-out-hook 'garbage-collect)

;;; Set up package
(require 'package)

(setq package-archives '(("gnu"           . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (add-to-list 'load-path "~/dotfiles/my_emacs_packages")
;; (require 'upbo)

;; (use-package upbo
;;   ;;:ensure t
;;   :load-path "~/masterpiece/upbo")

;; (upbo-define-test
;;  :path "~/masterpiece/tui.chart/"
;;  :browsers "ChromeHeadless"
;;  :conf-file "~/masterpiece/tui.chart/karma.conf.js")

(define-key prog-mode-map [(return)] 'newline-and-indent)

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package diminish
  :ensure t)

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :delight '(:eval "")
  :init
  (setq whitespace-cleanup-mode-only-if-initially-clean nil)
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'lsp-mode-hook 'whitespace-cleanup-mode))
;;(add-hook 'org-mode-hook 'whitespace-cleanup-mode))

;;;; Emacs extend
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 2)
  (setq which-key-max-description-length 40)
  (setq which-key-max-display-columns nil)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 20)
                          (bookmarks . 10)
                          (projects . 10))))

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-function)
  ("C-h F" . helpful-command)
  ("C-h v" . helpful-variable))

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; ;;;; Themes
;; (use-package zenburn-theme
;;   :disabled
;;   :ensure t
;;   :init
;;   (load-theme 'zenburn t))

;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t)
;;   :config
;;   (setq spacemacs-theme-org-agenda-height nil)
;;   (setq spacemacs-theme-org-height nil))

;; (use-package spaceline-config
;;   :ensure spaceline
;;   :init
;;   (setq powerline-default-separator 'arrow-fade)
;;   :config
;;   (spaceline-emacs-theme)
;;   (spaceline-toggle-buffer-id-on)
;;   (spaceline-toggle-input-method-on)
;;   (spaceline-toggle-buffer-modified-on)
;;   (spaceline-toggle-buffer-encoding-on)
;;   (spaceline-toggle-buffer-encoding-abbrev-off)
;;   (spaceline-toggle-process-on)
;;   (spaceline-toggle-projectile-root-on)
;;   (spaceline-toggle-version-control-on)
;;   (spaceline-toggle-flycheck-error-on)
;;   (spaceline-toggle-flycheck-info-on)
;;   (spaceline-toggle-flycheck-warning-on)
;;   (spaceline-toggle-battery-on)
;;   (spaceline-toggle-major-mode-off)
;;   (spaceline-toggle-minor-modes-on)
;;   (spaceline-toggle-line-column-on)
;;   (spaceline-toggle-org-clock-on)
;;   (spaceline-toggle-window-number-on)
;;   (spaceline-info-mode))

(use-package material-theme
  :ensure t
  :init
  :config
  (load-theme 'material t))

;; ;;;; Highlighting
(use-package paren
  :disabled
  :init
  (show-paren-mode 1)
  (setq show-paren-delay 0))

(use-package hl-line
  :disabled
  :init
  (global-hl-line-mode +1))

(use-package highlight-thing
  :disabled
  :ensure t
  :diminish highlight-thing-mode
  :init
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-limit-to-defun t)
  (add-hook 'prog-mode-hook 'highlight-thing-mode))

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :disabled
  :init
  :config
  (add-hook 'vue-html-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'vue-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1))

;; ;;;; Window
(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-j <SPC>"))
  (eyebrowse-mode t)
  :bind
  (:map eyebrowse-mode-map
        ("C-j ;" . eyebrowse-last-window-config)
        ("C-j 0" . eyebrowse-close-window-config)
        ("C-j 1" . eyebrowse-switch-to-window-config-1)
        ("C-j 2" . eyebrowse-switch-to-window-config-2)
        ("C-j 3" . eyebrowse-switch-to-window-config-3)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5))
  :bind ("C-x o" . ace-window))

(use-package writeroom-mode
  :diminish writeroom-mode
  :ensure t
  :init
  :config)

(use-package zoom
  :ensure t
  :init
  :config)

;; windmove
(windmove-default-keybindings)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;;; swiper and ivy
(use-package swiper
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  ;; number of result lines to display
  (setq ivy-height 12)
  ;; does not count candidates
  (setq ivy-count-format "")
  (setq ivy-switch-buffer-faces-alist
        '((emacs-lisp-mode . outline-1)
          (dired-mode . outline-2)
          (js2-mode . outline-4)
          (clojure-mode . outline-5)
          (org-mode . outline-3)))
  :bind
  (("M-x". counsel-M-x)
   ("C-x C-f". counsel-find-file)
   ("C-c r". counsel-recentf)
   ("C-c g". counsel-projectile-rg)
   ("C-c e". ivy-switch-buffer)
   ("C-c 4 e". ivy-switch-buffer-other-window)
   ("C-c o". counsel-imenu)
   ("C-c y" . counsel-yank-pop)
   ("C-x r l" . counsel-bookmark)
   ("C-j i". swiper)
   ("C-j o". swiper-all)
   :map ivy-mode-map
   ("S-SPC" . toggle-input-method)
   :map ivy-minibuffer-map
   ("C-j" . ivy-alt-done)))

;; Avy
(use-package avy
  :ensure t
  :bind
  ("C-j j". avy-goto-word-1)
  ("C-j C-j". avy-goto-word-1)
  ("C-j k". avy-goto-char-2)
  ("C-j g". avy-goto-line)
  ("C-j C-g". avy-goto-line))

;;;; Move&History
(use-package git-timemachine
  :ensure t
  :bind
  ("C-j t" . git-timemachine-toggle))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :bind
  ("C-z" . undo)
  ("C-S-z" . undo-tree-redo))

(use-package goto-last-change
  :ensure t
  :bind
  ("C-j l" . goto-last-change))

(use-package dumb-jump
  :ensure t
  :ensure-system-package rg
  :bind (("C-j n" . dumb-jump-go-other-window)
         ("C-j m" . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg))

;; ;;;; Editing
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/dotfiles/yaSnippets")
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

(use-package iedit
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  ("C-c C-v" . er/expand-region)
  ("C-c v" . er/expand-region))

(defun toggle-evilmode ()
  (interactive)
  (if (bound-and-true-p evil-local-mode)
      (progn
                                        ; go emacs
        (evil-local-mode (or -1 1))
        (undo-tree-mode (or -1 1))
        (evil-escape-mode -1)
        (set-variable 'cursor-type 'bar))

    (progn
                                        ; go evil
      (evil-local-mode (or 1 1))
      (evil-escape-mode t)
      (set-variable 'cursor-type 'box))))

(use-package evil
  :disabled
  :ensure t
  :bind
  ("C-z" . toggle-evilmode)
  :init
  (setq evil-toggle-key "C-`"))

(use-package evil-escape
  :disabled
  :ensure t
  :init
  (add-hook 'evil-mode-hook #'evil-escape-mode)
  :config
  (setq-default evil-escape-delay 0.2))

(use-package paredit
  :ensure t
  :config
  (define-key paredit-mode-map (kbd "C-j") nil)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  :bind
  (:map paredit-mode-map
        ("C-M-f" . nil)))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             ;;lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package multiple-cursors
  :ensure t)

;; ;; File & Buffer
(use-package recentf
  :init
  (setq recentf-max-saved-items 300
        recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                          "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                          ".gz" "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:"))
  (recentf-mode t))

(use-package ibuffer
  :ensure t
  :init
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (autoload 'ibuffer "ibuffer" "List buffers." t))

(use-package delight
  :ensure t)

(use-package projectile
  :ensure t
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :init
  (projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  ;;; 아무데서나 프로젝타일을 사용하게하려면 주석해제
  ;; (setq projectile-require-project-root nil)
  (setq projectile-indexing-method 'alien)
  ;;(setq projectile-indexing-method 'native)
  (setq projectile-globally-ignored-directories
        (append '(".DS_Store" ".git" ".svn" "out" "repl" "target" "dist" "lib" "node_modules" "libs" "deploy")
                projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-file-suffixes
        (append '(".#*" ".DS_Store" "*.tar.gz" "*.tgz" "*.zip" "*.png" "*.jpg" "*.gif")
                projectile-globally-ignored-file-suffixes))
  (setq grep-find-ignored-directories (append '("dist" "deploy" "node_modules") grep-find-ignored-directories))
  :bind
  ;; 오타방지용 바인드들
  ("C-c p f" . projectile-find-file)
  ("C-c p 4 f" . projectile-find-file-other-window)
  ("C-c p b" . projectile-switch-to-buffer)
  ("C-c p 4 b" . projectile-switch-to-buffer-other-window)
  ("C-c p D" . projectile-dired)
  ("C-c p d" . projectile-find-dir)
  ("C-c p j" . projectile-find-tag)
  ("C-c p r" . projectile-replace)
  ("C-c p o" . projectile-multi-occur)
  ("C-c p s s" . counsel-projectile-ag)
  ("C-c C-g" . counsel-projectile-rg)
  ("C-c p I" . projectile-ibuffer)
  ("C-c p p" . projectile-switch-project))

(use-package ibuffer-projectile
  :ensure t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))

(use-package wgrep
  :ensure t)

;;; Coding
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 2)

  (defvar company-mode/enable-yas t)
  "Enable yasnippet for all backends."

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)

  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled new-line idle-change idle-buffer-switch))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint typescript-tslint)))
  (setq flycheck-checkers '(javascript-eslint
                            tsx-tide
                            typescript-tide
                            emacs-lisp
                            emacs-lisp-checkdoc
                            css-csslint
                            markdown-mdl
                            sass
                            go-build
                            go-fmt
                            go-golint
                            package-lint
                            rust
                            sh-zsh))
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-indication-mode 'left-fringe)
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (setq-default flycheck-emacs-lisp-load-path load-path)
  (add-hook 'js2-init-hook
            '(lambda ()
               (setq next-error-function 'flycheck-next-error))))

(use-package flycheck-package
  :ensure t
  :init
  :config
  (flycheck-package-setup))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun shiren/use-eslint-from-node-modules ()
  "Use eslint from node modules."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))
    eslint))

(add-hook 'flycheck-mode-hook #'shiren/use-eslint-from-node-modules)

(use-package lsp-mode
  ;; :ensure-system-package
  ;; (typescript-language-server . "npm i -g typescript-language-server")
  ;; (vue-language-server . "npm install vue-language-server -g")
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'vue-mode-hook #'lsp)
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'cc-mode-hook #'lsp)
  ;; (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'swift-mode-hook #'lsp)
  :config
  (setq lsp-auto-guess-root nil)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq ls-ui-imenu-enable t)
  (setq lsp-auto-configure t)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-signature-render-all nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-symbol-highlighting nil)

  (setq lsp-javascript-implicit-project-config-experimental-decorators t))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil) ;; lsp로 커서 속도가 너무 느릴경우 nil
  (setq lsp-ui-sideline-show-hover nil) ;;  어노잉한 hover 정보 제거
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-live-reporting nil))

(use-package lsp-ivy
  :ensure t
  :init
  :config)

(use-package company-lsp
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'company-mode)
  (setq company-lsp-cache-candidates t)
  (push 'company-lsp company-backends))

;; ;;;; Emacs-lisp
(use-package suggest
  :ensure t)

;; ;;;; Web
(use-package web-mode
  :ensure t
  :init
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2))

  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

;;;; javascript
(setq js-indent-level 2)

(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook
            '(lambda ()
               (js2-imenu-extras-mode)))
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  (define-key js2-mode-map (kbd "C-c C-j") nil)
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (setq js2-include-node-externs t)
  (setq js2-pretty-multiline-declarations nil)
  (add-hook 'js2-mode-hook (lambda ()
                             ;;(add-hook 'after-save-hook 'eslint-fix nil t)
                             (setq tab-width 2)
                             (setq-default js2-basic-offset 2)
                             (setq js-switch-indent-offset 2)
                             (electric-indent-mode -1)
                             (js2-imenu-extras-mode)))
  (setq-default js2-basic-offset 2
                js1-bounce-indent-p nil)
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil))

(use-package js-doc
  :ensure t
  :bind
  (:map js2-mode-map
        ("\C-cd" . js-doc-insert-function-doc)
        ("@" . js-doc-insert-tag))
  :config
  (setq js-doc-mail-address "shirenbeat@gmail.com"
        js-doc-author (format "Sungho Kim <%s>" js-doc-mail-address)
        js-doc-url "shiren.github.io"
        js-doc-license "MIT"))

(use-package vue-mode
  :ensure t
  :init
  :config
  (setq vue-html-extra-indent 2)
  (setq css-indent-offset 2)
  (setq lsp-ui-flycheck-enable nil)
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (add-hook 'mmm-mode-hook
            (lambda ()
              (setq mmm-submode-decoration-level 2)
              (set-face-background 'mmm-default-submode-face nil))))

(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
  :config
  ;; (add-hook 'rjsx-mode-hook
  ;;           ((lambda ()
  ;;               (flycheck-add-next-checker 'lsp 'javascript-eslint))))
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil))

(use-package prettier-js
  :ensure t
  :init
  :config
  (setq prettier-js-args '(
                           "--singleQuote" "true"
                           "--printWidth" "100"
                           "--tabWidth" "2"
                           "--useTabs" "false"
                           "--semi" "true"
                           "--quoteProps" "as-needed"
                           "--jsxSingleQuote" "false"
                           "--trailingComma" "es5"
                           "--arrowParens" "always"
                           "--endOfLine" "lf"
                           "--bracketSpacing" "true"
                           "--jsxBracketSameLine" "false"
                           "--requirePragma" "false"
                           "--insertPragma" "false"
                           "--proseWrap" "preserve"
                           "--vueIndentScriptAndStyle" "false"))

  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'vue-mode-hook 'prettier-js-mode)
  (add-hook 'vue-html-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

(use-package json-mode
  :ensure t)

;; ;;; typescript
(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (define-key typescript-mode-map [(return)] 'newline-and-indent)
  (setq typescript-indent-level 2))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;;(flycheck-add-next-checker 'javascript-eslint 'typescript-tide)
  (flycheck-mode +1)
  (flycheck-select-checker `typescript-tide)
  (setq flycheck-check-syntax-automatically '(save mode-enabled new-line idle-change idle-buffer-switch))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(defun setup-tide-tsx-mode ()
  (interactive)
  (tide-setup)
  (flycheck-add-mode 'tsx-tide 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'tsx-tide)
  (flycheck-mode +1)
  ;; (flycheck-select-checker `tsx-tide)
  (setq flycheck-check-syntax-automatically '(save mode-enabled new-line idle-change idle-buffer-switch))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))


(use-package tide
  :ensure t
  :init
  (setq typescript-indent-level 2)
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions nil :placeOpenBraceOnNewLineForFunctions nil :insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets nil :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis nil :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces nil :insertSpaceBeforeFunctionParenthesis nil))
  ;;(add-hook 'before-save-hook 'tide-format-before-save) ;; 포맷은 프리티어에게 맡긴다.
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  ;;(add-hook 'vue-mode-hook #'setup-tide-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-tsx-mode))))


  :config
  (define-key tide-mode-map [(return)] 'newline-and-indent)
  (flycheck-add-mode 'typescript-tide 'typescript-mode)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))


;;; Clojure setup
(use-package cider
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

(use-package clojure-mode
  :ensure t)

;;; Haskell
(use-package haskell-mode
  :ensure t
  :init
  :config)

(use-package lsp-haskell
  :ensure t
  :init
  :config)



;;; C# and Unity
(use-package csharp-mode
  :disabled
  :ensure t
  :init
  (add-hook 'csharp-mode-hook #'company-mode))

;;; C++
(use-package ccls
  :ensure t
  :init
  :config
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))


;; ;;; Swift
(use-package swift-mode
  :ensure t
  :config
  (setq swift-mode:basic-offset 2))

;; Sourcekittendaemon이 설치 되어 있어야함
;; https://github.com/terhechte/SourceKittenDaemon
(use-package company-sourcekit
  :disabled
  :ensure t
  :init
  (add-to-list 'company-backends 'company-sourcekit))

(use-package flycheck-swift
  :ensure t
  :config
  (flycheck-swift-setup))

(use-package flycheck-swiftlint
  :ensure t
  :ensure-system-package
  ((swiftLint . "brew install swiftlint"))
  :config
  (with-eval-after-load 'flycheck
    (flycheck-swiftlint-setup)))

(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "/usr/local/bin/sourcekit-lsp")))

;;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rust-mode
  :ensure t
  ;; :ensure-system-package
  ;; ((rustfmt . "cargo install rustfmt")
  ;;  (cargo-script . "cargo install cargo-script"))
  :init
  :config
  (setq rust-indent-offset 2)
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'rust-format-buffer))))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (cargo-minor-mode)
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :ensure t
  ;; :ensure-system-package
  ;; (racer . "cargo install racer")
  :init
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  ;;  (setq racer-rust-src-path "/Users/julien/Code/rust/src") ;; Rust source code PATH
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rust-playground
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; go
(add-to-list 'exec-path "~/masterpeice/go/bin")
(add-hook 'before-save-hook 'gofmt-before-save)

(defun my-go-mode-hook ()
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet && ./go"))
                                        ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-c") 'compile)
  (set (make-local-variable 'company-backends) '(company-go)))

(use-package go-mode
  :ensure t
  ;; :ensure-system-package
  ;; ((godef . "go get github.com/rogpeppe/godef")
  ;;  (gstool . "go get golang.org/x/tools/cmd/..."))
  :config
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package company-go
  :ensure t)
;; :ensure-system-package (gscode . "go get -u github.com/nsf/gocode")


;;; Utilities
(use-package google-translate
  :ensure t
  :init
  (require 'google-translate)
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist
        '(("en" . "ko") ("ko" . "en")))
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-output-destination 'echo-area)
  (setq max-mini-window-height 0.5)
  :bind
  ("C-c n" . google-translate-smooth-translate))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

;;; Tools
(use-package treemacs
  :ensure t
  :init
  :config
  :bind
  ("M-0" . treemacs-select-window))

;;; org
(use-package ob-swift
  :ensure t)

(use-package ob-typescript
  :ensure t)

(use-package ob-go
  :ensure t)

(use-package ob-rust
  :ensure t)

(use-package ox-gfm
  :ensure t)

(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.3.0/"))

(use-package org-tree-slide
  :ensure t)

(use-package ob-restclient
  :ensure t)

(use-package hyperbole
  :disabled
  :ensure t
  :init
  :config)

(use-package org
  :ensure t
  :bind
  (("\C-cl" . org-store-link)
   ("\C-ca" . org-agenda)
   ("\C-cc" . org-capture)
   ("\C-cb" . org-iswitchb))

  :init
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-agenda-files (file-expand-wildcards "~/org/agenda/*.org"))
  (setq org-default-notes-file "~/org/agenda/index.org")
  (setq org-capture-templates '(("t" "Task" entry
                                 (file+headline "~/org/agenda/index.org" "Task")
                                 "* TODO %?")
                                ("o" "Task @office" entry
                                 (file+headline "~/org/agenda/nhn.org" "Task")
                                 "* TODO %?\nSCHEDULED: %t")
                                ("e" "English" item
                                 (file+headline "~/org/agenda/english.org" "Inbox")
                                 "%i%?")
                                ("l" "LogTime" entry
                                 (file+datetree "~/org/agenda/timelogs.org")
                                 "** %U - %^{Activity|Coding|Work|Study|Rest|Meeting|Talk|Workout|Productivity|Commute|etc} %?")
                                ("d" "dev note" entry
                                 (file+datetree "~/org/note/devnote.org")
                                 "* %? %^g")))

  (setq org-refile-targets '((org-agenda-files :level . 1) (("~/org/note/devnote.org") :level . 1)))
  (setq org-todo-keywords '((sequence "MONTH(2)" "WEEK(2)" "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "HOLD(h@/!)" "DONE(d)" "CANCELLED(c@/!)")))
  (setq org-tag-alist '((:startgroup . nil)
                        ("@coding" . ?c) ("@writing" . ?w) ("@music" .  ?m) ("@work" . ?w)
                        (:startgroup . nil)
                        ("IDEA" . ?I) ("PIN". ?P)))
  (setq org-agenda-custom-commands
        '(("o" "Custom View"
           ((agenda "")
            (tags "PIN"
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE") 'notscheduled))))
            (todo "TODO"
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp 'regexp "desparche" 'todo '("SOMEDAY")))
                   (org-agenda-overriding-header "Unscheduled tasks")))
            (todo "SOMEDAY")))
          ("ja" "Agenda search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/agenda/*.org"))))
          ("jd" "Document search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/note/*.org"))))))

  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-delay-if-deadline t)
  (setq org-babel-clojure-backend 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (emacs-lisp . t)
     (clojure . t)
     (typescript . t)
     (haskell . t)
     (plantuml . t)
     (swift . t)
     (rust . t)
     (restclient . t)))

  (setq org-agenda-restore-windows-after-quit t)

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-strip-leading-and-trailing-blank-lines t)
  (setq org-log-done t)
  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-adapt-indentation t)
  (eval-after-load "org"
    '(require 'ox-gfm nil t))
  (setq org-plantuml-jar-path
        (expand-file-name "~/plantuml/plantuml.jar"))

  ;;yasnippet 하고 tab 충돌 해결
  (defun yas/org-very-safe-expand ()
    (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas-expand-from-trigger-key)
              (setq yas-expand-from-trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas-next-field)))

  ;; org에서 linewrap 되게
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
  :config
  (global-set-key (kbd "C-C C-x C-x") 'org-clock-in-last)
  (global-set-key (kbd "C-C C-x C-o") 'org-clock-out)
  (global-set-key (kbd "C-C C-x C-j") 'org-clock-goto)
  (global-set-key (kbd "C-C C-x C-d") 'org-clock-display)
  (global-set-key (kbd "C-C C-x C-q") 'org-clock-cancel)
  (global-set-key (kbd "C-C C-x C-e") 'org-clock-modify-effort-estimate)
  (global-set-key (kbd "C-C C-x C-z") 'org-resolve-clocks)
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "M-j") 'org-return-indent)
  (define-key org-mode-map (kbd "<return>") 'org-return-indent))

;;(add-hook 'org-clock-out-hook (lambda () (shiren-org-log-time-for-entry))))

;; (defun shiren-org-get-formatted-time-stamp (time)
;;   (let ((fmt "[%Y-%m-%d %a %H:%M]"))
;;     (format-time-string fmt time)))

;; (defun shiren-org-log-time-for-entry ()
;;   (let ((start-ts (shiren-org-get-feormatted-time-stamp org-clock-start-time))
;;         (end-ts (shiren-org-get-formatted-time-stamp (float-time)))
;;         (today-datetree (format-time-string "%Y-%m-%d %A" (float-time))))
;;     (with-current-buffer (find-file-noselect "~/org/agenda/timelogs.org")
;;       (org-element-map (org-element-parse-buffer) 'headline
;;         (lambda (h)
;;           (when (string= (org-element-property :raw-value h) today-datetree)
;;             (goto-char (org-element-property :contents-end h))
;;             (insert (concat "**** " start-ts "-" end-ts " - - " org-clock-current-task "\n"))))))))

(use-package org-bullets
  :after org
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/bin/zsh")
  :bind
  ;;("C-c i" . multi-term))
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-raw-map (kbd "C-j")
                (lookup-key (current-global-map) (kbd "C-j"))))))

;; terminal(멀티텀포함)에서 C-j를 글로벌 맵이용하도록

(use-package vterm
  :ensure t
  :bind
  ("C-c i" . vterm))

;; (defun auto-commit-files (list)
;;   (interactive
;;    (list (list (buffer-file-name (current-buffer)))))
;;   "LIST to be auto commit"
;;   (while list
;;     (let* ((file (car list))
;;            (file-buffer (get-file-buffer file)))
;;       (when file-buffer
;;         (set-buffer file-buffer)
;;         (when (magit-anything-modified-p nil file)
;;           (magit-call-git "add" file)
;;           (magit-call-git "commit" "-m" (concat file " update"))
;;           (magit-call-git "push" "origin")
;;           (magit-refresh)
;;           (print (concat file " is pushed!!!")))))
;;     (setq list (cdr list))))

(use-package magit
  :commands magit-get-top-dir
  :diminish auto-revert-mode
  :ensure t
  :init
  ;; magit 오토 리버트시 버퍼의 브랜치명까지 갱신하도록
  (setq auto-revert-check-vc-info t)
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/site-lisp/magit/Documentation/"))
  ;;; 이맥스가 기본적으로 제공하는 Git 백엔드를 켜두면 매우 느려진다. magit만 쓴다.
  (setq vc-handled-backends nil)
  :config
  (setq vc-follow-symlinks t)
  (setq find-file-visit-truename t)
  (setq magit-refresh-status-buffer 'switch-to-buffer)
  (setq magit-rewrite-inclusive 'ask)
  (setq magit-save-some-buffers t)
  (setq magit-set-upstream-on-push 'askifnotset)
  :bind
  ("C-c m" . magit-status))

(use-package forge
  :ensure t)

(use-package prodigy
  :ensure t
  :bind
  ("C-c f" . prodigy)
  :init
  (prodigy-define-service
    :name "RDF: server"
    :command "npm"
    :cwd "~/masterpiece/rdf"
    :args '("run" "serve")
    :port 8080
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    :tags '(webpack-server))

  (prodigy-define-service
    :name "RDF: server api mock"
    :command "npm"
    :cwd "~/masterpiece/rdf"
    :args '("run" "serve:apimock")
    :port 8080
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    :tags '(webpack-server))

  (prodigy-define-service
    :name "RDF: storybook"
    :command "npm"
    :cwd "~/masterpiece/rdf"
    :args '("run" "storybook")
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    :tags '(storybook))

  (prodigy-define-service
    :name "RDF: test"
    :command "npm"
    :cwd "~/masterpiece/rdf"
    :args '("test")
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    :tags '(jest))

  (prodigy-define-tag
    :name 'webpack-server
    :ready-message "Compiled successfully")

  (prodigy-define-tag
    :name 'storybook
    :ready-message "Storybook [0-9.]+ started")

  (prodigy-define-tag
    :name 'karma
    :ready-message " Executed [0-9]+ of [0-9]+ .+")

  (prodigy-define-tag
    :name 'tomcat
    :ready-message "Running war on http://localhost:[0-9]+/"))

(use-package restclient
  :ensure t)

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-yellow ((t (:foreground nil :background nil :underline t)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "ef280e6d5105f7d3906ae43a40aff5490970337796cd5f8a53207568b7e784d0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (c++-mode c++ ccls lsp-haskell haskell-mode haskell mu4e json-mode lsp-treemacs treemacs lsp-ivy multi-libvterm zoom yasnippet-snippets wttrin writeroom-mode whitespace-cleanup-mode which-key wgrep web-mode vue-mode use-package-ensure-system-package use-package-chords tide swift-mode suggest spacemacs-theme spaceline shut-up rust-playground rjsx-mode rainbow-mode rainbow-delimiters racer prodigy prettier-js pocket-reader parinfer paredit ox-reveal ox-gfm org-tree-slide org-bullets ob-typescript ob-swift ob-rust ob-restclient ob-go nov multiple-cursors multi-term material-theme lsp-ui lsp-sourcekit lsp-javascript-typescript js-doc indent-guide iedit ibuffer-projectile hyperbole highlight-thing highlight-indent-guides helpful graphql goto-last-change google-translate git-timemachine git-gutter forge flycheck-swiftlint flycheck-swift flycheck-rust flycheck-package eyebrowse expand-region exec-path-from-shell evil-escape evil dumb-jump diminish delight dashboard counsel-projectile company-sourcekit company-lsp company-go cider cargo beacon ace-window))))
(put 'set-goal-column 'disabled nil)
