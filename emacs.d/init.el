;;; Commentary:
;;; Begin initialization
;;; Turn off mouse interface early in startup to avoid momentary display
;;; -*- lexical-binding: t -*-
;;; Code:
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq user-full-name "Sungho Kim(shiren)"
      user-mail-address "shirenbeat@gmail.com")

(set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(set-variable 'cursor-type 'bar)

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)
(setq read-process-output-max (* 1024 1024))


(fset 'yes-or-no-p 'y-or-n-p)

(setq native-comp-async-report-warnings-errors nil)

;; 기본 편집 설정
(setq tab-width 2)

(fset 'yes-or-no-p 'y-or-n-p)
;;;; Paste setup
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

;;;; Scroll setup
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-conservatively 200) ;; 스크롤 도중에 센터로 커서 이동하지 않도록
(setq scroll-margin 3) ;; 스크롤시 남기는 여백

;;;; 백업들 끄기
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;;;; No popup frame(새버퍼열때 현재 프레임에서 열기)
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

;;;; 소리 끄고 비쥬얼벨로
(setq visible-bell t)

;;;; 풀스크린키 변경
(define-key global-map (kbd "C-M-f") 'toggle-frame-fullscreen)

;;;; 저장키 변경
(define-key global-map (kbd "M-s") 'save-buffer)

(when (and window-system (eq system-type 'darwin))
  ;; (set-face-attribute 'default nil :family "Source Code Pro" :height 140 :weight 'normal)
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 140 :weight 'normal)
  (set-fontset-font t 'hangul (font-spec :name "D2Coding"))
  (setq-default line-spacing 0))

;;;; 탭인덴트 생략
(setq-default indent-tabs-mode nil)

;;;; 디폴트 설정 취소
(define-key global-map (kbd "C-j") nil)

;; Set up package
(require 'package)

(setq package-archives '(("gnu"           . "http://elpa.gnu.org/packages/")
;;                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
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

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; 편의 기능 확장
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

;; theme
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package delight
  :ensure t)

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

(use-package rotate
  :ensure t
  :bind ("C-x j" . rotate-window))

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


;; toolset
(use-package consult
  :ensure t
  :bind
  ("C-c r". consult-recent-file)
  ("C-c g". consult-ripgrep)
  ("C-c e". consult-buffer)
  ("C-c 4 e". consult-buffer-other-window)
  ("C-c o". consult-outline)
  ("C-c y" . consult-yank-pop)
  ("C-x r l" . consult-bookmark)
  ("C-j i". consult-line)
  ("C-j I". consult-buffer)
  ("C-j C-i". consult-line)
  ("C-j C-I". consult-buffer))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq completion-styles '(substring basic)))

;; 코드 일반
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")     
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled new-line idle-change idle-buffer-switch))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint typescript-tslint)))
  (setq flycheck-checkers '(javascript-eslint
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
  (setq-default flycheck-emacs-lisp-load-path load-path)
  (add-hook 'js2-init-hook
            (lambda ()
              (setq next-error-function 'flycheck-next-error))))

(use-package flycheck-package
  :ensure t
  :init
  :config
  (flycheck-package-setup))

;; Editing
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/dotfiles/yaSnippets")
  (yas-reload-all))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0)
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
  (define-key company-active-map (kbd "C-p") #'company-select-previous)

  (defun my-tab ()
   (interactive)
   (or (copilot-accept-completion)
       (company-indent-or-complete-common nil)))

  ; modify company-mode behaviors
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends)

    (define-key company-mode-map (kbd "<tab>") 'my-tab)
    (define-key company-mode-map (kbd "TAB") 'my-tab)
    (define-key company-active-map (kbd "<tab>") 'my-tab)
    (define-key company-active-map (kbd "TAB") 'my-tab)))

(use-package iedit
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  ("C-c C-v" . er/expand-region)
  ("C-c v" . er/expand-region))

(use-package multiple-cursors
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'typescript-mode-hook #'smartparens-mode))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'copilot-mode))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; File & Buffer
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
  (autoload 'ibuffer "ibuffer" "List buffers." t)
  :bind
  ("C-x b" . ibuffer)
  ("C-x C-b" . ibuffer))

;;;; Move&History
(use-package git-timemachine
  :ensure t
  :bind
  ("C-j t" . git-timemachine-toggle))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
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
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read)
  (setq dumb-jump-force-searcher 'rg))

;; 프로젝트
(use-package projectile
  :ensure t
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :init
  (projectile-mode)
  :config
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
  ("C-c p f" . consult-projectile-find-file)
  ("C-c p 4 f" . projectile-find-file-other-window)
  ("C-c p b" . consult-projectile-switch-to-buffer)
  ("C-c p 4 b" . projectile-switch-to-buffer-other-window)
  ("C-c p D" . projectile-dired)
  ("C-c p d" . consult-projectile-find-dir)
  ("C-c p j" . projectile-find-tag)
  ("C-c p R" . projectile-replace)
  ("C-c p o" . projectile-multi-occur)
  ("C-c C-g" . projectile-ripgrep)
  ("C-c p I" . projectile-ibuffer)
  ("C-c p p" . consult-projectile-switch-project)
  ("C-c p r" . conslut-projectile-recentf))

(use-package ibuffer-projectile
  :ensure t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

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

;; lsp
(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . lsp-deferred))
  :config
  (setq lsp-auto-guess-root nil)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq ls-ui-imenu-enable t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-signature-render-all nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-prefer-capf t)
  
  (setq lsp-auto-configure t)
  (setq lsp-completion-enable t)
  )

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil) ;; lsp로 커서 속도가 너무 느릴경우 nil
  (setq lsp-ui-sideline-show-hover nil) ;;  어노잉한 hover 정보 제거
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-live-reporting nil))

;; javascript
(setq js-indent-level 2)

(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook (lambda ()
                             ;;(add-hook 'after-save-hook 'eslint-fix nil t)
                             (setq tab-width 2)
                             (setq-default js2-basic-offset 2)
                             (setq js-switch-indent-offset 2)
                             (electric-indent-mode -1)
                             (js2-imenu-extras-mode)))

  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  (define-key js2-mode-map (kbd "C-c C-j") nil)
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (setq js2-include-node-externs t)
  (setq js2-pretty-multiline-declarations nil)
  (setq-default js2-basic-offset 2
                js1-bounce-indent-p nil)
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil))

(use-package lsp-tailwindcss
  :ensure t
  :init
  (setq lsp-tailwindcss-add-on-mode t))


;; typescript
(use-package typescript-mode
  :ensure t
  :config
  ;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  ;; (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  (define-key typescript-mode-map [(return)] 'newline-and-indent)
  (setq typescript-indent-level 2)
  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  ;;(add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
  )

;; org
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

  ;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  ;; (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  ;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

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

 ;; (setq org-startup-folded t)
 ;; (setq org-hide-block-startup t)
 :config
 (let* (
        (base-font-color     (face-foreground 'default nil 'default))
        (headline           `(:inherit default :weight bold :box nil :background nil :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline))))
     `(org-level-7 ((t (,@headline))))
     `(org-level-6 ((t (,@headline))))
     `(org-level-5 ((t (,@headline :foreground "#733B36" :height 1.2))))
     `(org-level-4 ((t (,@headline :foreground "#F2CBBD" :height 1.2))))
     `(org-level-3 ((t (,@headline :foreground "#BF785E" :height 1.2))))
     `(org-level-2 ((t (,@headline :foreground "#A65B4B"  :height 1.2))))
     `(org-level-1 ((t (,@headline :foreground "#6393A6" :height 1.2))))
     `(org-document-title ((t (,@headline  :height 1.5 :underline nil))))))

 (global-set-key (kbd "C-C C-x C-x") 'org-clock-in-last)
 (global-set-key (kbd "C-C C-x C-o") 'org-clock-out)
 (global-set-key (kbd "C-C C-x C-j") 'org-clock-goto)
 (global-set-key (kbd "C-C C-x C-d") 'org-clock-display)
 (global-set-key (kbd "C-C C-x C-q") 'org-clock-cancel)
 (global-set-key (kbd "C-C C-x C-e") 'org-clock-modify-effort-estimate)
 (global-set-key (kbd "C-C C-x C-z") 'org-resolve-clocks)
 (define-key org-mode-map (kbd "C-j") nil)
 (define-key org-mode-map (kbd "M-j") 'org-return)
 (define-key org-mode-map (kbd "<return>") 'org-return))

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
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n l" . org-roam)
         ("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d T" . org-roam-dailies-goto-today)
         ("C-c n d t" . org-roam-dailies-capture-today)
         ("C-c n d y" . org-roam-dailies-capture-yesterday)
         ("C-c n d Y" . org-roam-dailies-goto-yesterday)
         ("C-c n d m" . org-roam-dailies-capture-tomorrow)
         ("C-c n d M" . org-roam-dailies-goto-tomorrow)
         ("C-c n d d" . org-roam-dailies-capture-date)
         ("C-c n d D" . org-roam-dailies-goto-date)
         ("C-c n d f" . org-roam-dailies-goto-next-note)
         ("C-c n d b" . org-roam-dailies-goto-previous-note))
  :config
  (setq org-roam-completion-everywhere t)
  (setq org-roam-extract-new-file-path "${slug}.org")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%H:%M> %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :if-new (file+head "${slug}.org" "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+filetags:${tag}\n\n")
           :unnarrowed t
           :immediate-finish t)

          ("m" "people" plain "%?"
           :if-new (file+head "${slug}.org" "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+filetags: :kkoent:people:\n\n")
           :unnarrowed t
           :immediate-finish t)

          ("p" "project" plain "%?"
           :if-new (file+head "${slug}.org" "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+filetags: :kkoent:project:\n\n")
           :unnarrowed t
           :immediate-finish t)))
  (org-roam-db-autosync-mode))


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
  ("C-c x t" . google-translate-smooth-translate))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package multi-vterm
  ;; :ensure-system-package
  ;; ((libvterm . "brew install libvterm"))
  :ensure t
  :bind
  ("C-c t" . multi-vterm))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vertico consult use-package-chords use-package-ensure-system-package use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
