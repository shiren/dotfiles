;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sungho Kim (shiren)"
      user-mail-address "shirenbeat@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;

;; Basic
(set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;;(setq echo-keystrokes 0.001)
(setq tab-width 2)
(setq standard-indent 2)
(setq doom-modeline-enable-word-count nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(set-variable 'cursor-type 'bar)

(when (and window-system (eq system-type 'darwin))
  ;; (set-face-attribute 'default nil :family "Source Code Pro" :height 140 :weight 'normal)
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 140 :weight 'normal)
  (set-fontset-font t 'hangul (font-spec :height 140 :name "D2Coding ligature"))
  (setq-default line-spacing 0))

;;; Scroll setup
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-conservatively 200) ;; 스크롤 도중에 센터로 커서 이동하지 않도록
(setq scroll-margin 3) ;; 스크롤시 남기는 여백

(setq visible-bell t)

(setq gc-cons-threshold 100000000)

(setq doom-modeline-buffer-state-icon nil)
(setq doom-modeline-buffer-modification-icon nil)
(setq display-line-numbers-type nil)

;; Org
(after! org
  (setq org-agenda-files (file-expand-wildcards "~/org/agenda/*.org"))
  (setq org-default-notes-file "~/org/agenda/index.org")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-strip-leading-and-trailing-blank-lines t)
  (setq org-log-done t)
  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-adapt-indentation t)
  (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
  (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (setq org-refile-targets '((org-agenda-files :level . 1) (("~/org/note/devnote.org") :level . 1)))

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

  (setq org-agenda-restore-windows-after-quit t)

  (setq org-roam-directory (file-truename "~/org/roam"))
  (org-roam-db-autosync-mode)
  (setq org-journal-dir "~/org/journals")
  (setq org-journal-file-format "%Y_%m_%d.org")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-file-type 'weekly)
  (map! :leader
        :g "n j k" #'org-journal-open-current-journal-file))

;; javascript
(setq js-indent-level 2)
(after! js2-mode
  (add-hook 'js2-mode-hook (lambda ()
                             ;;(add-hook 'after-save-hook 'eslint-fix nil t)
                             (setq tab-width 2)
                             (setq-default js2-basic-offset 2)
                             (setq js-switch-indent-offset 2))))
                             ;; (electric-indent-mode -1)
                             ;;(js2-imenu-extras-mode)
                             

;; Web
(after! web-mode
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil))

;; prettier
;; (add-hook 'after-init-hook #'global-prettier-mode)

;; lsp
;; lsp 체커를 항상 넥스트 체커로 두자, 제대로 체크가 안된다.
(add-hook 'lsp-after-initialize-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'lsp)))

(setq-hook! 'js2-mode-hook flycheck-checker 'javascript-eslint)
(setq-hook! 'typescript-mode-hook flycheck-checker 'javascript-eslint)
(setq-hook! 'typescript-tsx-mode-hook flycheck-checker 'javascript-eslint)

;; (after! company-tabnine
;;   (setq company-show-numbers t)
;;   (add-to-list 'company-backends #'company-tabnine))


(after! ivy
  (map! :map ivy-minibuffer-map
        "S-SPC" nil
        "M-s" 'ivy-restrict-to-matches)
  (map! :leader
        :g "j i" #'swiper
        :g "j I" #'swiper-all))

(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)
         :map company-mode-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)))
