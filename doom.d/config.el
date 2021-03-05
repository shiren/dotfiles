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

(setq echo-keystrokes 0.001)
(setq tab-width 2)
(set-variable 'cursor-type 'bar)


(when (and window-system (eq system-type 'darwin))
  ;; (set-face-attribute 'default nil :family "Source Code Pro" :height 140 :weight 'normal)
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 140 :weight 'normal)
  (set-fontset-font t 'hangul (font-spec :name "D2Coding"))
  (setq-default line-spacing 0))


;; Org
;;
(require 'org)
(setq org-agenda-files (file-expand-wildcards "~/org/agenda/*.org"))

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
