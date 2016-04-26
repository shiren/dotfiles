;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

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
;;; C-c as general purpose escape key sequence.
   ;;;
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
   ;; Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ;; Note: As long as I return [escape] in normal-state, I don't need this.
   ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
;; Works around the fact that Evil uses read-event directly when in operator state, which
;; doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
;; Not sure what behavior this changes, but might as well set it, seeing the Elisp manual's
;; documentation of it.
;(set-quit-char "C-c")

;;; evil-escape
(package-install 'evil-escape)

(provide 'init) 
;;; init.el ends here
