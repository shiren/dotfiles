;;; upbo.el --- Karma Test Runner Emacs Integration
;;
;; Filename: upbo.el
;; Description: karma Test Runner Emacs Integration
;; Author: Sungho Kim(shiren)
;; Maintainer: Sungho Kim(shiren)
;; URL: http://github.com/shiren
;; Version: 0.0.0
;; Package-Requires: ((pkg-info "0.4") (emacs "24"))
;; Keywords: language, javascript, js, karma, testing

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Karma Test Runner Emacs Integration

;;  Usage:
;;  (add-to-list 'upbo-project-config '("~/masterpiece/tui.chart/" "~/masterpiece/tui.chart/karma.conf.js"))

;;; Code:
(defgroup upbo nil
  "Karma Test Runner Emacs Integration"
  :prefix "upbo-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/shiren")
  :link '(emacs-commentary-link :tag "Commentary" "karma"))

(defcustom upbo-project-config '()
            "Each element is a list of the form (KEY VALUE).")

(defun karma-start (args)
  (let ((inhibit-read-only t))
    (insert (concat "\n" "start karma\n")))

  ;; 프로세스 설정
  (when (process-live-p upbo-proc)
    (print "kill-proc")
    (kill-process upbo-proc))

  (let ((default-directory upbo-project-root))
    (setq upbo-proc (apply 'start-process-shell-command
                           (append (list "upboProcess" upbo-buffer-name "npx" "karma" "start" upbo-karma-conf-path)
                                   args))))
  ;; 프로세스 필터 설정
  (set-process-filter upbo-proc 'upbo-process-filter))

(defun karma-auto-watch ()
  (interactive)
  (karma-start '("--single-run" "--auto-watch")))

(defun karma-single-run ()
  (interactive)
  (karma-start '("--single-run" "--no-auto-watch")))

(defun kill-upbo-buffer ()
  "HELLO"
  (interactive)
  (kill-buffer (current-buffer)))

(defun upbo-process-filter (process output)
  (setq upbo-last-result (current-time-string))
  ;; 리드온리 버퍼에 무언가를 출력하려면 inhibit-read-only가 t여야함
  (let ((inhibit-read-only t))
    (set-buffer (process-buffer process))
    (insert output)
    ;; ansi 코드있는 버퍼 렌더링하기
    (ansi-color-apply-on-region (point-min) (point-max))
    ))

(defun git-root-dir ()
  "Returns the current directory's root Git repo directory, or
NIL if the current directory is not in a Git repo."
  (let ((dir (locate-dominating-file default-directory ".git")))
    (when dir
      (file-name-directory dir))))

(defun get-karma-conf-setting (project-root-path)
  (car (cdr (car (seq-filter
   (lambda (el)
     (string= (car el) project-root-path)) upbo-project-config)))))

(defun create-upbo-buffer (buffer-name)
  (let ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (upbo-view-mode)
      (switch-to-buffer buffer))))

(defun run-upbo ()
  (interactive)
  (let ((buffer-name (concat "*upbo:" (git-root-dir) "*")))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (create-upbo-buffer buffer-name))))

(define-key global-map (kbd "C-c u") 'run-upbo)

(defvar upbo-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") 'karma-auto-watch)
    (define-key map (kbd "r") 'karma-single-run)
    (define-key map (kbd "k") 'kill-upbo-buffer)
    map))

(define-key upbo-view-mode-map (kbd "w") 'karma-auto-watch)
(define-key upbo-view-mode-map (kbd "r") 'karma-single-run)
(define-key upbo-view-mode-map (kbd "k") 'kill-upbo-buffer)

(define-derived-mode upbo-view-mode special-mode "upbo-view"
  "Major mode for upbo"
  (use-local-map upbo-view-mode-map)

  (make-local-variable 'upbo-proc)
  (make-local-variable 'upbo-buffer-name)
  (make-local-variable 'upbo-proejct-root)
  (make-local-variable 'upbo-karma-conf-path)

  (setq upbo-proc nil)
  (setq upbo-buffer-name (concat "*upbo:" (git-root-dir) "*"))
  (setq upbo-project-root (git-root-dir))
  (setq upbo-karma-conf-path (get-karma-conf-setting upbo-project-root))

  (let ((inhibit-read-only t))
    (insert (concat "Project: " upbo-project-root "\n"))
    (insert (concat "Karma conf: " upbo-karma-conf-path "\n"))
    (insert "upbo started\nw: auto-watch, r: single-run, k: kill upbo")))

;; (define-key global-map (kbd "C-c u") 'run-upbo)

;;;;;;;; Minor
(defun karma-single-run-minor ()
  (interactive)
  (setq upbo-last-result "KARMA-START")
  (when (process-live-p upbo-proc)
    (print "kill-proc")
    (kill-process upbo-proc))

  (let ((default-directory upbo-project-root))
    (setq upbo-proc (apply 'start-process-shell-command
                           (append (list "upboProcess" nil "npx" "karma" "start" upbo-karma-conf-path "--single-run" "--reporters" "dots")))))
  ;; 프로세스 필터 설정
  (set-process-filter upbo-proc 'upbo-minor-process-filter))

(defun upbo-minor-process-filter (process output)
  (when (string-match "Executed \\([0-9]+\\) of \\([0-9]+\\)" output)
    (setq upbo-last-result (concat (match-string 1 output) "/" (match-string 2 output))))
  (force-mode-line-update))

(defvar upbo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key global-map (kbd "C-c u") 'run-upbo)
    (define-key global-map (kbd "C-c k") 'karma-single-run-minor)
    (define-key global-map (kbd "C-c h") 'update-mode-line)
    map)
  "The keymap used when `upbo-mode' is active.")

(defun upbo-mode-hook ()
  "Hook which enables `upbo-mode'"
  (upbo-mode 1))

(defun project-test-result ()
  (or upbo-last-result " "))

;;;###autoload
(define-minor-mode upbo-mode
  "Toggle upbo mode.
Key bindings:
\\{upbo-mode-map}"
  :lighter (:eval (format " upbo[%s]" (project-test-result)))
  :group 'upbo
  :global nil
  :keymap 'upbo-mode-map
  (make-local-variable 'upbo-proc)
  (make-local-variable 'upbo-last-result)
  (make-local-variable 'upbo-proejct-root)
  (make-local-variable 'upbo-karma-conf-path)

  (setq upbo-proc nil)
  (setq upbo-project-root (git-root-dir))
  (setq upbo-karma-conf-path (get-karma-conf-setting upbo-project-root))
  (setq upbo-last-result nil))

(add-hook 'js-mode-hook 'upbo-mode-hook)
(add-hook 'js2-mode-hook 'upbo-mode-hook)

(provide 'upbo)
;;; upbo.el ends here
