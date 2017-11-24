;;; upbo.el --- Karma Test Runner Emacs Integration ;;; -*- lexical-binding: t; -*-
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

(defvar project-result (make-hash-table :test 'equal))

;;;;;;;;; upbo-view-mode
(defun open-upbo-view ()
  (interactive)
  (let* ((buffer-name (get-upbo-view-buffer-name))
         (upbo-view-buffer (get-buffer buffer-name)))
    (unless upbo-view-buffer
      (generate-new-buffer buffer-name))
    (with-current-buffer upbo-view-buffer
      (unless (string= major-mode "upbo-view-mode")
        (upbo-view-mode))
      (switch-to-buffer upbo-view-buffer))))

(defun kill-upbo-buffer ()
  (interactive)
  (kill-buffer (get-upbo-view-buffer-name)))

(defvar upbo-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") 'karma-auto-watch)
    (define-key map (kbd "r") 'karma-single-run)
    (define-key map (kbd "k") 'kill-upbo-buffer)
    map))

(define-key upbo-view-mode-map (kbd "w") 'karma-auto-watch)
(define-key upbo-view-mode-map (kbd "r") 'karma-single-run)
(define-key upbo-view-mode-map (kbd "k") 'kill-upbo-buffer)

;;;###autoload
(define-derived-mode upbo-view-mode special-mode "upbo-view"
  "Major mode for upbo"
  (use-local-map upbo-view-mode-map))

  ;; (let ((inhibit-read-only t))
  ;;   (insert (concat "Project: " (git-root-dir) "\n"))
  ;;   (insert (concat "Karma conf: " (get-karma-conf-setting) "\n"))
  ;;   (insert "upbo started\nw: auto-watch, r: single-run, k: kill upbo"))


;;;;;;;; Minor
(defun karma-start (args upbo-view-buffer-name)
  (let ((upbo-process (get-buffer-process upbo-view-buffer-name)))
    (when (process-live-p upbo-process)
      (kill-process upbo-process)))

  (let ((default-directory (git-root-dir)))
    (apply 'start-process-shell-command
           (append
            (list "upboProcess"
                  upbo-view-buffer-name
                  "npx" "karma" "start"
                  (get-karma-conf)
                  "--reporters" "dots")
            args)))

  ;; 프로세스 필터 설정
  (set-process-filter (get-buffer-process upbo-view-buffer-name)
                      'upbo-minor-process-filter))

(defun karma-single-run ()
  (interactive)
  (karma-start '("--single-run")
               (get-upbo-view-buffer-name)))

(defun karma-auto-watch ()
  (interactive)
  (karma-start '("--no-single-run" "--auto-watch")
               (get-upbo-view-buffer-name)))

(defun parse-output-for-mode-line (buffer output)
  (with-current-buffer buffer
    (puthash (git-root-dir)
             ;; 숫자 of 숫자 (숫자 문자)  ===> 5 of 10 (5 FAILED)
             ;; 숫자 of 숫자 문자 ===> 5 of 10 ERROR
             ;; 숫자 of 숫자 (문자 숫자) 문자 5 of 10 (skipped 5) SUCCESS
             ;;
             (if (string-match "Executed \\([0-9]+\\) of \\([0-9]+\\) ?(?\\(?3:[0-9]+ FAILED\\|skipped [0-9]+\\)?)? ?\\(?4:SUCCESS\\)?"
                               output)
                 (concat (match-string 1 output)
                         "/"
                         (match-string 2 output)
                         (when (match-string 4 output)
                           (concat "/" (match-string 4 output)))
                         (when (match-string 3 output)
                           (concat "/" (match-string 3 output))))
               "~")
             project-result)))

(defun update-upbo-view-buffer (buffer output)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (orig-point-max (point-max)))
      (goto-char (point-max))
      (insert output)

      (handle-buffer-scroll buffer orig-point-max)

      ;; ansi 코드있는 버퍼 렌더링하기
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defun handle-buffer-scroll (buffer buffer-point-max)
  (with-current-buffer buffer
    (let ((windows (get-buffer-window-list buffer)))
      (dolist (window windows)
        (when (= (window-point window) buffer-point-max)
          (set-window-point window (point-max)))))))

(defun upbo-minor-process-filter (process output)
  (parse-output-for-mode-line (process-buffer process) output)
  (update-upbo-view-buffer (process-buffer process) output)
  (force-mode-line-update-to-all))

(defun force-mode-line-update-to-all ()
  (dolist (elt (buffer-list))
    (with-current-buffer elt
      (force-mode-line-update))))

(defun get-upbo-view-buffer-name ()
  (concat "*upbo:" (git-root-dir) "*"))

(defun git-root-dir ()
  "Returns the current directory's root Git repo directory, or
NIL if the current directory is not in a Git repo."
  (let ((dir (locate-dominating-file default-directory ".git")))
    (when dir
      (file-name-directory dir))))

(defun get-karma-conf-setting ()
  (car (cdr (car
             (seq-filter
              (lambda (el)
                (string= (car el) (git-root-dir)))
              upbo-project-config)))))

(defun find-karma-conf ()
  (let ((expected-karma-conf-path (concat (git-root-dir) "karma.conf.js")))
    (when (file-exists-p expected-karma-conf-path)
      expected-karma-conf-path)))

(defun get-karma-conf ()
  (or (get-karma-conf-setting) (find-karma-conf)))

(defvar upbo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key global-map (kbd "C-c u r") 'open-upbo-view)
    (define-key global-map (kbd "C-c u s") 'karma-single-run)
    (define-key global-map (kbd "C-c u w") 'karma-auto-watch)
    (define-key global-map (kbd "C-c u t") 'testtest)
    map)
  "The keymap used when `upbo-mode' is active.")

(defun upbo-mode-hook ()
  "Hook which enables `upbo-mode'."
  (upbo-mode 1))

(defun testtest ()
  "JUST test."
  (interactive)
  (print (hash-table-keys project-result))
  (print (hash-table-values project-result))
  (print (get-karma-conf)))

(defun project-test-result ()
  (let ((result (gethash (git-root-dir) project-result)))
    (if result
        (concat "[" result "]")
      "")))

;;;###autoload
(define-minor-mode upbo-mode
  "Toggle upbo mode.
Key bindings:
\\{upbo-mode-map}"
  :lighter (:eval (format " upbo%s" (project-test-result)))
  :group 'upbo
  :global nil
  :keymap 'upbo-mode-map)

(add-hook 'js-mode-hook 'upbo-mode-hook)
(add-hook 'js2-mode-hook 'upbo-mode-hook)

(provide 'upbo)
;;; upbo.el ends here
