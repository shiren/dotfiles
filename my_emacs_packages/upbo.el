;;; upbo.el --- Karma Test Runner Emacs Integration ;;; -*- lexical-binding: t; -*-
;;
;; Filename: upbo.el
;; Description: karma Test Runner Emacs Integration
;; Author: Sungho Kim(shiren)
;; Maintainer: Sungho Kim(shiren)
;; URL: http://github.com/shiren
;; Version: 0.0.0
;; Package-Requires: ((pkg-info "0.4") (emacs "24"))
;; Keywords: javascript, js, testing, karma

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
;; (upbo-define-test
;;  :path "~/tui.chart/"
;;  :browsers "ChromeHeadless"
;;  :conf-file "~/tui.chart/karma.conf.js")

;;; Code:
(require 'dash)
(require 'ansi-color)

(defgroup upbo nil
  "Karma Emacs Integration"
  :prefix "upbo-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/shiren")
  :link '(emacs-commentary-link :tag "Commentary" "karma"))

(defvar upbo-configs nil)

(defvar upbo-project-result (make-hash-table :test 'equal))

(defcustom upbo-karma-command nil
  "Upbo karma command.")

(defun upbo-define-test (&rest args)
  (let* ((project-name (plist-get args :path))
         (equal-project-name
          (lambda (config)
            (string= (plist-get config :path) project-name)))
         (config (-first equal-project-name upbo-configs)))
    (when config
      (setq upbo-configs (-reject equal-project-name upbo-configs)))
    (push args upbo-configs)))

;;;;;;;;; upbo-view-mode
(defun upbo-open-upbo-view ()
  "Open upbo view buffer of current buffer's project."
  (interactive)
  (let* ((buffer-name (upbo-get-view-buffer-name))
         (upbo-view-buffer (get-buffer buffer-name)))
    (unless upbo-view-buffer
      (generate-new-buffer buffer-name))
    (with-current-buffer upbo-view-buffer
      (unless (string= major-mode "upbo-view-mode")
        (upbo-view-mode))
      (switch-to-buffer upbo-view-buffer))))

(defun upbo-kill-upbo-buffer ()
  "Kill upbo buffer of current buffer's project."
  (interactive)
  (kill-buffer (upbo-get-view-buffer-name)))

(defun upbo-update-upbo-view-buffer (buffer output)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (orig-point-max (point-max)))
      (goto-char (point-max))
      (insert output)
      (upbo-handle-buffer-scroll buffer orig-point-max)
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defvar upbo-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") 'upbo-karma-auto-watch)
    (define-key map (kbd "s") 'upbo-karma-single-run)
    (define-key map (kbd "k") 'upbo-kill-upbo-buffer)
    map))

;;;###autoload
(define-derived-mode upbo-view-mode special-mode "upbo-view"
  "Major mode for upbo"
  (use-local-map upbo-view-mode-map))

;;;;;;;; Minor
(defun upbo-karma-start (args upbo-view-buffer-name)
  (let ((upbo-process (get-buffer-process upbo-view-buffer-name)))
    (when (process-live-p upbo-process)
      (kill-process upbo-process)))

  (let ((default-directory (upbo-git-root-dir))
        (process-args (append
                       (list "upboProcess"
                             upbo-view-buffer-name)
                       (upbo-get-karma-command)
                       (list
                        "start"
                        (upbo-get-karma-conf-from-config)
                        "--reporters" "dots")
                       (when (upbo-get-browsers-from-config)
                         (list "--browsers" (upbo-get-browsers-from-config)))
                       args)))
    (print process-args)
    (apply 'start-process-shell-command process-args))

  (set-process-filter (get-buffer-process upbo-view-buffer-name) 'upbo-minor-process-filter))

(defun upbo-karma-single-run ()
  "Karma single run."
  (interactive)
  (upbo-print-to-mode-line "Loading")
  (upbo-karma-start '("--single-run")
                    (upbo-get-view-buffer-name)))

(defun upbo-karma-auto-watch ()
  "Karma run with auto-watch."
  (interactive)
  (upbo-print-to-mode-line "Loading")
  (upbo-karma-start '("--no-single-run" "--auto-watch")
                    (upbo-get-view-buffer-name)))

(defun upbo-print-to-mode-line (str)
  (puthash (upbo-git-root-dir) str upbo-project-result))

(defun upbo-parse-output-for-mode-line (buffer output)
  (with-current-buffer buffer
    (upbo-print-to-mode-line
             ;; Num of Num (Num Char)  ===> 5 of 10 (5 FAILED)
             ;; Num of Num Char ===> 5 of 10 ERROR
             ;; Num of Num (Char Num) Char ===> 5 of 10 (skipped 5) SUCCESS
             (if (string-match "Executed \\(?1:[0-9]+\\) of \\(?2:[0-9]+\\) ?\\(?3:ERROR\\)?(?\\(?4:[0-9]+ FAILED\\|skipped [0-9]+\\)?)? ?\\(?5:SUCCESS\\)?"
                               output)
                 (concat (or (match-string 5 output) (match-string 3 output) (match-string 4 output))
                         "/"
                         (match-string 1 output)
                         "/"
                         (match-string 2 output))
               "~"))))

(defun upbo-handle-buffer-scroll (buffer buffer-point-max)
  (with-current-buffer buffer
    (let ((windows (get-buffer-window-list buffer nil t)))
      (dolist (window windows)
        (when (= (window-point window) buffer-point-max)
          (set-window-point window (point-max)))))))

(defun upbo-minor-process-filter (process output)
  (upbo-parse-output-for-mode-line (process-buffer process) output)
  (upbo-update-upbo-view-buffer (process-buffer process) output)
  (upbo-force-mode-line-update-to-all))

(defun upbo-force-mode-line-update-to-all ()
  (dolist (elt (buffer-list))
    (with-current-buffer elt
      (force-mode-line-update))))

(defun upbo-get-view-buffer-name ()
  (concat "*upbo:" (upbo-git-root-dir) "*"))

(defun upbo-git-root-dir ()
  "Return the current directory's root Git repo directory, or))
NIL if the current directory is not in a Git repo."
  (let ((dir (locate-dominating-file default-directory ".git")))
    (when dir
      (file-name-directory dir))))

(defun upbo-get-project-config-by-path (path)
  (-first (lambda (config)
            (string= path (plist-get config :path)))
          upbo-configs))

(defun upbo-get-current-config ()
  (upbo-get-project-config-by-path (upbo-git-root-dir)))

(defun upbo-get-karma-command ()
  (cond (upbo-karma-command
         (list upbo-karma-command))
        ((executable-find "karma")
         (list (executable-find "karma")))
        (t
         '("npx" "karma"))))

(defun upbo-find-karma-conf ()
  (let ((expected-karma-conf-path (concat (upbo-git-root-dir) "karma.conf.js")))
    (when (file-exists-p expected-karma-conf-path)
      expected-karma-conf-path)))

(defun upbo-get-karma-conf-from-config ()
  (or (plist-get (upbo-get-current-config) :conf-file)
      (upbo-find-karma-conf)))

(defun upbo-get-browsers-from-config ()
  (plist-get (upbo-get-current-config) :browsers))

(defvar upbo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c u r") 'upbo-open-upbo-view)
    (define-key map (kbd "C-c u s") 'upbo-karma-single-run)
    (define-key map (kbd "C-c u w") 'upbo-karma-auto-watch)
    (define-key map (kbd "C-c u t") 'upbo-testtest)
    map)
  "The keymap used when `upbo-mode' is active.")

(defun upbo-mode-hook ()
  "Hook which enables `upbo-mode'."
  (upbo-mode 1))

(defun upbo-testtest ()
  "Just for test."
  (interactive)
  (print (hash-table-keys upbo-project-result))
  (print (hash-table-values upbo-project-result))
  (print (upbo-get-karma-conf-from-config)))

(defun upbo-project-test-result ()
  (let ((result (gethash (upbo-git-root-dir) upbo-project-result)))
    (if result
        (concat "[" result "]")
      "")))

;;;###autoload
(define-minor-mode upbo-mode
  "Toggle upbo mode.
Key bindings:
\\{upbo-mode-map}"
  :lighter (:eval (format " upbo%s" (upbo-project-test-result)))
  :group 'upbo
  :global nil
  :keymap 'upbo-mode-map)

(add-hook 'js-mode-hook 'upbo-mode-hook)
(add-hook 'js2-mode-hook 'upbo-mode-hook)

(provide 'upbo)
;;; upbo.el ends here
