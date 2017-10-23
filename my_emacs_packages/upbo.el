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
;;

;;; Code:
(make-local-variable 'upbo-proc)
(make-local-variable 'upbo-buffer-name)
(make-local-variable 'upbo-proejct-root)

(setq upbo-proc nil)
(setq upbo-buffer-name nil)
(setq upbo-project-root nil)

(defun karma-start (args)
  (let ((inhibit-read-only t))
    (insert (concat "\n" upbo-project-root " start karma\n")))

  ;; 프로세스 설정
  (when (process-live-p upbo-proc)
    (print "kill-proc")
    (kill-process upbo-proc))

  (let ((default-directory upbo-project-root))
    (setq upbo-proc (apply 'start-process-shell-command
                           (append (list "upboProcess" upbo-buffer-name "npx" "karma" "start")
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

(defun create-upbo-buffer (upbo-buffer-name)
  (let ((buffer (generate-new-buffer upbo-buffer-name)))
    (with-current-buffer buffer
      (upbo-mode)
      (switch-to-buffer buffer))))

(defun run-upbo ()
  (interactive)
  (let ((upbo-buffer-name (concat "*upbo:" (git-root-dir) "*")))
    (if (get-buffer upbo-buffer-name)
        (switch-to-buffer upbo-buffer-name)
      (create-upbo-buffer upbo-buffer-name))))

(define-key global-map (kbd "C-c u") 'run-upbo)

(defvar upbo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") 'karma-auto-watch)
    (define-key map (kbd "r") 'karma-single-run)
    (define-key map (kbd "k") 'kill-upbo-buffer)
    map))

(define-key upbo-mode-map (kbd "w") 'karma-auto-watch)
(define-key upbo-mode-map (kbd "r") 'karma-single-run)
(define-key upbo-mode-map (kbd "k") 'kill-upbo-buffer)

(define-derived-mode upbo-mode special-mode "Upbo"
  "Major mode for upbo"
  (use-local-map upbo-mode-map)
  (setq upbo-proc nil)
  (setq upbo-buffer-name (concat "*upbo:" (git-root-dir) "*"))
  (setq upbo-project-root (git-root-dir))

  (let ((inhibit-read-only t))
    (insert "upbo started\nw: auto-watch, r: single-run, k: kill upbo")))

(provide 'upbo)
;;; upbo.el ends here
