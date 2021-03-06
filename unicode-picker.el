;;; unicode-picker.el --- Visual tool for searching unicode characters.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Juan Karlo Licudine

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; Keywords: tools
;; Version: 0.2
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(define-derived-mode unicode-picker-mode special-mode "unicode-picker-mode")

(defun unicode-picker--control-config ()
  "Initial config for setting of controls."
  (local-set-key (kbd "RET") 'unicode-picker-insert-character-then-return)
  (local-set-key (kbd "SPC") 'unicode-picker-insert-character))

(add-hook 'unicode-picker-mode-hook 'unicode-picker--control-config)
(add-hook 'post-command-hook 'unicode-picker--post-command-listener)

(defvar unicode-picker--highlighted-point-position 0 "Last point position that highlights a unicode character.")
(defvar unicode-picker--current-unicode-list nil "A list containing the (CHAR-NAME . CHAR-CODE) for each character shown in the buffer.")
(defvar unicode-picker--caller-buffer nil "The buffer where ‘unicode-picker’ was called.")
(defvar unicode-picker--buffer-name "*unicode-picker*" "The name of the buffer for the unicode picker.")

(defcustom unicode-picker--display-char-height 200 "The display height of the characters on the picker buffer."
  :group 'unicode-picker)
(defcustom unicode-picker--chars-per-row 20 "The number of chars to display per row."
  :group 'unicode-picker)

(defun unicode-picker (&optional regexp)
  "Search unicode characters using REGEXP and displays to a dedicated buffer.
Selected characters from dedicated buffer are inserted back to the point from the calling buffer."
  (interactive "sRegexp (default \".*\"): ")
  (let* ((regexp (or regexp ".*"))
	 (case-fold-search t)
	 (cmp (lambda (x y) (< (cdr x) (cdr y))))
	 (char-alist (sort (cl-remove-if-not (lambda (x) (string-match regexp (car x)))
					     (ucs-names))
			   cmp)))

    (setq unicode-picker--current-unicode-list nil)
    
    (when (not (string= unicode-picker--buffer-name (buffer-name)))
      (setq unicode-picker--caller-buffer (buffer-name)))

    (when (not (equal (buffer-name) unicode-picker--buffer-name))
      (if (fboundp 'devenv-smart-open-elisp-output-window)
	  (devenv-smart-open-elisp-output-window unicode-picker--buffer-name)
	(other-window 1)
	(switch-to-buffer unicode-picker--buffer-name)))
    
    (unicode-picker-mode)
    (let ((inhibit-read-only t)
	  (index 0))
      (erase-buffer)
      (font-lock-mode)
      (dolist (c char-alist)
	(when (>= index unicode-picker--chars-per-row)
	  (setq index 1)
	  (newline)
	  )
	(insert (propertize (char-to-string (cdr c)) 'font-lock-face '(:height unicode-picker--display-char-height)))
	(add-to-list 'unicode-picker--current-unicode-list c t)
	(incf index))
      (goto-char (point-min))
      (unicode-picker--display-unicode-detail-in-minibuffer)
      (setq unicode-picker--highlighted-point-position (point)))))

(defun unicode-picker-insert-character ()
  "Insert the character at point to the point at the calling buffer."
  (interactive)
  (unicode-picker-insert-character-then-return)
  (select-window (get-buffer-window unicode-picker--buffer-name))
  )

(defun unicode-picker-insert-character-then-return ()
  "Insert the character at point to the point at the calling buffer.
The control then returns to the character picker buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (kill-ring-save (point) (+ (point) 1))
    (select-window (get-buffer-window unicode-picker--caller-buffer))
    (yank)))

(defun unicode-picker--post-command-listener ()
  "Called when a command is triggered.  Determines whether the point has been moved."
  (when (and (string= (buffer-name) unicode-picker--buffer-name) (not (eq (point) unicode-picker--highlighted-point-position)))
    (unicode-picker--display-unicode-detail-in-minibuffer)
    (setq unicode-picker--highlighted-point-position (point))
    ))

(defun unicode-picker--display-unicode-detail-in-minibuffer ()
  "Display the detail of the character at point in the minibuffer."
  (let ((unicode-detail (unicode-picker--get-unicode-detail-at-point)))
    (when unicode-detail
      (message "%s - %s" (car unicode-detail) (format "0x%06X" (cdr unicode-detail))))))

(defun unicode-picker--get-unicode-detail-at-point ()
  "Gets the unicode detail at the position of point."
  (let* ((index (- (- (point) 1) (- (line-number-at-pos) 1))))
    (nth index unicode-picker--current-unicode-list)))

(provide 'unicode-picker)
;;; unicode-picker.el ends here
