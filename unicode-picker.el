;;; unicode-picker.el --- Makes it easier to browse and insert characters.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Juan Karlo Licudine

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; Keywords: tools
;; Version: 0.1
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

(defun unicode-picker (&optional regexp)
  "List REGEXP."
  (interactive "sRegexp (default \".*\"): ")
  (let* ((regexp (or regexp ".*"))
	 (case-fold-search t)
	 (cmp (lambda (x y) (< (cdr x) (cdr y))))
	 (char-alist (sort (cl-remove-if-not (lambda (x) (string-match regexp (car x)))
					     (ucs-names))
			   cmp)))
    (devenv-smart-open-elisp-output-window "*unicode-picker*")
    (read-only-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (font-lock-mode)
      (text-scale-set 5)
      (dolist (c char-alist)
	(insert (cdr c))))))

(provide 'unicode-picker)
;;; unicode-picker.el ends here
