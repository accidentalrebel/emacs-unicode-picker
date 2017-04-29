;;; test-helper --- Test helper for unicode-picker

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar unicode-picker-test-path
  (f-dirname (f-this-file)))

(defvar unicode-picker-root-path
  (f-parent unicode-picker-test-path))

(defvar unicode-picker-sandbox-path
  (f-expand "sandbox" unicode-picker-test-path))

(when (f-exists? unicode-picker-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" unicode-picker-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory unicode-picker-sandbox-path))
     (when (f-exists? unicode-picker-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir unicode-picker-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require 'unicode-picker)

;;; test-helper.el ends here
