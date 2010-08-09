(require :parenscript)
(in-package :parenscript)

;; SAFE-READ is based on SAFE-READ-FROM-STRING in _Let_Over_Lambda_ by Doug
;; Hoyte.

;; List of characters that are blacklisted from being read.
(defvar safe-read-blacklist '(#\# #\: #\|))

(let ((rt (copy-readtable nil)))

  ;; Function for reading a blacklisted character that just throws an error
  ;; immediately.
  (defun safe-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "reader error"))

  ;; Set each of the blaclisted characters' read functions to safe-reader-error.
  (dolist (c safe-read-blacklist)
    (set-macro-character c #'safe-reader-error nil rt))

  (defun safe-read (&optional (stream *standard-input*) fail)
    (if (streamp stream)
      (let ((*readtable* rt)
            (*read-eval* nil))
        (handler-bind
            ((error (lambda (condition)
                      (declare (ignore condition))
                      (return-from safe-read fail))))
          (read stream)))
      fail)))

(defun ps-safe-read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore eof-error-p eof-value recursive-p))
  `(macrolet ((lisp (&rest args)
                (declare (ignore args))
                "Unsecure form: lisp")
              (eval-when (&rest args)
                (declare (ignore args))
                "Unsecure form: eval-when")
              (defmacro+ps (&rest args)
                (declare (ignore args))
                "Unsecure form: defmacro+ps")
              (import-macros-from-lisp (&rest args)
                (declare (ignore args))
                "Unsecure form: import-macros-from-lisp")
              (symbol-macrolet (&rest args)
                (declare (ignore args))
                "Unsecure form: symbol-macrolet")
              (macrolet (&rest args)
                (declare (ignore args))
                "Unsecure form: macrolet"))
     ,(safe-read stream)))

(setf *ps-read-function* #'ps-safe-read)

(format *error-output* (ps-compile-stream *standard-input*))