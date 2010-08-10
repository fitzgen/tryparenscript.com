(require :parenscript)
(in-package :parenscript)

;; SAFE-READ is based on SAFE-READ-FROM-STRING in _Let_Over_Lambda_ by Doug
;; Hoyte.

;; List of characters that are blacklisted from being read.
(defvar safe-read-char-blacklist '(#\# #\: #\|))

;; List of forms that are blacklisted from being evaluated.
(defvar safe-read-form-blacklist '(lisp
                                   eval-when
                                   defmacro
                                   defmacro+ps
                                   import-macros-from-lisp
                                   symbol-macrolet
                                   macrolet))

(let ((rt (copy-readtable nil)))

  ;; Function for reading a blacklisted character that just throws an error
  ;; immediately.
  (defun safe-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "reader error"))

  ;; Set each of the blaclisted characters' read functions to safe-reader-error.
  (dolist (c safe-read-char-blacklist)
    (set-macro-character c #'safe-reader-error nil rt))

  (defun safe-read (&optional (stream *standard-input*) fail (eof-error-p t) eof-value recursive-p)
    (if (streamp stream)
      (let ((*readtable* rt)
            (*read-eval* nil))
        (handler-bind
            ((error (lambda (condition)
                      (declare (ignore condition))
                      (return-from safe-read fail))))
          (read stream eof-error-p eof-value recursive-p)))
      fail)))

(defun ps-safe-read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  `(macrolet ,(mapcar (lambda (form)
                        `(,form (&rest args)
                           (declare (ignore args))
                           (concatenate 'string "Unsecure form: " (symbol-name ,form))))
                      safe-read-form-blacklist)
     ,(safe-read stream
                 "Unsecure read error."
                 eof-error-p eof-value recursive-p)))

(setf *ps-read-function* #'ps-safe-read)

(format *error-output* (ps-compile-stream *standard-input*))