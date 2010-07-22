(require :parenscript)
(in-package :parenscript)
(format *error-output* (ps-compile-stream *standard-input*))
