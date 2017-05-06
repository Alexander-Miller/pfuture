;;; process-future.el --- a simple wrapper around asynchronous processes -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

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

;;; Code:

(require 'cl-macs)

(cl-defstruct process-future
  process result)

(defun pf-create (&rest args)
  "Create a new process future with ARGS.
This will return a struct (as created by `cl-defstruct') with 2 fields:
'process' which is the process object which will be created and 'result' which
will hold the output of the process after it has finished running.

Note that ARGS must be a list of strings as demanded by `make-prcess'.
For example '(pf-create \"git status\")' will not work, you must call
'(pf-create \"git\" \"status\")' instead."
  (let* ((future  (make-process-future))
         (process (make-process
                   :name "Process Future"
                   :connection-type 'pipe
                   :command args
                   :filter #'(lambda (_ msg)
                               (setf (process-future-result future) msg)))))
    (setf (process-future-process future) process)
    future))

(cl-defun pf-await (future &key (timeout 1000) (just-this-one t))
  "Wait until FUTURE has produced output and return it.
Wait at most TIMEOUT miliseconds. The returned value will be nil and the process
will keep running if the process is not finished after TIMEOUT miliseconds.
When JUST-THIS-ONE is non-nil accept output only from FUTURE and suspend reading
output from processes other than FUTURE. For details see documentation of
`accept-process-output'."
  (accept-process-output
   (process-future-process future) 0 timeout just-this-one)
  (process-future-result future))

(provide 'process-future)

;;; process-future.el ends here
