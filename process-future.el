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

(cl-defstruct pf::future
  process result)

(defun pf::new (&rest args)
  "Create a new process future with ARGS.
This will return a struct (as created by `cl-defstruct') with 2 fields:
'process' which is the process object that will be started and 'result', where
the process will be writing its output.

Note that ARGS must be a *list* of strings as demanded by `make-process'.
In other words
This is wrong: (pf::new \"git status\")
This is right: (pf::new \"git\" \"status\")"
  (let* ((future  (make-pf::future))
         (process (make-process
                   :name "Process Future"
                   :connection-type 'pipe
                   :command args
                   :filter #'(lambda (_ msg)
                               (let ((result (pf::result-of future)))
                                 (setf (pf::future-result future) (concat result msg)))))))
    (setf (pf::future-process future) process)
    future))

(cl-defun pf::await (future &key (timeout 1) (just-this-one t))
  "Block until FUTURE has produced output and return it.
The output will also be added to FUTURE's 'result' field.

Will accept the following optional keyword arguments:

TIMEOUT: The timeout in seconds to wait for the process. May be a float to
specify fractional number of seconds. In case of timeout nil will be returned.

JUST-THIS-ONE: When t only read from the process of FUTURE and no other. For
details see documentation of `accept-process-output'."
  (when (pf::is-alive? future)
    (accept-process-output
     (pf::process-of future) timeout nil just-this-one))
  (pf::result-of future))

(defun pf::await-to-finish (future)
  "Keep reading the output of FUTURE until it is done.
Same as `pf::await', but will keep reading (and blocking) so long as the
process associated with FUTURE is alive.
If the process never quits this method will block forever. Use with caution!"
  (let ((process (pf::process-of future)))
    (while (process-live-p process)
      (accept-process-output process nil nil t)))
  (pf::result-of future))

(defun pf::is-alive? (future)
  "Return whether the process associated with FUTURE is alive."
  (process-live-p (pf::process-of future)))

(defalias 'pf::process-of #'pf::future-process
  "Access a process future's process slot.")

(defalias 'pf::result-of #'pf::future-result
  "Access a process future's result slot.")

(provide 'process-future)

;;; process-future.el ends here
