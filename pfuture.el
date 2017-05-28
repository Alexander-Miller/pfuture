;;; pfuture.el --- a simple wrapper around asynchronous processes -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Package-Requires: ((emacs "25"))
;; Version: 1.0

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

(require 'cl-lib)

(cl-defstruct pfuture
  process result)

(defun pfuture-new (&rest args)
  "Create a new pfuture with process ARGS.
This will return a struct (as created by `cl-defstruct') with 2 fields:
'process' which is the process object that will be started and 'result', where
the process will be writing its output.

Note that ARGS must be a *list* of strings as demanded by `make-process'.
In other words
This is wrong: (pfuture-new \"git status\")
This is right: (pfuture-new \"git\" \"status\")"
  (let* ((future  (make-pfuture))
         (process (make-process
                   :name "Process Future"
                   :connection-type 'pipe
                   :command args
                   :filter #'(lambda (_ msg)
                               (let ((result (pfuture-result future)))
                                 (setf (pfuture-result future) (concat result msg)))))))
    (setf (pfuture-process future) process)
    future))

(cl-defun pfuture-await (future &key (timeout 1) (just-this-one t))
  "Block until FUTURE has produced output and return it.
The output will also be added to FUTURE's 'result' field.

Will accept the following optional keyword arguments:

TIMEOUT: The timeout in seconds to wait for the process. May be a float to
specify fractional number of seconds. In case of a timeout nil will be returned.

JUST-THIS-ONE: When t only read from the process of FUTURE and no other. For
details see documentation of `accept-process-output'."
  (when (pfuture-live-p future)
    (accept-process-output
     (pfuture-process future) timeout nil just-this-one))
  (pfuture-result future))

(defun pfuture-await-to-finish (future)
  "Keep reading the output of FUTURE until it is done.
Same as `pfuture-await', but will keep reading (and blocking) so long as the
process associated with FUTURE is *alive*.
If the process never quits this method will block forever. Use with caution!"
  (let ((process (pfuture-process future)))
    (while (process-live-p process)
      (accept-process-output process nil nil t)))
  (pfuture-result future))

(defun pfuture-live-p (future)
  "Return whether the process associated with FUTURE is alive."
  (process-live-p (pfuture-process future)))

(provide 'process-future)

;;; process-future.el ends here
