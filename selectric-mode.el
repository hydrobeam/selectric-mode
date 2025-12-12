;;; selectric-mode.el --- IBM Selectric mode for Emacs  -*- lexical-binding: t; -*-

;; Author: Ricardo Bánffy <rbanffy@gmail.com>
;; Maintainer: Ricardo Banffy <rbanffy@gmail.com>
;; URL: https://github.com/rbanffy/selectric-mode
;; Keywords: multimedia, convenience, typewriter, selectric
;; Version: 1.4.2

;; Copyright (C) 2015-2020  Ricardo Bánffy

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

;; This minor mode plays the sound of an IBM Selectric typewriter as
;; you type.

;;; Code:

(defconst selectric-files-path (file-name-directory load-file-name)
  "Directory containing the typewriter audio files.")

(defvar-local selectric-last-state nil
  "The last (buffer-size . point) seen by `selectric-mode'.")

(defvar selectric-process-mac "afplay")
(defvar selectric-process-linux "paplay")

;;; User-configurable volume
(defcustom selectric-volume 35000
  "Volume for Selectric typewriter sounds."
  :type 'integer
  :group 'selectric)

(defun selectric-play (sound-file)
  "Play SOUND-FILE using the platform-appropriate program."
  (let* ((absolute-path (expand-file-name sound-file selectric-files-path))
         (program (if (eq system-type 'darwin)
                      selectric-process-mac
                    selectric-process-linux)))
    (start-process "*Messages*" nil program
                   absolute-path
                   (format "--volume=%d" selectric-volume))))

(defun selectric-type ()
  "Make the sound of the printing element hitting the paper."
  (selectric-play "selectric-type.wav"))

(defun selectric-move ()
  "Make the carriage movement sound."
  (selectric-play "selectric-move.wav"))

(defun selectric-post-command ()
  "Added to `post-command-hook' to decide what sound to play."
  (let ((last-size (car selectric-last-state))
        (last-point (cdr selectric-last-state)))
    (setf selectric-last-state (cons (buffer-size) (point)))
    (cond ((not (eql (buffer-size) last-size)) (selectric-type))
          ((not (eql (point) last-point)) (selectric-move)))))

(defun selectric-after-save ()
  "Play the Selectric 'ping' sound after saving a file."
  (selectric-play "ping.wav"))

;;;###autoload
(define-minor-mode selectric-mode
  "Toggle Selectric mode.
When Selectric mode is enabled, your Emacs will sound like an IBM
Selectric typewriter."
  :global t
  :group 'selectric
  :hook ((post-command . selectric-post-command)
         (after-save   . selectric-after-save)))

(provide 'selectric-mode)

;;; selectric-mode.el ends here
