;; spaceleader.el --- leader-key implementation from spacemacs -*- lexical-binding: t -*-

;; Copyright (C) 2019 Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: lisp, internal
;; Version: 0.0.3
;; URL: https://github.com/mohkale/spaceleader

;;; License: GPLv3
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

;; Package-Requires: ((emacs "26.1") (general "0.1") (bind-map "1.1") (bind-key "2.4) (dash "2.17"))

;;; Commentary:
;; This script is largely forked from [[https://github.com/syl20bnr/spacemacs/blob/master/core/core-keybindings.el][spacemacs/core/core-keybindings.el]] and is
;; intended to be a replication of spacemacs' leader key features, without
;; requiring all of spacemacs into your configuration.
;;
;; My complete respect goes to the developers of spacemacs who created the most
;; ergonomic and responsive binding system I've ever seen :)

;;; Code:

(require 'seq)
(require 'dash)
;; TODO remove dependence on general.el
(require 'general)
(require 'bind-map)
(require 'bind-key)

;;                  _       _     _
;; __   ____ _ _ __(_) __ _| |__ | | ___  ___
;; \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;

(defgroup leader nil
  "A spacemacs like leader key package.")

;;;###autoload
(defvar leader-map (make-sparse-keymap)
  "map for leader key bindings.")

(defcustom leader-key "SPC"
  "key to access leader keys."
  :group 'leader)

(defcustom leader-nnorm-key "C-SPC"
  "alternative to `leader-key' for non-normal `evil' modes"
  :group 'leader)

(defcustom leader-major-mode-prefix "m"
  "prefix in `leader-map' for major-mode bindings."
  :group 'leader)

(defcustom leader-major-mode-key ","
  "shortcut for `leader-key'+`major-mode-leader-prefix'."
  :group 'leader)

(defvar leader--prefix nil)

(defcustom leader-simulate-major-mode-key nil
  (format "simulate '%s %s' when '%s' pressed in some states
instead of binding a map to '%s'. By default this is set to
behave exactly like spacemacs does.

This'll probably be quicker and less memory intensive, but you
won't recieve any key assistance through `leader-major-mode-key'
because a simulated key is actually a command, and not a prefix.

Notable changes when setting this option are:
* if your major-mode has no leader keys, the default binding for
  '%s' will be run, instead of an empty prefix message being issued.
* if you press '%s' `which-key' will show your input as '%s %s'
  instead of as '%s'.
* `substitute-command-keys' won't show any bindings accessed through
  '%s'. This'll be most noticeable in `org-capture' or `org-edit-src-code'.
"
          leader-key leader-major-mode-prefix leader-major-mode-key
          leader-major-mode-key leader-major-mode-key
          leader-major-mode-key leader-key leader-major-mode-prefix
          leader-major-mode-key leader-major-mode-key)
  :group 'leader)

(defvar leader-evil-states '(insert emacs normal visual motion
                             operator outer inner replace iedit
                             iedit-insert)
  "all the known emacs evil states")

(defvar leader-nnorm-states '(insert emacs hybrid iedit-insert replace)
  "all the known evil states which aren't inherited from normal mode")

(defvar leader-norm-states (seq-filter (lambda (x) (not (member x leader-nnorm-states))) leader-evil-states)
  "all the known evil states which do inherit from normal mode")

(defvar leader-major-mode-key-states leader-norm-states
  (format "evil states under which %s can be used to jump to the major modes leader map."
          leader-major-mode-key))

;;        _   _ _ _ _   _
;;  _   _| |_(_) (_) |_(_) ___  ___
;; | | | | __| | | | __| |/ _ \/ __|
;; | |_| | |_| | | | |_| |  __/\__ \
;;  \__,_|\__|_|_|_|\__|_|\___||___/

(defun leader--states-with-leaders (&optional include-major)
  (let ((entries (list (cons leader-norm-states  leader-key)
                       (cons leader-nnorm-states leader-nnorm-key))))
    (when include-major
      (push (cons leader-norm-states leader-major-mode-key) entries))

    entries))

;; activate leader-map in every known evil-state for the appropriate
;; leader keys.
(cl-loop for (states . leader) in (leader--states-with-leaders)
         do (general-define-key
              :states states
              leader '(:keymap leader-map :which-key "emacs-root")))

;; enable the major-mode-prefix.
(when (and leader-major-mode-key
           leader-simulate-major-mode-key)
  (general-define-key
   :states leader-major-mode-key-states
   :keymaps 'override
   leader-major-mode-key
   (eval
    `(general-simulate-key
       ,(concat leader-key " " leader-major-mode-prefix)))))

;;;###autoload
(defmacro leader/with-prefix (prefix &rest body)
  `(let ((leader--prefix (concat leader--prefix " " ,prefix)))
     ,@body))
(put 'leader/with-prefix 'lisp-indent-function 'defun)

;;;###autoload
(defmacro leader/with-major-mode-prefix (&rest body)
  `(leader/with-prefix ,leader-major-mode-prefix
     ,@body))
(put 'leader/with-major-mode-prefix 'lisp-indent-function 'defun)

(defun leader--init-major-mode-prefix-map (mode map &optional minor)
  "ensure a leader-key map for MODE exists and is bound to `leader-key',
`leader-nnorm-key' and `leader-major-mode-key' (when the MINOR is not true)
while that mode is active."
  (or (boundp mode) (set mode nil)) ;; needed to ensure key lookups work

  (let ((prefix-map (intern (format "%s-prefix" map))))
    (or (boundp prefix-map)
        (let ((bind-major-key-p (and (not minor)
                                     (not leader-simulate-major-mode-key)
                                     leader-major-mode-key)))
          (cl-loop for (states . leader) in (leader--states-with-leaders bind-major-key-p)
                   do (eval
                       `(bind-map ,map
                          :prefix-cmd ,prefix-map
                          :evil-keys (,(if (string-equal leader leader-major-mode-key)
                                           leader ;; if , don't append m, else do so.
                                         (if minor
                                             leader ;; unless your setting up a minor mode
                                           (concat leader " " leader-major-mode-prefix))))
                          :evil-states ,states
                          ,(if minor :minor-modes :major-modes) (,mode)))))
        (boundp prefix-map))))

(defun leader--init-minor-mode-map-in-major-mode-key (mode map)
  (format "when not simulating `leader-major-mode-key' you have to bind
a unique map for minor-modes to enable bindings that're active
both through '%s %s' and '%s'

NOTE maybe I should just bind directly to the minor-modes map instead
of constructing a whole new map for it :? "
          leader-key leader-major-mode-prefix leader-major-mode-key)
  (let* ((map (intern (concat (string-remove-suffix "-map"
                                                    (symbol-name map))
                              "--major-prefix-alias-map")))
         (prefix-map (intern (concat (symbol-name map) "-prefix"))))
    (unless (boundp prefix-map)
      (eval
       `(bind-map ,map
          :prefix-cmd ,prefix-map
          :evil-keys (,leader-major-mode-key)
          :evil-states ,leader-major-mode-key-states
          :minor-modes (,mode))))
    (symbol-value map)))

(defmacro leader--bindings-to-tuples (bindings)
  "convert an argument list of the type (KEY1 ARG1 KEY2 ARG2)
into a list of the form ((KEY1 . ARG1) (KEY2 . ARG2))."
  `(cl-loop with last = nil
            for (i . arg) in (-zip (number-sequence 0 (length ,bindings)) ,bindings)
            unless (zerop (% i 2))
              collect (cons last arg)
            else
              do (setq last arg)
            end))

(defmacro leader--key-in-major-mode-prefix-p (key)
  `(and
    (>= (length ,key) 1)
    (string-prefix-p leader-major-mode-prefix ,key)))

(defun leader--set-whick-key-prefix (key def mode)
  (let ((desc def) long-desc)
    (when (consp def)
      (setq desc      (car def)
            long-desc (cdr def)))

    ;; WARN which key doesn't support minor mode prefixes as of yet.
    (unless (and mode (eq (car mode) 'minor))
      (let* ((full-key (if mode (concat leader-major-mode-prefix " " key) key))
             (prefix-key       (concat leader-key       " " full-key))
             (prefix-nnorm-key (concat leader-nnorm-key " " full-key)))
        (with-eval-after-load 'which-key
          (if mode
              (progn
                (which-key-declare-prefixes-for-mode (cdr mode) prefix-key       def)
                (which-key-declare-prefixes-for-mode (cdr mode) prefix-nnorm-key def)

                (when leader-major-mode-key
                  (which-key-declare-prefixes-for-mode (cdr mode)
                    (concat leader-major-mode-key " " key) def)))

            (which-key-add-key-based-replacements prefix-key       def)
            (which-key-add-key-based-replacements prefix-nnorm-key def)))))))

(defun leader--set-bindings (map bindings &optional mode-desc)
  (let ((map-value (symbol-value map))
        major-key-alias-map
        (check-major-mode-key (and (eq (car mode-desc) 'minor)
                                   ;; major modes already have bindings under ","
                                   (not leader-simulate-major-mode-key)
                                   ;; if "," is simulated, this binding will be visible.
                                   leader-major-mode-key
                                   ;; if actively disabled, then no point in binding it.
                                   )))
    (cl-loop for (key . def) in (leader--bindings-to-tuples bindings)
             when leader--prefix
               do (setq key (string-trim-left (concat leader--prefix " " key)))
             end

             when (or (stringp def)
                      (and (consp def)
                           (stringp (car def))
                           (stringp (cdr def))))
               do (leader--set-whick-key-prefix key def mode-desc)
             else
               do (bind-key key def map-value)

               and when (and check-major-mode-key
                             (leader--key-in-major-mode-prefix-p key))
                 do (or major-key-alias-map
                        (setq major-key-alias-map
                              (leader--init-minor-mode-map-in-major-mode-key
                               (and (eq (car mode-desc) 'minor)
                                    (cdr mode-desc))
                               map)))
                 and do (let ((stripped-key (substring key (length leader-major-mode-prefix))))
                          (bind-key stripped-key def major-key-alias-map)))))

;;  _                _                _     _           _ _
;; | | ___  __ _  __| | ___ _ __     | |__ (_)_ __   __| (_)_ __   __ _ ___
;; | |/ _ \/ _` |/ _` |/ _ \ '__|____| '_ \| | '_ \ / _` | | '_ \ / _` / __|
;; | |  __/ (_| | (_| |  __/ | |_____| |_) | | | | | (_| | | | | | (_| \__ \
;; |_|\___|\__,_|\__,_|\___|_|       |_.__/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;                                                                |___/

;;;###autoload
(defun leader/set-keys (&rest bindings)
  (leader--set-bindings 'leader-map bindings))
(put 'leader/set-keys 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/set-keys-for-mode (mode &rest bindings)
  (let* ((map (intern (format "leader-%s-map" mode))))
    (when (leader--init-major-mode-prefix-map mode map t)
      (leader--set-bindings map bindings `(minor . ,mode)))))
(put 'leader/set-keys-for-mode 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/set-keys-for-major-mode (mode &rest bindings)
  (let* ((map (intern (format "leader-%s-map" mode))))
    (when (leader--init-major-mode-prefix-map mode map)
      (leader--set-bindings map bindings `(major . ,mode)))))
(put 'leader/set-keys-for-major-mode 'lisp-indent-function 'defun)

;;;###autoload
(defalias 'leader/set-keys-for-mode! #'leader/set-keys-for-major-mode)

;; pass mode argument as list to repeat for every member of list.
(let* ((multi-mode-batch-call
        (lambda (func mode &rest args)
          "pass mode argument as list, to repeat for every member of list."
          (if (listp mode)
              (dolist (m mode)
                (apply func m args))
            (apply func mode args))))
       (mode-funcs '(leader/set-keys-for-mode
                     leader/set-keys-for-major-mode)))
  (dolist (mode-func mode-funcs)
    (advice-add mode-func :around multi-mode-batch-call)))

(leader/set-keys
  leader-major-mode-prefix '("major-mode" . "major mode commands"))

(provide 'spaceleader)

;;; spaceleader.el ends here
