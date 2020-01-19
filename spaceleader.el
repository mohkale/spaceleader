;; spaceleader.el --- leader-key implementation from spacemacs -*- lexical-binding: t -*-

;; Copyright (C) 2019 Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkalsin@gmail.com>
;; Keywords: lisp, internal
;; Version: 0.0.2
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

;; Package-Requires: ((emacs "26.1") (general "0.1") (bind-map "1.1") (bind-key "2.4))

;;; Commentary:
;; This script is largely forked from spacemacs/core/core-keybindings.el and is
;; intended to be a replication of spacemacs' leader key features, without
;; requiring all of spacemacs into your configuration. My complete respect goes
;; to the developers of spacemacs who created the most ergonomic and responsive
;; binding system I've ever seen :)

;; mnemonics
;; functions ending with "for-mode" configure settings for a specific mode but
;; still affect the leader-map. If these functions also have a punctuation mark
;; "!" then these functions affect local bindings for the corresponding major-mode
;; map.

;;; Code:

(require 'seq)
(require 'general)
(require 'bind-map)
(require 'bind-key)
(require 'which-key nil t)

(put 'general-define-key 'lisp-indent-function 'defun)

;;                  _       _     _
;; __   ____ _ _ __(_) __ _| |__ | | ___  ___
;; \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;

(defgroup leader nil
  "A spacemacs like leader key package.")

(defvar leader-map (make-sparse-keymap)
  "core map for leader key bindings.")

(defcustom leader-key "SPC"
  "key from which you can access all leader bindings."
  :group 'leader)

(defcustom leader-nnorm-key "C-SPC"
  "key from which you can access all leader bindings in non-normal mode."
  :group 'leader)

(defcustom leader-major-mode-prefix "m"
  "prefix key in leader map for the active modes map."
  :group 'leader)

(defcustom leader-major-mode-key ","
  "key shortcut equivalent to `leader-key' then `major-mode-leader-prefix'.
set to nil to disable this feature."
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

(defun leader-key-with-prefix ()
  (if leader--prefix
      (concat leader-key " " leader--prefix)
    leader-key))

(defun leader-nnorm-key-with-prefix ()
  (if leader--prefix
      (concat leader-nnorm-key " " leader--prefix)
    leader-nnorm-key))

(defmacro leader-join-key-to-prefix (key)
  `(if leader--prefix
       (concat leader--prefix " " key)
     key))

;;;###autoload
(defmacro leader/with-prefix (prefix &rest body)
  "update the prefix used within the body of this macro."
  `(let ((leader--prefix (if leader--prefix
                             (concat leader--prefix " " ,prefix)
                           ,prefix)))
     ,@body))
(put 'leader/with-prefix 'lisp-indent-function 'defun)

;;;###autoload
(defmacro leader/with-major-mode-prefix (&rest body)
  "use the major mode prefix for the body of this macro."
  `(leader/with-prefix ,leader-major-mode-prefix
     ,@body))
(put 'leader/with-major-mode-prefix 'lisp-indent-function 'defun)

;;;###autoload
(defmacro leader/without-prefix (&rest body)
  "undo the affects of any and all `leader/with-prefix' calls."
  `(let (leader--prefix)
     ,@body))
(put 'leader/without-prefix 'lisp-indent-function 'defun)

(defun leader//key-in-major-mode-prefix-p (key)
  (and
   (>= (length key) 1)
   (string-prefix-p leader-major-mode-prefix key)))

(cl-defmacro leader//iterate-state-leaders
    ((leader states &optional include-major) &rest body)
  "iterates over a list of evil states & their corresponding leader keys.
STATES is the name of states variable, LEADER is the name of the leader-key
varible.

when INCLUDE-MAJOR is truthy, the `leader-major-mode-key' will also be
returned. "
  `(let* ((state-based-tuples
           (list (cons leader-norm-states  leader-key)
                 (cons leader-nnorm-states leader-nnorm-key))))
     (when ,include-major
       (push (cons leader-norm-states leader-major-mode-key)
             state-based-tuples))

     (dolist (tuple state-based-tuples)
       (let ((,states (car tuple))
             (,leader (cdr tuple)))
         ,@body))))
(put 'leader//iterate-state-leaders 'lisp-indent-function 'defun)

;; create bindings to leader map in every state
(leader//iterate-state-leaders (leader states)
  (general-define-key
    :states states
    leader '(:keymap leader-map :which-key "emacs-root")))

(when (and leader-major-mode-key leader-simulate-major-mode-key)
  (let ((major-leader-sequence (concat leader-key " " leader-major-mode-prefix)))
    (general-define-key
      ;; WARN hardcoded leader sequence... `general-simulate-key'
      ;;      can't handle variables yet.
      :states leader-major-mode-key-states
      leader-major-mode-key (general-simulate-key "SPC m"))))

;;                  __ _
;;  _ __  _ __ ___ / _(_)_  _____  ___
;; | '_ \| '__/ _ \ |_| \ \/ / _ \/ __|
;; | |_) | | |  __/  _| |>  <  __/\__ \
;; | .__/|_|  \___|_| |_/_/\_\___||___/
;; |_|
;;

(defmacro leader//name-to-prefix-cell (name long-name)
  "Convert NAME and LONG-NAME to an argument for `which-key'."
  `(if (consp ,name)
       ,name
     (cons ,name (or ,long-name ,name))))

;;;###autoload
(defun leader/declare-prefix (prefix name &optional long-name)
  "Declare a leader key prefix for PREFIX.
PREFIX is a string describing a key sequence. `which-key' will
automagically substitute the command (or prefix-map) tied to
PREFIX with name.

If PREFIX relates to a prefix map, `which-key' will display the
LONG-NAME in the minibuffer when a user enters that map."
  (setq name (leader//name-to-prefix-cell name long-name))

  (leader/with-prefix prefix
    (which-key-add-key-based-replacements (leader-key-with-prefix)       name)
    (which-key-add-key-based-replacements (leader-nnorm-key-with-prefix) name)))
(put 'leader/declare-prefix 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/declare-prefix* (prefix name &rest remaining)
  "Declares multiple leader key prefixes at once.
Accepts pairs of values and basically calls `leader/declare-prefix'
on each pair... You can specify a long name for a prefix by passing
a cons cell with the car being the NAME and the cons being the
LONG-NAME. "
  (while prefix
    (leader/with-prefix prefix
      (which-key-add-key-based-replacements (leader-key-with-prefix)       name)
      (which-key-add-key-based-replacements (leader-nnorm-key-with-prefix) name))
    (setq prefix (pop remaining) name (pop remaining))))
(put 'leader/declare-prefix* 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/declare-prefix-for-mode (mode prefix name &optional long-name)
  "declare a prefix within the `leader-map' for MODE.
the prefix is only visible while MODE is active.

WARN which-key doesn't support minor mode based substitutions yet :("
  (setq name (leader//name-to-prefix-cell name long-name))

  (leader/with-prefix prefix
    (which-key-declare-prefixes-for-mode mode (leader-key-with-prefix)       name)
    (which-key-declare-prefixes-for-mode mode (leader-nnorm-key-with-prefix) name)))
(put 'leader/declare-prefix-for-mode 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/declare-prefix-for-mode* (mode prefix name &rest remaining)
  "like `leader/declare-prefix*' but for `leader/declare-prefix-for-mode'."
  (while prefix
    (leader/declare-prefix-for-mode mode prefix name)
    (setq prefix (pop remaining) name (pop remaining))))
(put 'leader/declare-prefix-for-mode* 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/declare-prefix-for-mode! (mode prefix name &optional long-name)
  "declare a prefix within the major-mode leader map for MODE.
see also: `leader/declare-prefix-for-mode'."
  (setq name (leader//name-to-prefix-cell name long-name))

  (leader/with-major-mode-prefix
    (leader/declare-prefix-for-mode mode prefix name))

  ;; add prefix declaration for the major-mode leader shortcut key: ","
  (when leader-major-mode-key
    (let ((major-mode-leader-prefix (concat leader-major-mode-key " " prefix)))
      (which-key-declare-prefixes-for-mode mode major-mode-leader-prefix name))))
(put 'leader/declare-prefix-for-mode! 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/declare-prefix-for-mode!* (mode prefix name &rest remaining)
  "like `leader/declare-prefix*' but for `leader/declare-prefix-for-mode!'."
  (while prefix
    (leader/declare-prefix-for-mode! mode prefix name)
    (setq prefix (pop remaining) name (pop remaining))))
(put 'leader/declare-prefix-for-mode!* 'lisp-indent-function 'defun)

;; don't bother declaring a prefix when `which-key' isn't installed.
(let ((early-cancel (lambda (func &rest args)
                      (when (featurep 'which-key)
                        (apply func args))))
      (prefix-funcs '(leader/declare-prefix
                      leader/declare-prefix*
                      leader/declare-prefix-for-mode
                      leader/declare-prefix-for-mode*
                      leader/declare-prefix-for-mode!
                      leader/declare-prefix-for-mode!*)))
  (dolist (func prefix-funcs)
    (advice-add func :around early-cancel)))

;;  _                _                _     _           _ _
;; | | ___  __ _  __| | ___ _ __     | |__ (_)_ __   __| (_)_ __   __ _ ___
;; | |/ _ \/ _` |/ _` |/ _ \ '__|____| '_ \| | '_ \ / _` | | '_ \ / _` / __|
;; | |  __/ (_| | (_| |  __/ | |_____| |_) | | | | | (_| | | | | | (_| \__ \
;; |_|\___|\__,_|\__,_|\___|_|       |_.__/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;                                                                |___/

(defmacro leader//set-leader-bindings-iterator (map key def bindings &optional minor)
  "iterate for all the bindings in BINDINGS and set them in MAP.
set MINOR as the symbol tied to the minor mode for which this is being bound"
  `(let ((map-value (symbol-value ,map))
         major-key-alias-map

         (check-major-mode-key (and ,minor
                                    ;; major modes already have bindings under ","
                                    (not leader-simulate-major-mode-key)
                                    ;; if "," is simulated, this binding will be visible.
                                    leader-major-mode-key
                                    ;; if actively disabled, then no point in binding it.
                                    )))
     (while ,key
       (setq ,key (leader-join-key-to-prefix ,key))

       (bind-key ,key ,def map-value)

       ;; when key exists under the major-mode prefix, but current mode isn't a major mode.
       (when (and check-major-mode-key
                  (leader//key-in-major-mode-prefix-p ,key))
         (bind-key (substring ,key (length leader-major-mode-prefix))
                   ,def
                   (or major-key-alias-map
                       (setq major-key-alias-map
                             (leader//init-minor-mode-major-key-prefix-map ,minor ,map)))))

       (setq ,key (pop ,bindings) ,def (pop ,bindings)))))
(put 'leader//set-leader-bindings-iterator 'lisp-indent-function 'defun)

(defun leader//init-mode-prefix-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `leader-key'and `leader-nnorm-key'. "
  (or (boundp mode) (set mode nil)) ;; needed to ensure key lookups work

  (let ((prefix-map (intern (format "%s-prefix" map))))
    (or (boundp prefix-map)
        (let ((bind-major-key (and (not minor)
                                   (not leader-simulate-major-mode-key)
                                   leader-major-mode-key)))
          (leader//iterate-state-leaders (leader states bind-major-key)
            (eval `(bind-map ,map
                     :prefix-cmd ,prefix-map
                     :evil-keys (,(if (string-equal leader leader-major-mode-key)
                                     leader ;; if , don't append m, else do so.
                                    (if minor
                                        leader ;; unless your setting up a minor mode
                                      (concat leader " " leader-major-mode-prefix))))
                     :evil-states ,states
                     ,(if minor :minor-modes :major-modes) (,mode))))
          (boundp prefix-map)))))

(defun leader//init-minor-mode-major-key-prefix-map (mode map)
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
      (eval `(bind-map ,map
               :prefix-cmd ,prefix-map
               :evil-keys (,leader-major-mode-key)
               :evil-states ,leader-norm-states
               :minor-modes (,mode))))
    (symbol-value map)))

;;;###autoload
(defun leader/set-keys (key def &rest bindings)
  "set bindings in the leader key map for all modes.
supply as many key binding pairs as you wish to define."
  (leader//set-leader-bindings-iterator 'leader-map key def bindings))
(put 'leader/set-keys 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/set-keys-for-mode (mode key def &rest bindings)
  "set keys in the leader map for the given mode MODE.
see also: `leader/set-keys'"
  (let* ((map (intern (format "leader-%s-map" mode))))
    (when (leader//init-mode-prefix-map mode map t)
      (leader//set-leader-bindings-iterator map key def bindings mode))))
(put 'leader/set-keys-for-mode 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/set-keys-for-mode! (mode key def &rest bindings)
  "set keys in the leader map for the given major mode MODE.
see also: `leader/set-keys'"
  (let* ((map (intern (format "leader-%s-map" mode))))
    (when (leader//init-mode-prefix-map mode map)
      (leader//set-leader-bindings-iterator map key def bindings))))
(put 'leader/set-keys-for-mode! 'lisp-indent-function 'defun)

;; pass mode argument as list to repeat for every member of list.
(let* ((multi-mode-batch-call
        (lambda (func mode &rest args)
          "pass mode argument as list, to repeat for every member of list."
          (if (listp mode)
              (dolist (m mode)
                (apply func m args))
            (apply func mode args))))
       (mode-funcs '(leader/set-keys-for-mode
                     leader/set-keys-for-mode!
                     leader/declare-prefix-for-mode
                     leader/declare-prefix-for-mode*
                     leader/declare-prefix-for-mode!
                     leader/declare-prefix-for-mode!*)))
  (dolist (mode-func mode-funcs)
    (advice-add mode-func :around multi-mode-batch-call)))

(leader/declare-prefix leader-major-mode-prefix "major-mode" "major mode commands")

(provide 'spaceleader)

;;; spaceleader.el ends here
