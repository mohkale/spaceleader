;; spaceleader-base.el --- recommended bindings for spaceleader -*- lexical-binding: t -*-

;; Copyright (C) 2019 Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: lisp, internal
;; Version: 1.0.0
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

;; Package-Requires: ((spaceleader "1.0"))

;;; Commentary:
;; some basic bindings which can be used alongside [[file:spaceleader.el][spaceleader.el]].

;;; Code:

(require 'spaceleader)

(leader/set-keys
  "SPC" '("M-x" . execute-extended-command)
  "ESC" 'exit-recursive-edit
  "DEL" 'abort-recursive-edit

  "!" 'shell-command
  "&" 'async-shell-command
  "." 'repeat
  "U" 'universal-argument
  "u" 'smart-universal-argument

  "a"  "applications"
  "a:" 'eshell
  "aC" 'calc-dispatch
  "ac" 'calendar
  "ae" 'package-list-packages
  "af" 'list-faces-display
  "am" 'man
  "aM" 'woman
  "aP" 'proced
  "ap" 'list-processes
  "aX" 'customize

  "b"  "buffers"
  "bb" 'switch-to-buffer
  "bB" 'switch-to-buffer-other-window
  "bd" 'kill-this-buffer
  "bE" 'erase-buffer
  "bm" 'buffer-menu
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bc" 'rename-buffer
  "bC" 'rename-uniquely
  "bo" 'occur
  "b DEL" 'bury-buffer

  "c" "compile/comments"
  "cc" 'compile
  "cx" 'kill-compilation
  "cr" 'recompile
  "ck" 'comment-kill
  "c[" 'comment-box
  "ci" 'comment-indent

  "e"  "errors"
  "en" 'next-error
  "ep" 'previous-error

  "f"  "files/frames"
  "f\"" 'recover-this-file
  "f'" 'recover-file
  "fc" 'copy-file
  "fF" 'find-file-at-point
  "ff" 'find-file
  "fo" 'find-file-other-window
  "fl" 'find-file-literally
  "f|" 'find-file-at-point
  "sF" 'find-dired
  "fL" 'find-file-literally
  ;; frame
  "fx" 'delete-frame
  "fX" 'delete-other-frames
  "fm" '("maximize" . toggle-frame-maximized)
  "fn" 'make-frame-command
  "f TAB" 'other-frame

  "fe"  "emacs"
  "fev" 'emacs-version

  "fv"  "variables"
  "fvd" 'add-dir-local-variable
  "fvs" 'add-file-local-variable
  "fvl" 'add-file-local-variable-prop-line

  "fvc"  "copy"
  "fvcf" 'copy-dir-locals-to-file-locals
  "fvcd" 'copy-file-locals-to-dir-locals
  "fvcl" 'copy-dir-locals-to-file-locals-prop-line

  "fvr"  "remove"
  "fvrv" 'kill-local-variable
  "fvrd" 'delete-dir-local-variable
  "fvrf" 'delete-file-local-variable
  "fvrl" 'delete-file-local-variable-prop-line

  "j"  "jump"
  "jc" 'goto-char
  "jn" 'next-error
  "jp" 'previous-error
  "j TAB" 'move-to-column

  "h"  "help"
  "hn" 'view-emacs-news
  "h/" 'apropos-command
  "hg" 'describe-gnu-project
  "hl" 'view-lossage                                                            ; show last few entered commands
  "hw" 'where-is                                                                ; show where a command is bound
  "hp" 'view-emacs-problems
  "ht" 'view-emacs-todo
  "h RET" 'view-order-manuals

  "hd"  "help-describe"
  "hdo" 'describe-symbol
  "hdi" 'describe-input-method
  "hdL" 'describe-language-environment
  "hdb" 'describe-bindings
  "hdc" 'describe-char
  "hdu" 'describe-coding-system
  "hdF" 'describe-face
  "hdk" 'describe-key
  "hdK" 'describe-key-briefly
  "hdp" 'describe-package
  "hdP" 'finder-by-keyword
  "hds" 'describe-syntax
  "hdt" 'describe-theme
  "hdv" 'describe-variable

  "M" "modes"
  "Mz" 'zone
  "Ml" 'emacs-lisp-mode
  "ML" 'lisp-interaction-mode
  "Mc" 'c++-mode
  "MC" 'c-mode
  "Mf" 'flyspell-mode
  "MF" 'follow-mode
  "Mt" 'fundamental-mode
  "Mp" 'python-mode
  "Mr" 'ruby-mode
  "Ms" 'shell-script-mode
  "Mw" 'whitespace-mode
  "Mo" 'org-mode
  "Mx" 'hexl-mode
  "M?" 'toggle-rot13-mode
  "Mv" 'visual-line-mode
  "MD" 'decipher

  "n"  "narrrow/numbers"
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nw" 'widen

  "x"  "text"
  "xa" 'describe-text-properties

  "t"  "toggles"
  "tn" '("toggle-line-numbers" . display-line-numbers-mode)
  "tl" 'toggle-truncate-lines
  "tw" 'toggle-word-wrap
  "t|" 'toggle-scroll-bar
  "t-" 'toggle-horizontal-scroll-bar
  "td" 'toggle-debug-on-error
  "tq" 'toggle-debug-on-quit
  "tf" 'toggle-frame-fullscreen
  "tg" 'toggle-tool-bar-mode-from-frame
  "tG" 'toggle-menu-bar-mode-from-frame
  "tu" 'toggle-uniquify-buffer-names
  "tb" 'toggle-indicate-empty-lines
  "ta" 'toggle-text-mode-auto-fill
  "tR" '("toggle-read-only" . read-only-mode)
  "tc" 'toggle-case-fold-search
  "ti" 'toggle-input-method
  "ts" 'toggle-save-place-globally

  "s" "search/symbol"
  "sg" 'rgrep
  "sF" 'find-dired
  "sf" 'find-grep-dired

  "*"  "calculator"
  "*." 'calc-dispatch
  "**" 'calc
  "*y" 'calc-copy-to-buffer
  )

(with-eval-after-load 'undo-tree
  (leader/set-keys
    "au" '("undo-tree" . undo-tree-visualize)))

(with-eval-after-load 'winum
  (leader/set-keys
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
    "0" 'winum-select-window-by-number))

(with-eval-after-load 'emojify
  (leader/set-keys
    "Me" 'emojify-mode
    "xe" 'emojify-insert-emoji
    "he"  'emojify-apropos-emoji
    "hde" 'emojify-describe-emoji))

(provide 'spaceleader-base)

;;; spaceleader-base.el ends here
