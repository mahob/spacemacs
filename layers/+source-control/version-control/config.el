;;; config.el --- Version Control configuration File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defvar spacemacs--smerge-ts-full-hint-toggle nil
  "Display smerge transient-state documentation.")

(spacemacs|defc version-control-margin 'auto
  "Options to apply the margin for diff-tool.

For git-gutter it only checkes the option as nil or non-nil to
activate/diactivate the margin feature.

For diff-hl it supports:
`auto'/t: Activate the margin feature for TTY frames,
          and activate the fringe feature for graphic frame.
`global': Activate the margin globally.
`nil': do not activate the margin feature."
  '(choice (const auto) (const global) boolean))

(spacemacs|defc version-control-diff-tool 'diff-hl
  "Options are `diff-hl' (the preferred choice) or `git-gutter' to show
version-control markers, `nil' to disable this feature."
  '(choice (const diff-hl) (const git-gutter) nil))

(spacemacs|defc version-control-diff-side 'right
  "Side on which to show version-control markers.
Options are `left' and `right'."
  '(choice (const left) (const right)))
