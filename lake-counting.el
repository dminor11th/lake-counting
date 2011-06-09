;;; lake-counting.el --- animated lake counting problem solver in Emacs

;; Copyright (C) 2011 dminor11th <dminor11th@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To install, copy this file somewhere in your load-path and add this line to
;; your .emacs:
;;
;;    (require 'lake-counting)
;;
;; To launch it, run or bind the following commands:
;;
;;    M-x lake-counting
;;
;; To change the grid size, use prefix argument like this:
;;
;;    C-u 16 M-x lake-counting

;;; Code:

;; Things we need.

(eval-when-compile
  (require 'cl))

;; If customize isn't available just use defvar instead.
(eval-and-compile
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Customize options.

(defgroup lake-counting nil
  "Solve lake counting problem moving cursor."
  :group  'games
  :prefix "lake-counting-")

(defcustom lake-counting-grid-size 8
  "*Size of the area on which lakes are."
  :type 'number
  :group 'lake-counting)

(defcustom lake-counting-move-delay .05
  "*Delay in seconds when moving the cursor. If nil, move it as fast as possible."
  :type 'number
  :group 'lake-counting)

(defcustom lake-counting-sit-for 3
  "*Time, in seconds, it pauses showing the initial state of the lakes."
  :type 'number
  :group 'lake-counting)

;; Non-customize variables.

(defvar lake-counting-char-ground ?.
  "Character type data that denotes the ground.")

(defvar lake-counting-char-lake ?W
  "Character type data that denotes the lake.")

;; functions

(defun lake-counting (n)
  "Solve a lake counting problem of size n."
  (interactive
   (list (if (null current-prefix-arg)
             lake-counting-grid-size
           (prefix-numeric-value current-prefix-arg))))
  (if (< n 0)
      (error "Negative size"))
  (lake-counting-init n)
  (sit-for lake-counting-sit-for)
  (lake-counting-solve (point-min) (point-max)))

(defun lake-counting-init (n)
  "switch to a buffer and draw lakes in n-sized grid."
  (switch-to-buffer "*lake-counting*")
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq truncate-lines t)
  (loop for i from 1 to n do
        (loop for j from 1 to n do
              (insert (if (zerop (random 3))
                          lake-counting-char-lake
                        lake-counting-char-ground)))
        (if (< i n) ( insert "\n")))
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun lake-counting-solve (b e)
  (unwind-protect
      (let ((cnt 0)
            (first-line (line-number-at-pos b)) ;リージョンの先頭行番号を記録しておく
            (last-line (line-number-at-pos e))) ;リージョンの最終行番号を 〃
                (setq buffer-read-only nil)
        (loop for p from b to e do
              (goto-char p)
              (sit-for lake-counting-move-delay) ;カーソルの動きを観察するため一瞬停止
              (when (eq lake-counting-char-lake (char-after p))
                (dfs)
                (incf cnt) ))
        (message "Done: %d" cnt))           ;結果出力
    (setq buffer-read-only t)))

(defun dfs ()
  "補助関数。現在のポイントを置換する。8近傍に池があれば再帰的に適用する。"
  (let* ((cline (line-number-at-pos))
         (ccol (current-column))
         (poslist `((,(1- cline) ,(1- ccol)) ;8近傍を表す(行,列)のリスト
                    (,(1- cline) ,ccol)
                    (,(1- cline) ,(1+ ccol))
                    (,cline ,(1- ccol))
                    (,cline ,(1+ ccol))
                    (,(1+ cline) ,(1- ccol))
                    (,(1+ cline) ,ccol)
                    (,(1+ cline) ,(1+ ccol)) )))
    (delete-char 1) ; この2行で 池 を 地面 に置換
    (insert lake-counting-char-ground)  ; 〃
    (setf poslist
          (remove-if  ;リージョンの外に出る要素をリストから消す
           (lambda (pos)
             (or (< (first pos) first-line);上にはみ出す場合
                 (> (first pos) last-line) ;下にはみ出す場合
                 (< (second pos) 0)))      ;左にはみ出す場合
           poslist))
    (mapcar (lambda (pos) ;8近傍の各要素に対してラムダ式を適用
              (goto-line(first pos))
              (move-to-column (second pos)) ;近傍にポイントを移動
              (sit-for lake-counting-move-delay)  ;カーソルの動きを観察するため一瞬停止
              (if (eq lake-counting-char-lake (char-after))  ;池なら再帰的に呼び出す
                  (dfs)))
            poslist)))

(provide 'lake-counting)
