;;; anzu2.el --- Show number of matches in mode-line while searching -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-anzu2
;; Version: 0.62
;; Package-Requires: ((emacs "26.3"))

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

;; `anzu2.el' is an Emacs port of `anzu.vim'.
;;
;; `anzu2.el' provides a minor mode which displays 'current match/total
;; matches' in the mode-line in various search modes.  This makes it
;; easy to understand how many matches there are in the current buffer
;; for your search query.

;; To use this package, add following code to your init.el or .emacs
;;
;;   (global-anzu2-mode +1)
;;

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'subr-x)

(defgroup anzu2 nil
  "Show searched position in mode-line"
  :group 'isearch)

(defcustom anzu2-mode-lighter " Anzu2"
  "Lighter of anzu2-mode"
  :type 'string)

(defcustom anzu2-minimum-input-length 1
  "Minimum input length to enable anzu"
  :type 'integer)

(defcustom anzu2-search-threshold 1000
  "Limit of search number"
  :type 'integer)

(defcustom anzu2-replace-threshold 50
  "Limit of replacement overlays."
  :type 'integer)

(defcustom anzu2-regexp-search-commands '(isearch-forward-regexp
                                          isearch-backward-regexp)
  "Search function which use regexp."
  :type '(repeat function))

(defcustom anzu2-input-idle-delay 0.05
  "Idle second for updating modeline at replace commands"
  :type 'number)

(defcustom anzu2-replace-at-cursor-thing 'defun
  "Replace thing. This parameter is same as `thing-at-point'"
  :type 'symbol)

(defface anzu2-mode-line
  '((t (:foreground "magenta" :weight bold)))
  "face of anzu modeline")

(defface anzu2-mode-line-no-match
  '((t (:inherit anzu2-mode-line)))
  "face of anzu modeline in no match case")

(defface anzu2-replace-highlight
  '((t :inherit query-replace))
  "highlight of replaced string")

(defface anzu2-match-1
  '((((class color) (background light))
     :background "aquamarine" :foreground "black")
    (((class color) (background dark))
     :background "limegreen" :foreground "black")
    (t :inverse-video t))
  "First group of match.")

(defface anzu2-match-2
  '((((class color) (background light))
     :background "springgreen" :foreground "black")
    (((class color) (background dark))
     :background "yellow" :foreground "black")
    (t :inverse-video t))
  "Second group of match.")

(defface anzu2-match-3
  '((((class color) (background light))
     :background "yellow" :foreground "black")
    (((class color) (background dark))
     :background "aquamarine" :foreground "black")
    (t :inverse-video t))
  "Third group of match.")

(defface anzu2-replace-to
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "yellow"))
  "highlight of replace string")

(defvar anzu2--total-matched 0)
(defvar anzu2--current-position 0)
(defvar anzu2--overflow-p nil)
(defvar anzu2--last-isearch-string nil)
(defvar anzu2--cached-positions nil)
(defvar anzu2--last-command nil)
(defvar anzu2--state nil)
(defvar anzu2--cached-count 0)
(defvar anzu2--last-replace-input "")
(defvar anzu2--last-search-state nil)
(defvar anzu2--last-replaced-count nil)
(defvar anzu2--outside-point nil)
(defvar anzu2--history nil)
(defvar anzu2--query-defaults nil)

(defun anzu2--validate-regexp (regexp)
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defsubst anzu2--construct-position-info (count overflow positions)
  (list :count count :overflow overflow :positions positions))

(defun anzu2--case-fold-search (input)
  (if isearch-mode
      isearch-case-fold-search
    (when case-fold-search
      (let ((case-fold-search nil))
        (not (string-match-p "[A-Z]" input))))))

(defsubst anzu2--word-search-p ()
  (and (not (memq anzu2--last-command anzu2-regexp-search-commands))
       (not isearch-regexp)))

(defsubst anzu2--isearch-regexp-function ()
  (or (bound-and-true-p isearch-regexp-function)
      (bound-and-true-p isearch-word)))

(defun anzu2--transform-input (str)
  (cond ((eq (anzu2--isearch-regexp-function) 'isearch-symbol-regexp)
         (setq str (isearch-symbol-regexp str)))
        ((anzu2--word-search-p)
         (setq str (regexp-quote str)))
        (t str)))

(defun anzu2--search-all-position (str)
  (unless anzu2--last-command
    (setq anzu2--last-command last-command))
  (let ((input (anzu2--transform-input str)))
    (if (not (anzu2--validate-regexp input))
        anzu2--cached-positions
      (save-excursion
        (goto-char (point-min))
        (let ((positions '())
              (count 0)
              (overflow nil)
              (finish nil)
              (case-fold-search (anzu2--case-fold-search input)))
          (while (and (not finish) (re-search-forward input nil t))
            (push (cons (match-beginning 0) (match-end 0)) positions)
            (cl-incf count)
            (when (= (match-beginning 0) (match-end 0)) ;; Case of anchor such as "^"
              (if (eobp)
                  (setq finish t)
                (forward-char 1)))
            (when (>= count anzu2-search-threshold)
              (setq overflow t finish t)))
          (let ((result (anzu2--construct-position-info count overflow (reverse positions))))
            (setq anzu2--cached-positions (copy-sequence result))
            result))))))

(defun anzu2--where-is-here (positions here)
  ;; don't use loop for emacs 27 bug
  (let ((poss positions)
        (index 1)
        (ret 0))
    (while poss
      (let ((pos (car poss)))
        (if (and (>= here (car pos)) (<= here (cdr pos)))
            (setq ret index poss nil)
          (setq poss (cdr poss) index (1+ index)))))
    ret))

(defun anzu2--use-result-cache-p (input)
  (and (eq (anzu2--isearch-regexp-function) (car anzu2--last-search-state))
       (eq isearch-regexp (cdr anzu2--last-search-state))
       (string= input anzu2--last-isearch-string)))

(defun anzu2--update (query)
  (when (>= (length query) anzu2-minimum-input-length)
    (let ((result (if (anzu2--use-result-cache-p query)
                      anzu2--cached-positions
                    (anzu2--search-all-position query))))
      (let ((curpos (anzu2--where-is-here (plist-get result :positions) (point))))
        (setq anzu2--total-matched (plist-get result :count)
              anzu2--overflow-p (plist-get result :overflow)
              anzu2--current-position curpos
              anzu2--last-search-state (cons (anzu2--isearch-regexp-function) isearch-regexp)
              anzu2--last-isearch-string query)
        (force-mode-line-update)))))

(defun anzu2--update-post-hook ()
  (anzu2--update isearch-string))

(defconst anzu2--mode-line-format '(:eval (anzu2--update-mode-line)))

(defsubst anzu2--mode-line-not-set-p ()
  (and (consp mode-line-format)
       (member anzu2--mode-line-format mode-line-format)))

(defun anzu2--cons-mode-line-search ()
  (anzu2--cons-mode-line 'search))

(defun anzu2--cons-mode-line (state)
  (setq anzu2--state state)
  (when (not (anzu2--mode-line-not-set-p))
    (setq mode-line-format (cons anzu2--mode-line-format mode-line-format))))

(defsubst anzu2--reset-status ()
  (setq anzu2--total-matched 0
        anzu2--current-position 0
        anzu2--state nil
        anzu2--last-command nil
        anzu2--last-isearch-string nil
        anzu2--overflow-p nil))

(defun anzu2--reset-mode-line ()
  (anzu2--reset-status)
  (when (anzu2--mode-line-not-set-p)
    (setq mode-line-format (delete anzu2--mode-line-format mode-line-format))))

(defun anzu2--format-here-position (here total)
  (if (and anzu2--overflow-p (zerop here))
      (format "%d+" total)
    here))

(defun anzu2--update-mode-line ()
  (when anzu2--state
    (let ((status (cl-case anzu2--state
                    (search (format "(%s/%d%s)"
                                    (anzu2--format-here-position
                                     anzu2--current-position anzu2--total-matched)
                                    anzu2--total-matched (if anzu2--overflow-p "+" "")))
                    (replace-query (format "(%d replace)" anzu2--total-matched))
                    (replace (format "(%d/%d)" anzu2--current-position anzu2--total-matched))))
          (face (if (and (zerop anzu2--total-matched) (not (string-empty-p isearch-string)))
                    'anzu2-mode-line-no-match
                  'anzu2-mode-line)))
      (propertize status 'face face))))

;;;###autoload
(define-minor-mode anzu2-mode
  "minor-mode which display search information in mode-line."
  :init-value nil
  :global     nil
  :lighter    anzu2-mode-lighter
  (if anzu2-mode
      (progn
        (setq-local anzu2--state nil)
        (add-hook 'isearch-update-post-hook #'anzu2--update-post-hook nil t)
        (add-hook 'isearch-mode-hook #'anzu2--cons-mode-line-search nil t)
        (add-hook 'isearch-mode-end-hook #'anzu2--reset-mode-line nil t))
    (remove-hook 'isearch-update-post-hook #'anzu2--update-post-hook t)
    (remove-hook 'isearch-mode-hook #'anzu2--cons-mode-line-search t)
    (remove-hook 'isearch-mode-end-hook #'anzu2--reset-mode-line t)
    (anzu2--reset-mode-line)))

(defun anzu2--turn-on ()
  (unless (minibufferp)
    (anzu2-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-anzu2-mode anzu2-mode anzu2--turn-on)

(defsubst anzu2--query-prompt-base (use-region use-regexp)
  (concat "Query replace"
          (if current-prefix-arg " word" "")
          (if use-regexp " regexp" "")
          (if use-region " in region" ""))  )

(defun anzu2--query-prompt (use-region use-regexp at-cursor isearch-p)
  (let ((prompt (anzu2--query-prompt-base use-region use-regexp)))
    (if (and anzu2--query-defaults (not at-cursor) (not isearch-p))
        (format "%s (default %s -> %s) "
                prompt
                (query-replace-descr (caar anzu2--query-defaults))
                (query-replace-descr (cdar anzu2--query-defaults)))
      prompt)))

(defvar anzu2--replaced-markers nil)
(defsubst anzu2--set-marker (beg buf)
  (let ((m (make-marker)))
    (set-marker m beg buf)
    (push m anzu2--replaced-markers)))

(defun anzu2--make-overlay (begin end face prio)
  (let ((ov (make-overlay begin end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority prio)
    (overlay-put ov 'anzu2-overlay t)
    ov))

(defun anzu2--add-match-group-overlay (match-data groups)
  (when (>= groups 3)
    (anzu2--make-overlay (cl-fifth match-data) (cl-sixth match-data)
                         'anzu2-match-3 1001))
  (when (>= groups 2)
    (anzu2--make-overlay (cl-third match-data) (cl-fourth match-data)
                         'anzu2-match-2 1001))
  (anzu2--make-overlay (cl-first match-data) (cl-second match-data)
                       'anzu2-match-1 1001))

(defun anzu2--add-overlay (beg end)
  (let* ((match-data (match-data))
         (groups (/ (- (length match-data) 2) 2)))
    (when (>= groups 1)
      (anzu2--add-match-group-overlay (cddr match-data) groups))
    (let ((ov (anzu2--make-overlay beg end 'anzu2-replace-highlight 1000)))
      (overlay-put ov 'from-string (buffer-substring-no-properties beg end))
      (overlay-put ov 'anzu2-replace t))))

(defsubst anzu2--cleanup-markers ()
  (mapc (lambda (m) (set-marker m nil)) anzu2--replaced-markers)
  (setq anzu2--replaced-markers nil))

;; Return highlighted count
(defun anzu2--count-and-highlight-matched (buf str replace-beg replace-end
                                               use-regexp overlay-limit case-sensitive)
  (anzu2--cleanup-markers)
  (when (not use-regexp)
    (setq str (regexp-quote str)))
  (if (not (anzu2--validate-regexp str))
      anzu2--cached-count
    (with-current-buffer buf
      (save-excursion
        (let* ((backward (> replace-beg replace-end))
               (overlay-beg (if backward (max replace-end overlay-limit) replace-beg))
               (overlay-end (if backward replace-beg (min replace-end overlay-limit))))
          (goto-char replace-beg)
          (let ((count 0)
                (overlayed 0)
                (finish nil)
                (cmp-func (if backward #'< #'>))
                (search-func (if backward #'re-search-backward #'re-search-forward))
                (step (if backward -1 1))
                (case-fold-search (if case-sensitive
                                      nil
                                    (anzu2--case-fold-search str))))
            (while (and (not finish) (funcall search-func str replace-end t))
              (cl-incf count)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (when (= beg end)
                  (if (eobp)
                      (setq finish t)
                    (forward-char step)))
                (when (and replace-end (funcall cmp-func (point) replace-end))
                  (setq finish t))
                (when (and (>= beg overlay-beg) (<= end overlay-end) (not finish))
                  (cl-incf overlayed)
                  (anzu2--add-overlay beg end))))
            (setq anzu2--cached-count count)
            overlayed))))))

(defun anzu2--search-outside-visible (buf input beg end use-regexp)
  (let* ((regexp (if use-regexp input (regexp-quote input)))
         (backward (> beg end))
         (searchfn (if backward #'re-search-backward #'re-search-forward)))
    (when (anzu2--validate-regexp regexp)
      (with-selected-window (get-buffer-window buf)
        (goto-char beg)
        (when (funcall searchfn regexp end t)
          (setq anzu2--outside-point (match-beginning 0))
          (let ((overlay-limit (anzu2--overlay-limit backward)))
            (anzu2--count-and-highlight-matched buf input beg end use-regexp
                                                overlay-limit nil)))))))

(defconst anzu2--from-to-separator
  (propertize
   (or (ignore-errors
         (if (char-displayable-p ?\u2192) " \u2192 " " -> "))
       " -> ")
   'face 'minibuffer-prompt))

(defsubst anzu2--separator ()
  (propertize "\0" 'display anzu2--from-to-separator 'separator t))

(defun anzu2--check-minibuffer-input (buf beg end use-regexp overlay-limit)
  (let* ((content (minibuffer-contents))
         (to (when (and (string-match (anzu2--separator) content)
                        (get-text-property (match-beginning 0) 'separator content))
               (substring-no-properties content (match-end 0))))
         (from (or (and to (substring-no-properties content 0 (match-beginning 0)))
                   content))
         (empty-p (string-empty-p from))
         (overlayed (if empty-p
                        (setq anzu2--cached-count 0)
                      (anzu2--count-and-highlight-matched buf from beg end use-regexp
                                                          overlay-limit nil))))
    (when anzu2--outside-point
      (setq anzu2--outside-point nil)
      (with-selected-window (get-buffer-window buf)
        (goto-char beg)))
    (when (and (not empty-p) (zerop overlayed))
      (anzu2--search-outside-visible buf from beg end use-regexp))
    (when to
      (setq anzu2--last-replace-input "")
      (anzu2--append-replaced-string to buf beg end use-regexp overlay-limit from))
    (setq anzu2--total-matched anzu2--cached-count)
    (force-mode-line-update)))

(defun anzu2--clear-overlays (buf beg end)
  (with-current-buffer buf
    (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
      (when (overlay-get ov 'anzu2-overlay)
        (delete-overlay ov)))))

(defun anzu2--transform-from-to-history ()
  (let ((separator (anzu2--separator)))
    (append (mapcar (lambda (from-to)
                      (concat (query-replace-descr (car from-to))
                              separator
                              (query-replace-descr (cdr from-to))))
                    anzu2--query-defaults)
            (symbol-value query-replace-from-history-variable))))

(defun anzu2--read-from-string (prompt beg end use-regexp overlay-limit)
  (let ((curbuf (current-buffer))
        (blink-matching-paren nil)
        (anzu2--history (anzu2--transform-from-to-history))
        timer is-input)
    (unwind-protect
        (minibuffer-with-setup-hook
            #'(lambda ()
                (setq timer (run-with-idle-timer
                             (max anzu2-input-idle-delay 0.01)
                             'repeat
                             (lambda ()
                               (anzu2--clear-overlays curbuf nil nil)
                               (with-selected-window (or (active-minibuffer-window)
                                                         (minibuffer-window))
                                 (anzu2--check-minibuffer-input
                                  curbuf beg end use-regexp overlay-limit))))))
          (prog1 (read-from-minibuffer (format "%s: " prompt)
                                       nil nil nil 'anzu2--history nil t)
            (setq is-input t)))
      (when timer
        (cancel-timer timer)
        (setq timer nil)
        (unless is-input
          (goto-char beg))))))

(defun anzu2--query-validate-from-regexp (from)
  (when (string-match "\\(?:\\`\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
    (let ((match (match-string 1 from)))
      (cond
       ((string= match "\\n")
        (message "`\\n' here doesn't match a newline; type C-q C-j instead!!"))
       ((string= match "\\t")
        (message "\\t' here doesn't match a tab; to do that, just type TAB!!")))
      (sit-for 2))))

(defun anzu2--query-from-string (prompt beg end use-regexp overlay-limit)
  (let* ((from (anzu2--read-from-string prompt beg end use-regexp overlay-limit))
         (is-empty (string-empty-p from)))
    (when (and (not is-empty) (not anzu2--query-defaults))
      (setq anzu2--last-replaced-count anzu2--total-matched))
    (if (and is-empty anzu2--query-defaults)
        (cons (query-replace-descr (caar anzu2--query-defaults))
              (query-replace-compile-replacement
               (query-replace-descr (cdar anzu2--query-defaults)) use-regexp))
      (add-to-history query-replace-from-history-variable from nil t)
      (when use-regexp
        (unless (anzu2--validate-regexp from)
          (error "'%s' is invalid regexp." from))
        (anzu2--query-validate-from-regexp from))
      from)))

(defun anzu2--compile-replace-text (str)
  (let ((compiled (ignore-errors
                    (query-replace-compile-replacement str t))))
    (when compiled
      (cond ((stringp compiled) compiled)
            ((and (consp compiled) (functionp (car compiled)))
             compiled)
            ((and (consp compiled) (stringp (car compiled)))
             (car compiled))))))

(defun anzu2--evaluate-occurrence (ov to-regexp replacements fixed-case from-regexp)
  (let ((from-string (overlay-get ov 'from-string))
        (compiled (anzu2--compile-replace-text to-regexp)))
    (if (not compiled)
        ""
      (with-temp-buffer
        (insert from-string)
        (goto-char (point-min))
        (when (re-search-forward from-regexp nil t)
          (or (ignore-errors
                (if (consp compiled)
                    (replace-match (funcall (car compiled) (cdr compiled)
                                            replacements) fixed-case)
                  (replace-match compiled fixed-case))
                (buffer-substring (point-min) (point-max)))
              ""))))))

(defun anzu2--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst anzu2--overlays-in-range (beg end)
  (cl-loop for ov in (overlays-in (min beg end) (max beg end))
           when (overlay-get ov 'anzu2-replace)
           collect ov into anzu2-overlays
           finally
           return
           (let ((sorted (sort anzu2-overlays 'anzu2--overlay-sort)))
             (cl-subseq sorted 0 (min (length sorted) anzu2-replace-threshold)))))

(defsubst anzu2--replaced-literal-string (ov replaced from)
  (let ((str (buffer-substring-no-properties
              (overlay-start ov) (overlay-end ov))))
    (when (string-match (regexp-quote str) from)
      (replace-match replaced (not case-fold-search) t str))))

(defun anzu2--append-replaced-string (content buf beg end use-regexp overlay-limit from)
  (let ((replacements 0))
    (unless (string= content anzu2--last-replace-input)
      (setq anzu2--last-replace-input content)
      (with-current-buffer buf
        (let ((case-fold-search (anzu2--case-fold-search from)))
          (dolist (ov (anzu2--overlays-in-range beg (min end overlay-limit)))
            (let ((replace-evaled
                   (if (not use-regexp)
                       (anzu2--replaced-literal-string ov content from)
                     (prog1 (anzu2--evaluate-occurrence ov content replacements
                                                        (not case-fold-search) from)
                       (cl-incf replacements)))))
              (overlay-put ov 'after-string
                           (propertize (concat " => " replace-evaled) 'face 'anzu2-replace-to)))))))))

(defsubst anzu2--outside-overlay-limit (orig-beg orig-limit)
  (save-excursion
    (goto-char (+ anzu2--outside-point (- orig-limit orig-beg)))
    (line-end-position)))

(defun anzu2--read-to-string (from prompt beg end use-regexp overlay-limit)
  (let ((curbuf (current-buffer))
        (orig-beg beg)
        (to-prompt (format "%s %s with: " prompt (query-replace-descr from)))
        (history-add-new-input nil)
        (blink-matching-paren nil)
        timer is-input)
    (setq anzu2--last-replace-input "")
    (when anzu2--outside-point
      (setq beg anzu2--outside-point
            overlay-limit (anzu2--outside-overlay-limit orig-beg overlay-limit)
            anzu2--outside-point nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            #'(lambda ()
                (setq timer (run-with-idle-timer
                             (max anzu2-input-idle-delay 0.01)
                             'repeat
                             (lambda ()
                               (with-selected-window (or (active-minibuffer-window)
                                                         (minibuffer-window))
                                 (anzu2--append-replaced-string
                                  (minibuffer-contents)
                                  curbuf beg end use-regexp overlay-limit from))))))
          (prog1 (read-from-minibuffer to-prompt
                                       nil nil nil
                                       query-replace-from-history-variable nil t)
            (setq is-input t)))
      (when timer
        (cancel-timer timer)
        (setq timer nil)
        (unless is-input
          (goto-char orig-beg))))))

(defun anzu2--query-replace-read-to (from prompt beg end use-regexp overlay-limit)
  (query-replace-compile-replacement
   (let ((to (anzu2--read-to-string from prompt beg end use-regexp overlay-limit)))
     (add-to-history query-replace-to-history-variable to nil t)
     (add-to-history 'anzu2--query-defaults (cons from to) nil t)
     to)
   use-regexp))

(defun anzu2--overlay-limit (backward)
  (save-excursion
    (move-to-window-line (if backward 1 -1))
    (forward-line (if backward -1 1))
    (point)))

(defun anzu2--query-from-at-cursor (buf beg end overlay-limit)
  (let ((symbol (thing-at-point 'symbol)))
    (unless symbol
      (error "No symbol at cursor!!"))
    (let ((symbol-regexp (concat "\\_<" (regexp-quote symbol) "\\_>")))
      (anzu2--count-and-highlight-matched buf symbol-regexp beg end t overlay-limit t)
      (setq anzu2--total-matched anzu2--cached-count)
      (force-mode-line-update)
      symbol-regexp)))

(defun anzu2--query-from-isearch-string (buf beg end use-regexp overlay-limit)
  (anzu2--count-and-highlight-matched buf isearch-string beg end use-regexp overlay-limit t)
  (setq anzu2--total-matched anzu2--cached-count)
  (force-mode-line-update)
  (add-to-history query-replace-from-history-variable isearch-string nil t)
  isearch-string)

(defun anzu2--thing-begin (thing backward)
  (let ((bound (bounds-of-thing-at-point thing)))
    (if bound
        (if backward (cdr bound) (car bound))
      (let ((fallback-bound (bounds-of-thing-at-point 'symbol)))
        (if fallback-bound
            (if backward (cdr bound) (car fallback-bound))
          (point))))))

(defsubst anzu2--thing-end (thing backward)
  (let ((bound (bounds-of-thing-at-point thing)))
    (if bound
        (if backward (car bound) (cdr bound))
      (point-max))))

(defun anzu2--region-begin (use-region thing backward)
  (cond (use-region (region-beginning))
        (thing (anzu2--thing-begin thing backward))
        (backward (point))
        (current-prefix-arg (line-beginning-position))
        (t (point))))

(defsubst anzu2--line-end-position (num)
  (save-excursion
    (forward-line (1- num))
    (line-end-position)))

(defun anzu2--region-end (use-region thing backward)
  (cond (use-region (region-end))
        (backward (point-min))
        (current-prefix-arg
         (anzu2--line-end-position (prefix-numeric-value current-prefix-arg)))
        (thing (anzu2--thing-end thing backward))
        (t (point-max))))

(defun anzu2--begin-thing (at-cursor thing)
  (cond ((and at-cursor thing) thing)
        ((and at-cursor (not thing)) 'symbol)
        (t nil)))

(defsubst anzu2--replace-backward-p (prefix)
  (and prefix (< prefix 0)))

(defun anzu2--construct-perform-replace-arguments (from to delimited beg end backward query)
  (if backward
      (list from to query t delimited nil nil beg end backward)
    (list from to query t delimited nil nil beg end)))

(defun anzu2--construct-query-replace-arguments (from to delimited beg end backward)
  (if backward
      (list from to delimited beg end backward)
    (list from to delimited beg end)))

(defsubst anzu2--current-replaced-index (curpoint)
  (cl-loop for m in anzu2--replaced-markers
           for i = 1 then (1+ i)
           for pos = (marker-position m)
           when (= pos curpoint)
           return i))

(defadvice replace-highlight (before anzu2-replace-highlight activate)
  (when (and (eq anzu2--state 'replace) anzu2--replaced-markers)
    (let ((index (anzu2--current-replaced-index (ad-get-arg 0))))
      (when (or (not index) (/= index anzu2--current-position))
        (force-mode-line-update)
        (setq anzu2--current-position (or index 1))))))

(defun anzu2--set-replaced-markers (from beg end use-regexp)
  (save-excursion
    (goto-char beg)
    (cl-loop with curbuf = (current-buffer)
             with backward = (> beg end)
             with input = (if use-regexp from (regexp-quote from))
             with search-func = (if backward #'re-search-backward #'re-search-forward)
             with cmp-func = (if backward #'< #'>)
             with step = (if backward -1 1)
             while (funcall search-func input end t)
             do
             (progn
               (anzu2--set-marker (match-beginning 0) curbuf)
               (when (= (match-beginning 0) (match-end 0))
                 (if (eobp)
                     (cl-return nil)
                   (forward-char step)))
               (when (and end (funcall cmp-func (point) end))
                 (cl-return nil))))))

(cl-defun anzu2--query-replace-common (use-regexp
                                       &key at-cursor thing prefix-arg (query t) isearch-p)
  (anzu2--cons-mode-line 'replace-query)
  (let* ((use-region (use-region-p))
         (orig-point (point))
         (backward (anzu2--replace-backward-p prefix-arg))
         (overlay-limit (anzu2--overlay-limit backward))
         (beg (anzu2--region-begin use-region (anzu2--begin-thing at-cursor thing) backward))
         (end (anzu2--region-end use-region thing backward))
         (prompt (anzu2--query-prompt use-region use-regexp at-cursor isearch-p))
         (delimited (and current-prefix-arg (not (eq current-prefix-arg '-))))
         (curbuf (current-buffer))
         (clear-overlay nil))
    (when use-region
      (deactivate-mark t))
    (unwind-protect
        (let* ((from (cond ((and at-cursor beg)
                            (setq delimited nil)
                            (anzu2--query-from-at-cursor curbuf beg end overlay-limit))
                           (isearch-p
                            (anzu2--query-from-isearch-string
                             curbuf beg end use-regexp overlay-limit))
                           (t (anzu2--query-from-string
                               prompt beg end use-regexp overlay-limit))))
               (to (cond ((consp from)
                          (prog1 (cdr from)
                            (setq from (car from)
                                  anzu2--total-matched anzu2--last-replaced-count)))
                         ((string-match "\0" from)
                          (let ((replaced (substring-no-properties from (match-end 0))))
                            (setq from (substring-no-properties from 0 (match-beginning 0)))
                            (if use-regexp
                                (anzu2--compile-replace-text replaced)
                              replaced)))
                         (t
                          (anzu2--query-replace-read-to
                           from prompt beg end use-regexp overlay-limit)))))
          (anzu2--clear-overlays curbuf (min beg end) (max beg end))
          (anzu2--set-replaced-markers from beg end use-regexp)
          (setq anzu2--state 'replace anzu2--current-position 0
                anzu2--replaced-markers (reverse anzu2--replaced-markers)
                clear-overlay t)
          (let ((case-fold-search (and case-fold-search (not at-cursor))))
            (if use-regexp
                (apply #'perform-replace (anzu2--construct-perform-replace-arguments
                                          from to delimited beg end backward query))
              (apply #'query-replace (anzu2--construct-query-replace-arguments
                                      from to delimited beg end backward)))))
      (progn
        (unless clear-overlay
          (anzu2--clear-overlays curbuf (min beg end) (max beg end)))
        (when (zerop anzu2--current-position)
          (goto-char orig-point))
        (anzu2--cleanup-markers)
        (anzu2--reset-mode-line)
        (force-mode-line-update)))))

;;;###autoload
(defun anzu2-query-replace-at-cursor (arg)
  "Replace symbol at cursor with to-string."
  (interactive "p")
  (anzu2--query-replace-common t :at-cursor t :prefix-arg arg))

;;;###autoload
(defun anzu2-query-replace-at-cursor-thing ()
  "Replace symbol at cursor within `anzu2-replace-at-cursor-thing' area."
  (interactive)
  (anzu2--query-replace-common t :at-cursor t :thing anzu2-replace-at-cursor-thing))

;;;###autoload
(defun anzu2-query-replace (arg)
  "anzu version of `query-replace'."
  (interactive "p")
  (anzu2--query-replace-common nil :prefix-arg arg))

;;;###autoload
(defun anzu2-query-replace-regexp (arg)
  "anzu version of `query-replace-regexp'."
  (interactive "p")
  (anzu2--query-replace-common t :prefix-arg arg))

;;;###autoload
(defun anzu2-replace-at-cursor-thing ()
  "anzu2-query-replace-at-cursor-thing without query."
  (interactive)
  (let ((orig (point-marker)))
    (anzu2--query-replace-common t
                                 :at-cursor t
                                 :thing anzu2-replace-at-cursor-thing
                                 :query nil)
    (goto-char (marker-position orig))
    (set-marker orig nil)))

(defun anzu2--isearch-query-replace-common (use-regexp arg)
  (isearch-done nil t)
  (isearch-clean-overlays)
  (let ((isearch-recursive-edit nil)
        (backward (< (prefix-numeric-value arg) 0)))
    (when (and isearch-other-end
               (if backward
                   (> isearch-other-end (point))
                 (< isearch-other-end (point)))
               (not (and transient-mark-mode mark-active
                         (if backward
                             (> (mark) (point))
                           (< (mark) (point))))))
      (goto-char isearch-other-end))
    (anzu2--query-replace-common use-regexp :prefix-arg arg :isearch-p t)))

;;;###autoload
(defun anzu2-isearch-query-replace (arg)
  "anzu2 version of `isearch-query-replace'."
  (interactive "p")
  (anzu2--isearch-query-replace-common nil arg))

;;;###autoload
(defun anzu2-isearch-query-replace-regexp (arg)
  "anzu2 version of `isearch-query-replace-regexp'."
  (interactive "p")
  (anzu2--isearch-query-replace-common t arg))

(provide 'anzu2)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; anzu2.el ends here
