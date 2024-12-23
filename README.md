# anzu2.el  ![](https://github.com/syohex/emacs-anzu2/workflows/CI/badge.svg)

This is my own anzu.el implementation

## Introduction

`anzu2.el` is an Emacs port of [anzu.vim](https://github.com/osyo-manga/vim-anzu).
`anzu2.el` provides a minor mode which displays *current match* and *total matches*
information in the mode-line in various search modes.


## Screenshot

![Screencast of anzu.gif](image/anzu.gif)


## Basic Usage

##### `global-anzu2-mode`

Enable global anzu mode:

```lisp
(global-anzu2-mode +1)
```

##### `anzu2-mode`

Enable anzu minor mode:

```lisp
(anzu2-mode +1)
```

##### `anzu2-query-replace`

Same as `query-replace` except anzu information in mode-line

##### `anzu2-query-replace-regexp`

Same as `query-replace-regexp` except anzu information in mode-line


Add following S-exp in your configuration if you want to use anzu's replace commands by default.

```lisp
(global-set-key [remap query-replace] 'anzu2-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu2-query-replace-regexp)
```

[Screencast of anzu2-query-replace-at-cursor-thing](image/anzu-replace-demo.gif)


##### `anzu2-query-replace-at-cursor`

Same as `anzu2-query-replace` except *from-string* is symbol at cursor

##### `anzu2-query-replace-at-cursor-thing`

Same as `anzu2-query-replace-at-cursor` except replaced region is
specified by `anzu2-replace-at-cursor-thing`.

##### `anzu2-replace-at-cursor-thing`

Same as `anzu2-query-replace-at-cursor-thing` except not query.
This command is useful in refactoring such as changing variable name
in the function.

![Screencast of anzu2-replace-at-cursor-thing](image/anzu-replace-demo-noquery.gif)


##### `anzu2-isearch-query-replace`

Anzu version of `isearch-query-replace`

##### `anzu2-isearch-query-replace-regexp`

Anzu version of `isearch-query-replace-regexp`

## Customization

##### `anzu2-mode-line`

Face of mode-line anzu information

##### `anzu2-mode-line-no-match`

Face of mode-line at no matching case

##### `anzu2-replace-highlight`

Face of from-string of replacement

##### `anzu2-replace-to`

Face of to-string of replacement

##### Screenshot

![anzu2-any-position](image/anzu-any-position.png)


##### `anzu2-input-idle-delay`(Default is `0.05`)

Delay second of updating mode-line information when you input from-string

##### `anzu2-regexp-search-commands`

Commands which have regexp input. If the last command is a member of this list,
`anzu2.el` treats input as regular expression.

The default value is `'(isearch-forward-regexp isearch-backward-regexp)`.

##### `anzu2-search-threshold`(Default is `1000`)

Threshold of searched words. If there are searched word more than this value,
`anzu2.el` stops to search and display total number like `1000+`(as default).

![anzu2-threshold](image/anzu-threshold.png)

##### `anzu2-replace-threshold`(Default is `50`)

Threshold of replacement overlays.

##### `anzu2-minimum-input-length`(Default is 1)

Minimum input length to enable anzu.

##### `anzu2-replace-at-cursor-thing`(Default is 'defun)

Thing at point of `anzu2-query-replace-at-cursor-thing`.
This parameter is same as `thing-at-point`.


## Sample Configuration

```lisp
(require 'anzu)
(global-anzu2-mode +1)

(set-face-attribute 'anzu2-mode-line nil
                    :foreground "yellow" :weight 'bold)

(custom-set-variables
 '(anzu2-search-threshold 1000)
 '(anzu2-replace-threshold 50))

(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu2-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu2-isearch-query-replace-regexp)
```
