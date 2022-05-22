[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/company-fuzzy-badge.svg)](https://melpa.org/#/company-fuzzy)
[![MELPA Stable](https://stable.melpa.org/packages/company-fuzzy-badge.svg)](https://stable.melpa.org/#/company-fuzzy)

# company-fuzzy
> Fuzzy matching for `company-mode'.

[![CI](https://github.com/jcs-elpa/company-fuzzy/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/company-fuzzy/actions/workflows/test.yml)

<p align="center">
  <img src="./etc/demo.gif" width="70%"/>
</p>

Pure `elisp` fuzzy completion for `company-mode`. This plugin search through
all the buffer local `company-backends` and fuzzy search all candidates.

## 🏆 Features

* *Work across all backends* - Any backend that gives list of string should work.
* *Only uses native `elisp` code* - I personally don't prefer any external
program unless is necessary.
* *Combined all backends to one backend* - Opposite to [company-try-hard](https://github.com/Wilfred/company-try-hard),
hence all possible candidates will be shown in the auto-complete menu.

## 🧪 Differences from other alternatives

* [company-ycmd](https://github.com/abingham/emacs-ycmd)
  * Uses [ycmd](https://github.com/Valloric/ycmd) as backend to provide functionalities.
  * Quite hard to config properly.
* [company-flx](https://github.com/PythonNut/company-flx)
  * Uses library [flx](https://github.com/lewang/flx). (Optional for us)
  * Only works with `capf` backend currently.

## 💾 Quickstart

```el
(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-history-backends '(company-yasnippet)
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))
```

## 🔧 Usage

You can enable it globally by adding this line to your config
```el
(global-company-fuzzy-mode 1)
```
Or you can just enable it in any specific buffer/mode you want.
```el
(company-fuzzy-mode 1)
```

Make sure you call either of these functions after all
`company-backends` are set and config properly. Because
this plugin will replace all backends to this minor mode
specific backend (basically take all backends away, so
this mode could combine all sources and do the fuzzy work).

### 🔍 Sorting/Scoring backend

There are multiple sorting algorithms for auto-completion. You can choose your
own backend by customize `company-fuzzy-sorting-backend` variable like this.

```el
(setq company-fuzzy-sorting-backend 'alphabetic)
```

Currently supports these values,

* *`none`* - Gives you the raw result.
* *`alphabetic`* - Sort in the alphabetic order. (VSCode)
* *`flex`* - Use library [flex](https://github.com/jcs-elpa/flex) as matching engine.
* *`flx`* - Use library [flx](https://github.com/lewang/flx) as matching engine. (Sublime Text)
* *`flx-rs`* - Use library [flx-rs](https://github.com/jcs-elpa/flx-rs) as matching engine. (Sublime Text)
* *`flxy`* - Use library [flxy](https://github.com/jcs-elpa/flxy) as matching engine.
* *`fuz-skim`* - Use library [fuz.el](https://github.com/rustify-emacs/fuz.el)'s skim algorithm.
* *`fuz-clangd`* - Use library [fuz.el](https://github.com/rustify-emacs/fuz.el)'s clangd algorithm.
* *`fuz-bin-skim`* - Use library [fuz-bin](https://github.com/jcs-elpa/fuz-bin)'s skim algorithm.
* *`fuz-bin-clangd`* - Use library [fuz-bin](https://github.com/jcs-elpa/fuz-bin)'s clangd algorithm.
* *`liquidmetal`* - Use library [liquidmetal](https://github.com/jcs-elpa/liquidmetal) similar to [Quicksilver](https://qsapp.com/) algorithm.
* *`sublime-fuzzy`* - Use library [sublime-fuzzy](https://github.com/jcs-elpa/sublime-fuzzy) as matching engine. (Sublime Text)

Or implements your sorting algorithm yourself? Assgin the function to
`company-fuzzy-sorting-function` variable like this.

```el
(setq company-fuzzy-sorting-function (lambda (candidates)
                                       (message "%s" candidates)
                                       candidates))  ; Don't forget to return the candidaites!
```

### 🔍 Prefix On Top

If you wish the prefix matchs on top of all other selection, customize
this variable to `t` like the line below.

```el
(setq company-fuzzy-prefix-on-top t)
```

*P.S.
If you set `company-fuzzy-sorting-backend` to `'flx` then
you probably don't need this to be on because the `flx` scoring engine
already take care of that!*

### 🔍 For annotation

You can toggle `company-fuzzy-show-annotation` for showing annotation or not.

```el
(setq company-fuzzy-show-annotation t)
```

You can also customize annotation using `format` variable.

* `company-fuzzy-annotation-format` => ` <%s>`

### 🔍 Excluding

You can customize variable `company-fuzzy-passthrough-backends` to exclude some
of the backends from polluting the fuzzy matching.

```el
(setq company-fuzzy-passthrough-backends '(company-capf))
```

*P.S. This is design to use with semantic backends, like [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
(uses [company-capf](https://github.com/company-mode/company-mode))
, or [eglot](https://github.com/joaotavora/eglot), etc.*

## 💬 Details

Since [company](https://github.com/company-mode/company-mode)
granted most control to users, every company backend developer
has different method of implementing company backend. It is hard
to manage all backends to one by varies of rules.

### ✒️ History

Some backends doesn't allow me to get the list of candidates by passing the
possible prefix; hence I have created this type of special scenario. If you
encountered a backend that sometimes does fuzzy, but sometimes does not;
try add the backend to `company-fuzzy-history-backends` like the following
code snippet. `'company-yasnippet` is one of the backend that does not
allow me to do that.

```el
(add-to-list 'company-fuzzy-history-backends 'company-yasnippet)
```

## Recommended Settings

There are something that `company` design it weirdly, in order to make this
plugin work smoothly I would recommend these `company`'s variables to be set.

```el
(use-package company
  :init
  (setq company-require-match nil            ; Don't require match, so you can still move your cursor as expected.
        company-tooltip-align-annotations t  ; Align annotation to the right side.
        company-eclim-auto-save nil          ; Stop eclim auto save.
        company-dabbrev-downcase nil)        ; No downcase when completion.
  :config
  ;; Enable downcase only when completing the completion.
  (defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t))
      (call-interactively fn)))
  (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around))
```

*P.S.
For the full configuration, you can check out my configuration
[here](https://github.com/jcs-emacs/jcs-emacs/blob/21a886dea0da6ff6459f4e853f1637bd3681a4ae/lisp/jcs-plugin.el#L160-L166).*

## :question: FAQ

#### 💫 Why is `company-fuzzy` not working?

Try to log out the `company-backends` and make sure `company-fuzzy-all-other-backends`
is the only backend in your list. If it's not, enable `company-fuzzy-mode` to swap
out all backends and hand it over to `company-fuzzy` to manage it.

```el
(message "%s" company-backends)         ; '(company-fuzzy-all-other-backends)
(message "%s" company-fuzzy--backends)  ; .. backends has been handed over to `company-fuzzy`
```

#### 💫 When should I call `company-fuzzy-mode`?

You should call `company-fuzzy-mode` after you have done configured
variable `company-backends`.

```el
(setq company-backends '(company-capf company-yasnippets)  ; configure backends

.. (other configuration)

(company-fuzzy-mode 1)                                     ; enable fuzzy matching at the very last
```

#### 💫 What if I want to add backends to specific `major-mode`?

You can add any backends as long as you call `company-fuzzy-mode` at the very end
of your mode hook. You can log out variable `company-fuzzy--backends` and see what
backends are currently handled by `company-fuzzy-mode`!

Or, you can hack through by configuring variable `company-fuzzye--backends` directly
but this is not recommended since after you disable `company-fuzzy-mode` it will
not be restored back to `company-backends`. Unless you change it with variable
`company-fuzzy--recorded-backends` simutamiously so it can be restored back to
your `company-backends`' true form.

❗ **UPDATE:** You can now use the following functions to accomplish these tasks
in a much elegant way:

```elisp
(company-fuzzy-backend-add    'company-capf)
(company-fuzzy-backend-remove 'company-yasnippets)
```

#### 💫 Why do some candidates aren't showing up?

This can cause by various reasons. The common causes are:

##### 🔎 1. Cause by Semantic backend rules

`company-fuzzy` respects backends' rule. Meaning the candidates can be restricted
by the backend you are using. For example,

```el
(defvar my-variable)  ; You declare a variable

(my-vari.. )          ; but you are trying to use the variable as a function
```

The `my-variable` would not show up since the backend thinks it should be a
function and not a variable.

##### 🔎 2. Cause by `completion-styles`

Candidates are first filtered by Emacs built-on completion engine. Try tweaking
the variable `completion-styles` with other possible options.

```elisp
(setq completion-styles '(partial-completion))
```

Or hook up with the company's hooks:

```elisp
(add-hook 'company-completion-started-hook
          (lambda ()
            (setq completion-styles '(partial-completion))))

(add-hook 'company-after-completion-hook
          (lambda ()
            ;; Revert `completion-styles' to original values
            (setq completion-styles ..)))
```

See [Completion Alternatives](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html)
for more information.

##### 🔎 3. Scores lower than 0

Another cause would be the candidate has been eliminated by the scoring engine
(it scores lower than 0); hence it would not be shown.

Best way to debug this, is to feed `query` and `candidate` to the scoring
functions. The following example uses `flx`:

```elisp
(flx-score "win-sys" "window-system")  ; return score and it's match data
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either
clone or make pull requests to this repository. Or you can
clone the project and establish your branch of this tool.
Any methods are welcome!
