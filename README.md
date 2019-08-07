[![Build Status](https://travis-ci.com/jcs090218/company-fuzzy.svg?branch=master)](https://travis-ci.com/jcs090218/company-fuzzy)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# company-fuzzy
> Fuzzy matching for `company-mode'.

<p align="center">
  <img src="./screenshot/demo.gif"/>
</p>


Pure `elisp` fuzzy completion for `company-mode`. This plugin search through
all the buffer local `company-backends` and fuzzy search all candidates.


## Features

* **Work across all backends** - Any backend that gives list of string should work.
* **Only uses native `elisp` code** - I personally don't prefer any external
program unless is necessary.
* **Combined all backends to one backend** - Opposite to [company-try-hard](https://github.com/Wilfred/company-try-hard),
hence all possible candidates will be shown in the auto-complete menu.


## Differences from other alternatives

* [company-ycmd](https://github.com/abingham/emacs-ycmd)
  * Uses [ycmd](https://github.com/Valloric/ycmd) as backend to provide functionalities.
  * Quite hard to config properly.
* [company-flx](https://github.com/PythonNut/company-flx)
  * Uses library [flx](https://github.com/lewang/flx).
  * Only works with `elisp-mode` currently.


## Usage

You can enable it globally by adding this line to your config
```el
(global-company-fuzzy-mode t)
```
Or you can just enable it in any specific buffer/mode you want.
```el
(company-fuzzy-mode t)
```

Make sure you call either of these functions after all
`company-backends` are set and config properly. Because
this plugin will replace all backends to this minor mode
specific backend (basically take all backends away, so
this mode could combine all sources and do the fuzzy work).

### Sorting/Scoring backend

There are multiple sorting algorithms for auto-completion. You can choose your
own backend by customize `company-fuzzy-sorting-backend` variable like this.

```el
(setq company-fuzzy-sorting-backend 'alphabetic)
```

Currently supports these values,

* *none* - Gives you the raw result.
* *alphabetic* - Sort in the alphabetic order. (VSCode)
* *flx* - Sort by [flx](https://github.com/lewang/flx) matching engine. (Sublime Text)

Or implements your sorting algorithm yourself? Assgin the function to
`company-fuzzy-sorting-function` variable like this.

```el
(setq company-fuzzy-sorting-function (lambda (candidates)
                                       (message "%s" candidates)
                                       candidates))  ; Don't forget to return the candidaites!
```

### Prefix ontop

If you wish the prefix matchs ontop of all other selection, customize
this variable to `t` like the line below.

```el
(setq company-fuzzy-prefix-ontop t)
```

*P.S.
If you set `company-fuzzy-sorting-backend` to `'flx` then
you probably don't need this to be on because the `flx` scoring engine
already take care of that!*


### For annotation

You can toggle `company-fuzzy-show-annotation` for showing annotation or not.

```el
(setq company-fuzzy-show-annotation t)
```

You can also customize annotation `prefix` and `postfix`.

* `company-fuzzy-anno-prefix` => `<`
* `company-fuzzy-anno-postfix` => `>`


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
