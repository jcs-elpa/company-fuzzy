[![Build Status](https://travis-ci.com/jcs090218/company-fuzzy.svg?branch=master)](https://travis-ci.com/jcs090218/company-fuzzy)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# company-fuzzy
> Fuzzy matching for `company-mode'

| company-mode | With company-fuzzy |
|:---|:---|
|<img src="./screenshot/normal.gif"/>|<img src="./screenshot/with-fuzzy.gif"/>|


Pure `elisp` fuzzy completion for `company-mode`. This plugin search through
all the buffer local `company-backends` and fuzzy search all candidates.


## Features

* Work across all backends.
* Only uses native `elisp` code.
* Combined all backends to one backend. (Opposite to [company-try-hard](https://github.com/Wilfred/company-try-hard))


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


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
