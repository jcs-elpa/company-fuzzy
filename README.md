[![Build Status](https://travis-ci.com/jcs090218/company-fuzzy.svg?branch=master)](https://travis-ci.com/jcs090218/company-fuzzy)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# company-fuzzy
> Fuzzy matching for `company-mode'


## Differences from other alternatives

* [company-ycmd](https://github.com/abingham/emacs-ycmd)
  * Using external software to provide functionalities.
* [company-flx](https://github.com/PythonNut/company-flx)
  * Only works inside `elisp-mode` currently.

## Usage

```el
(with-eval-after-load 'company
  (company-fuzzy-mode +1))
```


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
