# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 1.4.1 (Unreleased)
> Released N/A

* Make local hash map object work! (e63df5f47d47f335c4cdbeb66b815ada2a7161a3)

## 1.4.0
> Released Jan 27, 2022

* Improve some operations using some macros from `subr-x`.
* Drop support for Emacs version `24.x` and `25.x`.

## 1.3.0
> Released Nov 4, 2021

* Added support for [fuz.el](https://github.com/rustify-emacs/fuz.el).
* Added support for [sublime-fuzzy](https://github.com/jcs-elpa/sublime-fuzzy).
* Added support for [fuz-bin](https://github.com/jcs-elpa/fuz-bin).
* Added support for [flxy](https://github.com/jcs-elpa/flxy).
* Started support `async` backends.
* Added support for [flx-rs](https://github.com/jcs-elpa/flx-rs).
* Initialize sorting backends at start instead during runtime.

## 1.2.2
> Released Oct 9, 2021

* Added support for [flex](https://github.com/jcs-elpa/flex).
* Added support for [liquidmetal](https://github.com/jcs-elpa/liquidmetal).
* Fixed invalid output for command `company-diag`.
* Introduce new variable `company-fuzzy-passthrough-backends` to filter out already fuzzied candidates.
* Fixed error from `company-tng-mode`.
* Added functionality to adapt all commands from original company backends.

## 1.2.1
> Released Jan 24, 2021

* Used hash table library `ht`.

### 1.0.1
> Released Nov 19, 2020

* Fixed logic for candidates not initialize correctly.
