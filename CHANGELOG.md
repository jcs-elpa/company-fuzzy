# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 2.0.1 (Unreleased)
> Released N/A

* fix: Also apply suffix to passthrough backends ([`82ed914`](../../commit/82ed914f7fc3f82c9149e3b06c6fea935aa52efa))
* fix: No empty filter on match prefix ([`955c6b3`](../../commit/955c6b31f679fcf2af6af646339193730ac0b816))

## 2.0.0
> Released Apr 21, 2025

* Make local hash map object work! ([`e63df5f`](../../commit/e63df5f47d47f335c4cdbeb66b815ada2a7161a3))
* Fix long lasting `company-yasnippet` bug ([#30](../../pull/30))
* Compatible to backend `company-c-headers` ([#31](../../pull/31))
* Add use of furthest prefix ([#33](../../pull/33))
* Call prefix backend to respect their rules ([`0a81469`](../../commit/0a814694a5f6ed12926de07f29d04850789f19e2))
* Handle `lsp-mode` and `eglot` by default ([`3c8c960`](../../commit/3c8c960b4e5341f6806cf43aa794c0798051222d))
* feat: Work with `company-paths` ([#34](../../pull/34))
* Add method to add backend between backends ([#36](../../pull/36))
* Ensure backend name in other way ([`85dd0a7`](../../commit/85dd0a7852db70673d70387a834af01ed705c3f7))
* feat: Add the reset selection option ([`6a147b5`](../../commit/6a147b5ef93db50462968a816c7feff638d4386e))
* feat: Compatible to latest `company-1.0.0` ([#42](../../pull/42))

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
