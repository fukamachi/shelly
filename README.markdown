# Shelly - Run Common Lisp from shell easily.

## Usage

    $ shly ql:update-all-dists --prompt nil
    $ shly ql:system-apropos clack
    $ shly ql:quickload :clack
    $ shly -Lclack clackup /path/to/project/app.lisp
    $ shly -Ldrakma http-request http://www.hatena.com/

## Description

Shelly allows you to execute Common Lisp functions like a shell command.

Warning: This software is still ALPHA quality. The APIs will be likely to change.

## Requirements

- Lisp implementation (SBCL, Clozure CL, Allegro CL, GNU CLISP, CMUCL or ECL)
- Perl5
- [Quicklisp](http://beta.quicklisp.org/) or ASDF (Quicklisp is recommended)

## Dependencies

All dependencies will be resolved by [Quicklisp](http://beta.quicklisp.org/), so you don't need to know about this.

Though I recommend you to install [Quicklisp](http://beta.quicklisp.org/), if you decided to use ASDF for instead, you have to install these libraries before installation.

- [CL-FAD](http://weitz.de/cl-fad/)
- Swank (a part of [SLIME](http://common-lisp.net/project/slime/))
- [cl-annot](https://github.com/arielnetworks/cl-annot)

## Installation

    (ql:quickload :shelly)
    (shelly:install)

or

    $ curl -L http://xrl.us/shly | LISP_IMPL=ccl perl - install

Change `LISP_IMPL` to your Lisp implementation name which is one of `sbcl`, `ccl`, `alisp`, `clisp`, `cmucl` and `ecl`.

## Configuration

Add the following code to your Shell configuration file (such like .bashrc or .zshrc).

    PATH=$HOME/.shelly/bin:$PATH

## How to use

    $ shly --help

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD License.
