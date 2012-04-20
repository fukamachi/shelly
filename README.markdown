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

## Requirement

- Lisp implementation (SBCL, Clozure CL, Allegro CL, GNU CLISP, CMUCL or ECL)
- Perl5
- Quicklisp

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
