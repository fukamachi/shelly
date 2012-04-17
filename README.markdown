# Shelly - Run Common Lisp from shell easily.

## Usage

    $ shly ql:update-all-dists --prompt nil
    $ shly ql:quickload :clack
    $ shly -Lclack clack:clackup /path/to/project/app.lisp
    $ shly -Ldrakma drakma:http-request http://www.hatena.com/

## Description

Shelly allows you to execute Common Lisp functions like a shell command.

Warning: This software is still ALPHA quality. The APIs will be likely to change.

## Requirement

- Lisp implementation (SBCL, Clozure CL, Allegro CL, GNU CLISP, CMUCL or ECL)
- Perl5
- Quicklisp

## Installation

    $ curl -L http://xrl.us/shly | perl - --install

or

    (ql:quickload :shelly)
    (shelly:install-script)

## Configuration

    PATH="$HOME/.shelly/bin:$PATH"

## How to use

    $ shly --help

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD License.
