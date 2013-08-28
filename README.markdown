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
- [Quicklisp](http://beta.quicklisp.org/)

## Dependencies

All dependencies will be resolved by [Quicklisp](http://beta.quicklisp.org/), so you don't need to know about this.

Though I recommend you to install [Quicklisp](http://beta.quicklisp.org/), if you decided to use ASDF for instead, you have to install these libraries before installation.

- [CL-FAD](http://weitz.de/cl-fad/)
- Swank (a part of [SLIME](http://common-lisp.net/project/slime/))
- [cl-annot](https://github.com/arielnetworks/cl-annot)
- [Bordeaux Threads](http://common-lisp.net/project/bordeaux-threads/)
- [Drakma](http://weitz.de/drakma/)
- [FLEXI-STREAMS](http://weitz.de/flexi-streams/)
- [YASON](http://common-lisp.net/project/yason/)
- [Chipz](http://method-combination.net/lisp/chipz/)
- [ARCHIVE](https://github.com/froydnj/archive)

## Installation

    (ql:quickload :shelly)
    (shelly:install)

or

    $ curl -L http://xrl.us/shly | LISP_IMPL=ccl perl - install

Change `LISP_IMPL` to your Lisp implementation name which is one of `sbcl`, `ccl`, `alisp`, `clisp`, `cmucl` and `ecl`.

### Install from source

```
$ git clone https://github.com/fukamachi/shelly.git
$ cd shelly
$ SHELLY_PATH=. bin/shly install
```

## Configuration

Add the following code to your Shell configuration file (such like .bashrc or .zshrc).

    PATH=$HOME/.shelly/bin:$PATH

## How to use

    $ shly --help

## Copyright

Copyright (c) 2012-2013 Eitarow Fukamachi (e.arrows@gmail.com).

# License

Licensed under the BSD (2-Clause) License.
