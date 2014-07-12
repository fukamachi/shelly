# Shelly - Run Common Lisp from shell easily

## Usage

    $ shly -Lclack clackup /path/to/project/app.lisp
    $ shly -Ldrakma http-request http://www.hatena.com/

## Description

Shelly allows you to execute Common Lisp functions like a shell command.

<span style="color:red">Warning</span>: This software is still ALPHA quality. The APIs will be likely to change.

## Requirements

- [CIM](https://github.com/KeenS/CIM)

## Installation

### Install from Quicklisp

Shelly stable version is included in Quicklisp dist. I _don't_ recommend this version to use now, but this is the most easy way to try it anyway.

    (ql:quickload :shelly)
    (shelly:install)

or

    $ curl -L http://shlyfile.org/shly | /bin/sh

### Install from source

As Shelly is under active development, I highly recommend you to install the latest revision of it.

```
$ git clone https://github.com/fukamachi/shelly.git
$ cd shelly
$ SHELLY_PATH=. bin/shly install
```

## Configuration

Add the following code to your Shell configuration file (such like .bashrc or .zshrc).

    PATH=$HOME/.shelly/bin:$PATH

## How to use

Running `shly --help` or just `shly` will get you how to use Shelly.

    $ shly
    $ shly --help

## Upgrade

Shelly ver 0.5.0 or higher allows to specify `--version` to `install` command. You can install the latest version of Shelly by running the following command.

```
$ shly install --version latest
```

To use Shelly ver 0.6.1 or older versions, you need Perl5 and a Common Lisp implementation.

## Uninstall

```
$ rm -rf ~/.shelly
```

## Declaring Project specific commands

Shelly loads a local file which is named `shlyfile.lisp` if it exists. You can define project specific commands by writing functions in it.

```common-lisp
(defun test ()
  (asdf:test-system :your-app))

(defun build ()
  ;; Somthing to build your app.
  )
```

## Copyright

Copyright (c) 2012-2014 Eitaro Fukamachi (e.arrows@gmail.com).

# License

Licensed under the BSD (2-Clause) License.
