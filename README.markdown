# Shelly

## Usage

    $ shly -Lclack clackup /path/to/project/app.lisp
    $ shly -Ldrakma http-request http://www.hatena.com/

## Description

Shelly allows you to execute Common Lisp functions like a shell command.

* Converts arguments implicitly to handle easily from Common Lisp.
* Dumps a Lisp core for fast execution.
* Allows to define project specific commands. (shlyfile)

<strong><span style="color:red">Warning</span>: This software is still ALPHA quality. The APIs will be likely to change.</strong>

## How does it work

Shelly provides a shell script "shly". It takes some options, a command, and arguments for the command.

    Usage: shly [option,..] <command> [arg1,arg2..]

In this example, `ql:system-apropos` would be the command and `web` would be an argument.

    ;; Same as (ql:system-apropos "web")
    $ shly ql:system-apropos web
    #<SYSTEM bknr.modules / bknr-web-20140616-git / quicklisp 2014-06-16>
    #<SYSTEM bknr.web / bknr-web-20140616-git / quicklisp 2014-06-16>
    #<SYSTEM cl-web-crawler / cl-web-crawler-20130128-svn / quicklisp 2014-06-16>
    #<SYSTEM cl-webdav / cl-webdav-0.2.1 / quicklisp 2014-06-16>
    #<SYSTEM crane-web / crane-20140616-git / quicklisp 2014-06-16>
    #<SYSTEM hh-web / hh-web-20140616-git / quicklisp 2014-06-16>
    ...

If an argument starts with ":", it would be converted into a keyword.

    ;; Same as (asdf:system-source-file :hunchentoot).
    $ shly asdf:system-source-file :hunchentoot
    #P"/Users/nitro_idiot/quicklisp/dists/quicklisp/software/hunchentoot-1.2.21/hunchentoot.asd"

If an argument starts with "--", it also would be converted into a keyword. This is just like common shell commands.

    ;; Same as (ql:update-all-dists :prompt nil)
    $ shly ql:update-all-dists --prompt nil

If the command is imported to `COMMON-LISP-USER` package, you can omit the package prefix.

    $ shly list 1 2 3
    (1 2 3)

Or, if the package name is the same as the system name that is loaded by `-L` option, you can also omit it.

    $ shly -Ldrakma http-request http://www.hatena.com/
    $ shly -Lcl-project make-project /path/to/myapp/ --description "My sample app." --author "Eitaro Fukamachi"

## Requirements

- [CIM: Common Lisp Implementation Manager](https://github.com/KeenS/CIM)

## Installation

### Installing from Quicklisp

The stable version is included in Quicklisp dist.

    (ql:quickload :shelly)
    (shelly:install)

or

    $ curl -L http://shlyfile.org/shly | /bin/sh

### Installing from source

```
$ git clone https://github.com/fukamachi/shelly.git
$ cd shelly
$ SHELLY_PATH=. bin/shly install
```

## Configuration

If you use Bash or Zsh, the initialization code will be appended into your .bashrc or .zshrc automatically.

Otherwise, add ~/.shelly/bin to PATH manually.

    PATH=$HOME/.shelly/bin:$PATH

## How to use it

Running `shly --help` or just `shly` will get you how to use Shelly.

    $ shly
    $ shly --help

## Upgrading

Shelly ver 0.5.0 or higher allows to specify `--version` to `install` command. You can choose the version from the output of `shly available-versions`

```
$ shly available-versions
v0.6.1
v0.6.0
v0.5.8
v0.5.7
v0.5.6
v0.5.5
v0.5.4
v0.5.3
v0.5.2
v0.5.1
v0.5.0
v0.4.2
v0.4.1
v0.4.0
v0.3.1
v0.3
v0.2
v0.1
```

Or, you can install the latest version of Shelly by running the following command.

```
$ shly install --version latest
```

To use Shelly ver 0.6.1 or older versions, you need Perl5, a Common Lisp implementation and Quicklisp.

## Uninstalling

Just delete the ~/.shelly directory.

```
$ rm -rf ~/.shelly
```

## Declaring project specific commands

Shelly loads a local file which is named `shlyfile.lisp` if it exists. You can define project specific commands by writing functions in it. This is just like a `Makefile` in Common Lisp.

```common-lisp
;; shlyfile.lisp
(defun test ()
  (asdf:test-system :your-app))

(defun build ()
  ;; Somthing to build your app.
  )
```

Then, `shly test` and `shly build` would be available only in the directory.

```
$ shly test
$ shly build
```

Shelly also loads `~/.shelly/shlyfile.lisp` every time if it exists. If you have some commands you'd like to use everywhere, put them into that file.

## Commands

### help [&optional command]

Show the usage of the specified `command`.

    $ shly help ql:quickload
    Usage: ql:quickload (systems &key verbose prompt explain
                         (verbose *quickload-verbose*) (prompt *quickload-prompt*)
                         &allow-other-keys)
        Load SYSTEMS the quicklisp way. SYSTEMS is a designator for a list
           of things to be loaded.

If `command` is not specified, it shows all available commands. This is the same as `shly --help`.

    $ shly help

### install [&key version]

Install Shelly into your environment under "~/.shelly". You can install a specific version by using "--version".

    $ shly install --version v0.6.1

### available-versions

Show all the possible Shelly versions.

    $ shly available-versions

### dump-core

Dump Lisp core image file for faster startup.

    $ shly dump-core

### rm-core

Remove saved core image file which created by `dump-core`.

    $ shly rm-core

### local-dump-core [&rest systems]   \(Experimental)

Dump Lisp core image file to the current directory. This command takes system names to be included in the core.

    $ shly local-dump-core :myapp

## Copyright

Copyright (c) 2012-2014 Eitaro Fukamachi (e.arrows@gmail.com).

# License

Licensed under the BSD (2-Clause) License.
