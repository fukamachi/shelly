package App::shelly::impl;

use strict;
use warnings;

use Config::ENV 'LISP_IMPL';

use App::shelly::config;

common + {};

config ccl => +{
    impl_name     => 'ccl',
    eval          => '--eval',
    other_options => ['--quiet'],
    binary        => App::shelly::config::config('ccl')->{binary_path},
    core_option   => '-I',
    noinit_option => '--no-init',
};

config sbcl => +{
    impl_name     => 'sbcl',
    pre_options   => [qw(--noinform --disable-debugger)],
    eval          => '--eval',
    other_options => ['--quiet'],
    binary        => App::shelly::config::config('sbcl')->{binary_path},
    core_option   => '--core',
    noinit_option => '--no-userinit',
};

config alisp => +{
    impl_name     => 'alisp',
    pre_options   => ['-batch'],
    eval          => '-e',
    other_options => ['-kill'],
    binary        => App::shelly::config::config('alisp')->{binary_path},
    core_option   => '-I',
    init_option   => ['-e', '(loop for filename in excl:*init-file-names* for file = (merge-pathnames filename (user-homedir-pathname)) when (probe-file file) do (load file))'],
    noinit_option => '-qq',
};

config clisp => +{
    impl_name     => 'clisp',
    eval          => '-x',
    other_options => [qw(-q --quiet)],
    binary        => App::shelly::config::config('clisp')->{binary_path},
    core_option   => '-M',
    noinit_option => '-norc',
};

config cmucl => +{
    impl_name     => 'cmucl',
    eval          => '-eval',
    other_options => [qw(-quiet -batch)],
    binary => App::shelly::config::config('cmucl')->{binary_path} || 'lisp',
    core_option => '-core',
    noinit_option => '-noinit',
};

config ecl => +{
    impl_name     => 'ecl',
    eval          => '-eval',
    other_options => ['-q'],
    binary        => App::shelly::config::config('ecl')->{binary_path},
    noinit_option => '-norc',
};

1;
