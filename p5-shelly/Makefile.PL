use strict;
use warnings;
use ExtUtils::MakeMaker;

system $^X, "script/build.PL" and die $?;

WriteMakefile(
    NAME      => 'App::shelly',
    AUTHOR    => q{Eitarow Fukamachi <e.arrows@gmail.com>},
    VERSION   => '0.01',
    LICENSE   => 'BSD',
    ABSTRACT  => 'Interface for Common Lisp implementations.',
    PL_FILES  => {},
    PREREQ_PM => {
        'Test::More' => 0,
    },
    EXE_FILES => ['bin/shly'],
    dist      => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'App-shelly-*' },
);
