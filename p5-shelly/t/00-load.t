#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'App::shelly' ) || print "Bail out!\n";
}

diag( "Testing App::shelly $App::shelly::VERSION, Perl $], $^X" );
