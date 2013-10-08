use strict;
use warnings;

use FindBin qw($Bin);
use lib "$Bin/../p5-shelly/lib", "$Bin/../p5-shelly/fatlib";
use App::shelly;

use Pod::Usage;

unless (caller) {
    my $app = App::shelly->new;
    $app->parse_options(@ARGV);
    if ( $app->{help} ) {
        pod2usage( verbose => 1, -exitval => 'noexit' );
    }
    $app->doit or exit(1);
}
