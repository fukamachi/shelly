#!/usr/bin/env perl
use strict;
use warnings;

use FindBin qw($Bin);
use lib "$Bin/../lib";
use Pod::Select;
use Pod::Find qw(pod_where);

my $pod;
{
    open TEMPOUT, '>', \$pod;
    local *STDOUT = *TEMPOUT;
    podselect( pod_where( { -inc => 1 }, 'App::shelly' ) );
    close TEMPOUT
}

open my $in,  '<', "$Bin/shly.PL"     or die $!;
open my $out, '>', "$Bin/../bin/shly" or die $!;

local $/ = undef;
my $startup = <$in>;
print $out '#!/usr/bin/env perl', "\n";
print $out scalar `$^X -e "use App::FatPacker -run_script" file`;
print $out $startup;
print $out "\n__END__\n\n";
print $out $pod;
