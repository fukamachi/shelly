package App::shelly::config;

use strict;
use warnings;
use Exporter::Lite;
use Path::Class qw(dir);

our @EXPORT_OK = qw(config config_path lisp_impl dumped_core_path shelly_path local_path);

my $local_base_path = $ENV{HOME} . '/.shelly/';

sub local_path {
    return $local_base_path . $_[0];
}

sub config_path {
    return local_path('config');
}

sub shelly_path {
    if (exists $ENV{SHELLY_PATH}) {
        return $ENV{SHELLY_PATH};
    }
    if (-e local_path('shelly/')) {
        return local_path('shelly/')
    }
}

my $_lisp_impl_cache = undef;
sub lisp_impl {
    $_lisp_impl_cache ||= do {
        my $lisp_impl = `[ -s "$ENV{CIM_HOME}/config/current.$ENV{CIM_ID}" ] && . "$ENV{CIM_HOME}/config/current.$ENV{CIM_ID}" && echo "\$LISP_IMPL"`;
        chomp $lisp_impl;

        $lisp_impl;
    };
}

sub dumped_core_path {
    my $core_path = 'dumped-cores/' . ( $_[0] || ( lisp_impl . '.core' ) );
    return -e $core_path ? $core_path : local_path($core_path);
}

sub config {
    my $config_file = &config_path;

    my $config =
      -e $config_file
      ? do $config_file
      : {};

    return $config;
}

1;
