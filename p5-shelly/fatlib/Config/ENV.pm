package Config::ENV;

use strict;
use warnings;

use Carp;

our $VERSION = '0.12';

sub import {
	my $class   = shift;
	my $package = caller(0);

	no strict 'refs';
	if (__PACKAGE__ eq $class) {
		my $name    = shift;
		my %opts    = @_;

		push @{"$package\::ISA"}, __PACKAGE__;

		for my $method (qw/common config parent load/) {
			*{"$package\::$method"} = \&{__PACKAGE__ . "::" . $method}
		}

		no warnings 'once';
		${"$package\::data"} = +{
			common  => {},
			envs    => {},
			name    => $name,
			default => $opts{default} || 'default',
			export  => $opts{export},
		};
	} else {
		my %opts    = @_;
		my $data = _data($class);
		if (my $export = $opts{export} || $data->{export}) {
			*{"$package\::$export"} = sub () { $class };
		}
	}
}

sub _data {
	my $package = shift || caller(1);
	no strict 'refs';
	no warnings 'once';
	${"$package\::data"};
}

sub common ($) { ## no critic
	my ($hash) = @_;
	_data->{common} = $hash;
}

sub config ($$) { ## no critic
	my ($name, $hash) = @_;
	_data->{envs}->{$name} = $hash;
	undef _data->{_merged}->{$name};
}

sub load ($) {
	my $filename = shift;
	my $hash = do "$filename";

	croak $@ if $@;
	croak $! unless defined $hash;
	unless (ref($hash) eq 'HASH') {
		croak "$filename does not return HashRef.";
	}

	wantarray ? %$hash : $hash;
}

sub parent ($) { ## no critic
	my ($name) = @_;
	%{ _data->{envs}->{$name} || {} };
}

sub current {
	my ($package) = @_;
	my $data = _data($package);

	my $vals = $data->{_merged}->{$package->env} ||= +{
		%{ $data->{common} },
		%{ $data->{envs}->{$package->env} || {} },
		(map { %$_ } @{ $data->{_local} || []}),
	};
}

sub param {
	my ($package, $name) = @_;
	$package->current->{$name};
}

sub local {
	my ($package, %hash) = @_;
	not defined wantarray and croak "local returns guard object; Can't use in void context.";

	my $data = _data($package);
	$data->{_local} ||= [];
	push @{ $data->{_local} }, \%hash;
	undef $data->{_merged};

	bless sub {
		pop @{ $data->{_local} };
		undef $data->{_merged};
	}, 'Config::ENV::Local';
}

sub env {
	my ($package) = @_;
	my $data = _data($package);
	$ENV{$data->{name}} || $data->{default};
}

{
	package
		Config::ENV::Local;

	sub DESTROY {
		my $self = shift;
		$self->();
	}
};

1;
__END__

=encoding utf8

=head1 NAME

Config::ENV - Various config determined by %ENV

=head1 SYNOPSIS

  package MyConfig;
  
  use Config::ENV 'PLACK_ENV'; # use $ENV{PLACK_ENV} to determine config
  
  common +{
    name => 'foobar',
  };
  
  config development => +{
    dsn_user => 'dbi:mysql:dbname=user;host=localhost',
  };
  
  config test => +{
    dsn_user => 'dbi:mysql:dbname=user;host=localhost',
  };
  
  config production => +{
    dsn_user => 'dbi:mysql:dbname=user;host=127.0.0.254',
  };
  
  config production_bot => +{
    parent('production'),
    bot => 1,
  };

  # Use it

  use MyConfig;
  MyConfig->param('dsn_user'); #=> ...

=head1 DESCRIPTION

Config::ENV is for switching various configurations by environment variable.

=head1 CONFIG DEFINITION

use this module in your config package:

  package MyConfig;
  use Config::ENV 'FOO_ENV';

  common +{
    name => 'foobar',
  };

  config development => +{};
  config production  => +{};

  1;

=over 4

=item common($hash)

Define common config. This $hash is merged with specific environment config.

=item config($env, $hash);

Define environment config. This $hash is just enabled in $env environment.

=item parent($env);

Expand $env configuration to inherit it.

=item load($filename);

`do $filename` and expand it. This can be used following:

  # MyConfig.pm
  common +{
    API_KEY => 'Set in config.pl',
    API_SECRET => 'Set in config.pl',
    load('config.pl),
  };

  # config.pl
  +{
    API_KEY => 'XFATEAFAFASG',
    API_SECRET => 'ced3a7927fcf22cba72c2559326be2b8e3f14a0f',
  }

=back

=head2 EXPORT

You can specify default export name in config class. If you specify 'export' option as following:

  package MyConfig;
  use Config::ENV 'FOO_ENV', export => 'config';

  ...;

and use it with 'config' function.

  package Foobar;
  use MyConfig; # exports 'config' function

  config->param('...');

=head1 METHODS

=over 4

=item config->param($name)

Returns config variable named $name.

=item $guard = config->local(%hash)

This is for scope limited config. You can use this when you use other values in temporary. Returns guard object.

  is config->param('name'), 'original value';
  {
    my $guard = config->local(name => 'localized');
    is config->param('name'), 'localized';
  };
  is config->param('name'), 'original value';

=item config->env

Returns current environment name.

=item config->current

Returns current configuration as HashRef.

=back

=head1 AUTHOR

cho45 E<lt>cho45@lowreal.netE<gt>

=head1 SEE ALSO

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
