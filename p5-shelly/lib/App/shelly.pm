package App::shelly;

use strict;
use warnings;

use Getopt::Long qw(:config gnu_getopt pass_through);
use File::Which qw(which);

use App::shelly::impl;
use App::shelly::config qw(config dumped_core_path local_path);
use App::shelly::command;

sub impl {
    sub { App::shelly::impl->param(@_); }
}

sub new {
    my ($class) = @_;

    return bless {
        lisp_impl => $ENV{LISP_IMPL} || config->{default_lisp},
        load_libraries => [],
        argv           => [],
    }, $class;
}

sub parse_options {
    my ( $self, @argv ) = @_;

    local @ARGV = @{ $self->{argv} };
    push @ARGV, @argv;

    GetOptions(
        'impl|I=s'  => \$self->{lisp_impl},
        'load|L=s'  => \my $libraries,
        'file|f=s'  => \$self->{shlyfile},
        'verbose'   => \$self->{verbose},
        'debug'     => \$self->{debug},
    );

    if ($libraries) {
        $self->{load_libraries} = [ split ',', $libraries ];
    }

    $self->{argv} = \@ARGV;

    if (@ARGV == 1 && $ARGV[0] =~ /^(?:--version|-V)$/) {
        $self->{version} = 1;
    }

    if (!@ARGV || (@ARGV == 1 && $ARGV[0] =~ /^(?:--help|-h)$/)) {
        $self->{help} = 1;
        $self->{argv} = ['shelly::help'];
    }
}

sub doit {
    my ($self) = @_;

    if ($self->{version}) {
        printf "Shelly ver %s\n", config->{version};
        exit;
    }

    unless ( $self->{lisp_impl} ) {
        $self->{lisp_impl} = $self->detect_installed_lisp;
    }

    local $ENV{LISP_IMPL} = $self->{lisp_impl};

    unless ( impl->('impl_name') ) {
        print "Unsupported CL implementation: @{[ $self->{lisp_impl} ]}\n";
        print qq(It must be one of "sbcl", "ccl", "alisp", "clisp", "cmucl" or "ecl".\n);
        exit 1;
    }


    my $lisp_bin = $ENV{LISP_BINARY} || impl->('binary') || $self->{lisp_impl};
    $ENV{LISP_BINARY} = $lisp_bin;

    my $command = $self->build_command;

    if ( $self->{debug} ) {
        print $command->stringify, "\n";
    }

    local $ENV{SHELLY_VERSION} = config->{version};

    exec($command->arrayfy);
}

sub build_command {
    my ($self) = @_;

    my $task = $self->{argv}->[0] || '';

    return
        $task eq 'install'   ? $self->_build_command_for_install
      : $task eq 'dump-core' ? $self->_build_command_for_dump_core
      : $self->_build_command_for_others;
}

sub _build_command_for_install {
    my ($self) = @_;

    my $command = App::shelly::command->new;

    if (impl->('init_option')) {
        $command->add_option(@{ impl->('init_option') });
    }
    $command->requires_quicklisp;
    $command->load_shelly;
    $command->load_libraries($self->{load_libraries});
    $command->run_shelly_command($self->{argv}, $self->{verbose});

    return $command;
}

sub _build_command_for_dump_core {
    my ($self) = @_;

    my $command = App::shelly::command->new;

    $command->add_option(impl->('noinit_option'));

    $command->load_quicklisp;
    $command->requires_quicklisp;
    $command->load_shelly;
    $command->check_shelly_version;
    $command->load_libraries($self->{load_libraries});
    $command->run_shelly_command($self->{argv}, $self->{verbose});

    return $command;
}

sub _build_command_for_others {
    my ($self) = @_;

    my $command = App::shelly::command->new;

    $command->add_option(impl->('noinit_option'));

    if (!exists $ENV{SHELLY_PATH} && -e dumped_core_path) {
        $command->set_core(dumped_core_path);
    }
    else {
        if (!exists $ENV{SHELLY_PATH} && $self->{lisp_impl} ne 'ecl') {
            print STDERR
                "Warning: Core image wasn't found. It is probably slow, isn't it? Try \"shly dump-core\".\n";
        }

        $command->load_quicklisp;
        $command->requires_quicklisp;
        $command->load_shelly;
    }

    $command->check_shelly_version;
    $command->load_libraries($self->{load_libraries});

    $command->add_eval_option(q{(shelly.util::load-global-shlyfile)});
    $command->add_eval_option(
        defined $self->{shlyfile}
            ? (sprintf q{(shelly.util::load-local-shlyfile #P"%s")}, $self->{shlyfile})
            : q{(shelly.util::load-local-shlyfile)}
    );

    $command->run_shelly_command($self->{argv}, $self->{verbose});

    return $command;
}

sub detect_installed_lisp {
    print "LISP_IMPL isn't set. Auto detecting...\n";

    my (@lisp_impl) =
      grep { which($_) } qw(sbcl ccl alisp clisp cmucl lisp ecl);
    @lisp_impl = map { $_ eq 'lisp' ? 'cmucl' : $_ } @lisp_impl;

    unless (@lisp_impl) {
        print "Couldn't detect installed Lisp.\n";
        exit 1;
    }

    print "Installed Lisp: " . ( join ', ', @lisp_impl ) . "\n";

    if ( @lisp_impl > 1 ) {
        print "Which do you prefer? [@{[ $lisp_impl[0] ]}] : ";

        my $input = <STDIN>;

        {
            no warnings 'uninitialized';
            chomp $input;
        }

        unless ($input) {
            $input = $lisp_impl[0];
            print $input, "\n";
        }

        return $input;
    }

    return $lisp_impl[0];
}

1;

__END__

=head1 NAME

App::shelly

=head1 SYNOPSIS

$ shly [options] [atom...]

=head1 OPTIONS

=over 4

=item B<-h, --help>

Show this help.

=item B<-I, --impl [implementation]>

Tell what Lisp implementation to use. The default is $LISP_IMPL.

=item B<-L, --load [library1,library2,...]>

Load libraries before executing the expression.

=item B<-V, --version>

Print the version of Shelly and exit.

=item B<--verbose>

Print some informations.

=item B<--debug>

This flag is for Shelly developers.

=back

=cut
