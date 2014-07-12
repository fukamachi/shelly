package App::shelly;

use strict;
use warnings;

use Getopt::Long qw(:config gnu_getopt pass_through require_order);

use App::shelly::config qw(config lisp_impl dumped_core_path local_path);
use App::shelly::command;

sub new {
    my ($class) = @_;

    return bless {
        load_path      => [],
        load_libraries => [],
        argv           => [],
    }, $class;
}

sub parse_options {
    my ( $self, @argv ) = @_;

    local @ARGV = @{ $self->{argv} };
    push @ARGV, @argv;

    GetOptions(
        'I=s@'      => \$self->{load_path},
        'load|L=s@' => \$self->{load_libraries},
        'file|f=s'  => \$self->{shlyfile},
        'verbose'   => \$self->{verbose},
        'debug'     => \$self->{debug},
    );

    $self->{argv} = \@ARGV;

    if (@ARGV == 1 && $ARGV[0] =~ /^(?:--version|-V)$/) {
        $self->{version} = 1;
    }

    if (!@ARGV || (@ARGV == 1 && $ARGV[0] =~ /^(?:--help|-h)$/)) {
        $self->{help} = 1;
        $self->{argv} = ['shelly::help'];
    }
}

sub task {
    return $_[0]->{argv}->[0];
}

sub doit {
    my ($self) = @_;

    if ($self->{version}) {
        printf "Shelly ver %s\n", config->{version};
        exit;
    }

    unless (exists $ENV{CIM_HOME}) {
        print STDERR "Error: CIM_HOME is not set.\nIf you've never installed CIM, install it from https://github.com/KeenS/CIM first.\n";
        exit 1;
    }

    my $command = $self->build_command;

    if ( $self->{debug} ) {
        print $command->stringify, "\n";
    }

    local $ENV{SHELLY_VERSION} = config->{version};

    exec($command->arrayfy);
}

sub build_command {
    my ($self) = @_;

    my $task = $self->task;

    return
        $task eq 'install'   ? $self->_build_command_for_install
      : $task eq 'dump-core' ? $self->_build_command_for_dump_core
      : $self->_build_command_for_others;
}

sub _build_command_for_dump_core {
    my ($self) = @_;

    my $command = App::shelly::command->new(verbose => $self->{verbose});

    $command->load_shelly;
    $command->check_shelly_version;
    $command->load_libraries($self->{load_libraries});
    $command->run_shelly_command($self->{argv});

    return $command;
}

sub _build_command_for_install {
    my ($self) = @_;

    my $command = App::shelly::command->new(verbose => $self->{verbose});

    $command->load_shelly;
    $command->load_libraries($self->{load_libraries});
    $command->run_shelly_command($self->{argv});

    return $command;
}

sub _build_command_for_others {
    my ($self) = @_;

    my $command = App::shelly::command->new(verbose => $self->{verbose});

    if (!exists $ENV{SHELLY_PATH} && -e dumped_core_path) {
        $command->add_option('--core', dumped_core_path);
        $command->add_eval_option(q{(shelly.util:shadowing-use-package :shelly)});
    }
    else {
        if (!exists $ENV{SHELLY_PATH} && lisp_impl =~ /^(?:sbcl|clisp|ccl|alisp)/) {
            print STDERR
                "Warning: Core image wasn't found for @{[ lisp_impl ]}. It is probably slow, isn't it? Try \"shly dump-core\".\n";
        }

        $command->load_shelly;
    }

    $command->check_shelly_version;
    $command->add_load_path($self->{load_path});
    $command->load_libraries($self->{load_libraries});

    $command->add_eval_option(q{(shelly.util::load-global-shlyfile)});
    $command->add_eval_option(
        defined $self->{shlyfile}
            ? (sprintf q{(shelly.util::load-local-shlyfile #P"%s")}, $self->{shlyfile})
            : q{(shelly.util::load-local-shlyfile)}
    );

    $command->run_shelly_command($self->{argv});

    return $command;
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

=item B<-I [directory]>

Specify asdf:*central-registry* directory (several -I's allowed).

=item B<-L, --load [library]>

Specify a library to be loaded before executing the expression (several -L's allowed).

=item B<-V, --version>

Print the version of Shelly and exit.

=item B<--verbose>

Print some informations.

=item B<--debug>

This flag is for Shelly developers.

=back

=cut
