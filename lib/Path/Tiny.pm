use 5.008001;
use strict;
use warnings;

package Path::Tiny;
# ABSTRACT: File path utility
our $VERSION = '0.013'; # VERSION

# Dependencies
use autodie 2.14; # autodie::skip support
use Exporter 5.57   (qw/import/);
use File::Spec 3.40 ();
use File::Temp 0.18 ();
use Carp       ();
use Cwd        ();
use Fcntl      (qw/:flock SEEK_END/);
use File::Copy ();
use File::stat ();
{ no warnings; use File::Path 2.07 (); } # avoid "2.07_02 isn't numeric"

our @EXPORT = qw/path/;

use constant {
    PATH  => 0,
    CANON => 1,
    VOL   => 2,
    DIR   => 3,
    FILE  => 4,
    TEMP  => 5,
};

use overload (
    q{""}    => sub    { $_[0]->[PATH] },
    bool     => sub () { 1 },
    fallback => 1,
);

my $TID = 0; # for thread safe atomic writes

sub CLONE { $TID = threads->tid }; # if cloning, threads should be loaded

sub DOES { return $_[1] eq 'autodie::skip' } # report errors like croak

my $HAS_UU;                                  # has Unicode::UTF8; lazily populated

sub _check_UU {
    eval { require Unicode::UTF8; Unicode::UTF8->VERSION(0.58); 1 };
}

#--------------------------------------------------------------------------#
# Constructors
#--------------------------------------------------------------------------#


sub path {
    my $path = shift;
    Carp::croak("path() requires a defined, positive-length argument")
      unless defined $path && length $path;
    # join stringifies any objects, too, which is handy :-)
    $path = join( "/", ( $path eq '/' ? "" : $path ), @_ ) if @_;
    my $cpath = $path = File::Spec->canonpath($path); # ugh, but probably worth it
    $path =~ tr[\\][/];                               # unix convention enforced
    $path =~ s{/$}{} if $path ne "/"; # hack to make splitpath give us a basename
    bless [ $path, $cpath ], __PACKAGE__;
}


sub new { shift; path(@_) }


sub cwd { shift; path(Cwd::getcwd) }


sub rootdir { path( File::Spec->rootdir ) }


sub tempfile {
    my $class = shift;
    my ( $maybe_template, $args ) = _parse_file_temp_args(@_);
    # File::Temp->new demands TEMPLATE
    $args->{TEMPLATE} = $maybe_template->[0] if @$maybe_template;
    my $temp = File::Temp->new( TMPDIR => 1, %$args );
    close $temp;
    my $self = path($temp)->absolute;
    $self->[TEMP] = $temp; # keep object alive while we are
    return $self;
}


sub tempdir {
    my $class = shift;
    my ( $maybe_template, $args ) = _parse_file_temp_args(@_);
    # File::Temp->newdir demands leading template
    my $temp = File::Temp->newdir( @$maybe_template, TMPDIR => 1, %$args );
    my $self = path($temp)->absolute;
    $self->[TEMP] = $temp; # keep object alive while we are
    return $self;
}

# normalize the various ways File::Temp does templates
sub _parse_file_temp_args {
    my $leading_template = ( scalar(@_) % 2 == 1 ? shift(@_) : '' );
    my %args = @_;
    %args = map { uc($_), $args{$_} } keys %args;
    my @template = (
          exists $args{TEMPLATE} ? delete $args{TEMPLATE}
        : $leading_template      ? $leading_template
        :                          ()
    );
    return ( \@template, \%args );
}

#--------------------------------------------------------------------------#
# Private methods
#--------------------------------------------------------------------------#

sub _splitpath {
    my ($self) = @_;
    @{$self}[ VOL, DIR, FILE ] = File::Spec->splitpath( $self->[PATH] );
}

#--------------------------------------------------------------------------#
# Public methods
#--------------------------------------------------------------------------#


sub absolute {
    my ( $self, $base ) = @_;
    return $self if $self->is_absolute;
    return path( join "/", ( defined($base) ? $base : Cwd::getcwd ), $_[0]->[PATH] );
}


sub append {
    my ( $self, @data ) = @_;
    my $args = ( @data && ref $data[0] eq 'HASH' ) ? shift @data : {};
    my $binmode = $args->{binmode};
    $binmode = ( ( caller(0) )[10] || {} )->{'open>'} unless defined $binmode;
    my $fh = $self->filehandle( ">>", $binmode );
    flock( $fh, LOCK_EX );
    seek( $fh, 0, SEEK_END ); # ensure SEEK_END after flock
    print {$fh} map { ref eq 'ARRAY' ? @$_ : $_ } @data;
    close $fh;                # force immediate flush
}


sub append_raw { splice @_, 1, 0, { binmode => ":unix" }; goto &append }


sub append_utf8 {
    if ( defined($HAS_UU) ? $HAS_UU : $HAS_UU = _check_UU() ) {
        my $self = shift;
        append( $self, { binmode => ":unix" }, map { Unicode::UTF8::encode_utf8($_) } @_ );
    }
    else {
        splice @_, 1, 0, { binmode => ":unix:encoding(UTF-8)" };
        goto &append;
    }
}


sub basename {
    my ($self) = @_;
    $self->_splitpath unless defined $self->[FILE];
    return $self->[FILE];
}


sub canonpath { $_[0]->[CANON] }


sub child {
    my ( $self, @parts ) = @_;
    my $path = $self->[PATH];
    return path( join( "/", ( $path eq '/' ? "" : $path ), @parts ) );
}


# XXX take a match parameter?  qr or coderef?
sub children {
    my ($self) = @_;
    opendir my $dh, $self->[PATH];
    return
      map { path( $self->[PATH] . "/$_" ) } grep { $_ ne '.' && $_ ne '..' } readdir $dh;
}


# XXX do recursively for directories?
sub copy {
    File::Copy::copy( $_[0]->[PATH], "$_[1]" ) or Carp::croak("copy failed: $!");
}


sub dirname {
    my ($self) = @_;
    $self->_splitpath unless defined $self->[DIR];
    return length $self->[DIR] ? $self->[DIR] : ".";
}


sub exists { -e $_[0]->[PATH] }


# Note: must put binmode on open line, not subsequent binmode() call, so things
# like ":unix" actually stop perlio/crlf from being added

sub filehandle {
    my ( $self, $mode, $binmode ) = @_;
    $mode    = "<" unless defined $mode;
    $binmode = ""  unless defined $binmode;
    open my $fh, "$mode$binmode", $self->[PATH];
    return $fh;
}


sub is_absolute { substr( $_[0]->dirname, 0, 1 ) eq '/' }


sub is_dir { -d $_[0]->[PATH] }


sub is_file { -f $_[0]->[PATH] }


sub is_relative { substr( $_[0]->dirname, 0, 1 ) ne '/' }


sub iterator {
    my ($self) = @_;
    opendir( my $dh, $self->[PATH] );
    return sub {
        return unless $dh;
        my $next;
        while ( defined( $next = readdir $dh ) ) {
            return $self->child($next) if $next ne '.' && $next ne '..';
        }
        undef $dh;
        return;
    };
}


sub lines {
    my ( $self, $args ) = @_;
    $args = {} unless ref $args eq 'HASH';
    my $binmode = $args->{binmode};
    $binmode = ( ( caller(0) )[10] || {} )->{'open<'} unless defined $binmode;
    my $fh = $self->filehandle( "<", $binmode );
    flock( $fh, LOCK_SH );
    my $chomp = $args->{chomp};
    my @lines;
    # XXX more efficient to read @lines then chomp(@lines) vs map?
    if ( $args->{count} ) {
        return map { chomp if $chomp; $_ } map { scalar <$fh> } 1 .. $args->{count};
    }
    elsif ($chomp) {
        return map { chomp; $_ } <$fh>;
    }
    else {
        return <$fh>;
    }
}


sub lines_raw {
    $_[1] = {} unless ref $_[1] eq 'HASH';
    if ( $_[1]->{chomp} && !$_[1]->{count} ) {
        return split /\n/, slurp_raw( $_[0] ); ## no critic
    }
    else {
        $_[1]->{binmode} = ":raw";
        goto &lines;
    }
}


sub lines_utf8 {
    $_[1] = {} unless ref $_[1] eq 'HASH';
    if (   ( defined($HAS_UU) ? $HAS_UU : $HAS_UU = _check_UU() )
        && $_[1]->{chomp}
        && !$_[1]->{count} )
    {
        return split /\n/, slurp_utf8( $_[0] ); ## no critic
    }
    else {
        $_[1]->{binmode} = ":raw:encoding(UTF-8)";
        goto &lines;
    }
}


sub lstat { File::stat::lstat( $_[0]->[PATH] ) }


sub mkpath {
    my ( $self, $args ) = @_;
    $args = {} unless ref $args eq 'HASH';
    my $err;
    $args->{err} = \$err unless defined $args->{err};
    my @dirs = File::Path::make_path( $self->[PATH], $args );
    if ( $err && @$err ) {
        my ( $file, $message ) = %{ $err->[0] };
        Carp::croak("mkpath failed for $file: $message");
    }
    return @dirs;
}


sub move { rename $_[0]->[PATH], $_[1] }


my %opens = (
    opena  => ">>",
    openr  => "<",
    openw  => ">",
    openrw => "+<"
);

while ( my ( $k, $v ) = each %opens ) {
    no strict 'refs';
    # must check for lexical IO mode hint
    *{$k} = sub {
        my ( $self, $binmode ) = @_;
        $binmode = ( ( caller(0) )[10] || {} )->{ 'open' . substr( $v, -1, 1 ) }
          unless defined $binmode;
        $self->filehandle( $v, $binmode );
    };
    *{ $k . "_raw" }  = sub { $_[0]->filehandle( $v, ":raw" ) };
    *{ $k . "_utf8" } = sub { $_[0]->filehandle( $v, ":raw:encoding(UTF-8)" ) };
}


# XXX this is ugly and coverage is incomplete.  I think it's there for windows
# so need to check coverage there and compare
sub parent {
    my ($self) = @_;
    $self->_splitpath unless defined $self->[FILE];
    if ( length $self->[FILE] ) {
        if ( $self->[FILE] eq '.' || $self->[FILE] =~ /\.\./ ) {
            return path( $self->[PATH] . "/.." );
        }
        else {
            return path( _non_empty( $self->[VOL] . $self->[DIR] ) );
        }
    }
    elsif ( length $self->[DIR] ) {
        if ( $self->[DIR] =~ /\.\./ ) {
            return path( $self->[VOL] . $self->[DIR] . "/.." );
        }
        else {
            return path("/") if $self->[DIR] eq "/";
            ( my $dir = $self->[DIR] ) =~ s{/[^\/]+/$}{/};
            return path( $self->[VOL] . $dir );
        }
    }
    else {
        return path( _non_empty( $self->[VOL] ) );
    }
}

sub _non_empty {
    my ($string) = shift;
    return ( ( defined($string) && length($string) ) ? $string : "." );
}


sub realpath { return path( Cwd::realpath( $_[0]->[PATH] ) ) }


# Easy to get wrong, so wash it through File::Spec (sigh)
sub relative { path( File::Spec->abs2rel( $_[0]->[PATH], $_[1] ) ) }


sub remove { return -e $_[0]->[PATH] ? unlink $_[0]->[PATH] : 0 }


sub remove_tree {
    my ( $self, $args ) = @_;
    return 0 unless -e $self->[PATH];
    $args = {} unless ref $args eq 'HASH';
    my $err;
    $args->{err}  = \$err unless defined $args->{err};
    $args->{safe} = 1     unless defined $args->{safe};
    my $count = File::Path::remove_tree( $self->[PATH], $args );
    if ( $err && @$err ) {
        my ( $file, $message ) = %{ $err->[0] };
        Carp::croak("remove_tree failed for $file: $message");
    }
    return $count;
}


sub slurp {
    my ( $self, $args ) = @_;
    $args = {} unless ref $args eq 'HASH';
    my $binmode = $args->{binmode};
    $binmode = ( ( caller(0) )[10] || {} )->{'open<'} unless defined $binmode;
    my $fh = $self->filehandle( "<", $binmode );
    flock( $fh, LOCK_SH );
    if ( ( defined($binmode) ? $binmode : "" ) eq ":unix"
        and my $size = -s $fh )
    {
        my $buf;
        read $fh, $buf, $size; # File::Slurp in a nutshell
        return $buf;
    }
    else {
        local $/;
        return scalar <$fh>;
    }
}


sub slurp_raw { $_[1] = { binmode => ":unix" }; goto &slurp }


sub slurp_utf8 {
    if ( defined($HAS_UU) ? $HAS_UU : $HAS_UU = _check_UU() ) {
        return Unicode::UTF8::decode_utf8( slurp( $_[0], { binmode => ":unix" } ) );
    }
    else {
        $_[1] = { binmode => ":raw:encoding(UTF-8)" };
        goto &slurp;
    }
}


# XXX add "unsafe" option to disable flocking and atomic?  Check benchmarks on append() first.
sub spew {
    my ( $self, @data ) = @_;
    my $args = ( @data && ref $data[0] eq 'HASH' ) ? shift @data : {};
    my $binmode = $args->{binmode};
    $binmode = ( ( caller(0) )[10] || {} )->{'open>'} unless defined $binmode;
    my $temp = path( $self->[PATH] . $TID . $$ );
    my $fh = $temp->filehandle( ">", $binmode );
    flock( $fh, LOCK_EX );
    seek( $fh, 0, 0 );
    truncate( $fh, 0 );
    print {$fh} map { ref eq 'ARRAY' ? @$_ : $_ } @data;
    flock( $fh, LOCK_UN );
    close $fh;
    $temp->move( $self->[PATH] );
}


sub spew_raw { splice @_, 1, 0, { binmode => ":unix" }; goto &spew }


sub spew_utf8 {
    if ( defined($HAS_UU) ? $HAS_UU : $HAS_UU = _check_UU() ) {
        my $self = shift;
        spew( $self, { binmode => ":unix" }, map { Unicode::UTF8::encode_utf8($_) } @_ );
    }
    else {
        splice @_, 1, 0, { binmode => ":unix:encoding(UTF-8)" };
        goto &spew;
    }
}


# XXX break out individual stat() components as subs?
sub stat { File::stat::stat( $_[0]->[PATH] ) }


sub stringify { $_[0]->[PATH] }


sub touch {
    my ($self) = @_;
    if ( -e $self->[PATH] ) {
        my $now = time();
        utime $now, $now, $self->[PATH];
    }
    else {
        close $self->openw;
    }
}


sub touchpath {
    my ($self) = @_;
    my $parent = $self->parent;
    $parent->mkpath unless $parent->exists;
    $self->touch;
}


sub volume {
    my ($self) = @_;
    $self->_splitpath unless defined $self->[VOL];
    return $self->[VOL];
}

1;


# vim: ts=4 sts=4 sw=4 et:

__END__

=pod

=head1 NAME

Path::Tiny - File path utility

=head1 VERSION

version 0.013

=head1 SYNOPSIS

  use Path::Tiny;

  # creating Path::Tiny objects

  $dir = path("/tmp");
  $foo = path("foo.txt");

  $subdir = $dir->child("foo");
  $bar = $subdir->child("bar.txt");

  # stringifies as cleaned up path

  $file = path("./foo.txt");
  print $file; # "foo.txt"

  # reading files

  $guts = $file->slurp;
  $guts = $file->slurp_utf8;

  @lines = $file->lines;
  @lines = $file->lines_utf8;

  $head = $file->lines( {count => 1} );

  # writing files

  $bar->spew( @data );
  $bar->spew_utf8( @data );

  # reading directories

  for ( $dir->children ) { ... }

  $iter = $dir->iterator;
  while ( my $next = $iter->() ) { ... }

=head1 DESCRIPTION

This module attempts to provide a small, fast utility for working with
file paths.  It is friendlier to use than L<File::Spec> and provides
easy access to functions from several other core file handling modules.

It doesn't attempt to be as full-featured as L<IO::All> or L<Path::Class>,
nor does it try to work for anything except Unix-like and Win32 platforms.
Even then, it might break if you try something particularly obscure or
tortuous.  (Quick!  What does this mean: C<< ///../../..//./././a//b/.././c/././ >>?
And how does it differ on Win32?)

All paths are forced to have Unix-style forward slashes.  Stringifying
the object gives you back the path (after some clean up).

File input/output methods C<flock> handles before reading or writing,
as appropriate.

The C<*_utf8> methods (C<slurp_utf8>, C<lines_utf8>, etc.) operate in raw
mode without CRLF translation.  Installing L<Unicode::UTF8> 0.58 or later
will speed up several of them and is highly recommended.

It uses L<autodie> internally, so most failures will be thrown as exceptions.

=head1 CONSTRUCTORS

=head2 path

    $path = path("foo/bar");
    $path = path("/tmp", "file.txt"); # list
    $path = path(".");                # cwd

Constructs a C<Path::Tiny> object.  It doesn't matter if you give a file or
directory path.  It's still up to you to call directory-like methods only on
directories and file-like methods only on files.  This function is exported
automatically by default.

The first argument must be defined and have non-zero length or an exception
will be thrown.  This prevents subtle, dangerous errors with code like
C<< path( maybe_undef() )->remove_tree >>.

=head2 new

    $path = Path::Tiny->new("foo/bar");

This is just like C<path>, but with method call overhead.  (Why would you
do that?)

=head2 cwd

    $path = Path::Tiny->cwd; # path( Cwd::getcwd )

Gives you the absolute path to the current directory as a C<Path::Tiny> object.
This is slightly faster than C<< path(".")->absolute >>.

=head2 rootdir

    $path = Path::Tiny->rootdir; # /

Gives you C<< File::Spec->rootdir >> as a C<Path::Tiny> object if you're too
picky for C<path("/")>.

=head2 tempfile

    $temp = Path::Tiny->tempfile( @options );

This passes the options to C<< File::Temp->new >> and returns a C<Path::Tiny>
object with the file name.  The C<TMPDIR> option is enabled by default.

The resulting C<File::Temp> object is cached. When the C<Path::Tiny> object is
destroyed, the C<File::Temp> object will be as well.

C<File::Temp> annoyingly requires you to specify a custom template in slightly
different ways depending on which function or method you call, but
C<Path::Tiny> lets you ignore that and can take either a leading template or a
C<TEMPLATE> option and does the right thing.

    $temp = Path::Tiny->tempfile( "customXXXXXXXX" );             # ok
    $temp = Path::Tiny->tempfile( TEMPLATE => "customXXXXXXXX" ); # ok

The tempfile path object will normalized to have an absolute path, even if
created in a relative directory using C<DIR>.

=head2 tempdir

    $temp = Path::Tiny->tempdir( @options );

This is just like C<tempfile>, except it calls C<< File::Temp->newdir >> instead.

=head1 METHODS

=head2 absolute

    $abs = path("foo/bar")->absolute;
    $abs = path("foo/bar")->absolute("/tmp");

Returns a new C<Path::Tiny> object with an absolute path.  Unless
an argument is given, the current directory is used as the absolute base path.
The argument must be absolute or you won't get an absolute result.

This will not resolve upward directories ("foo/../bar") unless C<canonpath>
in L<File::Spec> would normally do so on your platform.  If you need them
resolved, you must call the more expensive C<realpath> method instead.

=head2 append

    path("foo.txt")->append(@data);
    path("foo.txt")->append(\@data);
    path("foo.txt")->append({binmode => ":raw"}, @data);

Appends data to a file.  The file is locked with C<flock> prior to writing.  An
optional hash reference may be used to pass options.  The only option is
C<binmode>, which is passed to C<binmode()> on the handle used for writing.

=head2 append_raw

    path("foo.txt")->append_raw(@data);

This is like C<append> with a C<binmode> of C<:unix> for fast, unbuffered, raw write.

=head2 append_utf8

    path("foo.txt")->append_utf8(@data);

This is like C<append> with a C<binmode> of C<:unix:encoding(UTF-8)>.

If L<Unicode::UTF8> 0.58+ is installed, a raw append will be done instead on the data
encoded with C<Unicode::UTF8>.

=head2 basename

    $name = path("foo/bar.txt")->basename; # bar.txt

Returns the file portion or last directory portion of a path.

=head2 canonpath

    $canonical = path("foo/bar")->canonpath; # foo\bar on Windows

Returns a string with the canonical format of the path name for
the platform.  In particular, this means directory separators
will be C<\> on Windows.

=head2 child

    $file = path("/tmp")->child("foo.txt"); # "/tmp/foo.txt"
    $file = path("/tmp")->child(@parts);

Returns a new C<Path::Tiny> object relative to the original.  Works
like C<catfile> or C<catdir> from File::Spec, but without caring about
file or directories.

=head2 children

    @paths = path("/tmp")->children;

Returns a list of C<Path::Tiny> objects for all file and directories
within a directory.  Excludes "." and ".." automatically.

=head2 copy

    path("/tmp/foo.txt")->copy("/tmp/bar.txt");

Copies a file using L<File::Copy>'s C<copy> function.

=head2 dirname

    $name = path("/tmp/foo.txt")->dirname; # "/tmp/"

Returns the directory name portion of the path.  This is roughly
equivalent to what L<File::Spec> would give from C<splitpath> and thus
usually has the trailing slash. If that's not desired, stringify directories
or call C<parent> on files.

=head2 exists

    if ( path("/tmp")->exists ) { ... }

Just like C<-e>.

=head2 filehandle

    $fh = path("/tmp/foo.txt")->filehandle($mode, $binmode);

Returns an open file handle.  The C<$mode> argument must be a Perl-style
read/write mode string ("<" ,">", "<<", etc.).  If a C<$binmode>
is given, it is set during the C<open> call.

See C<openr>, C<openw>, C<openrw>, and C<opena> for sugar.

=head2 is_absolute

    if ( path("/tmp")->is_absolute ) { ... }

Boolean for whether the path appears absolute or not.

=head2 is_dir

    if ( path("/tmp")->is_dir ) { ... }

Just like C<-d>.  This means it actually has to exist on the filesystem.
Until then, it's just a path.

=head2 is_file

    if ( path("/tmp")->is_file ) { ... }

Just like C<-f>.  This means it actually has to exist on the filesystem.
Until then, it's just a path.

=head2 is_relative

    if ( path("/tmp")->is_relative ) { ... }

Boolean for whether the path appears relative or not.

=head2 iterator

    $iter = path("/tmp")->iterator;
    while ( $path = $iter->() ) {
        ...
    }

Returns a code reference that walks a directory lazily.  Each invocation
returns a C<Path::Tiny> object or undef when the iterator is exhausted.

This iterator is B<not> recursive.  For recursive iteration, use
L<Path::Iterator::Rule> instead.

=head2 lines

    @contents = path("/tmp/foo.txt")->lines;
    @contents = path("/tmp/foo.txt")->lines(\%options);

Returns a list of lines from a file.  Optionally takes a hash-reference of
options.  Valid options are C<binmode>, C<count> and C<chomp>.  If C<binmode>
is provided, it will be set on the handle prior to reading.  If C<count> is
provided, up to that many lines will be returned. If C<chomp> is set, lines
will be chomped before being returned.

Because the return is a list, C<lines> in scalar context will return the number
of lines (and throw away the data).

    $number_of_lines = path("/tmp/foo.txt")->lines;

=head2 lines_raw

    @contents = path("/tmp/foo.txt")->lines_raw;

This is like C<lines> with a C<binmode> of C<:raw>.  We use C<:raw> instead
of C<:unix> so PerlIO buffering can manage reading by line.

=head2 lines_utf8

    @contents = path("/tmp/foo.txt")->lines_utf8;

This is like C<lines> with a C<binmode> of C<:raw:encoding(UTF-8)>.

If L<Unicode::UTF8> 0.58+ is installed, a raw UTF-8 slurp will be done and then the
lines will be split.  This is actually faster than relying on C<:encoding(UTF-8)>,
though a bit memory intensive.  If memory use is a concern, consider C<openr_utf8>
and iterating directly on the handle.

=head2 lstat

    $stat = path("/some/symlink")->lstat;

Like calling C<lstat> from L<File::stat>.

=head2 mkpath

    path("foo/bar/baz")->mkpath;
    path("foo/bar/baz")->mkpath( \%options );

Like calling C<make_path> from L<File::Path>.  An optional hash reference
is passed through to C<make_path>.  Errors will be trapped and an exception
thrown.  Returns the list of directories created or an empty list if
the directories already exist, just like C<make_path>.

=head2 move

    path("foo.txt")->move("bar.txt");

Just like C<rename>.

=head2 openr, openw, openrw, opena

    $fh = path("foo.txt")->openr($binmode);  # read
    $fh = path("foo.txt")->openr_raw;
    $fh = path("foo.txt")->openr_utf8;

    $fh = path("foo.txt")->openw($binmode);  # write
    $fh = path("foo.txt")->openw_raw;
    $fh = path("foo.txt")->openw_utf8;

    $fh = path("foo.txt")->opena($binmode);  # append
    $fh = path("foo.txt")->opena_raw;
    $fh = path("foo.txt")->opena_utf8;

    $fh = path("foo.txt")->openrw($binmode); # read/write
    $fh = path("foo.txt")->openrw_raw;
    $fh = path("foo.txt")->openrw_utf8;

Returns a file handle opened in the specified mode.  The C<openr> style methods
take a single C<binmode> argument.  All of the C<open*> methods have
C<open*_raw> and C<open*_utf8> equivalents that use C<:raw> and
C<:raw:encoding(UTF-8)>, respectively.

=head2 parent

    $parent = path("foo/bar/baz")->parent; # foo/bar
    $parent = path("foo/wibble.txt")->parent; # foo

Returns a C<Path::Tiny> object corresponding to the parent
directory of the original directory or file.

=head2 realpath

    $real = path("/baz/foo/../bar")->realpath;
    $real = path("foo/../bar")->realpath;

Returns a new C<Path::Tiny> object with all symbolic links and upward directory
parts resolved using L<Cwd>'s C<realpath>.  Compared to C<absolute>, this is
more expensive as it must actually consult the filesystem.

=head2 relative

    $rel = path("/tmp/foo/bar")->relative("/tmp"); # foo/bar

Returns a C<Path::Tiny> object with a relative path name.
Given the trickiness of this, it's a thin wrapper around
C<< File::Spec->abs2rel() >>.

=head2 remove

    path("foo.txt")->remove;

B<Note: as of 0.012, remove only works on files>.

This is just like C<unlink>, except if the path does not exist, it returns
false rather than throwing an exception.

=head2 remove_tree

    # directory
    path("foo/bar/baz")->remove_tree;
    path("foo/bar/baz")->remove_tree( \%options );
    path("foo/bar/baz")->remove_tree( { safe => 0 } ); # force remove

Like calling C<remove_tree> from L<File::Path>, but defaults to C<safe> mode.
An optional hash reference is passed through to C<remove_tree>.  Errors will be
trapped and an exception thrown.  Returns the number of directories deleted,
just like C<remove_tree>.

If you want to remove a directory only if it is empty, use the built-in
C<rmdir> function instead.

    rmdir path("foo/bar/baz/");

=head2 slurp

    $data = path("foo.txt")->slurp;
    $data = path("foo.txt")->slurp( {binmode => ":raw"} );

Reads file contents into a scalar.  Takes an optional hash reference may be
used to pass options.  The only option is C<binmode>, which is passed to
C<binmode()> on the handle used for reading.

=head2 slurp_raw

    $data = path("foo.txt")->slurp_raw;

This is like C<slurp> with a C<binmode> of C<:unix> for
a fast, unbuffered, raw read.

=head2 slurp_utf8

    $data = path("foo.txt")->slurp_utf8;

This is like C<slurp> with a C<binmode> of C<:unix:encoding(UTF-8)>.

If L<Unicode::UTF8> 0.58+ is installed, a raw slurp will be done instead and the
result decoded with C<Unicode::UTF8>.  This is is just as strict and is roughly
an order of magnitude faster than using C<:encoding(UTF-8)>.

=head2 spew

    path("foo.txt")->spew(@data);
    path("foo.txt")->spew(\@data);
    path("foo.txt")->spew({binmode => ":raw"}, @data);

Writes data to a file atomically.  The file is written to a temporary file in
the same directory, then renamed over the original.  An optional hash reference
may be used to pass options.  The only option is C<binmode>, which is passed to
C<binmode()> on the handle used for writing.

=head2 spew_raw

    path("foo.txt")->spew_raw(@data);

This is like C<spew> with a C<binmode> of C<:unix> for a fast, unbuffered, raw write.

=head2 spew_utf8

    path("foo.txt")->spew_utf8(@data);

This is like C<spew> with a C<binmode> of C<:unix:encoding(UTF-8)>.

If L<Unicode::UTF8> 0.58+ is installed, a raw spew will be done instead on the data
encoded with C<Unicode::UTF8>.

=head2 stat

    $stat = path("foo.txt")->stat;

Like calling C<stat> from L<File::stat>.

=head2 stringify

    $path = path("foo.txt");
    say $path->stringify; # same as "$path"

Returns a string representation of the path.  Unlike C<canonpath>, this method
returns the path standardized with Unix-style C</> directory separators.

=head2 touch

    path("foo.txt")->touch;

Like the Unix C<touch> utility.  Creates the file if it doesn't exist, or else
changes the modification and access times to the current time.

=head2 touchpath

    path("bar/baz/foo.txt")->touchpath;

Combines C<mkpath> and C<touch>.  Creates the parent directory if it doesn't exist,
before touching the file.

=head2 volume

    $vol = path("/tmp/foo.txt")->volume;

Returns the volume portion of the path.  This is equivalent
equivalent to what L<File::Spec> would give from C<splitpath> and thus
usually is the empty string on Unix-like operating systems.

=for Pod::Coverage openr_utf8 opena_utf8 openw_utf8 openrw_utf8
openr_raw opena_raw openw_raw openrw_raw
DOES

=head1 CAVEATS

=head2 utf8 vs UTF-8

All the C<*_utf8> methods use C<:encoding(UTF-8)> -- either as
C<:unix:encoding(UTF-8)> (unbuffered) or C<:raw:encoding(UTF-8)> (buffered) --
which is strict against the Unicode spec and disallows illegal Unicode
codepoints or UTF-8 sequences.

Unfortunately, C<:encoding(UTF-8)> is very, very slow.  If you install
L<Unicode::UTF8> 0.58 or later, that module will be used by some C<*_utf8>
methods to encode or decode data after a raw, binary input/output operation,
which is much faster.

If you need the performance and can accept the security risk,
C<< slurp({binmode => ":unix:utf8"}) >> will be faster than C<:unix:encoding(UTF-8)>
(but not as fast as C<Unicode::UTF8>).

Note that the C<*_utf8> methods read in B<raw> mode.  There is no CRLF
translation on Windows.  If you must have CRLF translation, use the regular
input/output methods with an appropriate binmode:

  $path->spew_utf8($data);                            # raw
  $path->spew({binmode => ":encoding(UTF-8)"}, $data; # LF -> CRLF

Consider L<PerlIO::utf8_strict> for a faster L<PerlIO> layer alternative to
C<:encoding(UTF-8)>, though it does not appear to be as fast as the
C<Unicode::UTF8> approach.

=head2 Default IO layers and the open pragma

If you have Perl 5.10 or later, file input/output methods (C<slurp>, C<spew>,
etc.) and high-level handle opening methods ( C<openr>, C<openw>, etc. but not
C<filehandle>) respect default encodings set by the C<-C> switch or lexical
L<open> settings of the caller.  For UTF-8, this is almost certainly slower
than using the dedicated C<_utf8> methods if you have L<Unicode::UTF8>.

=head1 TYPE CONSTRAINTS AND COERCION

A standard L<MooseX::Types> library is available at
L<MooseX::Types::Path::Tiny>.

=head1 SEE ALSO

=over 4

=item *

L<File::Fu>

=item *

L<IO::All>

=item *

L<Path::Class>

=back

Probably others.  Let me know if you want me to add a module to the list.

=head1 BENCHMARKING

I benchmarked a naive file-finding task: finding all C<*.pm> files in C<@INC>.
I tested L<Path::Iterator::Rule> and different subclasses of it that do file
manipulations using file path helpers L<Path::Class>, L<IO::All>, L<File::Fu>
and C<Path::Tiny>.

    Path::Iterator::Rule    0.474s (no objects)
    Path::Tiny::Rule        0.938s (not on CPAN)
    IO::All::Rule           1.355s
    File::Fu::Rule          1.437s (not on CPAN)
    Path::Class::Rule       4.673s

This benchmark heavily stressed object creation and determination of
a file's basename.

=for :stopwords cpan testmatrix url annocpan anno bugtracker rt cpants kwalitee diff irc mailto metadata placeholders metacpan

=head1 SUPPORT

=head2 Bugs / Feature Requests

Please report any bugs or feature requests through the issue tracker
at L<https://github.com/dagolden/path-tiny/issues>.
You will be notified automatically of any progress on your issue.

=head2 Source Code

This is open source software.  The code repository is available for
public review and contribution under the terms of the license.

L<https://github.com/dagolden/path-tiny>

  git clone git://github.com/dagolden/path-tiny.git

=head1 AUTHOR

David Golden <dagolden@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2013 by David Golden.

This is free software, licensed under:

  The Apache License, Version 2.0, January 2004

=cut
