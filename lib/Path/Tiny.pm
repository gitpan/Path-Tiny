use v5.10;
use strict;
use warnings;

package Path::Tiny;
# ABSTRACT: File path utility
our $VERSION = '0.003'; # VERSION

# Dependencies
use autodie 2.00;
use Cwd        ();
use Exporter   (qw/import/);
use Fcntl      (qw/:flock SEEK_END/);
use File::Copy ();
use File::stat ();
use File::Path 2.07 ();
use File::Spec 3.40 ();
use File::Temp 0.18 ();

our @EXPORT = qw/path/;

use constant {
    PATH => 0,
    VOL  => 1,
    DIR  => 2,
    FILE => 3,
    TEMP => 4,
};

use overload (
    q{""}    => sub    { $_[0]->[PATH] },
    bool     => sub () { 1 },
    fallback => 1,
);

my $TID = 0; # for thread safe atomic writes

sub CLONE { $TID = threads->tid }; # if cloning, threads should be loaded

#--------------------------------------------------------------------------#
# Constructors
#--------------------------------------------------------------------------#


sub path {
    my $path = shift // ".";
    $path = "." unless length $path;
    # join stringifies any objects, too, which is handy :-)
    $path = join( "/", ( $path eq '/' ? "" : $path ), @_ ) if @_;
    $path = File::Spec->canonpath($path); # ugh, but probably worth it
    $path =~ tr[\\][/];                   # unix convention enforced
    $path =~ s{/$}{} if $path ne "/";     # hack to make splitpath give us a basename
    bless [$path], __PACKAGE__;
}


sub new { path( $_[1] ) }


sub rootdir { path( File::Spec->rootdir ) }


sub tempfile { shift; unshift @_, 'new'; goto &_temp }


sub tempdir { shift; unshift @_, 'newdir'; goto &_temp }

sub _temp {
    my ( $method, @args ) = @_;
    my $temp = File::Temp->$method( TMPDIR => 1, @args );
    close $temp if $method eq 'new'; # so we can unlink it safely if needed
    my $self = path($temp);
    $self->[TEMP] = $temp;           # keep object alive while we are
    return $self;
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
    return path( join "/", $base // Cwd::getcwd, $_[0]->[PATH] );
}


sub append {
    my ( $self, @data ) = @_;
    my $args = ( @data && ref $data[0] eq 'HASH' ) ? shift @data : {};
    my $fh = $self->opena( $args->{binmode} );
    flock( $fh, LOCK_EX );
    seek( $fh, 0, SEEK_END );
    print {$fh} $_ for @data;
    flock( $fh, LOCK_UN );
    close $fh;
}


sub append_utf8 { splice @_, 1, 0, { binmode => ":encoding(UTF-8)" }; goto &append }


sub basename {
    my ($self) = @_;
    $self->_splitpath unless defined $self->[FILE];
    return $self->[FILE];
}


sub child {
    my ( $self, @parts ) = @_;
    my $path = $self->[PATH];
    return path( join( "/", ( $path eq '/' ? "" : $path ), @parts ) );
}


# XXX take a match parameter?  qr or coderef?
sub children {
    my ($self) = @_;
    opendir my $dh, $self->[PATH];
    return map { $self->child($_) } grep { $_ ne '.' && $_ ne '..' } readdir $dh;
}


# XXX do recursively for directories?
sub copy { File::Copy::copy( $_[0]->[PATH], "$_[1]" ) or die "Copy failed: $!" }


sub dirname {
    my ($self) = @_;
    $self->_splitpath unless defined $self->[DIR];
    return length $self->[DIR] ? $self->[DIR] : ".";
}


sub exists { -e $_[0]->[PATH] }


sub filehandle {
    my ( $self, $mode, $binmode ) = @_;
    my $fh;
    if ( 0 && defined $self->[TEMP] ) {
        $fh = $self->[TEMP];
    }
    else {
        $mode //= "<";
        open $fh, $mode, $self->[PATH];
    }
    binmode( $fh, $binmode ) if $binmode;
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
    my $fh    = $self->openr( $args->{binmode} );
    my $chomp = $args->{chomp};
    if ( $args->{count} ) {
        return map { chomp if $chomp; $_ } map { scalar <$fh> } 1 .. $args->{count};
    }
    else {
        return map { chomp if $chomp; $_ } <$fh>;
    }
}


sub lines_utf8 {
    $_[1] = {} unless ref $_[1] eq 'HASH';
    $_[1]->{binmode} = ":encoding(UTF-8)";
    goto &lines;
}


sub lstat { File::stat::lstat( $_[0]->[PATH] ) }


sub mkpath {
    my ( $self, $opts ) = @_;
    return File::Path::make_path( $self->[PATH], ref($opts) eq 'HASH' ? $opts : () );
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
    *{$k} = sub { $_[0]->filehandle( $v, $_[1] ) };
    *{ $k . "_utf8" } = sub { $_[0]->filehandle( $v, ":encoding(UTF-8)" ) };
}


sub parent {
    my ($self) = @_;
    $self->_splitpath unless defined $self->[FILE];
    if ( length $self->[FILE] ) {
        if ( $self->[FILE] eq '.' || $self->[FILE] =~ /\.\./ ) {
            return path( $self->[PATH] . "/.." );
        }
        else {
            return path( $self->[VOL] . $self->[DIR] );
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
        return path( $self->[VOL] );
    }
}


# Easy to get wrong, so wash it through File::Spec (sigh)
sub relative {
    my ( $self, $base ) = @_;
    return path( File::Spec->abs2rel( $self->[PATH], $base ) );
}


sub remove {
    my ( $self, $opts ) = @_;
    if ( -d $self->[PATH] ) {
        return File::Path::remove_tree( $self->[PATH], ref($opts) eq 'HASH' ? $opts : () );
    }
    else {
        return ( -e $self->[PATH] ) ? unlink $self->[PATH] : 1;
    }
}


sub slurp {
    my ( $self, $args ) = @_;
    $args = {} unless ref $args eq 'HASH';
    my $fh = $self->openr( $args->{binmode} );
    local $/;
    return scalar <$fh>;
}


sub slurp_utf8 { push @_, { binmode => ":encoding(UTF-8)" }; goto &slurp }


sub spew {
    my ( $self, @data ) = @_;
    my $args = ( @data && ref $data[0] eq 'HASH' ) ? shift @data : {};
    my $temp = path( $self->[PATH] . $TID . $$ );
    my $fh   = $temp->openw( $args->{binmode} );
    print {$fh} $_ for @data;
    close $fh;
    $temp->move( $self->[PATH] );
}


sub spew_utf8 { splice @_, 1, 0, { binmode => ":encoding(UTF-8)" }; goto &spew }


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

version 0.003

=head1 SYNOPSIS

  use Path::Tiny;

  # creating Path::Tiny objects

  $dir = path("/tmp");
  $foo = path("foo.txt");

  $subdir = $dir->child("foo");
  $bar = $subdir->child("bar.txt");

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

=head1 CONSTRUCTORS

=head2 path

    $path = path("foo/bar");
    $path = path("/tmp/file.txt");
    $path = path(); # like path(".")

Constructs a C<Path::Tiny> object.  It doesn't matter if you give a file or
directory path.  It's still up to you to call directory-like methods only on
directories and file-like methods only on files.  This function is exported
automatically by default.

=head2 new

    $path = Path::Tiny->new("foo/bar");

This is just like C<path>, but with method call overhead.  (Why would you
do that?)

=head2 rootdir

    $path = Path::Tiny->rootdir; # /

Gives you C<< File::Spec->rootdir >> as a C<Path::Tiny> object if you're too
picky for C<path("/")>.

=head2 tempfile

    $temp = Path::Tiny->tempfile( @options );

This passes the options to C<< File::Temp->new >> and returns a C<Path::Tiny>
object with the file name.  If you want a template, you must use a C<TEMPLATE>
named argument.  The C<TMPDIR> option is enabled by default.

The resulting C<File::Temp> object is cached. When the C<Path::Tiny> object is
destroyed, the C<File::Temp> object will be as well.

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

=head2 append

    path("foo.txt")->append(@data);
    path("foo.txt")->append({binmode => ":raw"}, @data);

Appends data to a file.  The file is locked with C<flock> prior to writing.  An
optional hash reference may be used to pass options.  The only option is
C<binmode>, which is passed to C<binmode()> on the handle used for writing.

=head2 append_utf8

    path("foo.txt")->append_utf8(@data);

This is like C<append> with a C<binmode> of C<:encoding(UTF-8)>.

=head2 basename

    $name = path("foo/bar.txt")->basename; # bar.txt

Returns the file portion or last directory portion of a path.

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
is given, it is passed to C<binmode> on the handle.

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

=head2 lines_utf8

    @contents = path("/tmp/foo.txt")->lines_utf8;
    @contents = path("/tmp/foo.txt")->lines({chomp => 1});

This is like C<lines> with a C<binmode> of C<:encoding(UTF-8)>.

=head2 lstat

    $stat = path("/some/symlink")->lstat;

Like calling C<lstat> from L<File::stat>.

=head2 mkpath

    path("foo/bar/baz")->mkpath;
    path("foo/bar/baz")->mkpath( \%options );

Like calling C<make_path> from L<File::Path>.  An optional hash reference
is passed through to C<make_path>.

=head2 move

    path("foo.txt")->move("bar.txt");

Just like C<rename>.

=head2 openr, openw, openrw, opena

    $fh = path("foo.txt")->openr($binmode);  # read
    $fh = path("foo.txt")->openr_utf8;

    $fh = path("foo.txt")->openw($binmode);  # write
    $fh = path("foo.txt")->openw_utf8;

    $fh = path("foo.txt")->opena($binmode);  # append
    $fh = path("foo.txt")->opena_utf8;

    $fh = path("foo.txt")->openrw($binmode); # read/write
    $fh = path("foo.txt")->openrw_utf8;

Returns a file handle opened in the specified mode.  The C<openr> style
methods take a single C<binmode> argument.  All of the C<open*> methods have
C<open*_utf8> equivalents that use C<:encoding(UTF-8)>.

=head2 parent

    $parent = path("foo/bar/baz")->parent; # foo/bar
    $parent = path("foo/wibble.txt")->parent; # foo

Returns a C<Path::Tiny> object corresponding to the parent
directory of the original directory or file.

=head2 relative

    $rel = path("/tmp/foo/bar")->relative("/tmp"); # foo/bar

Returns a C<Path::Tiny> object with a relative path name.
Given the trickiness of this, it's a thin wrapper around
C<< File::Spec->abs2rel() >>.

=head2 remove

    # directory
    path("foo/bar/baz")->remove;
    path("foo/bar/baz")->remove( \%options );

    # file
    path("foo.txt")->remove;

For directories, this is like like calling C<remove_tree> from L<File::Path>.  An
optional hash reference is passed through to C<remove_tree>.

For files, the file is unlinked if it exists.  Unlike C<unlink>, if the file
does not exist, this silently does nothing and returns a true value anyway.

=head2 slurp

    $data = path("foo.txt")->slurp;
    $data = path("foo.txt")->slurp( {binmode => ":raw"} );

Reads file contents into a scalar.  Takes an optional hash reference may be
used to pass options.  The only option is C<binmode>, which is passed to
C<binmode()> on the handle used for reading.

=head2 slurp_utf8

    $data = path("foo.txt")->slurp_utf8;

This is like C<slurp> with a C<binmode> of C<:encoding(UTF-8)>.

=head2 spew

    path("foo.txt")->spew(@data);
    path("foo.txt")->spew({binmode => ":raw"}, @data);

Writes data to a file atomically.  The file is written to a temporary file in
the same directory, then renamed over the original.  An optional hash reference
may be used to pass options.  The only option is C<binmode>, which is passed to
C<binmode()> on the handle used for writing.

=head2 spew_utf8

    path("foo.txt")->spew_utf8(@data);

This is like C<spew> with a C<binmode> of C<:encoding(UTF-8)>.

=head2 stat

    $stat = path("foo.txt")->stat;

Like calling C<stat> from L<File::stat>.

=head2 stringify

    $path = path("foo.txt");
    say $path->stringify; # same as "$path"

Returns a string representation of the path.

=head2 touch

    path("foo.txt")->touch;

Like the Unix C<touch> utility.  Creates the file if it doesn't exist, or else
changes the modification and access times to the current time.

=head2 volume

    $vol = path("/tmp/foo.txt")->volume;

Returns the volume portion of the path.  This is equivalent
equivalent to what L<File::Spec> would give from C<splitpath> and thus
usually is the empty string on Unix-like operating systems.

=for Pod::Coverage openr_utf8 opena_utf8 openw_utf8 openrw_utf8

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
