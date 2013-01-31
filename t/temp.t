use 5.008001;
use strict;
use warnings;
use Test::More 0.96;
use Test::Deep '!blessed';
use File::Spec::Unix;

use Path::Tiny;

subtest "tempdir" => sub {
    my $tempdir = Path::Tiny->tempdir;
    my $string  = $tempdir->stringify;
    ok( $tempdir->exists, "tempdir exists" );
    undef $tempdir;
    ok( !-e $string, "tempdir destroyed" );
};

subtest "tempfile" => sub {
    my $tempfile = Path::Tiny->tempfile;
    my $string   = $tempfile->stringify;
    ok( $tempfile->exists, "tempfile exists" );
    undef $tempfile;
    ok( !-e $string, "tempfile destroyed" );
};

subtest "tempfile handle" => sub {
    my $tempfile = Path::Tiny->tempfile;
    my $fh       = $tempfile->filehandle;
    is( ref $tempfile->[4], 'File::Temp', "cached File::Temp object" );
    is( fileno $tempfile->[4], undef, "cached handle is closed" );
};

done_testing;
#
# This file is part of Path-Tiny
#
# This software is Copyright (c) 2013 by David Golden.
#
# This is free software, licensed under:
#
#   The Apache License, Version 2.0, January 2004
#
