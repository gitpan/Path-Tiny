use 5.008001;
use strict;
use warnings;
use Test::More 0.96;
use Test::Deep '!blessed';
use File::Temp 0.18 ();
use File::Spec::Unix;

use Path::Tiny;

my $tempdir = File::Temp->newdir;

my @kids = qw/apple banana carrot/;
path($tempdir)->child($_)->touch for @kids;

my @expected = map { path( File::Spec::Unix->catfile( $tempdir, $_ ) ) } @kids;

cmp_deeply(
    [ sort { $a cmp $b } path($tempdir)->children ],
    [ sort @expected ],
    "children correct"
);

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
