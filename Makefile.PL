
use strict;
use warnings;

use 5.010000;

use ExtUtils::MakeMaker 6.30;



my %WriteMakefileArgs = (
  "ABSTRACT" => "File path utility",
  "AUTHOR" => "David Golden <dagolden\@cpan.org>",
  "BUILD_REQUIRES" => {
    "File::Find" => 0,
    "File::Spec::Unix" => 0,
    "Test::Deep" => 0,
    "Test::Fatal" => 0,
    "Test::More" => "0.96"
  },
  "CONFIGURE_REQUIRES" => {
    "ExtUtils::MakeMaker" => "6.30"
  },
  "DISTNAME" => "Path-Tiny",
  "EXE_FILES" => [],
  "LICENSE" => "apache",
  "NAME" => "Path::Tiny",
  "PREREQ_PM" => {
    "Cwd" => 0,
    "Exporter" => 0,
    "Fcntl" => 0,
    "File::Copy" => 0,
    "File::Path" => "2.07",
    "File::Spec" => "3.40",
    "File::Stat" => 0,
    "File::Temp" => "0.18",
    "autodie" => "2.00",
    "constant" => 0,
    "overload" => 0,
    "strict" => 0,
    "warnings" => 0
  },
  "VERSION" => "0.001",
  "test" => {
    "TESTS" => "t/*.t"
  }
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.56) } ) {
  my $br = delete $WriteMakefileArgs{BUILD_REQUIRES};
  my $pp = $WriteMakefileArgs{PREREQ_PM};
  for my $mod ( keys %$br ) {
    if ( exists $pp->{$mod} ) {
      $pp->{$mod} = $br->{$mod} if $br->{$mod} > $pp->{$mod};
    }
    else {
      $pp->{$mod} = $br->{$mod};
    }
  }
}

delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
  unless eval { ExtUtils::MakeMaker->VERSION(6.52) };

WriteMakefile(%WriteMakefileArgs);



