
use strict;
use warnings;

use 5.008001;

use ExtUtils::MakeMaker 6.30;



my %WriteMakefileArgs = (
  "ABSTRACT" => "File path utility",
  "AUTHOR" => "David Golden <dagolden\@cpan.org>",
  "BUILD_REQUIRES" => {},
  "CONFIGURE_REQUIRES" => {
    "ExtUtils::MakeMaker" => "6.30"
  },
  "DISTNAME" => "Path-Tiny",
  "EXE_FILES" => [],
  "LICENSE" => "apache",
  "NAME" => "Path::Tiny",
  "PREREQ_PM" => {
    "Carp" => 0,
    "Cwd" => 0,
    "Digest" => "1.03",
    "Digest::SHA" => "5.45",
    "Exporter" => "5.57",
    "Fcntl" => 0,
    "File::Copy" => 0,
    "File::Path" => "2.07",
    "File::Spec" => "3.40",
    "File::Temp" => "0.18",
    "File::stat" => 0,
    "autodie::exception" => "2.14",
    "constant" => 0,
    "overload" => 0,
    "strict" => 0,
    "threads" => 0,
    "warnings" => 0
  },
  "TEST_REQUIRES" => {
    "Devel::Hide" => 0,
    "ExtUtils::MakeMaker" => 0,
    "File::Find" => 0,
    "File::Spec::Functions" => 0,
    "File::Spec::Unix" => 0,
    "File::pushd" => 0,
    "List::Util" => 0,
    "Test::Deep" => 0,
    "Test::FailWarnings" => 0,
    "Test::Fatal" => 0,
    "Test::More" => "0.96",
    "open" => 0
  },
  "VERSION" => "0.027",
  "test" => {
    "TESTS" => "t/*.t"
  }
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.63_03) } ) {
  my $tr = delete $WriteMakefileArgs{TEST_REQUIRES};
  my $br = $WriteMakefileArgs{BUILD_REQUIRES};
  for my $mod ( keys %$tr ) {
    if ( exists $br->{$mod} ) {
      $br->{$mod} = $tr->{$mod} if $tr->{$mod} > $br->{$mod};
    }
    else {
      $br->{$mod} = $tr->{$mod};
    }
  }
}

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



