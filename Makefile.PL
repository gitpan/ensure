use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'ensure',
    AUTHOR              => 'Chris Hall <GMCH@cpan.org>',
    VERSION_FROM        => 'lib/ensure.pm',
    ABSTRACT_FROM       => 'lib/ensure.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'ensure-*' },
);
