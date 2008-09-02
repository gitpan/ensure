#!perl -T

use Test::More tests => 1 ;

BEGIN { use_ok( 'ensure' ) ; } ;

no ensure 'TODO' ;

diag( "Testing ensure $ensure::VERSION, Perl $], $^X" ) ;
