##########################################################################################
#
# Testing for module ensure
#
#   v1.10  14-Oct-2008
#
#            * finally arranged to get tests to run in WIN32 environment.
#
#   v1.09  13-Oct-2008
#
#            * fixed delta() in t::Imports to not include '&' for CODE elements.
#
#   v1.08  10-Oct-2008
#
#            * fixed test that $^X works to avoid being fooled by exotic characters in
#              $^X -- in particular '@' and '$' !
#
#   v1.07   9-Oct-2008
#
#            * changed how Perl is invoked to use $^X
#
#   v1.06  Not released
#
#   v1.05  15-Aug-2008  -- initial public release 

use strict ;
use warnings ;

use constant TESTS => 23 ;	# After we're sure about the version of Perl

use Test::More tests => TESTS + 1 ;

# Make sure we can run a copy of Perl

my $perl = $^X ; if ($perl =~ m/\s/) { $perl = '"'.$perl.'"' ; } ; 

{ my $want  = "sprintf('%s -- v%vd', \$^X, \$^V)" ;
  my $get   = `$perl -w -e "print $want"` ;
  my $have  = eval $want ;

  if ($get eq $have) { pass("Testing under '$get'") ;                   }
                else { BAIL_OUT("Running '$have', but got '$get' ??") ; } ;
} ;

# Here we work through the __DATA__ below and construct the tests -- see below.

my @tests = () ;		# Each test is [file, expectations, ....]
my @files = () ;		# Files we created, so we can delete them at the end
my $FH    = undef ;
my $test  = undef ;

while (<DATA>) {
  if    ($_ =~ /^___/) {
    if (defined($FH)) { close $FH ; $FH = undef ; } ;

    if    ($_ =~ /^___\<([^>]*)\>/) {
      my $file = $1 ;
      push @files, $file ;

      # Another file

      open $FH, ">$file"   or BAIL_OUT("Failed to create $file") ;

      if (!defined($test)) { $test = [$file] ; } ;
    } 
    elsif ($_ =~ /^___E/) {

      # Expectations section

      if (!defined($test)) {
        BAIL_OUT("Unexpected 'Expect' section after test no. ".scalar(@tests)) ;
      } ;
    }
    else {

      # Separator

      if ($test) { push @tests, $test ; $test = undef ; } ;

    } ;
  }
  elsif (defined($FH)) {
    print $FH $_ ;
  }
  elsif (defined($test)) {
    chomp ;
    push @$test, $_ ;
  } ;
} ;

if (scalar(@tests) != TESTS) {
  BAIL_OUT("Have ".scalar(@tests)." tests but expected ".TESTS) ;
} ;

# Now we run the tests.
#
# Two basic forms, those with an explicit '.pl', and those that use a dummy.

foreach my $test (@tests) {
  my $name = shift(@$test) ;

  my $pl = $name ;

  if ($pl =~ s/\.pm$//) {
    $pl =~ s/[\\\/]/::/g ;
    $pl = "-e \"use strict; use warnings; use ensure; use $pl;\"" ;
  } ;

  my @result = split(/\n/, `$perl -w $pl 2>&1`) ;

  my @diag = () ;
  my $i = 0 ;
  while ($i <= $#$test) {
    if ($i > $#result) { $result[$i] = '' ; } ;
    if ($result[$i] ne $$test[$i]) {
      push @diag, "$i: expected '". $$test[$i] ."' got '". $result[$i] ."'" ;
    } ;
    $i++ ;
  } ;
  if ($i <= $#result) { push @diag, "Beyond expected got:" ; } ;
  while ($i <= $#result) {
    push @diag, "$i: '". $result[$i] ."'" ;
    $i++ ;
  } ;

  if (@diag) {
    diag("Failed $name\n  ", join("\n  ", @diag)) ;
    fail($name) ;
  }
  else {
    pass($name) ;
  } ;
} ;

unlink @files ;

#===============================================================================
# DATA
#
#   * Lines starting with 4 or more '_' are separators/terminators.
#
#     Anything after a separator line is ignored, other than '_' lines.
#     Anything up to the first separator line is ignored.
#
#     There MUST be a separator line after each test.
#
#   * Lines starting '___<' precede files.
#
#     If given test has a '.pl', it must be the first file.
#     If given test has a '.pm' which is to be included in a dummy .pl, it must
#     be the first file.
#
#   * Line starting '___E' precede the expected output of the test.

__DATA__

________________________________________________________________________________
___<t/test_01.pl>___undefined values in main::__________________________________
use strict ;
use warnings ;

use ensure ;

our $main_scalar ;
our @main_array  ;
our %main_hash   ;

my $z = *main_glob ;

sub hide_error {
  main_sub(*main_glob) ;
  my $y = \&main_sub_ref ;
} ;
___Expect_________________________________________
+++ ensure: main::main_glob is undefined
+++ ensure: main::main_scalar is undefined
+++ ensure: main::main_sub is undefined
+++ ensure: main::main_sub_ref is undefined
*** ensure: 4 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_02.pl>___as t/test_01, but with some 'no ensure'_____________________
use strict ;
use warnings ;

use ensure ;

no ensure qw(@main_array  %main_hash  *main_glob) ;
no ensure qw(main_sub_ref) ;

our $main_scalar ;
our @main_array  ;
our %main_hash   ;

my $z = *main_glob ;

sub hide_error {
  main_sub(*main_glob) ;
  my $y = \&main_sub_ref ;
} ;
___Expect_________________________________________
+++ ensure: main::main_scalar is undefined
+++ ensure: main::main_sub is undefined
*** ensure: 2 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_03.pl>___as t/test_01, but with all 'no ensure'______________________
use strict ;
use warnings ;

use ensure ;

no ensure qw(@main_array  %main_hash  *main_glob) ;
no ensure qw(main_sub_ref) ;
no ensure qw($main_scalar  main_sub  &main_sub) ;

our $main_scalar ;
our @main_array  ;
our %main_hash   ;

my $z = *main_glob ;

sub hide_error {
  main_sub(*main_glob) ;
  my $y = \&main_sub_ref ;
} ;
________________________________________________________________________________
___<t/test_04.pl>___as t/test_01, but with scalar and sub defined_______________
use strict ;
use warnings ;

use ensure ;

no ensure qw(@main_array  %main_hash  *main_glob) ;
no ensure qw(main_sub_ref) ;

our $main_scalar = 0 ;
our @main_array  ;
our %main_hash   ;

my $z = *main_glob ;

sub hide_error {
  main_sub(*main_glob) ;
  my $y = \&main_sub_ref ;
} ;

sub main_sub {
  return $_[0] ;
} ;
___Expect_________________________________________
+++ ensure: main::main_scalar is undefined
*** ensure: 1 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_05.pm>___undefined values in package t::test_05______________________
package t::test_05 ;

use strict ;
use warnings ;

use ensure ;

our $test_scalar ;
our @test_array ;
our %test_hash ;

my $z = *test_glob ;

sub hide_error {
  test_sub(*test_glob) ;
  my $y = \&test_sub_ref ;
} ;

1 ;
___Expect_________________________________________
+++ ensure: t::test_05::test_glob is undefined
+++ ensure: t::test_05::test_scalar is undefined
+++ ensure: t::test_05::test_sub is undefined
+++ ensure: t::test_05::test_sub_ref is undefined
*** ensure: 4 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_06.pm>___as test_05.pl, but with some 'no ensure'____________________
package t::test_06 ;

use strict ;
use warnings ;

use ensure ;

no ensure qw(@test_array    test_array) ;
no ensure qw(%test_hash    $test_hash) ;
no ensure qw(&test_sub_ref  test_sub_ref) ;
no ensure qw(*test_glob    &test_glob) ;

our $test_scalar ;
our @test_array ;
our %test_hash ;

my $z = *test_glob ;

sub hide_error {
  test_sub(*test_glob) ;
  my $y = \&test_sub_ref ;
} ;

1 ;
___Expect_________________________________________
+++ ensure: t::test_06::test_scalar is undefined
+++ ensure: t::test_06::test_sub is undefined
*** ensure: 2 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_07.pm>___as t/test_05, but with all 'no ensure'______________________
package t::test_07 ;

use strict ;
use warnings ;

use ensure ;

no ensure qw(@test_array    test_array) ;
no ensure qw(%test_hash    $test_hash) ;
no ensure qw(&test_sub_ref  test_sub_ref) ;
no ensure qw(*test_glob    &test_glob) ;

no ensure qw($test_scalar  *test_sub) ;

our $test_scalar ;
our @test_array ;
our %test_hash ;

my $z = *test_glob ;

sub hide_error {
  test_sub(*test_glob) ;
  my $y = \&test_sub_ref ;
} ;

1 ;
________________________________________________________________________________
___<t/test_08.pm>___as t/test_05, but with scalar & sub defined_________________
package t::test_08 ;

use strict ;
use warnings ;

use ensure ;

no ensure qw(@test_array    test_array) ;
no ensure qw(%test_hash    $test_hash) ;
no ensure qw(&test_sub_ref  test_sub_ref) ;
no ensure qw(*test_glob    &test_glob) ;

our $test_scalar = 0 ;
our @test_array ;
our %test_hash ;

my $z = *test_glob ;

sub hide_error {
  test_sub(*test_glob) ;
  my $y = \&test_sub_ref ;
} ;

sub test_sub {
  return $_[0] ;
} ;

1 ;
________________________________________________________________________________
___<t/test_09.pm>___undefined sub, not masked by undefined scalar_______________
package t::test_09 ;

use strict ;
use warnings ;

use ensure ;

our $test_sub ;

sub hide_error {
  test_sub() ;
} ;

1 ;
___Expect_________________________________________
+++ ensure: t::test_09::test_sub is undefined
*** ensure: 1 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_10.pm>___undefined sub, masked by scalar_____________________________
package t::test_10 ;

use strict ;
use warnings ;

use ensure ;

our $test_sub = 0 ;

sub hide_error {
  test_sub() ;
} ;

1 ;
________________________________________________________________________________
___<t/test_11.pm>___undefined exports___________________________________________
package t::test_11 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT    = qw($test_scalar  %test_hash  test_const  &test_const) ;
our @EXPORT_OK = qw(@test_array   *test_glob  test_sub  &test_sub) ;

1 ;
___Expect_________________________________________
+++ ensure: '$test_scalar' is exported by 't::test_11', but is not defined
+++ ensure: '%test_hash' is exported by 't::test_11', but is not defined
+++ ensure: '&test_const' is exported by 't::test_11', but is not defined
+++ ensure: '&test_sub' is exported by 't::test_11', but is not defined
+++ ensure: '*test_glob' is exported by 't::test_11', but is not defined
+++ ensure: '@test_array' is exported by 't::test_11', but is not defined
+++ ensure: 'test_const' is exported by 't::test_11', but is not defined
+++ ensure: 'test_sub' is exported by 't::test_11', but is not defined
+++ ensure: main::test_scalar is undefined
+++ ensure: t::test_11::test_scalar is undefined
*** ensure: 10 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_12.pm>___undefined exports, some no ensure___________________________
package t::test_12 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT    = qw($test_scalar  %test_hash  test_const  &test_const) ;
our @EXPORT_OK = qw(@test_array   *test_glob  test_sub  &test_sub) ;

no ensure qw(@test_array %test_hash &test_const) ;
no ensure qw(*test_glob) ;

1 ;
___Expect_________________________________________
+++ ensure: '$test_scalar' is exported by 't::test_12', but is not defined
+++ ensure: '&test_sub' is exported by 't::test_12', but is not defined
+++ ensure: 'test_sub' is exported by 't::test_12', but is not defined
+++ ensure: main::test_scalar is undefined
+++ ensure: t::test_12::test_scalar is undefined
*** ensure: 5 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_13.pm>___undefined exports, all no ensure____________________________
package t::test_13 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT    = qw($test_scalar  %test_hash  test_const  &test_const) ;
our @EXPORT_OK = qw(@test_array   *test_glob  test_sub  &test_sub) ;

no ensure qw(@test_array %test_hash &test_const) ;
no ensure qw(*test_glob test_sub  $test_scalar) ;

1 ;
________________________________________________________________________________
___<t/test_14.pm>___exports, all defined except no value for scalar_____________
package t::test_14 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT    = qw($test_scalar  %test_hash  test_const  &test_const) ;
our @EXPORT_OK = qw(@test_array   *test_glob  test_sub  &test_sub) ;

our $test_scalar ;
our @test_array ;
our %test_hash ;
*test_glob = \@test_array ;
sub test_sub {
} ;

use constant test_const => 133 ;

1 ;
___Expect_________________________________________
+++ ensure: '$test_scalar' is exported by 't::test_14', but is not defined
+++ ensure: main::test_scalar is undefined
+++ ensure: t::test_14::test_scalar is undefined
*** ensure: 3 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_15.pm>___exports, all defined including a value for scalar___________
package t::test_15 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT    = qw($test_scalar  %test_hash  test_const  &test_const) ;
our @EXPORT_OK = qw(@test_array   *test_glob  test_sub  &test_sub) ;

our $test_scalar = 0 ;
our @test_array ;
our %test_hash ;
*test_glob = \@test_array ;
sub test_sub {
} ;

use constant test_const => 143 ;

1 ;
________________________________________________________________________________
___<t/test_16.pm>___checking %EXPORT_TAGS against @EXPORT & @EXPORT_OK__________
package t::test_16 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT      = qw($test_a  test_b) ;
our @EXPORT_OK   = qw( test_c  &test_d) ;

our %EXPORT_TAGS = (
  IMPLICIT  => [qw(test_a test_b $test_c &test_d  test_q)],
  other     => [qw($test_b  $test_a  test_c  test_q)],
  empty     => [],
  final     => [qw(&test_d test_b test_c $test_a)],
) ;

our @EXPORT_FAIL = qw(test_a test_q  test_c) ;

no ensure qw($test_a) ;

our $test_a ;

sub test_d { return $_[0] ; } ;

use constant {
  test_b  => 'b',
  test_c  => 'c',
} ;

1 ;
___Expect_________________________________________
+++ ensure: '$test_c' is in 't::test_16's 'IMPLICIT' tag list, but not in @EXPORT
+++ ensure: '&test_d' is in 't::test_16's 'IMPLICIT' tag list, but not in @EXPORT
+++ ensure: 'test_a' is in 't::test_16's 'IMPLICIT' tag list, but not in @EXPORT
+++ ensure: 'test_q' is in 't::test_16's 'IMPLICIT' tag list, but not in @EXPORT
+++ ensure: '$test_b' is in 't::test_16's 'other' tag list, but not in @EXPORT or @EXPORT_OK
+++ ensure: 'test_q' is in 't::test_16's 'other' tag list, but not in @EXPORT or @EXPORT_OK
+++ ensure: 'test_a' is in 't::test_16's @EXPORT_FAIL, but not in @EXPORT or @EXPORT_OK
+++ ensure: 'test_q' is in 't::test_16's @EXPORT_FAIL, but not in @EXPORT or @EXPORT_OK
*** ensure: 8 errors found
CHECK failed--call queue aborted.
________________________________________________________________________________
___<t/test_17.pm>___exports, all defined and all OK_____________________________
package t::test_17 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT      = qw($test_a  @test_q  test_b) ;
our @EXPORT_OK   = qw( test_c  &test_d test_z) ;

our %EXPORT_TAGS = (
  IMPLICIT  => [qw($test_a test_b)],
  other     => [qw(test_b  $test_a  test_c  @test_q)],
  empty     => [],
  final     => [qw(&test_d test_b test_c $test_a)],
) ;

our @EXPORT_FAIL = qw(&test_d  test_z) ;

no ensure qw($test_a) ;

our $test_a ;
our @test_q ;

sub test_d { return $_[0] ; } ;

use constant {
  test_b  => 'b',
  test_c  => 'c',
  test_z  => 'z',
} ;

1 ;
________________________________________________________________________________
___<t/test_18.pm>___exports, all defined and all OK -- empty @EXPORT_FAIL, empty IMPLICIT
package t::test_18 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT      = qw($test_a  @test_q  test_b) ;
our @EXPORT_OK   = qw( test_c  &test_d test_z) ;

our %EXPORT_TAGS = (
  IMPLICIT  => [],
  other     => [qw(test_b  $test_a  test_c  @test_q)],
  empty     => [],
  final     => [qw(&test_d test_b test_c $test_a)],
) ;

our @EXPORT_FAIL = () ;

no ensure qw($test_a) ;

our $test_a ;
our @test_q ;

sub test_d { return $_[0] ; } ;

use constant {
  test_b  => 'b',
  test_c  => 'c',
  test_z  => 'z',
} ;

1 ;
________________________________________________________________________________
___<t/test_19.pm>___exports, all defined and all OK -- no @EXPORT_FAIL, no IMPLICIT
package t::test_19 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT      = qw($test_a  @test_q  test_b) ;
our @EXPORT_OK   = qw( test_c  &test_d test_z) ;

our %EXPORT_TAGS = (
  other     => [qw(test_b  $test_a  test_c  @test_q)],
  empty     => [],
  final     => [qw(&test_d test_b test_c $test_a)],
) ;

no ensure qw($test_a) ;

our $test_a ;
our @test_q ;

sub test_d { return $_[0] ; } ;

use constant {
  test_b  => 'b',
  test_c  => 'c',
  test_z  => 'z',
} ;

1 ;
________________________________________________________________________________
___<t/test_20.pm>___exports, all defined and all OK -- no @EXPORT_FAIL, empty %EXPORT_TAGS
package t::test_20 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT      = qw($test_a  @test_q  test_b) ;
our @EXPORT_OK   = qw( test_c  &test_d test_z) ;

our %EXPORT_TAGS = () ;

no ensure qw($test_a) ;

our $test_a ;
our @test_q ;

sub test_d { return $_[0] ; } ;

use constant {
  test_b  => 'b',
  test_c  => 'c',
  test_z  => 'z',
} ;

1 ;
________________________________________________________________________________
___<t/test_21.pl>___imports, with not empty IMPLICIT tag________________________
use strict ;
use warnings ;

use ensure ;

use t::Imports ;

my $snap ;

BEGIN { $snap = snap() ; } ;

# Basic imports

use t::tests_import_1 ;
BEGIN { delta($snap, '') ; } ;

use t::tests_import_1 qw(other_a) ;
BEGIN { delta($snap,    'other_a') ; } ;

use t::tests_import_1 () ;
BEGIN { delta($snap, '()') ; } ;

use t::tests_import_1 qw(:NONE) ;
BEGIN { delta($snap,    ':NONE') ; } ;

use t::tests_import_1 qw(:DEFAULT) ;
BEGIN { delta($snap,    ':DEFAULT') ; } ;

use t::tests_import_1 qw(:IMPLICIT) ;
BEGIN { delta($snap,    ':IMPLICIT') ; } ;

use t::tests_import_1 qw(:ALL) ;
BEGIN { delta($snap,    ':ALL') ; } ;

# Slightly obscure '!'

use t::tests_import_1 qw(!:DEFAULT) ;
BEGIN { delta($snap, '!:DEFAULT') ; } ;

use t::tests_import_1 qw(!:IMPLICIT) ;
BEGIN { delta($snap, '!:IMPLICIT') ; } ;

# Extended imports

use t::tests_import_1 qw(:NONE !rest_a other_b) ;
BEGIN { delta($snap,    ':NONE !rest_a other_b') ; } ;

use t::tests_import_1 qw(:DEFAULT rest_c !default_a !other_b) ;
BEGIN { delta($snap,    ':DEFAULT rest_c !default_a !other_b') ; } ;

use t::tests_import_1 qw(:IMPLICIT !default_a !implicit_c other_b) ;
BEGIN { delta($snap,    ':IMPLICIT !default_a !implicit_c other_b') ; } ;

use t::tests_import_1 qw(:ALL !default_a !implicit_b !other_b !rest_c) ;
BEGIN { delta($snap,    ':ALL !default_a !implicit_b !other_b !rest_c') ; } ;

___<t/tests_import_1.pm>__________________________
package t::tests_import_1 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT    = qw(default_a  default_b  default_c  implicit_a  implicit_b) ;
our @EXPORT_OK = qw(other_a  other_b  other_c  rest_a  rest_b  rest_c
                    default_a  implicit_b) ;

our %EXPORT_TAGS = (
  IMPLICIT => [qw(implicit_a  implicit_b)],
  other    => [qw(other_a     other_b       other_c)],
  rest     => [qw(rest_a      rest_b        rest_c)],
) ;

use constant {
  default_a  =>  1,
  default_b  =>  2,
  default_c  =>  3,
  implicit_a =>  4,
  implicit_b =>  5,
  other_a    =>  6,
  other_b    =>  7,
  other_c    =>  8,
  rest_a     =>  9,
  rest_b     => 10,
  rest_c     => 11,
} ;

1 ;
___Expect_________________________________________
Importing: default_a default_b default_c implicit_a implicit_b
Importing qw(other_a): implicit_a implicit_b other_a
Importing ():
Importing qw(:NONE):
Importing qw(:DEFAULT): default_a default_b default_c implicit_a implicit_b
Importing qw(:IMPLICIT): implicit_a implicit_b
Importing qw(:ALL): default_a default_b default_c implicit_a implicit_b other_a
  + other_b other_c rest_a rest_b rest_c
Importing qw(!:DEFAULT):
Importing qw(!:IMPLICIT): default_a default_b default_c
Importing qw(:NONE !rest_a other_b): other_b
Importing qw(:DEFAULT rest_c !default_a !other_b): default_b default_c implicit_a
  + implicit_b rest_c
Importing qw(:IMPLICIT !default_a !implicit_c other_b): implicit_a implicit_b
  + other_b
Importing qw(:ALL !default_a !implicit_b !other_b !rest_c): default_b default_c
  + implicit_a other_a other_c rest_a rest_b
___<t/Imports.pm>___utility package used by the import testing__________________
package t::Imports ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT = qw(snap delta) ;

# Filtering out stash entries we don't want to worry about

sub filter {
  my ($key) = @_ ;
  return ($key =~ /^\w+$/)	# want only simple names
      && ($key !~ /^[_\d]/) ;	# and not stuff starting '_' or digit
} ;

# Take snap shot of state of given package's stash.

sub snap {
  my $pkg = scalar(caller) ;

  my $st = do { no strict 'refs' ; *{$pkg.'::'}{HASH} ; } ;

  return [$pkg, $st, [grep filter($_), keys %$st]] ;
} ;

# See what has appeared in the stash, and delete it.
#
# prints single line to STDERR showing the imports

sub delta {
  my ($snap, $title) = @_ ;

  my ($pkg, $st, $was) = @$snap ;

  if ($title) {
    if ($title eq '()') { $title = " $title" ;     }
                   else { $title = " qw($title)" ; } ;
  } ;

  my $delta = "Importing$title:" ;

  my %was = map { ($_, 1) } @$was ;

  my @keys = sort(grep filter($_), keys %$st) ;
  foreach my $key (@keys) {
    if (!$was{$key}) {

      if (length($delta)+length($key) > 80) {
        print STDERR $delta, "\n" ;
        $delta = "  +" ;
      } ;

      my $rv = $st->{$key} ;
      if (ref($rv))               { $delta .=   " $key" ;  }
      else {
        my $n = 0 ;
        if (my $rs = *$rv{SCALAR}) {
          if (defined($$rs))      { $delta .= " \$$key" ; $n++ ; } ;
        } ;
        if (defined(*$rv{ARRAY})) { $delta .= " \@$key" ; $n++ ; } ;
        if (defined(*$rv{HASH} )) { $delta .= " \%$key" ; $n++ ; } ;
        if (defined(*$rv{CODE} )) { $delta .=   " $key" ; $n++ ; } ;
        if ($n == 0)              { $delta .= " \*$key" ;        } ;
      } ;
      delete $st->{$key} ;
    } ;
  } ;

  print STDERR $delta, "\n" ;
} ;

1 ;
________________________________________________________________________________
___<t/test_22.pl>___imports, with empty IMPLICIT tag____________________________
use strict ;
use warnings ;

use ensure ;

use t::Imports ;

my $snap ;

BEGIN { $snap = snap() ; } ;

# Basic imports

use t::tests_import_2 ;
BEGIN { delta($snap, '') ; } ;

use t::tests_import_2 qw(other_a) ;
BEGIN { delta($snap,    'other_a') ; } ;

use t::tests_import_2 () ;
BEGIN { delta($snap, '()') ; } ;

use t::tests_import_2 qw(:NONE) ;
BEGIN { delta($snap,    ':NONE') ; } ;

use t::tests_import_2 qw(:DEFAULT) ;
BEGIN { delta($snap,    ':DEFAULT') ; } ;

use t::tests_import_2 qw(:IMPLICIT) ;
BEGIN { delta($snap,    ':IMPLICIT') ; } ;

use t::tests_import_2 qw(:ALL) ;
BEGIN { delta($snap,    ':ALL') ; } ;

# Slightly obscure '!'

use t::tests_import_2 qw(!:DEFAULT) ;
BEGIN { delta($snap, '!:DEFAULT') ; } ;

use t::tests_import_2 qw(!:IMPLICIT) ;
BEGIN { delta($snap, '!:IMPLICIT') ; } ;

# Extended imports

use t::tests_import_2 qw(:NONE !rest_a other_b) ;
BEGIN { delta($snap,    ':NONE !rest_a other_b') ; } ;

use t::tests_import_2 qw(:DEFAULT rest_c !default_a !other_b) ;
BEGIN { delta($snap,    ':DEFAULT rest_c !default_a !other_b') ; } ;

use t::tests_import_2 qw(:IMPLICIT !default_a other_b) ;
BEGIN { delta($snap,    ':IMPLICIT !default_a other_b') ; } ;

use t::tests_import_2 qw(:ALL !default_a !other_b !rest_c) ;
BEGIN { delta($snap,    ':ALL !default_a !other_b !rest_c') ; } ;

___<t/tests_import_2.pm>__________________________
package t::tests_import_2 ;

use strict ;
use warnings ;

use ensure ;

our @EXPORT    = qw(default_a  default_b  default_c) ;
our @EXPORT_OK = qw(other_a  other_b  other_c  rest_a  rest_b  rest_c
                    default_a) ;

# Same as t::tests_import_1, but IMPLICIT is empty.

our %EXPORT_TAGS = (
  IMPLICIT => [],
  other    => [qw(other_a     other_b       other_c)],
  rest     => [qw(rest_a      rest_b        rest_c)],
) ;

use constant {
  default_a  =>  1,
  default_b  =>  2,
  default_c  =>  3,
  other_a    =>  6,
  other_b    =>  7,
  other_c    =>  8,
  rest_a     =>  9,
  rest_b     => 10,
  rest_c     => 11,
} ;

1 ;
___Expect_________________________________________
Importing: default_a default_b default_c
Importing qw(other_a): other_a
Importing ():
Importing qw(:NONE):
Importing qw(:DEFAULT): default_a default_b default_c
Importing qw(:IMPLICIT):
Importing qw(:ALL): default_a default_b default_c other_a other_b other_c rest_a
  + rest_b rest_c
Importing qw(!:DEFAULT):
Importing qw(!:IMPLICIT): default_a default_b default_c
Importing qw(:NONE !rest_a other_b): other_b
Importing qw(:DEFAULT rest_c !default_a !other_b): default_b default_c rest_c
Importing qw(:IMPLICIT !default_a other_b): other_b
Importing qw(:ALL !default_a !other_b !rest_c): default_b default_c other_a
  + other_c rest_a rest_b
________________________________________________________________________________
___<t/test_23.pl>___imports, with no IMPLICIT tag_______________________________
use strict ;
use warnings ;

use ensure ;

use t::Imports ;

my $snap ;

BEGIN { $snap = snap() ; } ;

# Basic imports

use t::tests_import_3 ;
BEGIN { delta($snap, '') ; } ;

use t::tests_import_3 qw(other_a) ;
BEGIN { delta($snap,    'other_a') ; } ;

use t::tests_import_3 () ;
BEGIN { delta($snap, '()') ; } ;

use t::tests_import_3 qw(:NONE) ;
BEGIN { delta($snap,    ':NONE') ; } ;

use t::tests_import_3 qw(:DEFAULT) ;
BEGIN { delta($snap,    ':DEFAULT') ; } ;

use t::tests_import_3 qw(:IMPLICIT) ;
BEGIN { delta($snap,    ':IMPLICIT') ; } ;

use t::tests_import_3 qw(:ALL) ;
BEGIN { delta($snap,    ':ALL') ; } ;

# Slightly obscure '!'

use t::tests_import_3 qw(!:DEFAULT) ;
BEGIN { delta($snap, '!:DEFAULT') ; } ;

# Extended imports

use t::tests_import_3 qw(:NONE !rest_a other_b) ;
BEGIN { delta($snap,    ':NONE !rest_a other_b') ; } ;

use t::tests_import_3 qw(:DEFAULT rest_c !default_a !other_b) ;
BEGIN { delta($snap,    ':DEFAULT rest_c !default_a !other_b') ; } ;

use t::tests_import_3 qw(:IMPLICIT !default_a other_b) ;
BEGIN { delta($snap,    ':IMPLICIT !default_a other_b') ; } ;

use t::tests_import_3 qw(:ALL !default_a !other_b !rest_c) ;
BEGIN { delta($snap,    ':ALL !default_a !other_b !rest_c') ; } ;

___<t/tests_import_3.pm>__________________________
package t::tests_import_3 ;

use strict ;
use warnings ;

use ensure ;

# Same as t::tests_import_1, but no IMPLICIT tag at all.

our @EXPORT    = qw(default_a  default_b  default_c) ;
our @EXPORT_OK = qw(other_a  other_b  other_c  rest_a  rest_b  rest_c
                    default_a) ;

our %EXPORT_TAGS = (
  other    => [qw(other_a     other_b       other_c)],
  rest     => [qw(rest_a      rest_b        rest_c)],
) ;

use constant {
  default_a  =>  1,
  default_b  =>  2,
  default_c  =>  3,
  other_a    =>  6,
  other_b    =>  7,
  other_c    =>  8,
  rest_a     =>  9,
  rest_b     => 10,
  rest_c     => 11,
} ;

1 ;
___Expect_________________________________________
Importing: default_a default_b default_c
Importing qw(other_a): other_a
Importing ():
Importing qw(:NONE):
Importing qw(:DEFAULT): default_a default_b default_c
Importing qw(:IMPLICIT):
Importing qw(:ALL): default_a default_b default_c other_a other_b other_c rest_a
  + rest_b rest_c
Importing qw(!:DEFAULT):
Importing qw(:NONE !rest_a other_b): other_b
Importing qw(:DEFAULT rest_c !default_a !other_b): default_b default_c rest_c
Importing qw(:IMPLICIT !default_a other_b): other_b
Importing qw(:ALL !default_a !other_b !rest_c): default_b default_c other_a
  + other_c rest_a rest_b
________________________________________________________________________________
