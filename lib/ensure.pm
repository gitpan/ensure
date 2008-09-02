#=========================================================================================
package ensure ;          # a pragma
#=========================================================================================

use 5.006 ; use v5.8.8 ;  # Not tested for anything less

use strict ;
use warnings ;

our $VERSION = '1.05' ;

#=========================================================================================

use Exporter () ;         # Exporter::import used explicitly

our @EXPORT    = qw(import) ;
our @EXPORT_OK = qw(register) ;

#=========================================================================================
# We work on the basis that in all entries in the symbol table have a defined {SCALAR}
# part -- whether there is a scalar or not.

test_scalar_symbol() or die "*** symbol table form changed -- package 'ensure' broken\n" ;

sub test_scalar_symbol {
  return defined(*{stash(__PACKAGE__)->{'test_scalar_symbol'}}{SCALAR}) ;
} ;

#=========================================================================================
# Tables of packages and variables known to ensure 

my %packages  = () ;    # Keys   = packages registered for ensure CHECK processing
                        # Values = ref:Stash for registered packages

my %exporters = () ;    # Keys   = packages which we've seen export stuff
                        # Values = true => package includes IMPLICIT tag

my %no_ensure = () ;    # Keys   = packages with things declared no_ensure
                        # Values = [name, name, ...]

my %no_scalar = () ;    # Keys   = address of undefined scalar declared 'no ensure'
                        # Values = exporting package

my $ensure_errors = 0 ; # Count of errors.  Dies at end of ensure CHECK if != 0.

#=========================================================================================
# Manual registration

register(__PACKAGE__) ; # Register ourselves

sub register {          # May be used, eg, to register 'main'
  my ($p) = @_ ;
  $packages{$p} ||= stash($p) ;
} ;

#=========================================================================================
# Two small utilities

sub err {               # Issue warning message and increment $ensure_errors
  warn '+++ ensure: ', @_, "\n" ;
  return $ensure_errors++ ;
} ;

sub crunch {		# Die
  die  '*** ensure: ', @_, "\n" ;
} ;

sub suq {               # Sort given list and ensure all entries are unique
  my %l = map { ($_, undef) } @_ ;
  return sort keys %l ;
} ;

#=========================================================================================
# ensure::import
# ==============
#
# This will be invoked:
#
#   a. when a package does 'use ensure', which:
#
#       - registers the package for the ensure CHECK block checks.
#
#       - imports into the package the ensure::import function.
#
#   b. when a package which has done 'use ensure' is itself used:
#
#       - the first time this happens, the package's exports are checked.
#
#       - in all cases the import list extensions (:ALL :NONE :IMPLICIT) are
#         implemented, before jumping to the standard Exporter::import.
#
# Requires: $ep      -- package which is being imported from      ) passed to...
#           @imports -- import list, from "use Fred (@import) ;"  ) ...Exporter::import
#
# Returns:  nothing

sub import {
  my $ep = $_[0] ;

  # If we are running the import on behalf of ourselves, we register importing package.

  my $ip = '' ;
  if ($ep eq __PACKAGE__) { register($ip = scalar(caller)) ; } ;

  # If this is the first time we have seen this package export stuff, we run checks
  # across the export declarations.

  my $implicit = exists($exporters{$ep}) ? $exporters{$ep} : check_exports($ep) ;

  # Now we deal with the import list, if it is not empty

  if (scalar(@_) > 1) {

    if   ($_[1] eq ':ALL') {

      # Importing ':ALL' -- replace ':ALL' by contents of @EXPORT and @EXPORT_OK

      my $st = $packages{$ep} ;
      splice( @_, 1, 1, suq(@{stash_value($st, '@EXPORT'   ) || []},
                            @{stash_value($st, '@EXPORT_OK') || []}) ) ;
    }
    elsif ( ($_[1] eq ':NONE') || (!$implicit && ($_[1] eq ':IMPLICIT')) ) {

      # Importing ':NONE' or ':IMPLICIT' when no IMPLICIT tag exists.
     
      my $i = 2 ;
      while (defined($_[$i]) && ($_[$i] =~ m/^!/)) { $i++ ; } ;

      splice(@_, 1, $i-1) ;     # Drop :NONE/:IMPLICIT and following '!'

      # Give up now if nothing left of list

      if (scalar(@_) == 1) { return ; } ; # Give up now if nothing left of list
    }
    elsif ( $implicit && ($_[1] ne ':IMPLICIT') && ($_[1] !~ m/^!/) ) {

      # Exporting package has 'IMPLICIT' tag and import list a) is not empty,
      #                                                  and b) does not start ':IMPLICIT'
      #                                                  and c) does not start '!...'

      splice(@_, 1, 0, ':IMPLICIT') ;
    } ;
  } ;

  # Now we can proceed to standard import !

  if (($ip eq 'main') && (scalar(@_) == 1))
    { return ; } ;		# (Unless importing self to main, in default fashion.)

  goto &Exporter::import ;      # As if called in the first place
} ;

#=========================================================================================
# check_exports: run checks across exports & establish whether has 'IMPLICIT' tag.
#
# Checks that:
#
#   a) everything in @EXPORT & @EXPORT_OK is defined, except where declared 'no ensure'
#
#   b) everything in %EXPORT_TAGS (other than 'IMPLICIT') must appear in @EXPORT or
#      @EXPORT_OK.
#
#   c) everything in any 'IMPLICIT' tag must appear in @EXPORT.
#
#   d) everything in @EXPORT_FAIL must appear in @EXPORT or @EXPORT_OK.
#
# Sets $exporters{$ep} = true iff there is an 'IMPLICIT' tag, false otherwise.
#
# NB: to be called the first time the package is seen exporting stuff.
#
# Requires: $ep    -- name of package which is exporting stuff -- default is caller !
#
# Returns:  true => exporting package has an 'IMPLICIT' tag

sub check_exports {
  my ($ep) = @_ ;

  #----------------------------------------------------------------------------------
  # Get the stash for the exporting package -- must be registered already !!

  my $st = $packages{$ep}
                   or crunch "check_exports: package $ep not registered" ;

  #----------------------------------------------------------------------------------
  # a) check contents of @EXPORT & @EXPORT_OK, given any 'no ensure' declarations
  #
  #   Names in @EXPORT & @EXPORT_OK are checked thus:
  #
  #     * name   -- requires: glob{CODE}, SCALAR or REF
  #     * $name  -- requires: glob{SCALAR} to have a defined value 
  #     * @name  -- requires: glob{ARRAY}
  #     * %name  -- requires: glob{HASH}
  #     * &name  -- requires: glob{CODE}
  #     * *name  -- requires: the name to exist as glob
  #
  #   Note that the undecorated name works for 5.10.0 and onwards constant values.
  #
  #   Note that for $name this means that it must have some value other than 'undef'.
  #   (This is because it is not possible to distinguish no $name declaration at all
  #   from a declaration which leaves the value undefined.)
  #
  #   Names declared 'no ensure' *must* fail the above.

  my $exp = stash_value($st, '@EXPORT'   ) || [] ;
  my $eok = stash_value($st, '@EXPORT_OK') || [] ;
  my $nen = $no_ensure{$ep} || [] ;

  # Collect all exports (from EXPORT and EXPORT_OK) & all 'no ensure' names.

  my %all_exports = map { ($_, 1) } @$exp, @$eok ;
  my %undefined   = map { ($_, 1) } @$nen ;

  foreach my $name (sort keys %all_exports) {
    my ($id, $t) = undecorate($name) ;

    my $rv  = $st->{$id} ;
    my $def = defined($rv) ;
    if ($def) {
      if (!ref($rv)) {
        if    ($t eq 'SCALAR') { $def = defined(${$rv = *$rv{$t}}) ;  }
        elsif ($t ne 'GLOB')   { $def = defined(*$rv{$t || 'CODE'}) ; } ;
      }
      else {
        $def = ($t eq '') || ($t eq 'CODE') ;
      } ;
    } ;

    if (exists($undefined{$name}) || (($t eq 'CODE') && exists($undefined{$id}))
                                  || (($t eq '') && exists($undefined{'&'.$id}))) {
      if ($def) {
        err "'$name' in '$ep\' is declared 'no ensure', but is defined" ;
      }
      elsif ($t eq 'SCALAR') {
        if (!defined($rv)) { no strict 'refs' ; $rv = \${"$ep\:\:$id"} ; } ;
        $no_scalar{"$rv"} = $ep ;
      } ;
    }
    else {
      if (!$def) {
        err "'$name' is exported by '$ep', but is not defined" ;
      } ;
    } ;
  } ;

  #----------------------------------------------------------------------------------
  # b) check that everything in the %EXPORT_TAGS is in @EXPORT or @EXPORT_OK
  #    (except for any IMPLICIT tag).
  # c) check that everything in any %EXPORT_TAGS{IMPLICIT} is in @EXPORT

  my $implicit = 0 ;

  if (my $etg = stash_value($st, '%EXPORT_TAGS')) {
    foreach my $tag (sort keys %$etg) {
      if ($tag ne 'IMPLICIT') {
        foreach my $name (suq @{$$etg{$tag}}) {
          if (!exists($all_exports{$name})) {
            err "'$name' is in '$ep\'s '$tag' tag list,",
                                               " but not in \@EXPORT or \@EXPORT_OK" ;
          } ;
        } ;
      }
      else {
        $implicit = 1 ;
        my %default = map { ($_, 1) } @$exp ; # That which is in @EXPORT
        foreach my $name (suq @{$$etg{IMPLICIT}}) {
          if (!exists($default{$name})) {
            err "'$name' is in '$ep\'s 'IMPLICIT' tag list, but not in \@EXPORT" ;
          } ;
        } ;
      } ;
    } ;
  } ;

  #----------------------------------------------------------------------------------
  # d) check that everything in the @EXPORT_FAIL is in @EXPORT or @EXPORT_OK

  if (my $ef = stash_value($st, '@EXPORT_FAIL')) {
    foreach my $name (suq(@$ef)) {
      if (!exists($all_exports{$name})) {
        err "'$name' is in '$ep\'s \@EXPORT_FAIL, but not in \@EXPORT or \@EXPORT_OK" ;
      } ;
    } ;
  } ;

  #----------------------------------------------------------------------------------
  # Done -- record exporting package and whether it has an 'IMPLICIT' tag

  return $exporters{$ep} = $implicit ;
} ;

#=========================================================================================
# ensure::unimport
# ================
#
# unimport: mechanics for no ensure qw(....) ; 
#
#   no ensure qw(name $name @name %name &name *name)
#
# The export checks use the full name, complete with decoration.  So if you want to
# export an undefined '@name' (for example) you need to be specific.
#
# The CHECK block ignores the decoration.  You can say, for example, that you expect
# '$name' to be undefined, the effect is that it is deemed OK if nothing at all is
# defined for 'name'.
#
# Requires: $self  = ourselves (__PACKAGE__) !
#           list of possibly decorated names
#
# Returns:  nothing

sub unimport {
  shift(@_) ;           # Discard self
  push @{$no_ensure{scalar(caller)} ||= []}, @_ ;
  return 1 ;
} ;

#=========================================================================================
# Post Compile-Time Checks -- the ensure CHECK block
# ==================================================
#
# For all packages that have been registered, we look for any completely undefined
# simple names -- which we treat as undefined subroutine errors, unless declared
# 'no ensure'.
#
# By simple we mean names starting '_' or alphabetic, excluding a small number of
# well known names.

CHECK {

  # These may appear undefined in the stash or are otherwise not worth checking.

  # a and b appear if sort is used ?
  # MODIFY_xxx_ATTRIBUTES appear and are undefined if a variable is declared ': shared'.

  my %except = map { ($_, 1) } (qw(a b BEGIN UNITCHECK CHECK INIT END
                                   DESTROY AUTOLOAD __ANON__
                                   MODIFY_SCALAR_ATTRIBUTES
                                   MODIFY_ARRAY_ATTRIBUTES
                                   MODIFY_HASH_ATTRIBUTES)) ;
                                 # ENV INC ARGV ARGVOUT SIG STDIN STDOUT STDERR _)) ;

  # Run checks across all registered packages

  foreach my $pkg (sort keys(%packages)) {

    # Collect any 'no ensure' names

    my %undefined = () ;
    if (exists($no_ensure{$pkg})) {
      %undefined = map { s/^[\$@%&*]// ; ($_, 1) } @{$no_ensure{$pkg}} ;
    } ;

    # Check the stash for this package 

    my $stash = $packages{$pkg} ; # Stash for package

    NAME: foreach my $name (sort keys %$stash) {
      if (($name !~ m/^\w/) || ($name =~ m/^\d/)  # Ignore names which are not simple...
                            || $except{$name}     # ...or which are exceptional
                            || $undefined{$name}) # ...or which are declared 'no ensure'
        { next NAME ; } ;

      my $rv = $stash->{$name} ;  # Get the stash entry

      # OK if stash entry is ref() (=> is 5.10.0 or later 'constant')
      #    or if have a {CODE} value -- these are the commonest cases !

      if (ref($rv) || defined(*$rv{CODE})) { next NAME ; } ;

      # OK if glob has a defined {SCALAR} value
      #          or if undefined {SCALAR} is import of an exported 'no ensure'

      my $rs = *$rv{SCALAR} ;
      if (defined($rs) && (defined($$rs) || exists($no_scalar{"$rs"}))) { next NAME ; } ;

      # OK if glob has at least one of these other types of value.

      foreach my $type (qw(ARRAY HASH IO FORMAT)) {
        if (defined(*$rv{$type})) { next NAME ; } ;
      } ;

      # Generate error for name with no defined value

      err "$pkg\:\:$name is undefined" ;
    } ;
  } ;

  # Now...  if any errors seen by ensure, give up !

 if ($ensure_errors) { crunch "$ensure_errors errors found" ; } ;
} ;

#=========================================================================================
# Stash Access

#-----------------------------------------------------------------------------------------
# stash: get ref:Stash for given package
#
# Requires: $pkg   -- package name -- no trailing '::'  -- ASSUMED VALID
#
# Returns:  ref:Stash -- i.e. hash containing symbols for given package

sub stash {
  my ($pkg) = @_ ;
  no strict qw(refs) ;
  return *{$pkg.'::'}{HASH}   or crunch "cannot find package '$pkg'" ;
} ;

#-----------------------------------------------------------------------------------------
# stash_value: get value of SCALAR, ARRAY or HASH from given package/stash
#
# Requires: $st       -- ref:Stash (as returned by stash())
#           $name     -- decorated name of value
#
# Returns:  value     -- if SCALAR and scalar is defined
#           ref:Value -- if ARRAY, HASH and value is defined
#           undef     -- name not found or value not defined 

sub stash_value {
  my ($st, $name) = @_ ;

  my ($id, $type) = undecorate($name) ;
  my $rv = $st->{$id} ;
  if (defined($rv)) {
    if (!ref($rv)) {
      $rv = *$rv{$type} ;
      if (defined($rv) && ($type eq 'SCALAR')) { $rv = $$rv ; } ;
    }
    else {
      $rv = undef ;     # ref:SCALAR or ref:REF => 5.10.0 type constant
    } ;
  } ;

  return $rv ;
} ;

#-----------------------------------------------------------------------------------------
# undecorate: remove decoration from name and return explicit type, if any
#
# Requires: $name     -- possibly decorated name
#
# Returns:  ($id, $type)  -- $id    = name less any decoration
#                            $type  = if decorated: SCALAR, ARRAY, HASH, CODE or GLOB
#                                        otherwise: ''

my %TYPE = qw($ SCALAR  @ ARRAY  % HASH  & CODE  * GLOB) ;

sub undecorate {
  my ($id) = @_ ;
  my $type = '' ;
  if ($id =~ s/^([\$@%&*])//) { $type = $TYPE{$1} ; } ;
  return ($id, $type) ;
} ;

#_________________________________________________________________________________________
1 ; # OK -- end of ensure

__END__

#=========================================================================================

=head1 NAME

ensure - Perl extension to ensure that things, particularly subroutines, are defined before
a program runs -- to avoid being bitten by C<Undefined subroutine &main::foo ...>
run-time errors.

C<use strict> will find undefined variables, but not undefined subroutines (except for
bareword subroutine references).  You could argue that C<use strict> is incomplete, and
I wouldn't disagree.  But in the meantime...

=head1 VERSION

Version 1.05.

=head1 SYNOPSIS

   use ensure ;

which, in a package, is intended to replace:

   use Exporter qw(import) ;

=head1 DESCRIPTION

The objective of the ensure package is to ensure that things are defined before a program
runs:

=over

=item *

check for undefined subroutines before program runs (as far as possible).

=item *

check exports are defined when package is used.

=back

First, C<use ensure> creates an C<CHECK> block to check that registered packages are
"complete": looks for names which have no part defined -- probably as the result of a
missing subroutine definition or a misspelling of a subroutine name.

All packages which C<use ensure> are registered for this check.

Second, C<use ensure> imports the C<ensure::import> function.  This sits on top of the
C<Exporter::import> function, and has two effects:

=over

=item *

the first time the package is itself used, the package's exports are checked.
(See below for what is checked).

=item *

every time the package is used the import list extensions (C<:ALL>, C<:NONE> &
C<:IMPLICIT>) are implemented, before jumping to the standard C<Exporter::import>.
(See below for what these extensions do.)

=back

Packages that C<use ensure> do not need to import the C<Exporter::import> (and must not).

NB: it would be perverse to C<use ensure ()>, because that inhibits all of the above.

C<no ensure> may be used to declare things which are not expected to be defined when
the CHECK block checks and the export checks are run.

=head2 The C<CHECK> block check

The check scans the registered packages' symbol tables, looking for names which have no
part defined.  This will find most undefined subroutines.

However, a reference to an undefined subroutine, eg C<fred()>, can be masked by any other
package (C<our>) variable of the same name -- but not by private (C<my>) variables.

This means that:

   sub freda { .... } ;
   sub bill {
     ...
     fred(...) ;
     ...
   } ;

will be generally be picked up, trapping (a) a missing definition for C<fred()>,
or (b) a spelling mistake in C<sub freda>, or (c) a spelling mistake in C<fred(...)>.

However the presence of (for example):

   our @fred ;

will mask the use of the undefined subroutine C<fred()>, so the error is not trapped.

The effect of:

   our $fred ;

is more complicated, because in the special case of C<SCALAR> variables, it is not
possible to tell the difference between a variable that has not been defined at all, and
a variable that has been defined but whose value is C<undef>.  So, if the value of
C<$fred> is C<undef> when the C<CHECK> block is run, it will B<not> mask the presence of
an undefined subroutine C<fred>, but it will if it has any other value.

I<[This could be improved if I knew how to spot subroutine names that have
been used, but not defined.]>

The check ignores names that do not start with an alphabetic character or C<'_'>.  It also
ignores a number of names which appear in package symbol tables and are often undefined:

    a  b
    BEGIN  UNITCHECK  CHECK  INIT  END  DESTROY   AUTOLOAD   __ANON__
    MODIFY_SCALAR_ATTRIBUTES  MODIFY_ARRAY_ATTRIBUTES  MODIFY_HASH_ATTRIBUTES

=head3 C<main> package C<SCALAR> variables

Any package (C<our>) C<SCALAR> variables in C<main> need to be declared C<no ensure>
(unless assigned a value in a C<BEGIN> block).

I<[This could be improved if I knew how to spot scalars that have been declared
but are yet to be assigned a value.]>

=head2 The export checks

The export checks examine the exporting package's C<@EXPORT>, C<@EXPORT_OK>,
C<%EXPORT_TAGS> and C<@EXPORT_FAIL> to ensure that:

=over

=item *

everything in C<@EXPORT> and C<@EXPORT_OK> is defined by the time the package is
first used -- or declared C<no ensure>.

I<This check for definition might have been left to the ensure> C<CHECK> I<block.
However, it seems that> C<Exporter::import> I<manages to define things, even
if there is no mention of them in the package itself -- so it is essential
to check before the first use of> C<Exporter::import>I<.>

=item *

everything in C<%EXPORT_TAGS> and in C<@EXPORT_FAIL> must appear in either
C<@EXPORT> or C<@EXPORT_OK>.

=item *

if there is a C<IMPLICIT> tag in C<%EXPORT_TAGS>, everything in that tag must
appear in C<@EXPORT> (ie C<IMPLICIT> must be a subset of C<DEFAULT>).

=back

=head2 The import list extensions

The import list extensions are:

=over

=item 1.

C<:NONE> pseudo tag

This is expected to be the B<only> item in the import list.  In any case it should
be the B<first> item.

The purpose of the C<:NONE> tag is to allow for nothing to be imported, while still
invoking the C<ensure::import> function to implement the export checks.

When the import list is processed, if the first tag is C<:NONE> it is removed, along
with any following '!' items (which are redundant, and would otherwise pull in the
C<:DEFAULT> set).

If the result is an empty list, then C<Exporter::import> will not be invoked !

It is slightly clearer than C<!:DEFAULT>, though essentially equivalent.

NB: C<!:NONE> has no meaning and will generate an error (unless an actual C<NONE>
tag exists in C<%EXPORT_TAGS>).  Similarly C<:NONE> as second or subsequent item.

=item 2.

C<:ALL> pseudo tag

If the B<first> item in the import list is C<:ALL>, it will be replaced by the
entirity of C<@EXPORT> and C<@EXPORT_OK>.

NB: C<!:ALL> has no meaning and will generate an error (unless an actual C<ALL>
tag exists in C<%EXPORT_TAGS>).  Similarly C<:ALL> as second or subsequent item.

=item 3.

C<IMPLICIT> tag -- specifies a minimum, required set of imports/exports.

When a package is used the items named in @C<EXPORTS> (aka the C<:DEFAULT> exports) are
automatically exported, except for:

=over

=item 1.

C<use Foo () ;       >  -- imports nothing.  C<Foo-<gt>import> is not called at all.

=item 2.

C<use Foo qw(a ...) ;>  -- explicitly imports only the named items.

=back

If the exporting package has some subset of the C<:DEFAULT> exports which it wants to
export even in case (2), then it can add an C<IMPLICIT> tag to C<%EXPORT_TAGS>,
containing that subset of C<@EXPORTS>.

If the importing package requires the absolute minimum of imports it can do:

   use Foo qw(:IMPLICIT) ;

Note that C<:IMPLICIT> can appear as the B<first> tag even if the package has no
C<IMPLICIT> tag, and will behave as if there was an empty C<IMPLICIT> tag.

Note that C<:NONE> will import nothing, not even C<:IMPLICIT> stuff.

NB: C<!:IMPLICIT> has no meaning and will generate an error (unless an actual
C<IMPLICIT> tag exists in C<%EXPORT_TAGS>).  Similarly C<:IMPLICIT> as second or
subsequent item.

Any package that uses C<IMPLICIT> should advertise the fact B<clearly>.

=back

=head2 no ensure

Names may be declared as C<no ensure>, thus:

   no ensure qw(a $a @a %a &a *a ...)

A package may contain a number of C<no ensure> declarations, each adding to its list of
C<no ensure> names. 

For the C<CHECK> block the decoration is ignored.  Any name declared in C<no ensure> is not
checked when the relevant package is checked.

For the export check the decoration is significant.  It is not an error to export
something which is undefined if that something is declared C<no ensure>.  On the other
hand, it is an error to export something which is defined but is also declared
C<no ensure>.

=head1 EXPORT

Exports C<import>.  Suggest replacing C<use Exporter qw(import) ;> by C<use ensure ;>. 

=head1 SUBROUTINES/METHODS

C<ensure::register(>I<package>C<)> may be used to register a given I<package> for
C<ensure CHECK> block processing.

=head1 DIAGNOSTICS

Generates various C<warn> messages when it finds:

=over

=item *

C<'>I<name>C<' in '>I<package>C<' is declared 'no ensure', but is defined>

=item *

C<'>I<name>C<' is exported by '>I<package>C<' but is not defined>

=item *

C<'>I<name>C<' is in '>I<package>C<'s %EXPORT_TAGS, but not in @EXPORT or @EXPORT_OK>

=item *

C<'>I<name>C<' is in '>I<package>C<'s IMPLICIT tag list, but not in @EXPORT>

=item *

C<'>I<name>C<' is in '>I<package>C<'s @EXPORT_FAIL, but not in @EXPORT or @EXPORT_OK>

=item *

C<'>I<package>C<::>I<name>C<' is undefined> 

=back

If any such warnings are issued, the C<ensure CHECK> will C<die> once all registered
packages have been checked:

=over

=item *

I<n>C< ensure errors>

=back

=head1 DEPENDENCIES

The C<ensure::import> function uses the C<Exporter::import> function.

=head1 INCOMPATIBILITIES

None known, but tested only on Perl v5.8.8 and v5.10.0.

=head1 BUGS AND LIMITATIONS

None known.

=head1 AUTHOR

Chris Hall <chris.hall@highwayman.com>

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2008 by Highwayman Associates Ltd.  All rights reserved

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
