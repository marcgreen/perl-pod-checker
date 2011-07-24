#############################################################################
# Pod/Checker.pm -- check pod documents for syntax errors
#
# Copyright (C) 1994-2000 by Bradford Appleton. All rights reserved.
# This file is part of "PodParser". PodParser is free software;
# you can redistribute it and/or modify it under the same terms
# as Perl itself.
#############################################################################

package Pod::Checker;
use strict;
use warnings;

our $VERSION = '1.45';  ## Current version of this package
require  5.005;    ## requires this Perl version or later

use Pod::ParseUtils; ## for hyperlinks

=head1 NAME

Pod::Checker, podchecker() - check pod documents for syntax errors

=head1 SYNOPSIS

  use Pod::Checker;

  $syntax_okay = podchecker($filepath, $outputpath, %options);

  my $checker = Pod::Checker->new(%options);
  $checker->parse_from_file($filepath, \*STDERR);

=head1 OPTIONS/ARGUMENTS

C<$filepath> is the input POD to read and C<$outputpath> is
where to write POD syntax error messages. Either argument may be a scalar
indicating a file-path, or else a reference to an open filehandle.
If unspecified, the input-file it defaults to C<\*STDIN>, and
the output-file defaults to C<\*STDERR>.

=head2 podchecker()

This function can take a hash of options:

=over 4

=item B<-warnings> =E<gt> I<val>

Turn warnings on/off. I<val> is usually 1 for on, but higher values
trigger additional warnings. See L<"Warnings">.

=item B<-quiet> =E<gt> I<val>

If C<num> is true, do not print any errors/warnings.

=back

=head1 DESCRIPTION

B<podchecker> will perform syntax checking of Perl5 POD format documentation.

Curious/ambitious users are welcome to propose additional features they wish
to see in B<Pod::Checker> and B<podchecker> and verify that the checks are
consistent with L<perlpod>.

The following checks are currently performed:

=over 4

=item *

Unknown '=xxxx' commands, unknown 'XE<lt>...E<gt>' interior-sequences,
and unterminated interior sequences.

=item *

Check for proper balancing of C<=begin> and C<=end>. The contents of such
a block are generally ignored, i.e. no syntax checks are performed.

=item *

Check for proper nesting and balancing of C<=over>, C<=item> and C<=back>.

=item *

Check for same nested interior-sequences (e.g.
C<LE<lt>...LE<lt>...E<gt>...E<gt>>).

=item *

Check for malformed or non-existing entities C<EE<lt>...E<gt>>.

=item *

Check for correct syntax of hyperlinks C<LE<lt>...E<gt>>. See L<perlpod>
for details.

=item *

Check for unresolved document-internal links. This check may also reveal
misspelled links that seem to be internal links but should be links
to something else.

=back

=head1 DIAGNOSTICS

=head2 Errors

=over 4

=item * empty =headn

A heading (C<=head1> or C<=head2>) without any text? That ain't no
heading!

=item * =over on line I<N> without closing =back

The C<=over> command does not have a corresponding C<=back> before the
next heading (C<=head1> or C<=head2>) or the end of the file.

=item * =item without previous =over

=item * =back without previous =over

An C<=item> or C<=back> command has been found outside a
C<=over>/C<=back> block.

=item * No argument for =begin

A C<=begin> command was found that is not followed by the formatter
specification.

=item * =end without =begin

A standalone C<=end> command was found.

=item * Nested =begin's

There were at least two consecutive C<=begin> commands without
the corresponding C<=end>. Only one C<=begin> may be active at
a time.

=item * =for without formatter specification

There is no specification of the formatter after the C<=for> command.

=item * unresolved internal link I<NAME>

The given link to I<NAME> does not have a matching node in the current
POD. This also happened when a single word node name is not enclosed in
C<"">.

=item * Unknown command "I<CMD>"

An invalid POD command has been found. Valid are C<=head1>, C<=head2>,
C<=head3>, C<=head4>, C<=over>, C<=item>, C<=back>, C<=begin>, C<=end>,
C<=for>, C<=pod>, C<=cut>

=item * Unknown interior-sequence "I<SEQ>"

An invalid markup command has been encountered. Valid are:
C<BE<lt>E<gt>>, C<CE<lt>E<gt>>, C<EE<lt>E<gt>>, C<FE<lt>E<gt>>,
C<IE<lt>E<gt>>, C<LE<lt>E<gt>>, C<SE<lt>E<gt>>, C<XE<lt>E<gt>>,
C<ZE<lt>E<gt>>

=item * nested commands I<CMD>E<lt>...I<CMD>E<lt>...E<gt>...E<gt>

Two nested identical markup commands have been found. Generally this
does not make sense.

=item * garbled entity I<STRING>

The I<STRING> found cannot be interpreted as a character entity.

=item * Entity number out of range

An entity specified by number (dec, hex, oct) is out of range (1-255).

=item * malformed link LE<lt>E<gt>

The link found cannot be parsed because it does not conform to the
syntax described in L<perlpod>.

=item * nonempty ZE<lt>E<gt>

The C<ZE<lt>E<gt>> sequence is supposed to be empty.

=item * empty XE<lt>E<gt>

The index entry specified contains nothing but whitespace.

=item * Spurious text after =pod / =cut

The commands C<=pod> and C<=cut> do not take any arguments.

=item * Spurious character(s) after =back

The C<=back> command does not take any arguments.

=back

=head2 Warnings

These may not necessarily cause trouble, but indicate mediocre style.

=over 4

=item * multiple occurrence of link target I<name>

The POD file has some C<=item> and/or C<=head> commands that have
the same text. Potential hyperlinks to such a text cannot be unique then.
This warning is printed only with warning level greater than one.

=item * line containing nothing but whitespace in paragraph

There is some whitespace on a seemingly empty line. POD is very sensitive
to such things, so this is flagged. B<vi> users switch on the B<list>
option to avoid this problem.

=begin _disabled_

=item * file does not start with =head

The file starts with a different POD directive than head.
This is most probably something you do not want.

=end _disabled_

=item * previous =item has no contents

There is a list C<=item> right above the flagged line that has no
text contents. You probably want to delete empty items.

=item * preceding non-item paragraph(s)

A list introduced by C<=over> starts with a text or verbatim paragraph,
but continues with C<=item>s. Move the non-item paragraph out of the
C<=over>/C<=back> block.

=item * =item type mismatch (I<one> vs. I<two>)

A list started with e.g. a bullet-like C<=item> and continued with a
numbered one. This is obviously inconsistent. For most translators the
type of the I<first> C<=item> determines the type of the list.

=item * I<N> unescaped C<E<lt>E<gt>> in paragraph

Angle brackets not written as C<E<lt>ltE<gt>> and C<E<lt>gtE<gt>>
can potentially cause errors as they could be misinterpreted as
markup commands. This is only printed when the -warnings level is
greater than 1.

=item * Unknown entity

A character entity was found that does not belong to the standard
ISO set or the POD specials C<verbar> and C<sol>.

=item * No items in =over

The list opened with C<=over> does not contain any items.

=item * No argument for =item

C<=item> without any parameters is deprecated. It should either be followed
by C<*> to indicate an unordered list, by a number (optionally followed
by a dot) to indicate an ordered (numbered) list or simple text for a
definition list.

=item * empty section in previous paragraph

The previous section (introduced by a C<=head> command) does not contain
any text. This usually indicates that something is missing. Note: A
C<=head1> followed immediately by C<=head2> does not trigger this warning.

=item * Verbatim paragraph in NAME section

The NAME section (C<=head1 NAME>) should consist of a single paragraph
with the script/module name, followed by a dash `-' and a very short
description of what the thing is good for.

=item * =headI<n> without preceding higher level

For example if there is a C<=head2> in the POD file prior to a
C<=head1>.

=back

=head2 Hyperlinks

There are some warnings with respect to malformed hyperlinks:

=over 4

=item * ignoring leading/trailing whitespace in link

There is whitespace at the beginning or the end of the contents of
LE<lt>...E<gt>.

=item * (section) in '$page' deprecated

There is a section detected in the page name of LE<lt>...E<gt>, e.g.
C<LE<lt>passwd(2)E<gt>>. POD hyperlinks may point to POD documents only.
Please write C<CE<lt>passwd(2)E<gt>> instead. Some formatters are able
to expand this to appropriate code. For links to (builtin) functions,
please say C<LE<lt>perlfunc/mkdirE<gt>>, without ().

=item * alternative text/node '%s' contains non-escaped | or /

The characters C<|> and C</> are special in the LE<lt>...E<gt> context.
Although the hyperlink parser does its best to determine which "/" is
text and which is a delimiter in case of doubt, one ought to escape
these literal characters like this:

  /     E<sol>
  |     E<verbar>

=back

=head1 RETURN VALUE

B<podchecker> returns the number of POD syntax errors found or -1 if
there were no POD commands at all found in the file.

=head1 EXAMPLES

See L</SYNOPSIS>

=head1 INTERFACE

While checking, this module collects document properties, e.g. the nodes
for hyperlinks (C<=headX>, C<=item>) and index entries (C<XE<lt>E<gt>>).
POD translators can use this feature to syntax-check and get the nodes in
a first pass before actually starting to convert. This is expensive in terms
of execution time, but allows for very robust conversions.

Since PodParser-1.24 the B<Pod::Checker> module uses only the B<poderror>
method to print errors and warnings. The summary output (e.g.
"Pod syntax OK") has been dropped from the module and has been included in
B<podchecker> (the script). This allows users of B<Pod::Checker> to
control completely the output behavior. Users of B<podchecker> (the script)
get the well-known behavior.

B<Note:> This module no longer uses B<poderror> to print errors and warnings 
as of PodChecker-1.46. This is due to the port to B<Pod::Simple>, which
uses its own B<whine> method to output errors.

=cut

#############################################################################

#use diagnostics;
use Carp qw(croak);
use Exporter 'import';
use base qw/Pod::Simple::Methody/;

our @EXPORT = qw(&podchecker);

##---------------------------------
## Function definitions begin here
##---------------------------------

sub podchecker {
    my ($infile, $outfile, %options) = @_;
    local $_;

    ## Set defaults
    $infile  ||= \*STDIN;
    $outfile ||= \*STDERR;

    ## Now create a pod checker
    my $checker = Pod::Checker->new(%options);

    ## Now check the pod document for errors
    $checker->parse_from_file($infile, $outfile);

    ## Return the number of errors found
    return $checker->num_errors();
}


##---------------------------------------------------------------------------

##-------------------------------
## Method definitions begin here
##-------------------------------

##################################

=over 4

=item C<Pod::Checker-E<gt>new( %options )>

Return a reference to a new Pod::Checker object that inherits from
Pod::Simple and is used for calling the required methods later. The
following options are recognized:

C<-warnings =E<gt> num>
  Print warnings if C<num> is true. The higher the value of C<num>,
the more warnings are printed. Currently there are only levels 1 and 2.

C<-quiet =E<gt> num>
  If C<num> is true, do not print any errors/warnings. This is useful
when Pod::Checker is used to munge POD code into plain text from within
POD formatters.

=cut

## sub new {
##     my $this = shift;
##     my $class = ref($this) || $this;
##     my %params = @_;
##     my $self = {%params};
##     bless $self, $class;
##     $self->initialize();
##     return $self;
## }

sub initialize {
    my $self = shift;
    ## Initialize number of errors, and setup an error function to
    ## increment this number and then print to the designated output.
    $self->{_NUM_ERRORS} = 0;
    $self->{_NUM_WARNINGS} = 0;
    $self->{-quiet} ||= 0;
    # set the error handling subroutine
    $self->errorsub($self->{-quiet} ? sub { 1; } : 'poderror');
    $self->{_commands} = 0; # total number of POD commands encountered
    $self->{_list_stack} = []; # stack for nested lists
    $self->{_have_begin} = ''; # stores =begin
    $self->{_links} = []; # stack for internal hyperlinks
    $self->{_nodes} = []; # stack for =head/=item nodes
    $self->{_index} = []; # text in X<>
    # print warnings?
    $self->{-warnings} = 1 unless(defined $self->{-warnings});
    $self->{_current_head1} = ''; # the current =head1 block
    $self->parseopts(-process_cut_cmd => 1, -warnings => $self->{-warnings});
}

##################################

=item C<$checker-E<gt>poderror( @args )>

=item C<$checker-E<gt>poderror( {%opts}, @args )>

Internal method for printing errors and warnings. If no options are given,
simply prints "@_". The following options are recognized and used to form
the output:

  -msg

A message to print prior to C<@args>.

  -line

The line number the error occurred in.

  -file

The file (name) the error occurred in.

  -severity

The error level, should be 'WARNING' or 'ERROR'.

=cut

# Invoked as $self->poderror( @args ), or $self->poderror( {%opts}, @args )
sub poderror {
    my $self = shift;
    my %opts = (ref $_[0]) ? %{shift()} : ();

    ## Retrieve options
    chomp( my $msg  = ($opts{-msg} || '')."@_" );
    my $line = (exists $opts{-line}) ? " at line $opts{-line}" : '';
    my $file = (exists $opts{-file}) ? " in file $opts{-file}" : '';
    unless (exists $opts{-severity}) {
       ## See if can find severity in message prefix
       $opts{-severity} = $1  if ( $msg =~ s/^\**\s*([A-Z]{3,}):\s+// );
    }
    my $severity = (exists $opts{-severity}) ? "*** $opts{-severity}: " : '';

    ## Increment error count and print message "
    ++($self->{_NUM_ERRORS})
        if(!%opts || ($opts{-severity} && $opts{-severity} eq 'ERROR'));
    ++($self->{_NUM_WARNINGS})
        if(!%opts || ($opts{-severity} && $opts{-severity} eq 'WARNING'));
    unless($self->{-quiet}) {
      my $out_fh = $self->output_handle() || \*STDERR;
      print $out_fh ($severity, $msg, $line, $file, "\n")
        if($self->{-warnings} || !%opts || $opts{-severity} ne 'WARNING');
    }
}

##################################

=item C<$checker-E<gt>num_errors()>

Set (if argument specified) and retrieve the number of errors found.

=cut

sub num_errors {
   return (@_ > 1) ? ($_[0]->{_NUM_ERRORS} = $_[1]) : $_[0]->{_NUM_ERRORS};
}

##################################

=item C<$checker-E<gt>num_warnings()>

Set (if argument specified) and retrieve the number of warnings found.

=cut

sub num_warnings {
   return (@_ > 1) ? ($_[0]->{_NUM_WARNINGS} = $_[1]) : $_[0]->{_NUM_WARNINGS};
}

##################################

=item C<$checker-E<gt>name()>

Set (if argument specified) and retrieve the canonical name of POD as
found in the C<=head1 NAME> section.

=cut

sub name {
    return (@_ > 1 && $_[1]) ?
        ($_[0]->{-name} = $_[1]) : $_[0]->{-name};
}

##################################

=item C<$checker-E<gt>node()>

Add (if argument specified) and retrieve the nodes (as defined by C<=headX>
and C<=item>) of the current POD. The nodes are returned in the order of
their occurrence. They consist of plain text, each piece of whitespace is
collapsed to a single blank.

=cut

sub node {
    my ($self,$text) = @_;
    if(defined $text) {
        $text =~ s/\s+$//s; # strip trailing whitespace
        $text =~ s/\s+/ /gs; # collapse whitespace
        # add node, order important!
        push(@{$self->{_nodes}}, $text);
        # keep also a uniqueness counter
        $self->{_unique_nodes}->{$text}++ if($text !~ /^\s*$/s);
        return $text;
    }
    @{$self->{_nodes}};
}

##################################

=item C<$checker-E<gt>idx()>

Add (if argument specified) and retrieve the index entries (as defined by
C<XE<lt>E<gt>>) of the current POD. They consist of plain text, each piece
of whitespace is collapsed to a single blank.

=cut

# set/return index entries of current POD
sub idx {
    my ($self,$text) = @_;
    if(defined $text) {
        $text =~ s/\s+$//s; # strip trailing whitespace
        $text =~ s/\s+/ /gs; # collapse whitespace
        # add node, order important!
        push(@{$self->{_index}}, $text);
        # keep also a uniqueness counter
        $self->{_unique_nodes}->{$text}++ if($text !~ /^\s*$/s);
        return $text;
    }
    @{$self->{_index}};
}

##################################

=item C<$checker-E<gt>hyperlink()>

Add (if argument specified) and retrieve the hyperlinks (as defined by
C<LE<lt>E<gt>>) of the current POD. They consist of a 2-item array: line
number and C<Pod::Hyperlink> object.

=back

=cut

# set/return hyperlinks of the current POD
sub hyperlink {
    my $self = shift;
    if($_[0]) {
        push(@{$self->{_links}}, $_[0]);
        return $_[0];
    }
    @{$self->{_links}};
}


# override Pod::Simple's whine() and scream() to use poderror()

# Note:
# Ignore $self->{'no_whining'} b/c $self->{'quiet'} takes care of it in poderror
# Don't bother incrementing $self->{'errors_seen'} -- it's not used
# Don't bother pushing to $self->{'errata'} b/c poderror() outputs immediately
# We don't need to set $new->no_errata_section(1) b/c of these overrides


sub whine {
    my ($self, $line, $complaint) = @_;

    $self->poderror({-line => $line,
                     -severity => 'ERROR',
                     -msg => $complaint });

    return 1; # assume everything is peachy keen
}

sub scream {
    my ($self, $line, $complaint) = @_;

    $self->poderror({-line => $line,
                     -severity => 'ERROR', # consider making severity 'FATAL'
                     -msg => $complaint });

    return 1;
}

}

        }
    }
}


    }
}

    }
}

    my $self = shift;
    }
}

1;

__END__

=head1 AUTHOR

Please report bugs using L<http://rt.cpan.org>.

Brad Appleton E<lt>bradapp@enteract.comE<gt> (initial version),
Marek Rouchal E<lt>marekr@cpan.orgE<gt>,
Marc Green E<lt>marcgreen@cpan.orgE<gt> (port to Pod::Simple)

Based on code for B<Pod::Text::pod2text()> written by
Tom Christiansen E<lt>tchrist@mox.perl.comE<gt>

=cut
