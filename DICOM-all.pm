# DICOM.pm
# Andrew Crabb (ahc@jhu.edu), May 2002.
# Jonathan Harlap (jharlap@bic.mni.mcgill.ca) 2003
# Alexandre Carmel-Veilleux (acveilleux@neurorx.com) 2004-2005
# Perl module to read DICOM headers.
# TODO: add support for sequences (SQ) (currently being skipped)
# $Id: DICOM-all.pm,v 1.1 2009/03/03 11:04:09 rotor Exp $

package DICOM;

use strict;
use vars qw($VERSION %dict);


# use DICOM::Element;
########### DICOM::Element.pm #################

# Element.pm ver 0.3
# Andrew Crabb (ahc@jhu.edu), May 2002.
# Element routines for DICOM.pm: a Perl module to read DICOM headers.
# $Id: DICOM-all.pm,v 1.1 2009/03/03 11:04:09 rotor Exp $

# Each element is a hash with the following keys:
#   group	Group (hex).
#   element	Element within group (hex).
#   offset	Offset from file start (dec).
#   code	Field code eg 'US', 'UI'.
#   length	Field value length (dec).
#   name	Field descriptive name.
#   value	Value.
#   header	All bytes up to value, for easy writing.

#use strict;
#use DICOM::VRfields;
#use vars qw($VERSION %VR);

#$VERSION = sprintf "%d.%03d", q$Revision: 1.1 $ =~ /: (\d+)\.(\d+)/;

#my %VR;			# Value Representations (DICOM Std PS 3.5 Sect 6.2)
my ($SHORT, $INT) = (2, 4);	# Constants: Byte sizes.
my ($FLOAT, $DOUBLE) = ('f', 'd');  # Constants: unpack formats
# Names of the element fields.
my @fieldnames = qw(group element offset code length name value header);
my $big_endian_machine = unpack("h*", pack("s", 1)) =~ /01/;

# Initialize VR hash only once.
# Fill in VR definitions from DICOM_fields.
BEGIN {
  foreach my $line (@VR) {
    next if ($line =~ /^\#/);
    my ($vr, $name, $len, $fix, $numeric, $byteswap) = split(/\t+/, $line);
    $VR{$vr} = [($name, $len, $fix, $numeric, $byteswap)];
  }
}

sub new {
  my $type = shift;
  my $self = {};
  return bless $self, $type;
}

# Fill in self from file.

sub fill {
  my $this = shift;
  my ($IN, $dictref, $big_endian_image, $parent) = @_;
  my %dict = %$dictref;
  #my ($group, $element, $offset, $code, $length, $name, $value, $header);
  my $vrstr;

  # Tag holds group and element numbers in two bytes each.
  $this->{'offset'} = tell($IN);
  $this->{'group'}    = sprintf "%04X", readInt($IN, $SHORT);
  $this->{'element'}  = sprintf "%04X", readInt($IN, $SHORT);
  # Next 4 bytes are either explicit VR or length (implicit VR).
  ($vrstr, $this->{'length'}) = readLength($IN);

  if ($this->{'length'} == ((2 ** 32) - 1)) {
    # -1 means infinite length.
    # we only handle the case where it's an image....
    #
    if ($this->{'group'} eq "7FE0" && $this->{'element'} eq "0010") {
      $this->{'length'} = $parent->value("7FE0", "0000");
    }
  }

  # Go to record start, read bytes up to value field, store in header.
  my $diff = tell($IN) - $this->{'offset'};
  seek($IN, $this->{'offset'}, 0);
  read($IN, $this->{'header'}, $diff);

  if (exists($dict{$this->{'group'}}{$this->{'element'}})) {
      ($this->{'code'},$this->{'name'}) = @{$dict{$this->{'group'}}{$this->{'element'}}};
  } else {
      ($this->{'code'}, $this->{'name'}) = ("--", "UNKNOWN");
      $this->{'code'} = $vrstr if defined $vrstr;
  }

  # Read in the value field.  Certain fields need to be decoded.
  $this->{'value'} = "";
  if ($this->{'length'} > 0) {
      #print "Reading $this->{'group'}:$this->{'element'} ($this->{'code'} | $this->{'name'}) length $this->{'length'}\n";
    SWITCH: {
      # Decode ints and shorts.
      if ($this->{'code'} eq "UL") {$this->{'value'} = readInt($IN, $INT, $this->{'length'});  last SWITCH;}
      if ($this->{'code'} eq "US") {$this->{'value'} = readInt($IN, $SHORT, $this->{'length'});last SWITCH;}
      # Certain VRs not yet implemented: Single and double precision floats.
      if ($this->{'code'} eq "FL") {$this->{'value'} = readFloat($IN, $FLOAT, $this->{'length'}); last SWITCH;}
      if ($this->{'code'} eq "FD") {$this->{'value'} = readFloat($IN, $DOUBLE, $this->{'length'}); last SWITCH;}
      if ($this->{'code'} eq "SQ") {$this->{'value'} = readSequence($IN, $this->{'length'}); last SWITCH; }
      # Made it to here: Read bytes verbatim.
      read($IN, $this->{'value'}, $this->{'length'}) or die "read($this->{'group'}, $this->{'element'}, $this->{'length'})";
    }

    # byte swap value if appropriate
#    if($vrbyteswap && ($big_endian_image xor $big_endian_machine)) {
#	byteswap(\$this->{'value'});
#    }

    # UI may be padded with single trailing NULL (PS 3.5: 6.2.1)
    ($this->{'code'} eq "UI") and $this->{'value'} =~ s/\0$//;
  }

  return $this;
}

# readInt(instream, bytelength, fieldlength).
#   instream:	Input file stream.
#   bytelength: SHORT (2) or INT (4) bytes.
#   fieldlength:Total number of bytes in the field.
# If fieldlength > bytelength, multiple values are read in and
# stored as a string representation of an array.

sub readInt {
  my ($IN, $bytes, $len) = @_;
  my ($buff, $val, @vals);
  # Perl little endian decode format for short (v) or int (V).
  my $format = ($bytes == $SHORT) ? "v" : "V";
  $len = $bytes unless (defined($len));

  read($IN, $buff, $len) or die;
  if ($len == $bytes) {
    $val = unpack($format, $buff)+0;
  } else {
    # Multiple values: Create array.
    for (my $pos = 0; $pos < $len; $pos += 2) {
      push(@vals, unpack("$format", substr($buff, $pos, 2))+0);
    }
    $val = "[" . join(", ", @vals) . "]";
  }

  return $val;
}

sub writeInt {
  my ($this, $OUT, $bytes) = @_;
  my $val = $this->{value};
  my $format = ($bytes == $SHORT) ? "v" : "V";

  # Arrays of values stored as string [val1, val2, val3].
  $val =~ s/[\[\]]//g;
  my @vals = split(/[^-+0-9]+/, $val);
  foreach my $elem (@vals) {
    my $buff = pack("$format", $elem);
    print $OUT $buff;
  }
}

sub readFloat {
    my ($IN, $format, $len) = @_;
    my ($buff, $val);

    read($IN, $buff, $len);

    $val = unpack($format, $buff);
    return sprintf("%e", $val);
}

sub writeFloat {
  my ($this, $OUT, $format) = @_;
  my $val = $this->{value};

  # Arrays of values stored as string [val1, val2, val3].
  $val =~ s/[\[\]]//g;
  my @vals = split(/[^-0-9.eE+]+/, $val);
  foreach my $elem (@vals) {
    my $buff = pack("$format", $elem);
    print $OUT $buff;
  }
}

sub readSequence {
    my ($IN, $len) = @_;
    my ($buff, $val);

#    print "0xFFFE has length: ".length(0xFFFE)." and looks like ".sprintf("%x", 0xFFFE)."\n";
#
#    printf "READING SQ AT ".tell($IN)." LENGTH: %x\n", $len;
#    if($len == 0xFFFFFFFF) { print "length is FFFF, FFFF\n"; }
#    else {print "length is NOT F's\n"; }

    # three different cases:
    # implicit VR, explicit length
    # explicit VR, undefined length, items of defined length (w/end delimiter)
    # implicit VR, undefined length, items of undefined length

    # defined length
    if($len > 0 and $len != 0xFFFFFFFF) {
#	printf "skipping forward 0x%x bytes\n", $len;
	read($IN, $buff, $len);
    } else {
      READLOOP:
	while(read($IN, $buff, 2)) {
	    $buff = unpack('v', $buff);
	    if($buff == 0xFFFE) {
#		print "found start of delimiter\n";
		read($IN, $buff, 2);
		$buff = unpack('v', $buff);
		if($buff == 0xE0DD) {
#		    print "found end of delimiter\n";
		    read($IN, $buff, 4);
		    $buff = unpack('v', $buff);
		    if($buff == 0x00000000) {
#			print "found length 0\n";
			last READLOOP;
		    } else {
			seek($IN, -4, 1);
		    }
		} else {
		    seek($IN, -2, 1);
		}
	    }
	}
    }

    return 'skipped';
}


# Return the Value Field length, and length before Value Field.
# Implicit VR: Length is 4 byte int.
# Explicit VR: 2 bytes hold VR, then 2 byte length.

sub readLength {
  my ($IN) = @_;
  my ($b0, $b1, $b2, $b3);
  my ($buff, $vrstr);

  # Read 4 bytes into b0, b1, b2, b3.
  foreach my $var (\$b0, \$b1, \$b2, \$b3) {
    read($IN, $$var, 1) or die("readLength: died reading $var\n");
    $$var = unpack("C", $$var);
  }
  # Temp string to test for explicit VR
  $vrstr = pack("C", $b0) . pack("C", $b1);
#print "Pos: ".tell($IN)." VR: $vrstr B0: $b0 B1: $b1 B2: $b2 B3: $b3\n";
  # Assume that this is explicit VR if b0 and b1 match a known VR code.
  # Possibility (prob 26/16384) exists that the two low order field length 
  # bytes of an implicit VR field will match a VR code.

  # DICOM PS 3.5 Sect 7.1.2: Data Element Structure with Explicit VR
  # Explicit VRs store VR as text chars in 2 bytes.
  # VRs of OB, OW, SQ, UN, UT have VR chars, then 0x0000, then 32 bit VL:
  #
  # +-----------------------------------------------------------+
  # |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 | 11 |
  # +----+----+----+----+----+----+----+----+----+----+----+----+
  # |<Group-->|<Element>|<VR----->|<0x0000->|<Length----------->|<Value->
  #
  # Other Explicit VRs have VR chars, then 16 bit VL:
  #
  # +---------------------------------------+
  # |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |
  # +----+----+----+----+----+----+----+----+
  # |<Group-->|<Element>|<VR----->|<Length->|<Value->
  #
  # Implicit VRs have no VR field, then 32 bit VL:
  #
  # +---------------------------------------+
  # |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |
  # +----+----+----+----+----+----+----+----+
  # |<Group-->|<Element>|<Length----------->|<Value->

  my $length = undef;
  if(defined($VR{$vrstr})) {
      # Have a code for an explicit VR: Retrieve VR element
      my $ref = $VR{$vrstr};
      my ($name, $bytes, $fixed, $numeric, $byteswap) = @$ref;
      if ($bytes == 0) {
	# This is an OB, OW, SQ, UN or UT: 32 bit VL field.
	# Have seen in some files length 0xffff here...
	$length = readInt($IN, $INT);
      } else {
	# This is an explicit VR with 16 bit length.
	$length = ($b3 << 8) + $b2;
    }
  } else {
      # Made it to here: Implicit VR, 32 bit length.
      $length = ($b3 << 24) + ($b2 << 16) + ($b1 << 8) + $b0 unless defined $length;
      $vrstr = undef;
  }

  # Return the value
  return ($vrstr, $length);
}

# Return the values of each field.

sub values {
  my $this = shift;
  my %hash = %$this;

  # Fieldnames are group element offset code length name value header.
  my @vals = @hash{@fieldnames};
  @vals = splice(@vals, 0, 6);	# Omit value & header.
  push(@vals, $this->valueString());		# Add value.
  return @vals;
}

# Print formatted representation of element to stdout.

sub print {
  my $this = shift;
  my ($gp, $el, $off, $code, $len, $name, $val) = $this->values();

  printf "(%04X, %04X) %s %6d: %-33s = %s\n", hex($gp), hex($el), $code, $len, $name, $val;
}

# Return a string representation of the value field (null if binary).

sub valueString {
  my $this = shift;
  my %hash = %$this;
  my ($code, $value) = @hash{qw(code value)};

  if ($code =~ /OX|SQ/) {
      $value = "";
  } elsif ($code eq '--') {
      # Don't return value if it contains binary characters.
      if(defined($this->{'length'})) {
	  foreach my $i (0..($this->{'length'} - 1)) {
              #print "$this->{'code'} $this->{'group'}:$this->{'element'} ($this->{'length'})\n";
	      my $val = ord(substr($value, $i, 1));
	      $value = "" if ($val > 0x0 and ($val < 0x20 or $val >= 0x80));
	  }
      } else {
	  $value = "";
      }
  } 
  
  return $value;
}

# Write this data element to disk.  All fields up to value are stored in 
# immutable field 'header' - write this to disk then value field.

sub write {
  my ($this, $OUTFILE) = @_;
  my %hash = %$this;
  my ($gp, $el, $offs, $code, $len, $name, $valstr) = $this->values();
  my ($hdr, $val) = @hash{qw(header value)};

  if ($gp eq '0018') {
  printf "Writing $code $gp:$el ($name/$len) : [$val]\n";
  }

  print $OUTFILE $hdr;
 SWITCH: {
  if ($code eq "UL") { $this->writeInt($OUTFILE, $INT);   last SWITCH; }
  if ($code eq "US") { $this->writeInt($OUTFILE, $SHORT); last SWITCH; }
  if ($code eq "FD") { $this->writeFloat($OUTFILE, $DOUBLE); last SWITCH; }
  if ($code eq "FL") { $this->writeFloat($OUTFILE, $FLOAT); last SWITCH; }
  
  # Trim value to length (may have been edited), null pad if necessary.
#    $val = substr($val, 0, $len);
  foreach my $i (1..($len - length($val))) {
    $val = "$val\0";
  }
  print $OUTFILE $val;
  }
}

sub value {
  my $this = shift;

  return $this->{'value'};
}

# Set the value field of this element.  Truncates to max length.

sub setValue {
  my $this = shift;
  my $code = $this->{'code'};
  my ($value) = @_;

  # If the field has a variable length, first adjust it.
  SWITCH: {
    if ($code eq "OB" or $code eq "OW" or $code eq "OF" or $code eq "SQ"
              or $code eq "UT" or $code eq "UN") {
      # XXX This is not binary safe!

      my $len = length $value;

      my @hdr = unpack "nnnnN", $this->{'header'};
      $hdr[4] = $len;
      $this->{'length'} = $len;
      $this->{'header'} = pack "nnnnV", @hdr;

      last SWITCH;
    }
    if ($code eq "US") { $this->fix_length(2, $value); last SWITCH; }
    if ($code eq "UL") { $this->fix_length(4, $value); last SWITCH; }
    if ($code eq "DA") { $this->fix_length(8, $value); last SWITCH; }
    if ($code eq "IS") { $this->fix_length(12, $value); last SWITCH; }
    if ($code eq "LO") { $this->fix_length(64, $value); last SWITCH; }
    if ($code eq "DT") { $this->fix_length(26, $value); last SWITCH; }
    if ($code eq "DS") { $this->fix_length(16, $value); last SWITCH; }
    if ($code eq "CS") { $this->fix_length(16, $value); last SWITCH; }
    if ($code eq "AE") { $this->fix_length(16, $value); last SWITCH; }
    if ($code eq "LT") { $this->fix_length(10240, $value); last SWITCH; }
    if ($code eq "PN") { $this->fix_length(64, $value); last SWITCH; }
    if ($code eq "SH") { $this->fix_length(16, $value); last SWITCH; }
    if ($code eq "ST") { $this->fix_length(1024, $value); last SWITCH; }
    if ($code eq "TM") { $this->fix_length(16, $value); last SWITCH; }
    if ($code eq "UI") { $this->fix_length(64, $value); last SWITCH; }
  }

  $value = substr($value, 0, $this->{'length'});
  $this->{'value'} = $value;
}

sub byteswap {
    my ($valref) = @_;
    
    my $packed = 0;
    if(length($$valref) % 2 != 0) {
	$packed = 1;
	substr($$valref, -1, 1) = "x".substr($$valref, -1, 1);
    }
    $$valref = pack('n*', unpack('v*', $$valref));
    if($packed) {
	substr($$valref, -1, 1) = '';
    }
}

sub fix_length {
  my ($this, $max, $value) = @_;
  my $len = length $value;

  if ($len > $max) { $len = $max; }

  my @hdr = unpack "nnnn", $this->{'header'};
  $hdr[3] = $len;
  $this->{'header'} = pack "nnnv", @hdr;

  $this->{'length'} = $len;
}

########## end DICOM::Element.pm ####################



#use DICOM::Fields;	# Standard header definitions.
#use DICOM::Private;	# Private or custom definitions.

$VERSION = sprintf "%d.%03d", q$Revision: 1.1 $ =~ /: (\d+)\.(\d+)/;

# Class variables.
my $sortIndex;		# Field to sort by.
my %opts;		# Command line options.
my $isdicm;		# Set to 1 if DICM file; 0 if NEMA.
my $currentfile;	# Currently open file.
my $preamblebuff = 0;	# Store initial 0x80 bytes.

# Initialize dictionary only once.
# Read the contents of the DICOM dictionary into a hash by group and element.
# dicom_private is read after dicom_fields, so overrides common fields.
BEGIN {
  foreach my $line (@dicom_fields, @dicom_private) {
    next if ($line =~ /^\#/);
    my ($group, $elem, $code, $numa, $name) = split(/\s+/, $line);
    my @lst = ($code, $name);
    $dict{$group}{$elem} = [@lst];
  }
}

sub new {
  my $class = shift;

  my $elements = {};
  bless $elements, $class;
  $elements->setIndex(2);
  return $elements;
}

# Store and process the command line options from hash ref.

sub processOpts {
  my $this = shift;
  my ($href) = @_;
  my $outfile;
  %opts = %$href;

  foreach my $key (keys %opts) {
    ($key eq 's') and $this->setIndex($opts{$key});	# Sort.
    ($key eq 'm') and $this->editHeader($opts{$key});	# Modify header.
    ($key eq 'o') and $outfile = $opts{$key};
  }
  # 'Save As' option is processed last.
  $this->write($outfile) if (defined($outfile));
}

# Fill in hash with header members from given file.

sub fill {
  my ($this, $infile, $big_endian_image) = @_;

  my($buff);
  $currentfile = $infile;
  open(INFILE, $infile) or return 1;
  binmode(INFILE);

  # Test for NEMA or DICOM file.  
  # If DICM, store initial preamble and leave file ptr at 0x84.
  read(INFILE, $preamblebuff, 0x80);
  read(INFILE, $buff, 4);
  $isdicm = ($buff eq 'DICM');
  die("Error: $infile is byte swapped\n") if ($buff eq 'IDMC');
  seek(INFILE, 0x00, 0) unless ($isdicm);

  until (eof(INFILE)) {
    my $element = DICOM::Element->new();
    ($element->fill(\*INFILE, \%dict, $big_endian_image, $this)) or return 1;
    my $gp = $element->{'group'};
    my $el = $element->{'element'};
    $this->{$gp}{$el} = $element;
  }
  close(INFILE);
  return 0;
}

# Write currently open file to given file name, or to current name 
# if no new name specified.  All fields before value are written
# verbatim; value field is stored as is (possibly edited).

sub write {
  my ($this, $outfile) = @_;
  
  my $header = "\0" x 128 . "DICM";
  
  $outfile = $currentfile unless (defined($outfile));
  
  open(OUTFILE, ">$outfile") or return 1;
  
  # write out the header
  syswrite(OUTFILE, $header, 0x84);

  # Do not forget the DICM!!!!
  # print OUTFILE $preamblebuff."DICM" if ($isdicm);
  # Ensure base class method called.
  
  $this->DICOM::printContents(\*OUTFILE);
  close(OUTFILE);
}

# Print all elements, to disk if file handle supplied.

sub printContents {
  my ($this, $OUTFILE) = @_;
  my %hash = %$this;
  my ($gpref, %gp, $el, %elem);

  foreach my $gpref (sort hexadecimally keys(%hash)) {
    %gp = %{$hash{$gpref}};
    foreach my $el (sort hexadecimally keys(%gp)) {
      if (defined($OUTFILE)) {
        $gp{$el}->write($OUTFILE);
      } else {
        $gp{$el}->print();
      }
    }
  }
}

# Return sorted array of references to element arrays.

sub contents {
  my $this = shift;
  my %hash = %$this;
  my @all;

  # Make an array of arrays of values for each element.
  my $row = 0;
  foreach my $gpref (sort hexadecimally keys(%hash)) {
    my %gp = %{$hash{$gpref}};
    foreach my $el (sort hexadecimally keys(%gp)) {
      my @values = $gp{$el}->values();
      $all[$row++] = \@values;
    }
  }

  @all = sort {sortByField()} @all;
  return @all;
}

# Set field index to sort by.  Return 1 if new index, else 0.

sub setIndex {
  my $this = shift;
  my ($val) = @_;

  # Don't sort by value.
  return 0 if ($val > 5);
  # Sorting by group or element equivalent to sorting by offset.
  $val = 2 if ($val <= 2);
  return 0 if (defined($sortIndex) and ($sortIndex == $val));
  $sortIndex = $val;
  return 1;
}

# Return sort index.

sub getIndex {
  return $sortIndex;
}

# Return value of the element at (group, element).

sub value {
  my $this = shift;
  my ($gp, $el) = @_;
  my $elem = $this->{uc($gp)}{uc($el)};
  return "" unless defined($elem);
  return (defined($elem->value())) ? $elem->value() : "";
}

# Return field of given index from element.
# Params: Group, Element, Field index.

sub field {
  my $this = shift;
  my ($gp, $el, $fieldname) = @_;
  my $elem = $this->{uc($gp)}{uc($el)};
  return "" unless defined($elem);
  return $elem->{$fieldname};
}

# Edit header value from string.
# String format: 'gggg,eeee=newvalue' or 'fieldname=newvalue'.
#   gggg, eeee = group, element (in hex); XXXX = new value.
#   fieldname = name of field from @dicom_fields.

sub editHeader {
  my ($this, $editstr) = @_;
  my ($gp, $el, $val);

  my $pos = index($editstr, '=');
  my $gpel = substr($editstr, 0, $pos);
  $val = substr($editstr, $pos + 1);
  if ($gpel =~ /^[0-9A-F]+,[0-9A-F]+/) {
    # Field specified as group and element.
    ($gp, $el) = split(/[^0-9A-F]+/, $gpel);
  } else {
    ($gp, $el) = $this->fieldByName($gpel);
  }
  return unless (defined($gp) and defined($el));
  $this->setElementValue($gp, $el, $val);
}

# Return group and element number of field with given name.

sub fieldByName {
  my ($this, $searchname) = @_;
  my ($gp, $el);

  # Field specified as field name: Search for it.
  my @allfields = $this->contents();
 FORE:
  foreach my $field (@allfields) {
    my @arr = @$field;
    if ($arr[5] eq $searchname) {
      ($gp, $el) = @arr[0, 1];
      last FORE;
    }
  }
  return ($gp, $el);
}

# Replace value of given element.

sub setElementValue {
  my $this = shift;
  my ($gp, $el, $newvalue) = @_;
  my $elem = $this->{uc($gp)}{uc($el)};
  $elem->setValue($newvalue);
}

#  ------------------------------------------------------------
#  Utility Functions (non-public)
#  ------------------------------------------------------------

sub hexadecimally {
  hex($a) <=> hex($b);
}

sub sortByField {
  my @aarr = @$a;
  my @barr = @$b;
  
  if ($aarr[$sortIndex] =~ /\D/) {
    return($aarr[$sortIndex] cmp $barr[$sortIndex]);
  } else {
    return($aarr[$sortIndex] <=> $barr[$sortIndex]);
  }
}

# Doesn't do anything in non-graphical case.
sub loop {}

1;
__END__

=head1 NAME

DICOM.pm is a Perl library that allows Perl programs to read the
headers of medical image files conforming to DICOM standards.

=head1 SYNOPSIS

  use DICOM;
  my $dicom = DICOM->new();
  $dicom->fill($dicomFile);
  my $patientName = $dicom->value('0010', '0010');
  
=head1 DESCRIPTION

DICOM (Digital Imaging and Communications in Medicine) is a standard
designed to allow medical image files to be transferred, stored and
viewed on different makes of computers. Perl is a multiplatform
language that excels at system tasks such as file manipulation. It's
easy to learn, particularly if you are familiar with C or the Unix
shells and utility programs.

This library provides the methods to read and parse a DICOM file, then
to recover the contents of each header element by their standard DICOM
group and element codes. Header element values can be edited (either
through the GUI or command line) and the modified file written back to
disk.

=head2 Methods

=over 4

=item * $object->fill($filename)

Fills the DICOM object with data from the file $filename

=back

=head1 SEE ALSO

The DICOM standard - http://medical.nema.org/

=head1 AUTHOR

Andrew Crabb, E<lt>ahc@jhu.eduE<gt>
Jonathan Harlap, E<lt>jharlap@bic.mni.mcgill.caE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2002 by Andrew Crabb
Some parts are Copyright (C) 2003 by Jonathan Harlap

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.6.0 or,
at your option, any later version of Perl 5 you may have available.

=cut
