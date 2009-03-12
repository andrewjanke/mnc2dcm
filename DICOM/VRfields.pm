# VRfields.pm
# Andrew Crabb (ahc@jhu.edu), May 2002.
# $Id: VRfields.pm,v 1.3 2009/03/12 02:46:24 rotor Exp $

package DICOM::VRfields;

use strict;
use vars qw(@ISA @EXPORT $VERSION %VR);

require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(%VR);
$VERSION = sprintf "%d.%03d", q$Revision: 1.3 $ =~ /: (\d+)\.(\d+)/;

# Value Representations (DICOM Standard PS 3.5 Sect 6.2)
# Bytes=0 => Undefined length.
# Fixed=1 => Exact field length, otherwise max length.
#
#  Code     Name         MaxBytes Fixed Numeric ByteSwap Pack

%VR = (
   'AE' => ['Application Entity',         16,  0,  0,  0, 'a' ],
   'AS' => ['Age String',                  4,  1,  0,  0, 'a4'],
   'AT' => ['Attribute Tag',               4,  1,  0,  1, 'v2'],
   'CS' => ['Code String',                16,  0,  0,  0, 'a' ],
   'DA' => ['Date',                        8,  1,  0,  0, 'c8'],
   'DS' => ['Decimal String',             16,  0,  1,  0, 'a' ],
   'DT' => ['Date Time',                  26,  0,  0,  0, 'a' ],
   'FD' => ['Floating Point Double',       8,  1,  1,  1, 'd' ],
   'FL' => ['Floating Point Single',       4,  1,  1,  1, 'f' ],
   'IS' => ['Integer String',             12,  0,  1,  0, 'a' ],
   'LO' => ['Long String',                64,  0,  0,  0, 'a' ],
   'LT' => ['Long Text',               10240,  0,  0,  0, 'a' ],
   'OB' => ['Other Byte String',           0,  0,  0,  0, 'c' ],
   'OF' => ['Other Float String',          0,  0,  0,  1, 'f' ],
   'OW' => ['Other Word String',           0,  0,  0,  1, 'v' ],
   'OX' => ['Binary Stream',               0,  0,  0,  1, '' ],
   'PN' => ['Person Name',                64,  0,  0,  0, 'a' ],
   'SH' => ['Short String',               16,  0,  0,  0, 'a' ],
   'SL' => ['Signed Long',                 4,  1,  1,  0, 'i' ],
   'SQ' => ['Sequence of Items',           0,  0,  0,  0, '' ],
   'SS' => ['Signed Short',                2,  1,  1,  1, 's' ],
   'ST' => ['Short Text',               1024,  0,  0,  0, 'a' ],
   'TM' => ['Time',                       16,  0,  0,  0, 'a' ],
   'UI' => ['Unique Identifier UID',      64,  0,  0,  0, 'a' ],
   'UL' => ['Unsigned Long',               4,  1,  1,  1, 'V' ],
   'UN' => ['Unknown',                     0,  0,  0,  0, 'c' ],
   'US' => ['Unsigned Short',              2,  1,  1,  1, 'S' ],
   'UT' => ['Unlimited Text',              0,  0,  0,  0, 'a' ],
   );

# unknowns: OT
