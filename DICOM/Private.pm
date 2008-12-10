# Definitions of (optional) private fields of DICOM headers.
# $Id: Private.pm,v 1.1 2008/12/10 02:13:37 rotor Exp $

package DICOM::Private;

use strict;
use vars qw(@ISA @EXPORT $VERSION @dicom_private);

require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(@dicom_private);
$VERSION = sprintf "%d.%03d", q$Revision: 1.1 $ =~ /: (\d+)\.(\d+)/;

# These definitions override those with the same group and element
# numbers in dicom_fields.

@dicom_private = (<<END_DICOM_PRIVATE =~ m/^\s*(.+)/gm);
# Example format:
# 0000   0000   UL   1      GroupLength
END_DICOM_PRIVATE
