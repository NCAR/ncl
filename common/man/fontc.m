.\"
.\"	$Id: fontc.m,v 1.3 1993-04-21 20:04:13 fred Exp $
.\"
.TH FONTC 1NCARG "April 1993" NCAR "NCAR GRAPHICS"
.SH NAME
fontc \- Fontcap preprocessor for NCAR Graphics
.SH SYNOPSIS
.B fontc
.I ascii-fontcap-name
.I binary-fontcap-name
.PP
.SH DESCRIPTION
.B fontc
converts an ascii-encoded fontcap,
.I ascii-fontcap-name,
into a binary-encoded fontcap,
.I binary-fontcap-name.
.PP
Binary encoded fontcaps
.I are
system dependent and should not be assumed to be portable across
different system architectures.
.SH SEE ALSO
ctrans(1NCARG), fontcap(5NCARG)
