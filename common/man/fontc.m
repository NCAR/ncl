.\"
.\"	$Id: fontc.m,v 1.2 1992-12-01 23:25:02 clyne Exp $
.\"
.TH FONTC 1NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
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
cgmtrans(1NCARG), ctrans(1NCARG), plt(1NCARG), fontcap(5NCARG)
