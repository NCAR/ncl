.\"
.\"	$Id: graphc.m,v 1.2 1992-12-01 23:25:05 clyne Exp $
.\"
.TH GRAPHC 1NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.SH NAME
graphc \- Graphcap preprocessor for NCAR Graphics
.SH SYNOPSIS
.B graphc
.I ascii-graphcap-name
.I binary-graphcap-name
.PP
.SH DESCRIPTION
.B graphc
converts an ascii-encoded graphcap,
.I ascii-graphcap-name,
into a binary-encoded graphcap,
.I binary-graphcap-name.
.PP
Binary encoded graphcaps
.I are
system dependent and should not be assumed to be portable across
different system architectures.
.SH SEE ALSO
cgmtrans(1NCARG), ctrans(1NCARG), plt(1NCARG), graphcap(5NCARG)
