.\"
.\"	$Id: graphc.m,v 1.3 1993-04-21 20:04:25 fred Exp $
.\"
.TH GRAPHC 1NCARG "April 1993" NCAR "NCAR GRAPHICS"
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
ctrans(1NCARG), graphcap(5NCARG)
