.\"
.\"	$Id: psblack.m,v 1.3 1993-05-03 21:49:01 fred Exp $
.\"
.TH PSBLACK 1NCARG "May 1993" NCAR "NCAR GRAPHICS"
.SH NAME
psblack \- PostScript filter.
.SH SYNOPSIS
.B psblack
.I < input_PostScript_file
.I > output_PostScript_file
.PP
.SH DESCRIPTION
.B psblack
is a filter which reads a PostScript file as input
and writes a PostScript file as output.  In
the output file the filter forces a black background
and scales the intensity values (values with
R=G=B) so that any intensity greater than or equal
to .8 is mapped to white.  This filter is potentially
useful for any PostScript files created by NCAR
Graphics.
.SH SEE ALSO
ctrans(1NCARG), pswhite(1NCARG)
