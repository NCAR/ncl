.\"
.\"	$Id: psblack.m,v 1.13 2008-07-27 03:34:10 haley Exp $
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
to .8 is mapped to white.  
This filter works only
on PostScript files produced by ctrans - the CGM translator
in the NCAR Graphics software.
.SH SEE ALSO
ctrans(1NCARG), pswhite(1NCARG)
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
