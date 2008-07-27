.\"
.\"	$Id: pswhite.m,v 1.12 2008-07-27 03:34:11 haley Exp $
.\"
.TH PSWHITE 1NCARG "September 1990" NCAR "NCAR GRAPHICS"
.SH NAME
pswhite \- PostScript filter.
.SH SYNOPSIS
.B pswhite
.I < input_PostScript_file
.I > output_PostScript_file
.PP
.SH DESCRIPTION
.B pswhite
is a filter which reads a PostScript file as input
and writes a PostScript file as output.  In
the output file the filter forces a white background
and complements the intensity values (values with
R=G=B) so that the default white foreground
appears as black on the white background.  
This filter works only
on PostScript files produced by ctrans - the CGM translator
in the NCAR Graphics software.
.SH SEE ALSO
ctrans(1NCARG), psblack(1NCARG)
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
