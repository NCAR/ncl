.\"
.\"	$Id: pswhite.m,v 1.6 1998-02-04 05:13:36 haley Exp $
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
appears as black on the white background.  This
filter will be useful only for PostScript files
created by NCAR Graphics in Version 3.00 or
earlier.  In Release 3.01 or later, the NCAR
CGM interpreters by default plot black lines
on a white background.
.SH SEE ALSO
ctrans(1NCARG), psblack(1NCARG)
.SH COPYRIGHT
Copyright (C) 1987-1998
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
