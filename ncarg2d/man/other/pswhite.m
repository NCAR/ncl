.\"
.\"	$Id: pswhite.m,v 1.1 1993-03-11 15:23:18 haley Exp $
.\"
.TH PSWHITE 1NCARG "SEPTEMBER 1990" NCAR "NCAR GRAPHICS"
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
