.\"
.\"	$Id: psblack.m,v 1.2 1993-04-05 17:44:59 haley Exp $
.\"
.TH PSBLACK 1NCARG "September 1990" NCAR "NCAR GRAPHICS"
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
R=G=B) so that the default value of .8 does
not appear as a light gray, but as white on
the black background.  This filter is potentially
useful for any PostScript files created by NCAR
Graphics.
.SH SEE ALSO
ctrans(1NCARG), pswhite(1NCARG)
