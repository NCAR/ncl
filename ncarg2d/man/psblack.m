.\"
.\"	$Id: psblack.m,v 1.1.1.1 1992-04-17 22:30:50 ncargd Exp $
.\"
.TH PSBLACK 1NCARG "SEPTEMBER 1990" NCAR "NCAR GRAPHICS"
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
