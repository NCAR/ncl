.\"
.\"	$Id: nriftohdf.m,v 1.9 1993-05-04 22:04:04 haley Exp $
.\"
.TH NRIFTOHDF 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
nriftohdf \- Filter to convert from nrif raster file to hdf format.
.SH SYNOPSIS
.B nriftohdf
.RB [ "\-o"
.IR "hdf_file" ]
[
.BR - ( c | r )
]
[
.B \-V
]
.RI [ " nrif_file " ".\|.\|." ]
.SH DESCRIPTION
.LP
.B nriftohdf
will accept any number of input nrif raster files and convert them to
hdf format. By default
.B nriftohdf 
reads from standard input. The default output file is 
.I ncar.hdf
.SH OPTIONS
.TP
.BI \-o " hdf_file"
Change the default output file to 
.IR hdf_file .
.TP
.B \-c
Compress the output.
.TP
.B \-r
Use run length encoding for data.
.TP
.B \-V
Print the version number and then exit.
.SH "SEE ALSO"
.BR rascat(1NCARG)
.SH BUGS/CAVEATS
.LP
.B nriftohdf 
will only accept 8 bit, indexed encoded nrif files. 
.B nriftohdf 
will not accept Bi-level, Bi-level run length,
indexed color run length, Direct color integrated,
Direct color integrated run length, Direct color segregated,
or Direct color segregated run length at any precision.
.LP
.B nriftohdf
is an obsolete utility which simply calls
.BR rascat(1NCARG).
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
