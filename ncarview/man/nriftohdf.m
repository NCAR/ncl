.TH XTOHDF 1-local "February 1990" NCAR "NCAR Local Command"
.SH NAME
nriftohdf \- Filter to convert from nrif raster file to hdf format.
.SH SYNOPSIS
.B nriftohdf
.RB [ "\-o"
.IR "hdf_file" ]
[
.BR - ( c | r )
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
.SH "SEE ALSO"
.BR ncartab (1-local),
.br
.ne 5
.SH BUGS/CAVEATS
.LP
.B nriftohdf 
will only accept 8 bit, indexed encoded nrif files. 
.B nriftohdf 
will not accept \fI Bi-level, Bi-level run length,
indexed color run length, Direct color integrated,
Direct color integrated run length, Direct color segregated,
\fRor \fIDirect color segregated run length\fR at any precision.
