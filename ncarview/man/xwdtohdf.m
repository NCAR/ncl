.\"
.\"	$Id: xwdtohdf.m,v 1.2 1991-01-09 10:46:28 clyne Exp $
.\"
.TH XTOHDF 1-local "February 1990" NCAR "NCAR Local Command"
.SH NAME
xtohdf \- Filter to convert from X11 raster file to hdf format.
.SH SYNOPSIS
.B xtohdf
.RB [ " \-o"
.IR "hdf_file " ]
[ 
.BR - ( c | r )
]
.RI [ " xdump_file " ".\|.\|." ]
.SH DESCRIPTION
.LP
.B xtohdf
will accept any number of input X11 raster files and convert them to
hdf format. By default
.B xtohdf 
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
.BR x11tab (1-local),
.BR xwd (1),
.BR xwud (1)
.br
.ne 5
.SH BUGS/CAVEATS
.LP
.B xtohdf 
does not support direct or true color encodings of X11 raster files. Nor 
does it accept color tables with greater than 256 entries.
