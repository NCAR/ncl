.\"
.\"	$Id: rasview.m,v 1.3 1992-06-25 21:47:20 clyne Exp $
.\"
.TH XTOHDF 1-local "March 1991" NCAR "NCAR Local Command"
.SH NAME
rasview \- X11 windows raster file previewer.
.SH SYNOPSIS
.B rasview
.RB [ " \-pal"
.IR "palette " ]
.RI [ " rasterfile.ext " ".\|.\|." ]
[
.B \-Version
]
.SH DESCRIPTION
.LP
.B rasview
displays raster imagery stored in a number of different file formats to
an X window. The currently supported raster formats are xwd (X11 Window Dump),
hdf (Hidden Data Format), Sun, and nrif (NCAR Raster Image Format). 
.B rasview
determines the format of a file by the file name extension. The extensions
for the aforementioned formats are 
.IR .xwd , " .hdf" , " .sun " and " .nrif" 
respectively.
.B rasview
will display any number of files each containing any number of images. 
.SH OPTIONS
.TP
.BI \-pal " palette"
Use the color palette contained in the file
.IR palette .
for displaying images. This palette will override the color palette stored
with the image.
.TP
.B \-Version
Print the version number and then exit.
.SH "SEE ALSO"
.BR palette (local),
.BR ctrans (local),
.BR xwd (1),
.BR xwud (1)
.br
.ne 5
.SH BUGS/CAVEATS
.LP
24-bit images are dithered to 8 bits.
.LP
.B rasview
has only been tested on devices which use "PseudoColor" as 
the default visual class.
