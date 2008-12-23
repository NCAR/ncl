.\"
.\"	$Id: rasgetpal.m,v 1.17 2008-12-23 00:04:16 haley Exp $
.\"
.TH RASGETPAL 1NCARG "January 1993" NCARG "NCAR VIEW"
.SH NAME
rasgetpal \- extract the color palette of a rasterfile and write it
to standard output
.SH SYNOPSIS
\fBrasgetpal\fP 
[\fB\-Version\fR]
\fIsrcfile\fP
[\fIdstfile\fP]
.SH DESCRIPTION
.sp
Given \fIsrcfile\fP and no \fIdstfile\fP, \fBrasgetpal\fP will extract
the color palette from \fIsrcfile\fP and print it in textual form to
standard output. Given \fIsrcfile\fP and \fIdstfile\fP, \fBrasgetpal\fP
will extract the color palette from \fIsrcfile\fP and save it
in file \fIdstfile\fP. If the extension of \fIdstfile\fP is ".txt",
the color palette is saved in textual form. If it is ".pal", the
palette is saved in a binary HDF-compatible format.
.sp
Once you have a ".pal" color palette you can use it with NCSA's
XImage or any other application that uses this format of color
palette. A textual color palette can be edited using a standard
text editor and then fed back to \fIctrans\fP, \fIrasview\fP,
or \fIrascat\fP in order to get a modified color palette. It's
also useful when you simply want to know what's in your color
palette. See "man ras_palette" for more information on these
different formats.
.SH OPTIONS
.TP
.BI \-help
Print help information.
.TP
.BI \-Version
Print the version number.
.sp
.SH "EXAMPLE"
.LP
Let's suppose you have an X Window Dump rasterfile called \fIwindow.xwd\fP
and you'd like to get a textual copy of the color palette.
.sp
.in +3.0
.nf
% rasgetpal window.xwd window.txt
% vi window.txt /* edit the color table */
% rasview -pal window.txt window.xwd
.fi
.in -3.0
.sp
.LP
You could also use the command below to get the same palette file:
.sp
.in +3.0
.nf
% rasgetpal window.xwd >window.txt
.fi
.in -3.0
.sp
.LP
Now suppose you'd like to get an HDF-compatible binary palette
from "window.xwd":
.sp
.in +3.0
.nf
% rasgetpal window.xwd new.pal
.fi
.in -3.0
.sp
.SH "CAVEATS"
A color map can be extracted from indexed rasterfiles but not
from direct-color rasterfiles.
.sp
.SH "SEE ALSO"
.LP
\fBrasview\fP(1NCARG), \fBrascat\fP(1NCARG), \fBrasls\fP(1NCARG),
\fBrassplit\fP(1NCARG), \fBras_formats\fP(5NCARG),
\fBras_palette\fP(5NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
