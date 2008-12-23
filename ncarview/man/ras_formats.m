'\" t
.TH RAS_FORMATS 1NCAR "February 1993" NCAR "NCAR VIEW"
.SH NAME
ras_formats - Information on raster formats supported by NCAR Graphics
and NCAR View.
.SH SYNOPSIS
Abekas A60, AVS, Generic Binary, HDF, HPPCL, NRIF, Sun, SGI, and XWD.
.SH DESCRIPTION
NCAR Graphics and NCAR View applications are built upon a common
library that provides read and write access to raster imagery. The
table below summarizes the support that is provided for all the
different formats.  Note that a format may be writeable but not
readable, and that a compressed image may be readable but not
writeable. By default, a raster image is written as compressed
if possible. For those applications that key upon the filename
to determine the raster format, or allow a \fI-ifmt\fP or \fI-ofmt\fP,
the \fIextension\fP, as listed below, should be used without the "."
in front (e.g. use "hdf", not ".hdf").
.LP
.TS
tab (;);
l l l l l
l l c c c.
Format(extensions);Encoding;Input;Output;Compression
_
.sp
Abekas A60(.a60);RGB24;Yes;Yes;None
AVS(.avs);RGB24;Yes;Yes;None
HDF(.hdf);Indexed-8;Yes;Yes;In/Out
HDF(.hdf);Direct-24;Yes;Yes;In/Out
HPPCL(.hppcl);Indexed-8;No;Yes;Out
NRIF(.nrif);Bilevel;No;No;None
NRIF(.nrif);Indexed-8;Yes;Yes;In/Out;
NRIF(.nrif);Direct-24;Yes;Yes;In/Out;
Sun(.sun);Indexed-8;Yes;Yes;In;
Sun(.sun);Direct-24;Yes;No;In;
SGI(.sgi,.rgb);Direct-24;Yes;Yes;In;
XWD(.xwd);Indexed-8;Yes;Yes;None;
.sp
_
.TE
.sp
.SH CAVEATS
.LP
Not all formats and encodings that are supported for output are
supported for input; the software has separate input and output
drivers for format/encoding pair.
.LP
For all the formats listed below, only 8-bit pixels and 8-bit run-lengths
(where applicable) are supported.
.LP
Experience with HDF's "advanced" compression option indicates that you
should use it only if you consider random pixels an advanced-appearing
image.
.sp
.SH "SEE ALSO"
.LP
rasview(1NCARG), rasls(1NCARG), rascat(1NCARG), rasgetpal(1NCARG),
rassplit(1NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
