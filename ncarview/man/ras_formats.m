.ds f. ras_formats.tbl
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
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \wFormat(extensions)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAbekas A60(.a60)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAVS(.avs)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wHDF(.hdf)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wHDF(.hdf)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wHPPCL(.hppcl)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNRIF(.nrif)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNRIF(.nrif)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNRIF(.nrif)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSun(.sun)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSun(.sun)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSGI(.sgi,.rgb)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wXWD(.xwd)
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wEncoding
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wRGB24
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wRGB24
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wIndexed-8
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDirect-24
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wIndexed-8
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wBilevel
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wIndexed-8
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDirect-24
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wIndexed-8
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDirect-24
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDirect-24
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wIndexed-8
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wInput
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wNo
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wNo
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wYes
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wNo
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wNo
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wYes
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wCompression
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNone
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNone
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wIn/Out
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wIn/Out
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wOut
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNone
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wIn/Out
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wIn/Out
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wIn
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wIn
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wIn
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNone
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 41 file ras_formats.tbl is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Format(extensions)\h'|\n(41u'Encoding\h'|\n(42u'Input\h'|\n(43u'Output\h'|\n(44u'Compression
.nr 36 \n(.v
.vs \n(.vu-\n(.sp
\h'|0'\s\n(33\l'|\n(TWu\(ul'\s0
.vs \n(36u
.sp
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Abekas A60(.a60)\h'|\n(41u'RGB24\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'None
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AVS(.avs)\h'|\n(41u'RGB24\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'None
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'HDF(.hdf)\h'|\n(41u'Indexed-8\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'In/Out
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'HDF(.hdf)\h'|\n(41u'Direct-24\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'In/Out
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'HPPCL(.hppcl)\h'|\n(41u'Indexed-8\h'|\n(42u'No\h'|\n(43u'Yes\h'|\n(44u'Out
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NRIF(.nrif)\h'|\n(41u'Bilevel\h'|\n(42u'No\h'|\n(43u'No\h'|\n(44u'None
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NRIF(.nrif)\h'|\n(41u'Indexed-8\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'In/Out
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NRIF(.nrif)\h'|\n(41u'Direct-24\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'In/Out
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Sun(.sun)\h'|\n(41u'Indexed-8\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'In
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Sun(.sun)\h'|\n(41u'Direct-24\h'|\n(42u'Yes\h'|\n(43u'No\h'|\n(44u'In
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SGI(.sgi,.rgb)\h'|\n(41u'Direct-24\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'In
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'XWD(.xwd)\h'|\n(41u'Indexed-8\h'|\n(42u'Yes\h'|\n(43u'Yes\h'|\n(44u'None
.sp
.nr 36 \n(.v
.vs \n(.vu-\n(.sp
\h'|0'\s\n(33\l'|\n(TWu\(ul'\s0
.vs \n(36u
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-21
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
rasssplit(1NCARG)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
