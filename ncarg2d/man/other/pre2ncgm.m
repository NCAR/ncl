.\"
.\"	$Id: pre2ncgm.m,v 1.12 2008-07-27 03:34:10 haley Exp $
.\"
.TH PRE2NCGM 1NCARG "OCTOBER 1990" NCAR "NCAR GRAPHICS"
.SH NAME
pre2ncgm \- convert a pre-CGM NCAR metafile to an NCAR CGM.
.SH SYNOPSIS
\fBpre2ncgm\fP
[\fB\-n\fR]
.SH DESCRIPTION
.LP
The "pre2ncgm" command reads a pre-CGM NCAR metafile from
standard  input  and writes an NCAR CGM file to standard output.
When plotted, the output file should have virtually the same
appearance as the input file.
.SH OPTIONS
.LP
.TP
.B \-n
When this option appears, all text and header records which
appear in the input file (created by PRSIM) are not copied
to the output file.
.SH EXAMPLES
The command
.sp 1
.in +4
.B pre2ncgm
< input-file > output-file
.sp 1
.in -4
causes  "input-file"  (which  should  be  a   pre-CGM   NCAR
metafile)  to be read and the equivalent NCAR CGM file to be
written to "output-file").
.SH DIAGNOSTICS
Various errors are detected by "pre2ncgm" and  are  written  to
/dev/tty.
.SH FILES
/usr/local/bin/pre2ncgm.run
.SH VERSION
1.00 (1-Aug-90)
.SH SEE ALSO
ctrans(1NCARG)
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
