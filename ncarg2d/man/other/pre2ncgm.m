.\"
.\"	$Id: pre2ncgm.m,v 1.11 2002-06-05 22:02:20 haley Exp $
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

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
