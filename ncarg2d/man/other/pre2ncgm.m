.\"
.\"	$Id: pre2ncgm.m,v 1.1 1993-03-11 15:23:14 haley Exp $
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
ctrans(1),ftrans(1),plt(1)






















NCAR                  Last change: 3-Jan-90                     1



