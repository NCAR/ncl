.\"
.\"	$Id: ncargrun.m,v 1.15 2008-07-27 03:34:10 haley Exp $
.\"
.TH NCARGRUN 1NCARG "JUNE 1988" NCAR "NCAR GRAPHICS"
.SH NAME
ncargrun \- to run a user program and redirect the metafile output
.SH SYNOPSIS
\fBncargrun\fP 
\fBprogram-name\fR
[\fB\-t\fR]
[\fB\-o metafile-name\fR]
.SH DESCRIPTION
.I ncargrun
is a script that invokes "program-name", which uses the NCAR GKS
library, and applies the specified options.
.LP
Prior to running your program, this script sets the environment variables
NCARG_GKS_OUTPUT, NCARG_GKS_PSOUTPUT, and NCARG_GKS_PDFOUTPUT to produce the
desired effect.
.SH OPTIONS
.BI \-o " " "filename"

specifies the name of the output metafile or PostScript file.  If
filename is replaced by "| translator-name", the effect is to
pipe the metafile to the named translator.

.LP
.BI \-t " "
specifies that metafile output should be piped to the default translator
"ctrans".
.sp
.SH "SEE ALSO"
.BR ncarg_gks(1NCARG),
.BR ncarg_env(5NCARG),
.BR ncargintro(5NCARG)
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
