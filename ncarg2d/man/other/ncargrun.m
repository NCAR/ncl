.\"
.\"	$Id: ncargrun.m,v 1.1 1993-03-11 15:23:06 haley Exp $
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
Prior to running your program, this script sets the environment variable
NCARG_GKS_OUTPUT to produce the desired effect.
.SH OPTIONS
.BI \-o " " "metafile-name"
specifies the name of the output metafile.  If metafile-name is replaced
by "| translator-name", the effect is to pipe the metafile to the named
translator.
.LP
.BI \-t " "
specifies that metafile output should be piped to the default translator
"ftrans".
.SH "SEE ALSO"
.LP
.I "NCAR Graphics UNIX Release"
.LP
