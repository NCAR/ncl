.\"
.\"	$Id: ncargccl.m,v 1.1.1.1 1992-04-17 22:30:46 ncargd Exp $
.\"
.TH NCARGF77 1NCARG "SEPTEMBER 1990" NCAR "NCAR GRAPHICS"
.SH NAME
ncargccl \- CFT-compatible loader for code that uses NCAR Graphics (UNICOS only).
.SH SYNOPSIS
\fBncargccl\fP
[\fB\-smooth\fR]
[\fB\-quick\fR]
[\fB\-super\fR]
[\fB\-agupwrtx\fR]
[\fB\-dashchar\fR]
[\fB\-dashline\fR]
[\fB\-dashsmooth\fR]
[\fB\-dashsuper\fR]
[\fB\-conrecsmooth\fR]
[\fB\-conrecsuper\fR]
[\fB\-conrecquick\fR]
[\fB\-conran\fR]
[\fB\-conransmooth\fR]
[\fB\-conranquick\fR]
.SH DESCRIPTION
.LP
\fIncargccl\fP is a script that invokes "segldr" with CFT-compatible versions
of the proper NCAR Graphics libraries.
Arguments presented above are associated with NCAR Graphics.  
All other arguments and options will simply be passed on to "segldr".
