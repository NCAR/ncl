.\"
.\"	$Id: ncargcc.m,v 1.1.1.1 1992-04-17 22:30:30 ncargd Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGCC 1NCARG "SEPTEMBER 1990" NCAR "NCAR GRAPHICS"
.SH NAME
ncargcc \- Command for compiling C code that uses NCAR Graphics
.SH SYNOPSIS
\fBncargcc\fP 
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
\fIncargcc\fP is a script that invokes the C compiler/linker (cc) with the
proper NCAR Graphics libraries.  Arguments presented above are
associated with NCAR Graphics.  All other arguments and options
are identical to the cc command on your particular machine;
arguments that include quoted strings may
have to be enclosed in single quotes.
.LP
When \fBncargcc\fR is invoked with the \fB\-ictrans\fR option the resulting
executable will, upon invocation, send its metafile output to the translator
\fBictrans\fR . The environment variable GRAPHCAP must be set to a valid
graphics output device whenever the executable is executed.
.SH "SEE ALSO"
.LP
.I "NCAR Graphics User's Guide"
.br
.BR ictrans(1NCARG),
.BR gcaps(1NCARG)
.LP
.SH BUGS
.LP
This script is provided for those people that perhaps do not know
about Makefiles or program development under UNIX. 
