.\"
.\"	$Id: ncargf77.m,v 1.1.1.1 1992-04-17 22:30:32 ncargd Exp $
.\"
.TH NCARGF77 1NCARG "SEPTEMBER 1990" NCAR "NCAR GRAPHICS"
.SH NAME
ncargf77 \- Command for compiling f77 code that uses NCAR Graphics
.SH SYNOPSIS
\fBncargf77\fP 
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
[\fB\-ictrans\fR] ...
.SH DESCRIPTION
.LP
\fIncargf77\fP is a script that invokes the FORTRAN 77 
compiler/linker (f77) with the proper NCAR Graphics libraries.  
Arguments presented above are associated with NCAR Graphics.  
All other arguments and options are identical to the f77 command 
on your particular machine;
arguments that include quoted strings may
have to be enclosed in single quotes.
.LP
Note that, on some systems, if you supply your own binary libraries in
addition to the ones automatically referenced by \fIncargf77\fR, all the
libraries must have been created in a similar fashion.  For example,
on a "Sun3", problems may arise if the "-fswitch" option was used for
some of the libraries and not for others.
.LP
When \fIncargf77\fR is invoked with the \fB\-ictrans\fR option the resulting
executable will, upon invocation, send its metafile output to the translator 
\fBictrans\fR  . The environment variable GRAPHCAP must be set to a valid
graphics output device whenever the executable is executed.
.SH "SEE ALSO"
.LP
.I "NCAR Graphics User's Guide",
.br
.BR ictrans(1NCARG),
.BR gcaps(1NCARG)
.LP
.SH BUGS
.LP
This script is provided for those people that perhaps do not know
about Makefiles or program development under Unix.
