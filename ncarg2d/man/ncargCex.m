.\"
.\"	$Id: ncargCex.m,v 1.1 1993-01-15 22:20:21 haley Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH NCARGCEX 1NCARG "January 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargCex \- NCAR Graphics C Examples
.SH SYNOPSIS
\fBncargCex\fP 
[\fB\-all, -A\fR]
[\fB\-clean\fR]
[\fB\-n\fR]
[\fB\-onebyone\fR]
\fBname ...\fR
.SH DESCRIPTION
.LP
.I ncargCex
provides the user with access to C example source code that
uses the NCAR Graphics C-bindings.  These examples correspond
with the Fortran examples that you can get with the \fIncargex\fP
command.  Not every Fortran example is included, here, however.
These examples should be enough to get one started in learning
how to use the NCAR Graphics C-bindings.
\fIncargCex\fP copies the source code for the specified example(s)
into the current directory and then compiles, links,
and executes the example, leaving an NCGM file
with the same name as the example, suffixed with
".ncgm". An option allows you to request that
only the source code be copied to your directory,
without compilation, linking, or execution.
Another option allows you to request that only the
NCGM file be left in your directory and that all other files
created by \fIncargCex\fP be deleted.
The argument \fIname\fP may be
selected from the lists that appear below.
.LP
.I OPTIONS
.LP
.IP "\-all, \-A"
Generate all available examples.
.LP
.IP \-clean " " ""
Remove everything but the ".ncgm" file.
.LP
.IP \-n " " ""
Specifies that the example should not be compiled, linked, or run.
.LP
.IP \-onebyone " " ""
Specifies that the selected examples and/or tests should be generated one
at a time and viewed as they are generated.  This is intended for use during
testing of new releases at NCAR.
.LP
.I "EXAMPLES AVAILABLE"
.LP
.I "AUTOGRAPH Examples:"
.nf
	c_agex07
.fi
.LP
.I "EZMAP Examples:"
.nf
	c_mpex05
.fi
.LP
.I "EZMAPA Examples:"
.nf
	c_eezmpa
.fi
.LP
.I "CONPACK Examples:"
.nf
	c_colcon
.fi
.LP
.I "LABELBAR Examples:"
.nf
	c_elblba
.fi
.LP
.I "SOFTFILL Examples:"
.nf
	c_sfex02
.fi
.LP
.I "PLOTCHAR Examples:"
.nf
	c_epltch
.fi
.LP
.I "Miscellaneous Examples:"
.nf
	c_cbex01 c_coex01 c_slex01
.fi
.LP
.SH "SEE ALSO"
ncarg_cbind, ncargex

