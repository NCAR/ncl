.\"
.\"	$Id: nhlcc.m,v 1.3 2008-07-27 03:41:28 haley Exp $
.\"
.TH NHLCC 1NCARG "April 1995" NCAR "NCAR GRAPHICS"
.SH NAME
nhlcc \- Command for compiling C code that uses the NCAR Graphics
high-level utilities.
.SH SYNOPSIS
\fBnhlcc\fP 
[\fB\-ngmath\fR]
[\fB\-ncarbd\fR]
[\fB\-ngmathbd\fR]
[\fB\-netcdf\fR]
[\fB\-hdf\fR] ...
.SH DESCRIPTION
\fInhlcc\fP is a script that invokes the C compiler/linker
with the proper NCAR Graphics LLU (low-level utility) and HLU
(high-level utility) libraries.  Arguments presented above are
associated with NCAR Graphics.  All other arguments and options are
identical to the cc command on your particular machine; arguments
that include quoted strings may have to be enclosed in single quotes.
.sp
If you don't want to use \fInhlcc\fP, you can just type it on the
command line to see what gets included in the link line, and then you
can add this information to your own Makefile or script.  It is
important to note that you must define the macro \fINeedFuncProto\fP
in order for function prototyping to work correctly.
.sp
In order to run \fInhlcc\fP, you must have your NCARG_ROOT
environment variable set to the directory pathname where the NCAR
Graphics libraries, binaries, and include files were installed.  If
you are not sure what NCARG_ROOT should be set to, please check with 
your system administrator or the site representative for NCAR Graphics.
If the NCAR Graphics libraries, binaries, and include files were not
installed under one root directory, then you will need to set the 
environment variables NCARG_LIB, NCARG_BIN, and NCARG_INCLUDE instead.
Please see "man ncargintro" for more information.
.sp
Note that, on some systems, if you supply your own binary libraries in
addition to the ones automatically referenced by \fInhlcc\fR, all the
libraries must have been created in a similar fashion.
.sp
.I OPTIONS
.IP "\-ngmath"
Links in the NCAR Graphics ngmath library.
.sp
.IP "\-ncarbd"
Use this option for compilers that appear to be having trouble
initializing blockdata variables. It will cause a small subroutine to
be linked in that helps force the loading of blockdata initialization
routines.
.sp
.IP "\-ngmathbd"
Just like with the \fB\-ncarbd\fR option, use this option for compilers
that appear to be having trouble initializing Ngmath-related blockdata
variables. It will cause a small subroutine to be linked in that helps
force the loading of Ngmath blockdata initialization routines.
.sp
Note: this option doesn't need to be specified separately if you are
already including the \fB\-ncarbd\fR  and \fB\-ngmath\fR options.
.sp
.IP "\-netcdf"
Links in the netCDF library.  This library is not part of NCAR Graphics,
so check with your system administrator if you need it installed.  You
can obtain a copy of it by doing an anonymous ftp to unidata.ucar.edu.
.sp
.IP "\-hdf"
Links in the HDF library.  This library is not part of NCAR Graphics,
so check with your system administrator if you need it installed.  You
can obtain a copy of it by doing an anonymous ftp to ftp.ncsa.uiuc.edu.
.sp
.SH SEE ALSO
Online:
.BR nhlf77(1NCARG),
.BR ncargf77(1NCARG),
.BR ncargcc(1NCARG),
.BR ncargintro(5NCARG)
.sp
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
