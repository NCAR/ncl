.\"
.\"	$Id: ncargpath.m,v 1.1 1994-11-11 22:09:40 haley Exp $
.\"
.TH NCARGPATH 8NCARG "November 1994" NCAR "NCAR GRAPHICS"
.SH NAME
ncargpath \- Return the full pathname to a given NCAR Graphics directory
or the default of a particular NCAR Graphics attribute.
.SH SYNOPSIS
.B ncargpath name
.PP
.SH DESCRIPTION
\fBncargpath\fP
has one argument which is the shortened name of the directory whose full
pathname you want returned or the name of the attribute whose default value
you want.  It is used by several NCAR Graphics
scripts to find the pathname to certain installables.  For example,
the script \fBncargex\fP uses \fBncargpath\fP to determine where the
directory that contains all the examples is installed.  The following
arguments are recognized:
.sp
.IP "bin" 18
directory where NCAR Graphics binaries are installed
.IP "config" 18
directory where NCAR Graphics configuration files are installed
.IP "database" 18
directory where NCAR Graphics databases (like the Ezmap database) 
are installed
.IP "examples" 18
directory where NCAR Graphics Fortran and C examples are installed
.IP "fontcap" 18
the default fontcap being used
.IP "fontcaps" 18
directory where NCAR Graphics fontcaps are installed
.IP "gks_output" 18
the default name of the metafile
.IP "graphcap" 18
the default graphcap being used
.IP "graphcaps" 18
directory where NCAR Graphics graphcaps are installed
.IP "include" 18
directory where NCAR Graphics include files are installed
.IP "lib" 18
directory where NCAR Graphics libraries are installed
.IP "man" 18
directory where NCAR Graphics man pages are installed
.IP "ncarg" 18
root directory where NCAR Graphics examples, databases resource files, etc. are installed
.IP "root" 18
parent directory where NCAR Graphics is installed
.IP "sysappres" 18
directory where the NCAR Graphics systems application resource file is installed
.IP "sysresfile" 18
directory where the NCAR Graphics system resource file is installed
.IP "tests" 18
directory where NCAR Graphics Fortran and C test examples are installed
.IP "tmp" 18
directory where NCAR Graphics temporary files will be written
.IP "tutorial" 18
directory where NCAR Graphics tutorial C and Fortran examples are installed
.IP "usrresfile" 18
directory where the NCAR Graphics user resource file is installed
.IP "xapp" 18
directory where NCAR Graphics X application default files are installed
.SH SEE ALSO
Online:
.BR ncarg_gks(3NCARG),ncargintro(5NCARG)
.sp
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993, 1994 University Corporation
for Atmospheric Research
.br
All Rights Reserved
