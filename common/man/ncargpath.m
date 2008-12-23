.\"
.\"	$Id: ncargpath.m,v 1.17 2008-12-23 00:03:53 haley Exp $
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
Directory where NCAR Graphics binaries are installed.
.IP "config" 18
Directory where NCAR Graphics configuration files are installed.
.IP "data" 18
Directory where NCAR Graphics HLU and NCL example data files are installed.
.IP "database" 18
Directory where NCAR Graphics databases (like the Ezmap database) 
are installed.
.IP "doc" 18
Directory where NCAR Graphics PostScript documents are installed.
.IP "examples" 18
Directory where NCAR Graphics LLU Fortran and C examples are installed.
.IP "fontcap" 18
The default fontcap being used; only returns a valid fontcap if the 
environment variable FONTCAP is set.
.IP "fontcaps" 18
Directory where NCAR Graphics fontcaps are installed.
.IP "gks_output" 18
The default name of the metafile.
.IP "graphcap" 18
The default graphcap being used; only returns a valid graphcap if the
environment variable GRAPHCAP is set.
.IP "graphcaps" 18
Directory where NCAR Graphics graphcaps are installed.
.IP "hluex" 18
Directory where NCAR Graphics HLU Fortran and C examples are installed.
.IP "include" 18
Directory where NCAR Graphics include files are installed.
.IP "lib" 18
Directory where NCAR Graphics libraries are installed.
.IP "man" 18
Directory where NCAR Graphics man pages are installed.
.IP "ncarg" 18
Root directory where NCAR Graphics examples, databases, resource files, etc.
are installed.
.IP "nclex" 18
Directory where NCAR Graphics NCL examples are installed.
.IP "ngwww" 18
Directory where NCAR Graphics HTML files are installed (if they
were installed).
.IP "ngurl" 18
URL for the NCAR Graphics documentation.
.IP "resfiles" 18
Directory where resource files for the NCL and HLU examples are installed.
.IP "root" 18
Parent directory where NCAR Graphics is installed.
.IP "sysappres" 18
Directory where the NCAR Graphics systems application resource file is
installed.
.IP "sysresfile" 18
Directory where the NCAR Graphics system resource file is installed.
.IP "tests" 18
Directory where NCAR Graphics LLU Fortran and C test examples are installed.
.IP "tmp" 18
Directory where NCAR Graphics temporary files will be written.
.IP "tutorial" 18
Directory where NCAR Graphics LLU tutorial C and Fortran examples are
installed.
.IP "usrresfile" 18
Directory where the NCAR Graphics user resource file is installed.
.IP "xapp" 18
Directory where NCAR Graphics X application default files are installed.
.SH SEE ALSO
Online:
.BR ncarg_env(5NCARG), ncarg_gks(3NCARG), ncargintro(5NCARG)
.sp
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
