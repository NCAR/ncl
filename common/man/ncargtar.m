.\"
.\"	$Id: ncargtar.m,v 1.5 1993-04-21 20:04:45 fred Exp $
.\"
.TH NCARGTAR 8NCARG "April 1993" NCAR "NCAR GRAPHICS"
.SH NAME
ncargtar \- Create tar file for NCAR Graphics.
.SH SYNOPSIS
.B ncargtar
.PP
.SH DESCRIPTION
.B ncargtar
has no arguments.  It creates, in the current directory, a tar file
containing all files written by the last installation of NCAR
Graphics.  The environment variable NCARG_ROOT or the collection of
environment variables NCARG_LIB, NCARG_BIN, NCARG_INCLUDE, and
NCARG_MAN must be set in order to run this script.  For more
information, see the Installer's Guide or the Release Notes.
.sp
The names of the files in the archive are given as full path names,
preceded by a period (for example, "./u2/ncargfx/3.2.0/bin/ncargtar").
Thus, if the files are extracted to the root directory "/", NCAR
Graphics is restored; if the files are extracted to some other
directory (for example, "/tmp/ncargfx", the directory hierarchy is
created and the files in it may be checked for correctness without
overwriting the current working version of NCAR Graphics.
