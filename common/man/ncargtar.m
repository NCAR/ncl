.\"
.\"	$Id: ncargtar.m,v 1.1.1.1 1992-04-17 22:30:08 ncargd Exp $
.\"
.TH NCARGTAR 1NCARG "JUNE 1991" NCAR "NCAR GRAPHICS"
.SH NAME
ncargtar \- Create tar file for NCAR Graphics.
.SH SYNOPSIS
.B ncargtar
.PP
.SH DESCRIPTION
.B ncargtar
has no arguments.  It creates, in the current directory, a tar file
containing all files written by the last installation of NCAR Graphics.
The names of the files in the archive are given as full path names,
preceded by a period (for example, "./u2/ncargfx/3.1.2/bin/ncargtar").
Thus, if the files are extracted to the root directory "/", NCAR Graphics
is restored; if the files are extracted to some other directory (for
example, "/tmp/ncargfx", the directory hierarchy is created and the files
in it may be checked for correctness without overwriting the current
working version of NCAR Graphics.
