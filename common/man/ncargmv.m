.\"
.\"	$Id: ncargmv.m,v 1.4 1993-04-20 14:42:02 haley Exp $
.\"
.TH NCARGMV 8NCARG "March 1993 NCAR "NCAR GRAPHICS"
.SH NAME
ncargmv \- Move an installed version of NCAR Graphics.
.SH SYNOPSIS
.B ncargmv
.PP
.SH DESCRIPTION
.B ncargmv
has no arguments and must be run interactively.  It is intended for
use by a system administrator, and will move an installed version of
NCAR Graphics (at revision level 3.2.0 or later) from its current
position to a new position.  The environment variable NCARG_ROOT or
the collection of environment variables NCARG_LIB, NCARG_BIN,
NCARG_INCLUDE, and NCARG_MAN must be set in order to run this script.
For more information on these environment variables, see the
Installer's Guide or the Release Notes.
