.\"
.\"	$Id: gopwk.m,v 1.3 1993-05-03 17:26:20 haley Exp $
.\"
.TH GOPWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GOPWK (Open workstation) - opens a GKS workstation.
.SH SYNOPSIS
CALL GOPWK (WKID, CONID, WKTYPE)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gopen_ws(Gint ws_id, const char *conn_id, Gint ws_type);
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - A number assigned to a workstation as an identifier 
that is to be used in all subsequent calls to GKS functions that
require a workstation identifier.  In NCAR GKS, WKID can 
be any non-negative integer. 
.IP CONID 12
(Integer, Input) - A connection identifier that has different 
meanings for different workstation types (see the USAGE section
below).
.IP WKTYPE 12
(Integer, Input) - An identifier specifying
the specific type of output device targeted.  Each GKS package has
an implementation-dependent set of workstation types that the
package supports.  There are five
legal workstation types in NCAR GKS:
.IP "             1" 18
-  CGM
.IP "             3" 18
-  WISS
.IP "             7" 18
-  pre-existing X11 window.
.IP "             8" 18
-  non-existing X11 window.
.IP "            10" 18
-  text dump of graphics output.
.SH USAGE
For workstation types of 1 (CGM) or 3 (WISS) 
the connection ID is used as a Fortran
logical unit number.  One can have at most one
workstation of type 1 open at a time and this also applies to
workstations of type 3.  One can have workstations of types 1 and
3 open simultaneously and they must have distinct connection IDs.  
To use the GFLASH package or any GKS
segmentation functions WISS must be opened first.
.sp
For workstations of type 7 the connection ID is the X11 window ID
for the existing window (obtained from xwininfo for example).
.sp
For workstations of type 8 an X11 window will be created at open
workstation time.  The connection ID is irrelevant for workstations
of type 8.
.sp
For workstations of type 10, the ASCII text is written to standard
output and the connection ID is irrelevant.
.sp
There can be a maximum of fifteen simultaneously open workstations
of all types.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gopks, gacwk, gdawk, gclwk, gclks, opngks, clsgks, gopen_ws
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
