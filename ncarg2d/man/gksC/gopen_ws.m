.\"
.\"	$Id: gopen_ws.m,v 1.2 1993-03-29 22:42:21 haley Exp $
.\"
.TH GOPEN_WS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gopen_ws (Open workstation) - opens a GKS workstation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gopen_ws(Gint ws_id, const char *conn_id, Gint ws_type);
.SH DESCRIPTION
.IP ws_id 12
(Input) - A number assigned to a workstation as an identifier 
that is to be used in all subsequent calls to GKS functions that
require a workstation identifier.  In NCAR GKS, ws_id can 
be any non-negative integer. 
.IP conn_id 12
(Input) - A connection identifier that has different 
meanings for different workstation types (see the USAGE section
below).
.IP ws_type 12
(Input) - An identifier specifying
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
For workstation types of 1 (CGM)
the connection ID is the name of the CGM file.
One can have at most one workstation of type 1 open at a time.  
.sp
For workstations of type 3 (WISS) the connection ID is irrelevant.
One can have at most one workstation of type 3 open.
One can have workstations of types 1 and 3 open simultaneously and they 
must have distinct connection IDs.  To use the GFLASH package or any GKS
segmentation functions WISS must be opened first.
.sp
For workstations of type 7 the connection ID is the X11 window ID
for the existing window.  This number should be passed as a string; for
example, "3" for an X11 window ID of 3.
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
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gopen_gks(3NCARG),
.BR gactivate_ws(3NCARG),
.BR gdeactivate_ws(3NCARG),
.BR gclose_ws(3NCARG),
.BR gclose_gks(3NCARG),
.BR opngks(3NCARG),
.BR clsgks(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
