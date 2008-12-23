.\"
.\"	$Id: gopwk.m,v 1.17 2008-12-23 00:03:02 haley Exp $
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
package supports.  The legal workstation types in NCAR GKS are:
.IP "             1" 18
-  NCGM
.IP "             3" 18
-  WISS
.IP "             7" 18
-  pre-existing X11 window.
.IP "             8" 18
-  non-existing X11 window.
.IP "            10" 18
-  text dump of graphics output.
.IP "            11" 18
-  PDF in portrait mode.
.IP "            12" 18
-  PDF in landscape mode.
.IP "            20" 18
-  color PostScript in portrait mode.
.IP "            21" 18
-  color Encapsulated PostScript (EPS) in portrait mode.
.IP "            22" 18
-  color Encapsulated PostScript Interchange format (EPSI) in portrait mode.
.IP "            23" 18
-  monochrome PostScript in portrait mode.
.IP "            24" 18
-  monochrome Encapsulated PostScript (EPS) in portrait mode.
.IP "            25" 18
-  monochrome Encapsulated PostScript Interchange format (EPSI) in portrait mode.
.IP "            26" 18
-  color PostScript in landscape mode.
.IP "            27" 18
-  color Encapsulated PostScript (EPS) in landscape mode.
.IP "            28" 18
-  color Encapsulated PostScript Interchange format (EPSI) in landscape mode.
.IP "            29" 18
-  monochrome PostScript in landscape mode.
.IP "            30" 18
-  monochrome Encapsulated PostScript (EPS) in landscape mode.
.IP "            31" 18
-  monochrome Encapsulated PostScript Interchange format (EPSI) in landscape mode.
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
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
