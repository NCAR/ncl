.\"
.\"	$Id: gclks.m,v 1.9 2000-07-11 23:03:07 haley Exp $
.\"
.TH GCLKS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCLKS (Close GSK) - closes the GKS package.
.SH SYNOPSIS
CALL GCLKS
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gclose_gks( void );
.SH USAGE
A call to GCLKS insures that the graphics package is properly
terminated.  No graphics calls depending on GKS should be made
during the time that GKS is closed.  If output to a CGM has been
the exclusive requirement of the graphics job, and the SPPS entry OPNGKS
has been used to open GKS and to open and activate a CGM workstation,
then CLSGKS can be used to deactivate and close the CGM workstation,
and to close GKS, making a direct call to GCLKS unnecessary in that
case.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gopks, gopwk, gacwk, gdawk, gclwk, gupwk, opngks, clsgks, gclose_gks
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
