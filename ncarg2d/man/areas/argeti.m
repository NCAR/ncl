.\"
.\"	$Id: argeti.m,v 1.1 1993-03-11 16:13:39 haley Exp $
.\"
.TH ARGETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ARGETI - Retrieves an integer parameter in Areas.
.SH SYNOPSIS
CALL ARGETI (PNAM, IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_argeti (char *pnam, int *ival)
.SH DESCRIPTION 
.IP "PNAM" 12
(Character string, Input) - 
The internal parameter name. 
.IP "IVAL" 12
(Integer, Input) - 
An integer variable in which the current value of the
parameter is to be returned.  That value may have been
selected by you or set by the package.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH USAGE
Currently Areas supports the following parameters.
.IP "AT" 12
The Arithmetic Type flag allows the user to check the type
of arithmetic to be used by Areas.
.RS
.IP "1" 
Real arithmetic is to be used.
.IP "2" 
Double precision arithmetic is to be used.
.IP "\(>= 4" 
AT specifies the base value.  (For example, the
value 100 would specify the use of base-100 integer
arithmetic.)
.RE
.IP "DB" 12
The debugging flag allows the user to study his area map.
.RS
.IP "0"
Debugging turned off.  This is the default.
.IP "\(>= 1" 
Debugging is on, and ARPRAM will produce plots
showing all edges belonging to the group with
identifier equal to the value of DB in the area
map at selected breakpoints.
.RE
.IP "DI" 12  
The direction flag tells the user which direction an
edge goes.  Note: this parameter should not be used
with ARSETI.
.RS
.IP "1" 
Edge of the area is given in counterclockwise order
(with the interior to the left).
.IP "2" 
Edge of the area is given in clockwise order (with
the interior to the right).
.sp
Note: these values are only meaningful when the ARGETI
call originates from the user-written routine (dummy
argument "APR") which is called by ARSCAM.  It gives
you information about the polygon defined by the values
of APR's arguments.
.RE
.IP LC 12
The largest coordinate parameter allows you to check
the largest coordinate allowed in an area map.  X and Y
coordinates in an area map are represented by integers
in the range from 0 to LC, inclusive; the default
value of LC is 1000000.  Attempting to give LC a
value less than 1000 will give it the value 1000.
The value of LC must not be greater than the largest
integer on the machine on which AREAS is running; its
value must also be exactly representable as a real
number on that machine.
.SH ACCESS
To use ARGETI, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_argeti, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ardbpa, ardrln, aredam, argeti, argtai, arinam, arpram, arscam, 
arseti, areas, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
