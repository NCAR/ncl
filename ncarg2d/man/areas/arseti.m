.\"
.\"	$Id: arseti.m,v 1.1 1993-03-11 16:13:53 haley Exp $
.\"
.TH ARSETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ARSETI - Sets an integer parameter in Areas.
.SH SYNOPSIS
CALL ARSETI (PNAM, IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_arseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP "PNAM" 12
(Character string, Input) - 
The internal parameter name.  The string can be of any
length, however, only the first 2 characters will be
examined.
.IP "IVAL" 12
(Integer, Input) - 
The integer value you select for the parameter.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH USAGE
Currently Areas supports the following parameters.
.IP "AT" 12
The Arithmetic Type flag allows the user to set the type
of arithmetic to be used by Areas.
.RS
.IP "\(<= 0" 
(The default value) allows Areas to decide what
sort of arithmetic to use.
.IP "1" 
Real arithmetic is to be used.
.IP "2" 
Double precision arithmetic is to be used.
.IP "3" 
Multiple precision integer arithmetic is to be used,
Areas will choose the base value to be used.
.IP "\(>= 4"  
AT specifies the base value.  (For example, the
value 100 would specify the use of base-100 integer
arithmetic.)
.sp
Warning: NCAR does not recommend changing the value of
AT without advice from a consultant.
.RE
.IP "DB" 12
The debugging flag allows the user to study his area map.
.RS
.IP "0" 
Debugging turned off.  This is the default.
.IP "\(>=1" 
Debugging is on, and ARPRAM will produce plots
showing all edges belonging to the group with
identifier equal to DB in the area map at
selected breakpoints.
.RE
.IP "LC" 12
The largest coordinate parameter allows you to change
the largest coordinate allowed in an area map.  X and Y
coordinates in an area map are represented by integers
in the range from 0 to LC, inclusive; the default
value of LC is 1000000.  Attempting to give LC a
value less than 1000 will give it the value 1000.
The value of LC must not be greater than the largest
integer on the machine on which AREAS is running; its
value must also be exactly representable as a real
number on that machine.
.sp
Warning: NCAR does not recommend changing the value of
LC without advice from a consultant.
.SH ACCESS
To use ARSETI, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_arseti, load 
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
