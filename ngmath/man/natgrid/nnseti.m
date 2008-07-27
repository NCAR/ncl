.\"
.\"     $Id: nnseti.m,v 1.6 2008-07-27 03:35:41 haley Exp $
.\"
.TH NNSETI 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NNSETI - Sets the value of an internal parameter of type INTEGER.
.SH SYNOPSIS
CALL NNSETI (PNAM,IVAL)
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the
parameter to be set. 
.IP IVAL 12
An INTEGER value that 
is the value to be assigned to the
internal parameter specified by PNAM.
.SH USAGE
This routine allows you to set the current value of
Natgrid parameters.  For a complete list of parameters available
in this utility, see the natgrid_params man page.
.SH ACCESS
To use NNSETI, load the NCAR Graphics library ngmath.
.SH SEE ALSO
natgrid,
natgrid_params, 
natgrids, 
nngeti.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
