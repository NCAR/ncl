.\"
.\"     $Id: dsgetc.m,v 1.5 2008-07-27 03:35:36 haley Exp $
.\"
.TH DSGETC 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DSGETC - Retrieves the value of an internal parameter of type CHARACTER.
.SH SYNOPSIS
CALL DSGETC (PNAM,CVAL)
.SH DESCRIPTION 
.IP PNAM 12
A character string that specifies the name of the
parameter to be retrieved. The name must appear as the first three
characters of the string.
.IP CVAL 12
A character string that is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
.SH USAGE
This routine allows you to retrieve the current value of
Dsgrid parameters.  For a complete list of parameters available
in this utility, see the dsgrid_params man page.
.SH ACCESS
To use DSGETC, load the NCAR Graphics library ngmath.
.SH SEE ALSO
dsgrid,
dsgrid_params,
dsgrid2s,
dsgrid3s,
dsgrid2d,
dsgrid3d,
dspnt2s,
dspnt2d,
dspnt3s,
dspnt3d
dssetc.
.sp
Complete documentation for Dsgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/dsgrid/dshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
