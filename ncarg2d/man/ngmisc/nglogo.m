.TH NGLOGO 3NCARG "April 2002" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGLOGO - Draws various NCAR and UCAR logos on a specified workstation.
.SH SYNOPSIS
CALL NGLOGO(IWK,X,Y,SIZE,ITYPE,ICOL1,ICOL2)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_nglogo(int iwk, float x, float y, float size, 
.br
int itype, int icol1, int icol2)
.br
.SH DESCRIPTION 
.IP IWK 12
(an input parameter of type INTEGER) The workstation identifier for 
the workstation you want the logo plotted to.
.IP X,Y 12
(input parameters of type REAL) specifying the X-Y coordinate 
position, in normalized device coordinates (between 0. and 1.), 
where the symbol is to be positioned.  
This position marks the horizontal and vertical center of the symbol.
.IP SIZE 12
(an input parameter of type REAL) The desired height of the logo, 
expressed in normalized device coordinates.
.IP ITYPE 12
(an input parameter of type INTEGER) the logo type.  The choices are:
.RS
.IP "'1'" 
\-  An NCAR logo.  This logo will be drawn in a single color if IWK 
is not a PostScript workstation.  A full color logo will be put to a 
PostScript workstation.
.IP "'2'" 
\-  A UCAR logo (just the UCAR star symbol).
.IP "'3'" 
\-  The text "NCAR" in Bell Gothic Black font.
.IP "'4'" 
\-  The text "UCAR" in Bell Gothic Black font.
.IP "'5'" 
\-  UCAR star logo, plus "UCAR" in Bell Gothic font at
half the height of the star.  In this case, the
coordinate (X,Y) specifies the center of the star part
of the logo.
.RE
.IP ICOL1 12
(an input parameter of type INTEGER) The color index to be used for the 
logo color.  For the NCAR logo on PostScript output, this argument 
is ignored.
.IP ICOL2 12
(an input parameter of type INTEGER) A secondary color index used 
only for logo type 5.  For that type, the UCAR star logo is drawn 
using color index ICOL1 and the text string "UCAR" is drawn using 
color index ICOL2.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Call this subroutine to produce the desired logo.
.sp
Use the ncargex command to see the following relevant
example: 
miex01.
.SH EXAMPLES
.IP "CALL NGLOGO(1, 0.5, 0.5, 0.1, 1, 1, 1)
Plots an NCAR logo on workstation with ID 1 in the center of the plot 
with height one-tenth the maximum plot size.  If going to a PostScript 
workstation, the logo will be a full-color logo, otherwise
the logo will be plotted using color index 1.
.IP "CALL NGLOGO(1, 0.5, 0.5, 0.1, 2, 2, 1)
Plots a UCAR star logo on workstation with ID 1 in the center of the plot
with height one-tenth the maximum plot size. 
The logo will be plotted using color index 2.
.SH ACCESS
To use NGLOGO or c_nglogo, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
plotchar (font 35) (3NCARG),
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
