.\"
.\"     $Id: ngwsym.m,v 1.2 1993-03-21 02:43:29 haley Exp $
.\"
.TH NGWSYM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGWSYM - Draws a symbol from the standard WMO/NOAA meteorological
fonts by reference to the font name and symbol number within that
font.
.SH SYNOPSIS
CALL NGWSYM(FTYPE,NUM,X,Y,SIZE,ICOLOR,IALT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngwsym(char *ftype, int num, float x, float y, float size, int icolor, int ialt);
.SH DESCRIPTION 
.IP FTYPE 12
(an input parameter of type CHARACTER) specifying the desired font.  Legal
values are:
.IP "            'WW'" 18
-  Present weather.
.IP "            'C'" 18
-  Cloud types.
.IP "            'CL'" 18
-  Low clouds.
.IP "            'CM'" 18
-  Medium clouds.
.IP "            'CH'" 18
-  High clouds.
.IP "            'W'" 18
-  Past weather.
.IP "            'N'" 18
-  Sky cover.
.IP "            'a'" 18
-  Pressure tendency.
.IP NUM 12
(an input parameter of type INTEGER) specifying the number of the
desired symbol within the specified font.
.IP X 12
(an input parameter of type REAL) specifying the X coordinate 
position, in world coordinates, where the symbol is to be positioned.  
This X position marks the horizontal center of the symbol.
.IP Y 12
(an input parameter of type REAL) specifying the Y coordinate 
position, in world coordinates, where the symbol is to be positioned.  
This Y position marks the vertical center of the symbol.
.IP SIZE 12
(an input parameter of type REAL) the value of which is the height, in 
world coordinates, of the symbol.
.IP ICOLOR 12
(an input parameter of type INTEGER) the value of which is the GKS 
color index specifying what color the symbol will be.
.IP IALT 12
(an input parameter of type INTEGER) indicating whether an alternate
representation for the specified symbol is to be used.   If IALT=1, then 
the alternate symbol for the one specified is drawn.  This applies 
only to a few symbols such as numbers 7, 93, 94, 95, 97 in the WW font 
and number 3 in the W font.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This function simply looks up the appropriate symbol in either
font 36 or 37 of PLOTCHAR and uses PLOTCHAR to draw the symbol.
.SH EXAMPLES
.sp
       CALL NGWSYM('N',5,.5,.5,.25,1,0)
.sp
would plot the symbol for six-tenths cloud cover
at position (.5,.5) and height .25 in the foreground color.
.sp
       CALL NGWSYM('a',6,.2,.8,.3,1,0)
.sp
would plot the symbol for barometric pressure that is falling then steady.
.sp
       CALL NGWSYM('WW',95,.5,.5,.2,1,1)
.sp
would plot the alternate symbol for slight or moderate 
thunderstorm without hail.
.SH ACCESS
To use NGWSYM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_ngwsym, load the
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and 
ncarg_loc, preferably in that order.
.SH MESSAGES
If an illegal font, or symbol number within a font, is requested,
then a warning is issued.
.SH SEE ALSO
Online:
plotchar
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
