.\"
.\"	$Id: labmod.m,v 1.1 1993-03-11 16:26:47 haley Exp $
.\"
.TH LABMOD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
LABMOD - Presets parameters controlling the appearance of
labels drawn by GRIDAL, GRIDL,... et al. LABMOD itself does
no plotting and, in order to have any effect, must be called
prior to the background-drawing routines for which it is
presetting parameters.
.SH SYNOPSIS
CALL LABMOD (FMTX, FMTY, NUMX, NUMY, ISZX, ISZY, IXDC, IYDC, IXOR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_labmod (char *fmtx, char *fmty, int numx, int numy, int iszx, int iszy, int ixdc, int iydc, int ixor)
.SH DESCRIPTION 
.IP "FMTX" 12
(Character, Input) - 
Contains format specifications for the 
X-axis numerical labels produced by GRIDAL. The 
specification must begin with a left parenthesis and end 
with a right parenthesis and must not be more than ten 
characters long. Conversions of types E, F, G, and I are 
allowed; for example, one might use '(F8.2)'. The default 
is '(E10.3)'.
.IP "FMTY" 12
(Character, Input) - 
Contains format specifications for the 
Y-axis numerical labels produced by GRIDAL. The 
specification must begin with a left parenthesis and end 
with a right parenthesis and must not be more than ten 
characters long. Conversions of types E, F, G, and I are 
allowed; for example, one might use '(E10.0)'. The default 
is '(E10.3)'.
.IP "NUMX" 12
(Integer, Input) - 
A nonzero value specifies the number of 
characters in each X-axis numeric label; if LBLX is a 
string produced by the format FMTX, then the label is the 
substring LBLX(1:NUMX). The default value is 0.
.RS
.IP "0"
The label is the substring LBLX(m:n), where LBLX(m:m) is 
the first non-blank character in LBLX, and LBLX(n:n) is 
the last nonblank character following LBLX(m:m).
.sp
Using a nonzero NUMX causes the labels to be centered 
differently than if a zero value is used.
.RE
.IP "NUMY" 12
(Integer, Input) - 
Same as NUMX, except it is used for Y-
axis labels.
.IP "ISZX,ISZY" 12
(Integer, Input) - 
Character sizes for the labels, specified 
in thousandths of a screen width, just as for the SPPS 
routine WTSTR. The default value for both is 10.
.IP "IXDC" 12
(Integer, Input) - 
The distance, in thousandths of a screen 
width, from the left edge of the current viewport to the 
label specified by FMTY, NUMY, and ISZY. There are two 
special values of IXDC:
.RS
.IP "0" 
The Y-axis labels end 20 thousandths of a screen width 
(0.02 NDCs) to the left of the viewport. This is 
equivalent to setting IXDC=20.
.IP "1"
Y-axis labels begin 0.02 NDCs to the right of the 
viewport. This is equivalent to setting IXDC=-20-w, 
where w/1024 is the width of the viewport in 
NDCs.
.sp
The default value is 20.
.sp
When GRIDAL is called with IGPH=2, 6, or 10, IXDC is the 
distance from the Y axis, rather than from the minimum 
viewport coordinate, and special values 0 and 1 are 
equivalent to 20 and -20.
.RE
.IP "IYDC" 12
(Integer, Input) - 
The distance, in thousandths of a screen 
width, from the bottom edge of the current viewport to 
the label specified by FMTX, NUMX, and ISZX. There are 
two special values of IYDC:
.RS
.IP "0"
The X-axis labels end 20 thousandths of a screen width 
(0.02 NDCs) below the viewport. This is equivalent to 
setting IYDC=20.
.IP "1"
The X-axis labels begin 0.02 NDCs above the viewport. 
This is equivalent to setting IYDC=-20-h, where h/1024  
is the height of the viewport in NDCs.
.sp
The default value is 20.
.sp
When GRIDAL is called with IGPH=2, 6, or 10, IYDC is the 
distance from the X axis, rather than from the minimum 
viewport coordinate, and special values 0 and 1 are 
equivalent to 20 and -20.
.RE
.IP "IXOR" 12
(Integer, Input) - 
Specifies the orientation of the X-axis 
labels.
.RS
.IP "0"
Horizontal.
.IP "1"
Vertical.
.sp
The default orientation is horizontal.
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use LABMOD load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_labmod, load
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gacolr, gagetc, gageti, gagetr, gasetc, gaseti, gasetr, grid, gridal,
gridl, halfax, labmod, perim, periml, tick4, ticks, ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

