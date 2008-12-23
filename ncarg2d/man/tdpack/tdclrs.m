.TH TDCLRS 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDCLRS - Does GKS calls to define colors for TDPACK.
.SH SYNOPSIS
CALL TDCLRS (IWID, IBOW, SHDE, SHDR, IOFC, IOLC, ILMT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdclrs(int iwid, int ibow, float shde, float shdr,
int iofc, int iolc, int ilmt)
.SH DESCRIPTION
This routine is called to do the GKS calls defining the following colors
on a specified workstation (NSHD is equal to IOLC-IOFC+1, the
number of elements in each block of color shades):
.sp
.nf
  Color 0 = background color (depends on IBOW)
  Color 1 = foreground color (depends on IBOW)
  Color 2 = red (RGB values 1,0,0)
  Color 3 = green (RGB values 0,1,0)
  Color 4 = blue (RGB values 0,0,1)
  Color 5 = cyan (RGB values 0,1,1)
  Color 6 = magenta (RGB values 1,0,1)
  Color 7 = yellow (RGB values 1,1,0)
  Colors IOFC - IOLC = grays, white to black
  Colors IOFC+NSHD - IOLC+NSHD = grays
  Colors IOFC+2*NSHD - IOLC+2*NSHD = reds
  Colors IOFC+3*NSHD - IOLC+3*NSHD = greens
  Colors IOFC+4*NSHD - IOLC+4*NSHD = blues
  Colors IOFC+5*NSHD - IOLC+5*NSHD = cyans
  Colors IOFC+6*NSHD - IOLC+6*NSHD = magentas
  Colors IOFC+7*NSHD - IOLC+7*NSHD = yellows
.fi
.sp
The colors defined by calling TDCLRS may be used for any purpose,
but they are particularly useful when calling TDPACK routines to
render surfaces.
.sp
The arguments of TDCLRS are as follows:
.IP "IWID" 8
(an input expression of type INTEGER) - the workstation identifier.
.IP "IBOW" 8
(an input expression of type INTEGER) - a flag specifying the basic color
scheme (white on black or black on white).  If IBOW is 0, the foreground
color is white and the background color is black; if IBOW is non-zero, the
opposite is true.
.IP "SHDE and SHDR" 8
(input expressions of type REAL) - each of these values is between 0
and 1, inclusive, specifying how color shades are to be generated.
Values of SHDE near 0 call for more intense shades to be used, while
values near 1 call for more nearly pastel shades to be used. Values of
SHDR near 0 say that a narrower range of shades is to be used, while
values near 1 say that a broader range of shades is to be used.
.IP "IOFC and IOLC" 8
(input expressions of type INTEGER) - these specify the first and last
integers in a block of color indices to be used for NSHD shades of gray
ranging from pure white to pure black (where NSHD=IOLC-IOFC+1). The next
NSHD indices (in numerical order) will be used for the shades of gray
selected by SHDE and SHDR; the next NSHD indices after that for selected
shades of red, the next NSHD indices after that for selected shades of
green, and so on.
.IP "ILMT" 8
(an input expression of type INTEGER) - Using a value between 1 and 7,
inclusive, says that only that many blocks of NSHD indices will be
defined. For example, if ILMT has the value 4, only the black-to-white
scale and the shades of gray, red, and green will be generated; shades
of blue, cyan, magenta, and yellow will not be. (This allows one to have
more shades of each color at the expense of using fewer colors.) Using a
value of ILMT less than 1 or greater than 7 will result in all 8*NSHD
sets of color shades being defined.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDCLRS or c_tdclrs, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri, tdlbla,
tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
