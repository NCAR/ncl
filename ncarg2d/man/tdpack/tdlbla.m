.TH TDLBLA 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDLBLA - This routine is called to put labels on a particular edge of a box.
.SH SYNOPSIS
CALL TDLBLA (IAXS, ILBL, NLBL, XAT0, XAT1, YAT0, YAT1, ANGD)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdlbla(int iaxs, char* ilbl, char* nlbl, float xat0, float xat1,
float yat0, float yat1, float angd)
.SH DESCRIPTION
It is assumed that TDPARA has been called to define the reference
parallelogram to be a rectangle in 3-space lying in one corner of one
face of the box being labelled. The sides of this rectangle are assumed to
be vectors of length 1 (that is to say, the rectangle defines a unit square
within that face of the box). It is also assumed that the value of the
internal parameter 'CS2' has been set in the same way that TDLBLS
would reset it, using a code sequence like
.sp
.nf
  CALL TDGETR ('CS1',CSM1)
  CSM2=CSM1*MIN(UMAX-UMIN,VMAX-VMIN,WMAX-WMIN)
  CALL TDSETR ('CS2',CSM2)
.fi
.sp
(where UMIN, UMAX, VMIN, VMAX, WMIN, and WMAX are as defined for a call to
TDLBLS).
.sp
The arguments of TDLBLA are as follows:
.IP "IAXS" 8
(an input expression of type INTEGER) - says which edge of the face is being
labelled (1 => left, 2 => right, 3 => bottom, and 4 => top, where the meanings
of "left", "right", "bottom", and "top" are defined by the orientation of the
reference parallelogram).
.IP "ILBL" 8
(input, of type CHARACTER) - a string to be used as an informational label.  If
the string is blank, no informational label is written.
.IP "NLBL" 8
(input, of type CHARACTER) - a string containing numeric labels. The labels
need not be in any particular order, but they have to be separated by blanks
and each has to be readable using a FORTRAN format of the form "En.0", where
\&"n" is the length of the label. If the string is blank, no informational
label is written.
.IP "XAT0 and XAT1" 8
(input expressions of type REAL) - the values of "X" associated with the left
and right edges of the face being labelled, where "left" and "right" are
defined in terms of the current reference parallelogram.
.IP "YAT0 and YAT1" 8
(input expressions of type REAL) - the values of "Y" associated with the bottom
and top edges of the face being labelled, where "bottom" and "top" are defined
in terms of the current reference parallelogram.
.IP "ANGD" 8
(an input expression of type REAL) - specifies the angle, in degrees, at which
the labels are to be written. This angle is defined with reference to the
current reference parallelogram.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDLBLA or c_tdlbla, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
