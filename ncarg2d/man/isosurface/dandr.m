.TH DANDR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DANDR - Draws an isosurface when used in conjunction with entry INIT3D.
.SH SYNOPSIS
CALL DANDR (NV,NW,ST1,LX,NX,NY,IS2,IU,S,IOBJS,MV)
.SH STATUS
This routine is part of the obsolete Isosrfhr utility.  Isosrfhr has
been replaced by the Isosurface utility.  To see the Isosurface
utility overview man page, type "man isosurface".
.SH DESCRIPTION 
.IP NV 12
(an input expression of type INTEGER) defines the range 
to be used for the V dimension of the 3-D UVW grid.
.IP NW 12
(an input expression of type INTEGER) defines the range 
to be used for the W dimension of the 3-D UVW grid.
.IP ST1 12
(an input array of type REAL) used as scratch work space.
It must be dimensioned at least of size NV*NW*2 words.
.IP LX 12
(an input array of type INTEGER) which is the first
dimension of array IS2.  It must be of a length
to hold NX bits.  On a 64 bit machine it would be
LX=(NX-1)/64+1.
.IP NX 12
(an input expression of type INTEGER) defines the range 
of cells in the X-direction of the XY pixel model of the
image plane.  A silhouette of
the parts of the picture processed so far is
stored in this model.  Lines to be drawn are
tested for visibility by examining the
silhouette.  Lines in the silhouette are
hidden.  Lines out of the silhouette are
visible.  The solution is approximate
because the silhouette is not formed exactly.
See IS2.
.IP NY 12
(an input expression of type INTEGER) defines the range 
of cells in the Y-direction of the XY pixel model of the
image plane.  NY is also the second dimension of array IS2.
.IP IS2 12
(an input array of type INTEGER) which holds the XY pixel
model of the image plane.  It is dimensioned LX by NY.
The pixel model contains NX by NY elements.  The sizes of
NX and NY vary with the choice of options.  For example,
if hidden lines are drawn, NX and NY (and LX) can be smaller than
when hidden lines are not drawn.  If some visible lines
are not drawn, NX and NY (and LX) must be made larger.
As a guide, some examples which have been adequate are:
.nf
     GIVEN                 USE

    NU  NV  NW            NX  NY
--------------------------------
   100 100  60           200 200
    60  60  60           110 110
    40  40  40            75  75

.fi
.IP IU 12
(an input expression of type INTEGER) defines the
unit number of a scratch file for the package.
ST1 will be written NU times on this file.
.IP S 12
(an input array of type REAL, dimensioned 4) contains
the world coordinates of the area where the picture
is to be drawn.  That is, all plotting
coordinates generated will be bounded as
follows:  X coordinates will be between S(1)
and S(2), Y coordinates will be between S(3)
and S(4).  To prevent distortion, have
S(2)-S(1) = S(4)-S(3).
.sp
All coordinates should be in a range between 10.
and 1010.
.IP IOBJS 12
(an input array of type INTEGER) which describes the
objects to be drawn in VW space.  It is of size
NV by NW.  The actual first dimension of IOBJS in the
calling program is MV.
.sp
Entry DANDR is called NU times to process any object
contours in the VW cross sections defined by IOBJS.
Slabs are processed from back to front such that on
the I-th call to DANDR, the slab at U = NU+1-I is
processed.  IOBJS(J,K) = 1 if any object contains the
point (NU+1-I,J,K) and IOBJS(J,K) = 0 otherwise.
.IP MV 12
(an input expression of type INTEGER) defines the actual
first dimension of the array IOBJS in the calling program.
When plotting all of IOBJS, NV = MV.
.SH USAGE
Entries DANDR and DANDR must be used in tandem to create a plot.
First call entry DANDR to create a set of 2-D perspective planes which
are output to a scratch file.  Then,
in the calling program you must loop over the U dimension of UVW space
starting at the last slab, NU, and moving forward to slab 1.
Initially set all elements of IOBJS to zeros.  Then, at all locations
(J,K) where the surface of an object would appear, set IOBJS(J,K) = 1.
When IOBJS(J,K) has been defined for that VW slab (U = NU - I + 1),
call the entry DANDR, which draws any object contours based on the
2-D perspective defined for that slab.
.sp
An Isosrfhr example of the logical structure of a program follows:
.nf

 C  Entry DANDR writes NU 2-D perspective planes, ST1(NV,NW,2),
 C   of the UVW grid, viewed from the position EYE, to a scratch file.
 C
      CALL DANDR(EYE,NU,NV,NW,ST1,LX,NY,IS2,IU,S)
 C
 C NU is the extent of the U dimension.
 C
      DO 1 IBKWDS = 1,NU
      I = NU+1-IBKWDS
 C
 C You must define the presence of the objects at each VW grid point for
 C all slabs in the U direction. Ones are used to indicate the presence
 C of an object.  Zeros indicate no object.
 C
 C      IF (OBJ.EQ.YES) IOBJS(J,K) = 1
 C      IF (OBJ.EQ.NO)  IOBJS(J,K) = 0
 C
 C  DANDR reads ST1(NV,NW,2) for slab I.
 C
    1 CALL DANDR(NV,NW,ST1,LX,NX,NY,IS2,IU,S,IOBJS,MV)

.fi
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:  tisohr
.SH ACCESS
To use DANDR load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
isosrfhr, init3d,
isosurface, isosurface_params, ezisos, 
isgeti, isgetr, isseti, issetr, pwrzi, 
ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
