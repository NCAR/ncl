.\"
.\"	$Id: velvct.m,v 1.1.1.1 1992-04-17 22:30:32 ncargd Exp $
.\"
.TH VELVCT 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " VELVCT - Draws 2-d velocity fields
.dsS1 " CALL EZVEC (U,V,M) if criteria below is met, else
.nrsN 1
.tr ~
.pn 393
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9VELVCT\s11@"
.EF "@\s\fB11%\s9\fR@@August 1987\s11@"
.OF "@\s9August 1987@@\s11\fB%\fR@"
.de hD          \" Heading macro level one
.br
.ne 5
.sp 2
.ps +3
.ft B           \" boldface, 14 pt.
\\$1
.ft R
.ps -3
.sp 
..
.de >>          \" display for indented lines
.in +.25i       \" usage: .>>
.sp
.nf
..              
.de <<          \" end of display for indented lines
.fi
.in -.25i       \" usage: .<<
.sp
..              
.de sf          \"start fortran (constant spacing)
.ps 10
.vs 12
.nf
.ft L
..
.de ef          \"end fortran (resume variable spacing & prev. size & font)
.ft
.fi
.ps
.vs
..
.br
.S 14
.S 11
SUBROUTINE VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
.R
.H 3 "Dimension of Arguments"
U(LU,N),V(LV,N),SPV(2)
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
VELVCT draws a representation of a two-dimensional 
velocity field by drawing arrows
from each data location.  The length of the
arrow is proportional to the strength of the
field at that location and the direction of
the arrow indicates the direction of the flow
at that location.
.H 3 "Usage"
If the following assumptions are met, use

CALL EZVEC (U,V,M,N)

Assumptions:
.BL
.LI
The whole array is processed.
.LI
The scale factor is chosen internally.
.LI
The perimeter is drawn.
.LI
FRAME is called after plotting.
.LI
There are no special values.
.LE

If these assumptions are not met, use

CALL VELVCT (U,LU,V,LV,M,N,FLO,HI,
NSET,LENGTH,ISPV,SPV)
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL .6i
.LI "\fBU,V\fR"
The (origins of the) two-dimensional arrays
containing the velocity field to be plotted.
The vector at the point (I,J) has magnitude
SQRT(U(I,J)**2+V(I,J)**2) and direction
ATAN2(V(I,J),U(I,J)).  Other representations,
such as (R,THETA), can be plotted by
changing statement functions in this routine.
.LI "\fBLU\fR"
The first dimension of U in the calling
program.
.LI "\fBLV\fR"
The first dimension of V in the calling
program.
.LI "\fBM\fR"
The number of data values to be plotted in
the X-direction (the first subscript
direction).  When plotting the entire array,
LU = LV = M.
.LI "\fBN\fR"
The number of data values to be plotted in
the Y-direction (the second subscript
direction).
.LI "\fBFLO\fR"
The minimum vector magnitude to be shown.
.LI "\fBHI\fR"
The maximum vector magnitude to be shown. (A
value less than or equal to zero causes the
maximum value of SQRT(U**2+V**2) to be used.)
.LI "\fBNSET\fR"
Flag to control scaling -

If NSET is zero, VELVCT establishes the
window and viewport to properly
scale plotting instructions to the standard
configuration.  PERIM is called to draw a
border.

If NSET is greater than zero, VELVCT assumes
that the user has established the window
and viewport in such a way as to properly
scale the plotting instructions generated
by VELVCT.  PERIM is not called.

If NSET is less than zero, VELVCT
places the contour plot
within the limits of the user's current
window and viewport.  PERIM is not called.
.LI "\fBLENGTH\fR"
The length, in Plotter Address Units (PAUs),
of a vector having magnitude HI
(or, if HI=0, the length in PAUs
of the longest vector).  If LENGTH=0, a
value is chosen such that the longest vector
could just reach to the tail of the next
vector.  If the horizontal and vertical
resolutions of the plotter are different,
LENGTH should be non-zero and specified as a
horizontal distance.
.LI "\fBISPV\fR"
Flag to control the special value feature.

0 means that the feature is not in use.

1 means that if the value of
U(I,J)=SPV(1) the vector will not be
plotted.

2 means that if the value of
V(I,J)=SPV(2) the vector will not be
plotted.

3 means that if either U(I,J)=SPV(1) or
V(I,J)=SPV(2) then the vector will not
be plotted.

4 means that if U(I,J)=SPV(1)
and V(I,J)=SPV(2), the vector
will not be plotted.
.LI "\fBSPV\fR"
An array of length 2 that gives the value
in the U array and the value in the V array
that denote missing values.
This argument is ignored if ISPV=0.
.H 3 "On Output"
All arguments remain unchanged.
.H 3 "Note"
The endpoints of each arrow drawn are (FX(X,Y),
FY(X,Y)) and 
.br
(MXF(X,Y,U,V,SFX,SFY,MX,MY), MYF(X,Y,U,V,SFX,SFY,MX,MY)) 
.br
where X=I, Y=J,
U=U(I,J), V=V(I,J), and SFX and SFY are scale
factors.  Here I is the X-index and J is the
Y-index.  (MX,MY) is the location of the tail.
Thus the actual length of the arrow is
SQRT(DX**2+DY**2) and the direction is
ATAN2(DX,DY), where DX=MX-MXF(. . .) and
DY=MY-MYF(. . .).
.H 3 "Entry Points"
VELVCT,EZVECT,DRWVEC,VELVEC,VELDAT
.H 3 "Common Blocks"
VEC1,VEC2
.H 3 "I/O"
Plots the vector field.
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "Required Library Routines"
GRIDAL and the SPPS
.H 3 "Required GKS Level"
0A
.H 3 "History"
Written and standardized in November 1973.
Revised in May 1975, to include MXF and MYF.
Revised in March 1981, to fix certain errors;
to use FL2INT and PLOTIT instead of MXMY,
FRSTPT, and VECTOR; and to make the arrowheads
narrower.  Converted to FORTRAN 77 and GKS
in July 1984.
.H 3 "Algorithm"
Each vector is examined, possibly transformed,
then plotted.
.H 3 "Portability"
FORTRAN 77
.H 3 "Note"
Using this routine to put vectors on an arbitrary background drawn by
SUPMAP is a bit tricky.  The arithmetic statement functions FX and FY
are easy to replace.  The problem arises in replacing MXF and MYF.
The following example may be helpful. (SUPMAP is an entry point in
the EZMAP package.)

Suppose that we have two arrays, CLON(36,9) and CLAT(36,9), which
contain the E-W and N-S components of a wind flow field on the surface
of the earth.  CLON(I,J) is the magnitude of the easterly flow.
CLAT(I,J) is the magnitude of the northerly flow at a longitude (I-1)
*10 degrees east of Greenwich and a latitude (J-1)*10 degrees north of
the equator.  SUPMAP is to be used to draw a polar projection of the
earth and VELVCT is to be used to superimpose vectors representing the
flow field on it.  The following steps would be necessary:
.AL
.LI
CALL SUPMAP (1,90.,0.,-90.,90.,90.,90.,90.,-4,10,0,1,IER)
to draw the map.
.LI
CALL VELVCT (CLON,36,CLAT,36,36,9,0.,0.,1,50,0,0.) to put
vectors on it.  Notice that NSET has the value 1 to tell
VELVCT that SUPMAP has done the required SET call.
.SK
.LI
In order to ensure that step 2 will work properly, delete
the arithmetic statement functions FX, FY, MXF, and MYF
from VELVCT and include the following functions.
.LE
.sp
.sf
FUNCTION FX(XX,YY)
CALL MAPTRN (10.*(YY-1.),10.*(XX-1.),X,Y)
FX=X
RETURN
END

FUNCTION FY(XX,YY)
CALL MAPTRN (10.*(YY-1.),10.*(XX-1.),X,Y)
FY=Y
RETURN
END

FUNCTION MXF(XX,YY,UU,VV,SFX,SFY,MX,MY)
CFCT=COS(.17453292519943*(YY-1.))
CALL MAPTRN(10.*(YY-1.)         ,10.*(XX-1.)              ,X1,Y1)
CALL MAPTRN(10.*(YY-1.)+1.E-6*VV,10.*(XX-1.)+1.E-6*UU/CFCT,X2,Y2)
U=((X2-X1)/SQRT((X2-X1)**2+(Y2-Y1)**2))*SQRT(UU**2+VV**2)
MXF=MX+IFIX(SFX*U)
RETURN
END

FUNCTION MYF(XX,YY,UU,VV,SFX,SFY,MX,MY)
CFCT=COS(.17453292519943*(YY-1.))
CALL MAPTRN(10.*(YY-1.)         ,10.*(XX-1.)              ,X1,Y1)
CALL MAPTRN(10.*(YY-1.)+1.E-6*VV,10.*(XX-1.)+1.E-6*UU/CFCT,X2,Y2)
V=((Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2))*SQRT(UU**2+VV**2)
MYF=MY+IFIX(SFY*V)
RETURN
END
.ef
.sp
The basic notion behind the coding of the MXF and MYF functions is as
follows.  Since UU and VV are the longitudinal and latitudinal components,
respectively, of a velocity vector having units of distance over time,
1.E-6*UU/COS(latitude) and 1.E-6*VV represent the change in longitude
and latitude, respectively, of a particle moving with the flow field
for a very short period of time.  The routine MAPTRN is used to find
the position of the particle's projection at the beginning and end of
that tiny time slice and, therefore, the direction in which to draw
the arrow representing the velocity vector so that it will be tangent
to a projected flow line of the field at that point.  The values U
and V are computed so as to give the arrow the length implied by UU
and VV.  (The code ensures that SQRT(U**2+V**2) is equal to
SQRT(UU**2+VV**2).)  The length of the arrow represents the magnitude
of the velocity vector, unaffected by perspective.  The scaling set
up by VELVCT will therefore be appropriate for the arrows drawn.

This method is rather heuristic and has three inherent problems.
First, the constant 1.E-6 may need to be made larger or smaller,
depending on the magnitude of your U/V data.  Second, the north and
south poles must be avoided.  At either pole, CFCT goes to zero,
giving a division by zero; in a small region near the pole, the
method may try to use MAPTRN with a latitude outside the range
(-90,+90).  Third, the projection must be set up so as to avoid
having vector base points at the exact edge of the map.  Vectors
there will be of the correct length, but they may be drawn in the
wrong direction (when the projected particle track determining the
direction crosses the edge and reappears elsewhere on the map).
With a little care, the desired results may be obtained.

