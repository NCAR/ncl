.TH CPCNRC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPCNRC - Draws black and white contours with a single call. 
the behavior of the old routine CONREC; it has the same
arguments and produces similar output.
.SH SYNOPSIS
 CALL CPCNRC (ZDAT, KZDT, MZDT, NZDT, FLOW, FHGH, FINC, 
.br
+ NSET, NHGH, NDSH)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpcnrc (float *zdat, int kzdt, int mzdt, int nzdt, 
.br
float flow, float fhgh, float finc, int nset, int nhgh, 
.br
int ndsh)
.SH DESCRIPTION 
.IP ZDAT 12
(REAL array, dimensioned KZDT x n, where "n" is 
greater than or equal to NZDT, input) is the array 
containing the data to be contoured.
.IP KZDT 12
(INTEGER, input) is the first dimension of the FORTRAN 
array ZDAT.
.IP MZDT 12
(INTEGER, input) is the first dimension of the array 
of data in ZDAT. MZDT must be less than or equal to KZDT.
.IP NZDT 12
(INTEGER, input) is the second dimension of the array 
of data in ZDAT. NZDT must be less than or equal to the 
declared second dimension of the array ZDAT.
.IP FLOW 12
(REAL, input) is the desired lowest contour level. If 
FLOW is equal to or greater than FHGH, Conpack will choose 
the set of contour levels.
.IP FHGH 12
(REAL, input) is the desired highest contour level. If 
FHGH is equal to or less than FLOW, Conpack will choose the 
set of contour levels.
.IP FINC 12
(REAL, input) says how contour levels are to be 
chosen. There are two possibilities:
.RS
.IP \(bu 4
If FINC is greater than zero, it specifies the desired 
contour interval to be used. In this case, if FLOW is less 
than FHGH, the intervals used will be FLOW, FLOW+FINC, 
FLOW+2*FINC, ... FLOW+n*FINC, where "n" is the largest 
integer such that FLOW+n*FINC is less than or equal to 
FHGH. If FLOW is greater than or equal to FHGH, the contour 
levels will be those integer multiples of FINC which fall 
between the minimum value in ZDAT and the maximum value in 
ZDAT.
.IP \(bu 4
If FINC is less than or equal to zero, Conpack will choose 
the contour interval in such a way as to give at least 16 
contour levels (if FINC is zero) or MAX(1,INT(-FINC)) 
contour levels (if FINC is less than zero) between the 
minimum and maximum values in ZDAT. All the contour levels 
will be integer multiples of the chosen interval. If FLOW 
is less than FHGH, no contour lines will be drawn for 
chosen contour levels which are outside the range 
(FLOW,FHGH).
.RE
.IP NSET 12
(INTEGER, input) says how the contour plot is to be 
mapped onto the plotter frame. There are three basic 
possibilities:
.RS
.IP \(bu 4
If the value of NSET, modulo three, is zero, the "standard 
configuration" will be used. Conpack will be directed to 
call SET. The portion of the plotter frame bounded by the 
lines "X=.05", "X=.95", "Y=.05", and "Y=.95" (in the 
"fractional" or "normalized-device-coordinate" system) will 
be used. The shape of the plot will be determined by the 
values of the internal parameters 'WDL', 'WDR', 'WDB', and 
\&'WDT'; by default, the ratio of the plot's width to its 
height will be MZDT/NZDT. If the ratio of the width to the 
height is less than 1/4 or greater than 4, the plot will be 
made square. CPBACK will be called to draw a perimeter.
.IP \(bu 4
If the value of NSET, modulo three, is equivalent to minus 
one, the contour plot will fill the current viewport. 
Conpack will be directed to call SET. The portion of the 
plotter frame used will be that bounded by the lines 
"X=xl", "X=xr", "Y=yb", and "Y=yt", where "xl", "xr", "yl", 
and "yr" are obtained by means of a
.sp
.nf
	CALL GETSET (xl,xr,yl,yr,...)
.sp
.fi
The plot will fill this entire area. CPBACK will not be 
called to draw a perimeter.
.IP \(bu 4
If the value of NSET, modulo three, is equivalent to plus 
one, Conpack will be directed not to call SET. It will be 
assumed that the user has done the appropriate call. CPBACK 
will not be called to draw a perimeter. Use this option 
when overlaying CPCNRC output on an existing background 
(one drawn by Ezmap, for example).
.RE
.IP ""
If the value of NSET is -1, 0, or 1, CPCNRC will, prior to 
returning control to the caller, restore the SET call to 
what it was on entry (which is how CONREC works). If the 
value of NSET is less than -1 or greater than 1, CPCNRC 
will not so restore the SET call; this may be useful if 
other objects are to be drawn on the plot produced by 
CPCNRC.
.IP NHGH 12
(INTEGER, input) says whether highs and lows or data 
points are to be labeled. There are three possibilities:
.RS
.IP \(bu 4
If NHGH is zero, each high is marked with an "H" and each 
low is marked with an "L"; the value is written as a 
subscript of the "H" or "L".
.IP \(bu 4
If NHGH is greater than zero, each data point is marked 
with the value at that point. No attempt is made to cull 
overlapping values, so using this option when there are too 
many data points may result in a mess. The values of the 
Conpack parameters 'HLA' and 'HLS' are retrieved and used 
to determine the angle at which the values are written and 
the size of the characters used. They may be used to reduce 
the clutter somewhat.
.IP \(bu 4
If NHGH is less than zero, neither of the above is done.
.RE
.IP NDSH 12
(INTEGER, input) may be used to specify a 10-bit dash 
pattern to be used. (If ABS(NDSH) is 0, 1, or 1023, all 
solid lines are used.) If NDSH is greater than zero, the 
specified dash pattern is used for all contour lines; if 
NDSH is less than zero, the dash pattern is used only for 
negative-valued contour lines. Note: the 10-bit pattern is 
actually made into a 16-bit pattern by appending a copy of
the high-order six bits.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exceptions:
.sp
.IP zdat 12
Dimensioned n by kzdt, where n is greater than or equal to nzdt.
.IP kzdt 12
The second dimension of the array zdat.
.IP mzdt 12
The second dimension of the array of data in zdat.  mzdt
must be less than or equal to kzdt.
.IP nzdt 12
The first dimension of the array of data in zdat. nzdt
must be less than or equal to the declared first
dimension of the array zdat.
.SH USAGE
The appearance of the plot produced by CPCNRC may vary,
depending on the setting of internal parameters of Conpack.
The following should be noted:
.IP \(bu 4
By default, contour lines will be labeled using the old
CONREC scheme of appending the label for a line to the dash
pattern for the line. To use one of the new schemes, place a
.sp
CALL CPSETI ('LLP - LINE LABEL POSITIONING',n)
.sp
where n has the value 2 or 3, before the call to CPCNRC.
CPCNRC retrieves the value of 'LLP' and, if the value is a 2
or a 3, modifies the calls that it performs so as to properly
shield labels from having contour lines drawn through them.
.IP \(bu 4
By default, high-quality characters will be used in the
informational label, in high/low labels, in point-value labels,
and in contour-line labels written when 'LLP' is 2 or 3. To
use lower-quality characters, place a
.sp
CALL PCSETI ('QUALITY',1)
.sp
or a 
.sp
CALL PCSETI ('QUALITY',2)
.sp
prior to the call to CPCNRC.
.sp
Note that these calls are to subroutines that start with the
letters PC, because the routine you are calling is in the
utility Plotchar. (Conpack uses Plotchar to plot characters.)
.IP \(bu 4
If the output from CPCNRC is to be overlaid on an Ezmap
background and longitude is a linear function of the first
subscript of the data array and latitude is a linear function
of the second subscript of the data array, you can
.RS 10
.sp
Call Ezmap to draw the background.
.sp
Set the parameter 'MAP' to 1.
.sp
Set the parameters 'XC1', 'XCM', 'YC1', and 'YCN' to
specify the longitude and latitude ranges.  Note: 'XCM'
must be numerically larger than 'XC1'.  If, for
example, your data run from 175 degrees east of
Greenwich at the left edge to 160 degrees west of
Greenwich at the right edge (which is -160 degrees
east), then you should set 'XC1' = 175 and 'XCM' = 200
to achieve the proper mapping.
.sp
Call CPCNRC to draw the contour plot.
.RE
.IP \(bu 4
To smooth the contour lines in the same way that they would
have been smoothed by the "smooth" or "super" versions of
CONREC, insert
.sp
CALL CPSETR ('T2D - TENSION ON THE 2D SPLINES',t),
.sp
where t is the desired tension, to turn on the smoother.
.IP \(bu 4
By default, no scale factor is used. (Because Conpack can
write contour line labels in scientific notation, it was
considered less important to use a scale factor.) If you want
to use a scale factor, the value of the internal parameter
\&'SFS' should be set prior to calling CPCNRC.
.IP \(bu 4
CPCNRC contains a real workspace array, an integer workspace
array, and an area map, which is used to prevent contour
lines from passing through labels when 'LLP' has the value 2
or 3. The lengths of these arrays are 5000, 2000, and 12000,
respectively. If this proves insufficient, you must obtain
a copy of the code for CPCNRC, adjust the dimensions as
required, and compile it in place of the default version.
.IP \(bu 4
Certain internal parameters are used by CPCNRC; user calls to
reset them prior to the call to CPCNRC will therefore have no
effect.  These parameters are as follows:
.sp
\&'AIA' 'AIB' 'CIS' 'CIU' 'CLC' 'CLD'
\&'CLL' 'CLS' 'CLV' 'CLU' 'CMN' 'CMX'
\&'HLT' 'HIT' 'LOT' 'LLC' 'LLT' 'NCL'
\&'SET' 'VPB' 'VPL' 'VPR' 'VPS' 'VPT'
.IP \(bu 4
Subroutines with names of the form CPCHxx (the change
subroutines) will be of more value when used in conjunction
with CPCNRC than when used in conjunction with the more basic
Conpack subroutines.  For example, the only way to change the
color of the zero contour line drawn by a call to CPCNRC is
to supply your own version of the routine CPCHCL; there is no
way, short of modifying the code of CPCNRC, to use the
internal parameter that controls the color of the zero
contour line.
.IP \(bu 4
Like CONREC, CPCNRC does no call to FRAME. To advance the
frame, put a CALL FRAME after the call to CPCNRC.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpcnrc,
cpex09,
tconpa.
.SH ACCESS
To use CPCNRC or c_cpcnrc, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
