.TH Conpack 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Conpack - 
Provides a sort of tool
kit of FORTRAN subroutines that can be called in various
combinations to draw different kinds of contour plots.
.SH SYNOPSIS
CPBACK - Draws a background for a contour plot.
.sp
CPCICA -
Incorporates into a user's cell array color
indices determined by examining where the user's contours
lie relative to the cell array.
.sp
CPCLAM - Adds contour lines to an area map. This is part of the
process of drawing a solid-fill contour plot.
.sp
CPCLDM -
Draws contour lines masked by an existing area
map. The object of this may be simply to avoid drawing
contour lines through label boxes, but the routine may be
used for more complicated tasks, like limiting the drawing
of contour lines to the ocean areas on an Ezmap background.
.sp
CPCLDR - Draws contour lines.
.sp
CPCLTR -
Traces the contour lines at a given level and
retrieves them for some sort of user-defined processing.
.sp
CPCNRC - Simulates the behavior of the old routine CONREC; it
has the same arguments and produces similar output.
.sp
CPEZCT - Simulates the behavior of the old subroutine EZCNTR in
Conrec; it has the same arguments and will produce similar
output.
.sp
CPGETC - Retrieves the current value of an internal
parameter of type CHARACTER.
.sp
CPGETI - Retrieves the current value of an internal
parameter of type INTEGER.
.sp
CPGETR - Retrieves the current value of an internal
parameter of type REAL. 
.sp
CPLBAM -
Adds label boxes (for the
informational label, high and low labels, and contour-line
labels) to an area map. The ultimate object of this will
usually be to prevent contour lines drawn by CPCLDM from
passing through labels or to prevent fill of the label
boxes as contour bands are filled.
.sp
CPLBDR -
Draws labels (an informational label, high and
low labels, and line labels).
.sp
CPPKCL - Picks a set of contour levels.
.sp
CPPKLB - Picks a set of labels for labeled contour levels.
.sp
CPRECT - Initializes the contouring of a rectangular array
of data.
.sp
CPRSET - Resets all parameters to their initial default
values.
.sp
CPSETC - Sets the value of an internal parameter of
type CHARACTER.
.sp
CPSETI -
Sets the value of an internal parameter of
type INTEGER.
.sp
CPSETR -
Sets the value of an internal parameter of
type REAL.
.sp
CPSPS1 -
Interpolates from an array of data
on a "sparse" rectangular grid which is regularly spaced in
X and Y to an array of data on a "dense" rectangular grid
and to initialize contouring from the array on the dense
grid. (By a "sparse" grid is meant one whose dimensions are
smaller than one would like, so that contour lines
constructed directly on it are composed of long straight
segments.) CPSPS1 may be viewed as a data smoothing routine.
.sp
CPSPS2 -
Interpolates from an array of data
on a "sparse" rectangular grid which is irregularly spaced
in X and Y to an array of data on a "dense" rectangular
grid and to initialize contouring from the array on the
dense grid. (By a "sparse" grid is meant one whose
dimensions are smaller than one would like, so that contour
lines constructed directly on it are composed of long
straight segments.) CPSPS2 may be viewed as a data
smoothing routine.
.SH C-BINDING SYNOPSIS
c_cpback
.br
c_cpcica
.br
c_cpclam
.br
c_cpcldm
.br
c_cpcldr
.br
c_cpcltr
.br
c_cpcnrc
.br
c_cpezct
.br
c_cpgetc
.br
c_cpgeti
.br
c_cpgetr
.br
c_cplbam
.br
c_cplbdr
.br
c_cppkcl
.br
c_cppklb
.br
c_cprect
.br
c_cprset
.br
c_cpsetc
.br
c_cpseti
.br
c_cpsetr
.br
c_cpsps1
.br
c_cpsps2
.SH USER-MODIFIABLE INTERNAL ROUTINES
CPCHCF -
Provides user control as a constant-field
message is drawn.
.sp
CPCHCL -
Provides user control as contour lines are
drawn.
.sp
CPCHHL -
Provides user control as high and low labels
are drawn.
.sp
CPCHIL - Provides user control as the informational label is
drawn.
.sp
CPCHLL - Provides user control as contour line labels are
drawn.
.sp
CPDRPL -
Provides a useful polyline-drawer for the
routine CPCLDM.
.sp
CPMPXY -
Maps Conpack output from a rectangular
coordinate system superimposed on the data grid to some
other coordinate system.
.sp
CPSCAE -
Updates a particular element of a user's cell array.	
.SH ACCESS 
To use Conpack, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use the C bindings, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
In this section are listed, in alphabetical order, all the
error messages that may be written by Conpack. Each error
message begins with the name of the Conpack routine in
which an error has been detected, followed by a dash,
followed by the error message itself. In every case but
two, these error messages are written by a call to the
error-handling support routine SETER, with a final argument
indicating that the error is fatal and that execution
should be terminated. The two exceptions are for CPGIWS and
CPGRWS; these will be pointed out. Each error message in
the list below is followed by a short description of the
condition which caused the error.
.IP "CPBACK - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPCFLB - ERROR EXIT FROM GQCLIP"
An attempt to get the current clipping state has resulted 
in an error. This probably indicates that GKS is in the 
wrong state.
.IP "CPCFLB - ERROR EXIT FROM GQFACI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPCFLB - ERROR EXIT FROM GQLWSC"
An attempt to get the current line width scale factor has 
resulted in an error. This probably indicates that GKS is 
in the wrong state.
.IP "CPCFLB - ERROR EXIT FROM GQPLCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPCFLB - ERROR EXIT FROM GQTXCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPCICA - CANNOT CONTINUE-CPMPXY DOES NOT DO INVERSE MAPPINGS"
To use the routine CPCICA, it is necessary that the routine 
CPMPXY be able to do inverse mappings. The user has 
supplied his/her own version of CPMPXY, but has not made it 
capable of doing the inverses.
.IP "CPCICA - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPCICA-ONE OF THE CORNER POINTS OF THE CELL ARRAY IS INCORRECT"
One of the points P and Q, which determine how the cell 
array is to be mapped onto the plotter frame, is outside 
the limits of the plotter frame.
.IP "CPCICA - THE DIMENSIONS OF THE CELL ARRAY ARE INCORRECT"
Either the specified first dimension of the FORTRAN array 
in which the cell array is stored is less than or equal to 
zero or one of the specified dimensions of the cell array 
itself is less than or equal to zero or the specified first 
dimension of the cell array is larger than the specified 
first dimension of the FORTRAN array in which it is stored.
.IP "CPCLAM - CONTRADICTORY AREA-IDENTIFIER INFORMATION"
The contour-level list has more than one entry for a given 
level (which is fine) but either the values of 'AIA' for 
that level or the values of 'AIB' for that level are 
inconsistent.
.IP "CPCLAM - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPCLDM - ERROR EXIT FROM GQLWSC"
An attempt to get the current line width scale factor has 
resulted in an error. This probably indicates that GKS is 
in the wrong state.
.IP "CPCLDM - ERROR EXIT FROM GQPLCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPCLDM - ERROR EXIT FROM GQTXCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPCLDM - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPCLDR - ERROR EXIT FROM GQLWSC"
An attempt to get the current line width scale factor has 
resulted in an error. This probably indicates that GKS is 
in the wrong state.
.IP "CPCLDR - ERROR EXIT FROM GQPLCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPCLDR - ERROR EXIT FROM GQTXCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPCLDR - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPCLTR - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPGETC - GETTING x - PAI INCORRECT"
An attempt has been made to get an element of the parameter 
array named 'x' and the current value of 'PAI' (the 
"parameter array index") is inappropriate for that 
parameter array.
.IP "CPGETC - PARAMETER NAME NOT KNOWN - x"
The given parameter name ('x') is not one of the legal 
parameter names.
.IP "CPGETC - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "CPGETI OR CPGETR - GETTING x - PAI INCORRECT"
An attempt has been made to get an element of the parameter 
array named 'x' and the current value of 'PAI' (the 
"parameter array index") is inappropriate for that 
parameter array.
.IP "CPGETI OR CPGETR - PARAMETER NAME NOT KNOWN - x"
The given parameter name ('x') is not one of the legal 
parameter names.
.IP "CPGETI OR CPGETR - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "CPGIWS - ARGUMENT ERROR - SEE SPECIALIST"
This probably indicates an error in the implementation of 
the package. See the Conpack specialist.
.IP "CPGIWS - INTEGER WORKSPACE OVERFLOW"
The parameter 'WSO' has a value indicating that integer 
workspace overflow should be treated as a fatal error, and 
such an overflow has occurred.
.IP "CPGIWS m WORDS REQUESTED n WORDS AVAILABLE"
This is the one of the two error messages which are not 
necessarily fatal. The values "m" and "n" indicate how much 
additional integer workspace would be needed to continue 
executing the current Conpack routine and how much 
additional integer workspace is currently available. Note 
that supplying "m" words on a subsequent attempt will get 
you past this point, but will not ensure immunity from a 
subsequent failure.
.IP "CPGRWS - ARGUMENT ERROR - SEE SPECIALIST"
This probably indicates an error in the implementation of 
the package. See the Conpack specialist.
.IP "CPGRWS - REAL WORKSPACE OVERFLOW"
The parameter 'WSO' has a value indicating that real 
workspace overflow should be treated as a fatal error, and 
such an overflow has occurred.
.IP "CPGRWS m WORDS REQUESTED n WORDS AVAILABLE"
This is the one of the two error messages which are not 
necessarily fatal. The values "m" and "n" indicate how much 
additional real workspace would be needed to continue 
executing the current Conpack routine and how much 
additional real workspace is currently available. Note that 
supplying "m" words on a subsequent attempt will get you 
past this point, but will not ensure immunity from a 
subsequent failure.
.IP "CPHLLB - ERROR EXIT FROM GQFACI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPHLLB - ERROR EXIT FROM GQLWSC"
An attempt to get the current line width scale factor has 
resulted in an error. This probably indicates that GKS is 
in the wrong state.
.IP "CPHLLB - ERROR EXIT FROM GQPLCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPHLLB - ERROR EXIT FROM GQTXCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPINLB - ERROR EXIT FROM GQCLIP"
An attempt to get the current clipping state has resulted 
in an error. This probably indicates that GKS is in the 
wrong state.
.IP "CPINLB - ERROR EXIT FROM GQFACI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPINLB - ERROR EXIT FROM GQLWSC"
An attempt to get the current line width scale factor has 
resulted in an error. This probably indicates that GKS is 
in the wrong state.
.IP "CPINLB - ERROR EXIT FROM GQPLCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPINLB - ERROR EXIT FROM GQTXCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPLBAM - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPLBDR - ERROR EXIT FROM GQCLIP"
An attempt to get the current clipping state has resulted 
in an error. This probably indicates that GKS is in the 
wrong state.
.IP "CPLBDR - ERROR EXIT FROM GQFACI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPLBDR - ERROR EXIT FROM GQLWSC"
An attempt to get the current line width scale factor has 
resulted in an error. This probably indicates that GKS is 
in the wrong state.
.IP "CPLBDR - ERROR EXIT FROM GQPLCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPLBDR - ERROR EXIT FROM GQTXCI"
An attempt to get a color index has resulted in an error. 
This probably indicates that GKS is in the wrong state.
.IP "CPLBDR - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPPKCL - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPPKCL - TOO MANY CONTOUR LEVELS"
Indicates that the parameter 'CLS' has been given a 
negative value whose absolute value is too large, 
requesting more than 256 contour levels.
.IP "CPPKLB - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPPKLP - INITIALIZATION CALL NOT DONE"
A call to CPRECT, CPSPS1, or CPSPS2 has been omitted.
.IP "CPSETC - PARAMETER NAME NOT KNOWN - x"
The given parameter name ('x') is not one of the legal 
parameter names.
.IP "CPSETC - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "CPSETC - SETTING x - PAI INCORRECT"
An attempt has been made to set an element of the parameter 
array named 'x' and the current value of 'PAI' (the 
"parameter array index") is inappropriate for that 
parameter array.
.IP "CPSETI OR CPSETR - NCL LESS THAN 1 OR GREATER THAN n"
An attempt has been made to set the number of contour 
levels to an illegal value. The value of "n" is the largest 
value which may be used.
.IP "CPSETI OR CPSETR - PARAMETER NAME NOT KNOWN - x"
The given parameter name ('x') is not one of the legal 
parameter names.
.IP "CPSETI OR CPSETR - PARAMETER NAME TOO SHORT - x"
The parameter name ("x") is less than three characters long.
.IP "CPSETI OR CPSETR - SETTING x - PAI INCORRECT"
An attempt has been made to set an element of the parameter 
array named 'x' and the current value of 'PAI' (the 
"parameter array index") is inappropriate for that 
parameter array.
.IP "CPSPS1 - CANNOT CONTINUE WITHOUT WORKSPACE"
Insufficient workspace has been provided for the execution 
of CPSPS1. The amount of space which it requires is 
entirely predictable. 
.IP "CPSPS1 - IZD1, IZDM, OR IZDN SET INCORRECTLY"
IZDS has been given a zero value, indicating that the user 
intends to supply the values of IZD1, IZDM, and IZDN, and 
their current values are incorrect.
.IP "CPSPS1 - SPECIAL-VALUE REPLACEMENT FAILURE"
There are no two adjacent values in the sparse array which 
are not special values. Most likely, the entire sparse 
array is filled with special values.
.IP "CPSPS2 - CANNOT CONTINUE WITHOUT WORKSPACE"
Insufficient workspace has been provided for the execution 
of CPSPS1. The amount of space which it requires is 
entirely predictable. 
.IP "CPSPS2 - ERROR IN CALL TO MSSRF1"
A non-zero error return has occurred from the Fitpack 
routine SURF1. This probably means that the X and Y 
coordinate arrays are not in strictly increasing numerical 
order.
.IP "CPSPS2 - IZD1, IZDM, OR IZDN SET INCORRECTLY"
IZDS has been given a zero value, indicating that the user 
intends to supply the values of IZD1, IZDM, and IZDN, and 
their current values are incorrect.
.IP "CPSPS2 - SPECIAL-VALUE REPLACEMENT FAILURE"
There are no two adjacent values in the sparse array which 
are not special values. Most likely, the entire sparse 
array is filled with special values.
.IP "CPTRES - ALGORITHM FAILURE - SEE SPECIALIST"
An error has occurred while trying to trace the edge of a 
special-value area. This should not be possible. See the 
Conpack specialist.
.SH SEE ALSO
Online: 
conpack_params, 
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cppkcl, cppklb, cprect, cprset, cpscae, cpsetc, cpseti,
cpsetr, cpsprs, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
