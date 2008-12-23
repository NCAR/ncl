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
CPCNRC - Draws black and white contours with a single call. 
Simulates the behavior of the old routine CONREC; it has the same
arguments and produces similar output.
.sp
CPEZCT - Draws black and white contours with a single call.
Simulates the behavior of the old subroutine EZCNTR in
Conrec_family; it has the same arguments and will produce similar
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
CPMVIW -
Moves the contents of an old integer workspace to a new one.
.sp
CPMVRW -
Moves the contents of an old real workspace to a new one.
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
To use the Conpack Fortran or C routines, load the NCAR Graphics
libraries ncarg, ncarg_gks, and ncarg_c, preferably in that order.
.SH MESSAGES
Various error conditions can occur in Conpack.  Each of these results in
a call to the error-handling routine SETER, with a final argument indicating
that the error is recoverable; by default, an error message is printed and
execution is terminated, but, if you turn on error recovery
(as described in the "man" page for "error_handling"), you
can get control back.
.sp
The error messages are as follows:
.sp
.in +5
CPBACK - INITIALIZATION CALL NOT DONE
.br
CPBACK - UNCLEARED PRIOR ERROR
.br
CPCFLB - ERROR EXIT FROM GQCLIP
.br
CPCFLB - ERROR EXIT FROM GQFACI
.br
CPCFLB - ERROR EXIT FROM GQLWSC
.br
CPCFLB - ERROR EXIT FROM GQPLCI
.br
CPCFLB - ERROR EXIT FROM GQTXCI
.br
CPCICA - CANNOT CONTINUE - CPMPXY DOES NOT DO INVERSE MAPPINGS
.br
CPCICA - INITIALIZATION CALL NOT DONE
.br
CPCICA - ONE OF THE CORNER POINTS OF THE CELL ARRAY IS INCORRECT
.br
CPCICA - THE DIMENSIONS OF THE CELL ARRAY ARE INCORRECT
.br
CPCICA - UNCLEARED PRIOR ERROR
.br
CPCLAM - CONTRADICTORY AREA-IDENTIFIER INFORMATION
.br
CPCLAM - INITIALIZATION CALL NOT DONE
.br
CPCLAM - UNCLEARED PRIOR ERROR
.br
CPCLDM - ERROR EXIT FROM GQLWSC
.br
CPCLDM - ERROR EXIT FROM GQPLCI
.br
CPCLDM - ERROR EXIT FROM GQTXCI
.br
CPCLDM - INITIALIZATION CALL NOT DONE
.br
CPCLDM - UNCLEARED PRIOR ERROR
.br
CPCLDR - ERROR EXIT FROM GQLWSC
.br
CPCLDR - ERROR EXIT FROM GQPLCI
.br
CPCLDR - ERROR EXIT FROM GQTXCI
.br
CPCLDR - INITIALIZATION CALL NOT DONE
.br
CPCLDR - UNCLEARED PRIOR ERROR
.br
CPCLTR - INITIALIZATION CALL NOT DONE
.br
CPCLTR - UNCLEARED PRIOR ERROR
.br
CPCNRC - UNCLEARED PRIOR ERROR
.br
CPEZCT - UNCLEARED PRIOR ERROR
.br
CPGETC - GETTING X - PAI INCORRECT
.br
CPGETC - PARAMETER NAME NOT KNOWN - X
.br
CPGETC - PARAMETER NAME TOO SHORT - X
.br
CPGETC - UNCLEARED PRIOR ERROR
.br
CPGETI - UNCLEARED PRIOR ERROR
.br
CPGETR - GETTING X - PAI INCORRECT
.br
CPGETR - PARAMETER NAME NOT KNOWN - X
.br
CPGETR - PARAMETER NAME TOO SHORT - X
.br
CPGETR - UNCLEARED PRIOR ERROR
.br
CPGIWS - ARGUMENT ERROR - SEE SPECIALIST
.br
CPGIWS - INTEGER WORKSPACE OVERFLOW
.br
CPGRWS - ARGUMENT ERROR - SEE SPECIALIST
.br
CPGRWS - REAL WORKSPACE OVERFLOW
.br
CPHLLB - ERROR EXIT FROM GQFACI
.br
CPHLLB - ERROR EXIT FROM GQLWSC
.br
CPHLLB - ERROR EXIT FROM GQPLCI
.br
CPHLLB - ERROR EXIT FROM GQTXCI
.br
CPINLB - ERROR EXIT FROM GQCLIP
.br
CPINLB - ERROR EXIT FROM GQFACI
.br
CPINLB - ERROR EXIT FROM GQLWSC
.br
CPINLB - ERROR EXIT FROM GQPLCI
.br
CPINLB - ERROR EXIT FROM GQTXCI
.br
CPLBAM - INITIALIZATION CALL NOT DONE
.br
CPLBAM - UNCLEARED PRIOR ERROR
.br
CPLBDR - ERROR EXIT FROM GQCLIP
.br
CPLBDR - ERROR EXIT FROM GQFACI
.br
CPLBDR - ERROR EXIT FROM GQLWSC
.br
CPLBDR - ERROR EXIT FROM GQPLCI
.br
CPLBDR - ERROR EXIT FROM GQTXCI
.br
CPLBDR - INITIALIZATION CALL NOT DONE
.br
CPLBDR - UNCLEARED PRIOR ERROR
.br
CPMVIW - NEW WORKSPACE ARRAY IS TOO SMALL
.br
CPMVIW - UNCLEARED PRIOR ERROR
.br
CPMVRW - NEW WORKSPACE ARRAY IS TOO SMALL
.br
CPMVRW - UNCLEARED PRIOR ERROR
.br
CPPKCL - INITIALIZATION CALL NOT DONE
.br
CPPKCL - TOO MANY CONTOUR LEVELS
.br
CPPKCL - UNCLEARED PRIOR ERROR
.br
CPPKLB - INITIALIZATION CALL NOT DONE
.br
CPPKLB - UNCLEARED PRIOR ERROR
.br
CPPKLP - INITIALIZATION CALL NOT DONE
.br
CPPKLP - UNCLEARED PRIOR ERROR
.br
CPRECT - UNCLEARED PRIOR ERROR
.br
CPRSET - UNCLEARED PRIOR ERROR
.br
CPSETC - PARAMETER NAME NOT KNOWN - X
.br
CPSETC - PARAMETER NAME TOO SHORT - X
.br
CPSETC - SETTING X - PAI INCORRECT
.br
CPSETC - UNCLEARED PRIOR ERROR
.br
CPSETI - UNCLEARED PRIOR ERROR
.br
CPSETR - NCL LESS THAN 1 OR GREATER THAN n
.br
CPSETR - PARAMETER NAME NOT KNOWN - X
.br
CPSETR - PARAMETER NAME TOO SHORT - X
.br
CPSETR - SETTING X - PAI INCORRECT
.br
CPSETR - UNCLEARED PRIOR ERROR
.br
CPSPRS - UNCLEARED PRIOR ERROR
.br
CPSPS1 - CANNOT CONTINUE WITHOUT WORKSPACE
.br
CPSPS1 - IZD1, IZDM, OR IZDN SET INCORRECTLY
.br
CPSPS1 - SPECIAL-VALUE REPLACEMENT FAILURE
.br
CPSPS1 - UNCLEARED PRIOR ERROR
.br
CPSPS2 - CANNOT CONTINUE WITHOUT WORKSPACE
.br
CPSPS2 - ERROR IN CALL TO MSSRF1
.br
CPSPS2 - IZD1, IZDM, OR IZDN SET INCORRECTLY
.br
CPSPS2 - SPECIAL-VALUE REPLACEMENT FAILURE
.br
CPSPS2 - UNCLEARED PRIOR ERROR
.br
CPTRES - ALGORITHM FAILURE - SEE SPECIALIST
.in -5
.sp
.SH SEE ALSO
Online: 
conpack_params, 
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsprs, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
