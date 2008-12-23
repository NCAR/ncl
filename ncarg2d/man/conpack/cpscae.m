.TH CPSCAE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPSCAE -
Called repeatedly by the routine CPCICA as it executes. Updates a particular
element of a user's cell array.  The default version of CPSCAE may be
replaced by the user with a version that does something different.
.SH SYNOPSIS
 CALL CPSCAE (ICRA, ICA1, ICAM, ICAN, XCPF, YCPF, XCQF, 
.br
+ YCQF, IND1, IND2, ICAF, IAID)
.SH DESCRIPTION 
The first eight arguments of CPSCAE describe the user's 
cell array and are taken directly from the user's call to 
CPCICA:
.IP ICRA 12
(INTEGER array, dimensioned ICA1 by "n", where "n" is 
greater than or equal to the value of the argument ICAN, 
input/output) is the user's cell array. Generally speaking, 
a user version of CPSCAE should only reset the value of the 
single element ICRA(IND1,IND2).
.IP ICA1 12
(INTEGER, input) is the first dimension of the FORTRAN 
array ICRA, which contains the user's cell array.
.IP ICAM 12
(INTEGER, input) is the first dimension of the user's 
cell array.
.IP ICAN 12
(INTEGER, input) is the second dimension of the user's 
cell array.
.IP "XCPF and YCPF" 12
(REAL, input) are the coordinates, in the 
fractional coordinate system, of a point P. P is the point 
at that corner of the rectangular area into which the cell 
array maps that corresponds to the cell (1,1).
.IP "XCQF and YCQF" 12
(REAL, input) are the coordinates, in the 
fractional coordinate system, of a point Q. Q is the point 
at that corner of the rectangular area into which the cell 
array maps that corresponds to the cell (ICAM,ICAN).
.IP "IND1 and IND2" 12
(INTEGER, input) are the indices of an 
element in the cell array that is to be reset.
.IP ICAF 12
(INTEGER, input) is the value of the internal 
parameter 'CAF', which will be a negative integer (because, 
when 'CAF' is greater than or equal to zero, the routine 
CPSCAE is not called). Different values of 'CAF' may be 
used in a user version of CPSCAE to select different 
schemes for picking color indices.
.IP IAID 12
(INTEGER, input) is the value of the area identifier 
associated with the midpoint of the cell (IND1,IND2). The 
four possibilities are as described for the routine CPCICA.
.SH USAGE
CPSCAE is not called by the user. When a user calls the
routine CPCICA and the internal parameter 'CAF' is
negative, CPSCAE is called by CPCICA exactly once for each
cell in the cell array. The default version of CPSCAE just
sets the cell's color index equal to the area identifier
associated with the cell, if that area identifier is
greater than or equal to zero; otherwise, it does nothing.
A user-supplied version of CPSCAE may do something more
complicated; for example, it may take into account both the
value of the area identifier associated with the cell and
the current value of the color index for the cell in
determining the new value of the color index for the cell.
.SH ACCESS
To use CPSCAE, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpsetc,
cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
