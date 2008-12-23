.TH CPCHHL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPCHHL - 
Called by Conpack routines as high/low labels are being drawn.  The default
version does nothing.  A user-written version may be supplied to provide
control as the labels are drawn.
.SH SYNOPSIS
CALL CPCHHL (IFLG)
.SH DESCRIPTION 
.IP IFLG 12
(INTEGER, input) is positive if an action is about to 
be taken, negative if an action has just been completed. 
The action in question is defined by the absolute value of 
IFLG, as follows:
.RS
.IP \(bu 4
The value 1 implies the determination of the size of the 
label for a high (by means of a call to PLCHHQ, in the 
package Plotchar, with ANGD = 360.).
.IP \(bu 4
The value 2 implies the filling of the box around the label 
for a high, which is done before the drawing of the label 
itself.
.IP \(bu 4
The value 3 implies the drawing of the label for a high (by 
means of a call to PLCHHQ, in the package Plotchar).
.IP \(bu 4
The value 4 implies the drawing of the box around the label 
for a high, which is done after the drawing of the label 
itself.
.IP \(bu 4
The value 5 implies the determination of the size of the 
label for a low (by means of a call to PLCHHQ, in the 
package Plotchar, with ANGD = 360.).
.IP \(bu 4
The value 6 implies the filling of the box around the label 
for a low, which is done before the drawing of the label 
itself.
.IP \(bu 4
The value 7 implies the drawing of the label for a low (by 
means of a call to PLCHHQ, in the package Plotchar).
.IP \(bu 4
The value 8 implies the drawing of the box around the label 
for a low, which is done after the drawing of the label 
itself.
.RE
.SH USAGE
The routine CPCHHL is not to be called by the user. It is
called several times by CPLBAM and/or CPLBDR while each
high or low label is positioned and drawn. The default
version of CPCHHL does nothing. A user-supplied replacement
may change attributes such as color and line width (by
calling the SPPS routine SETUSV or the appropriate GKS
routines). The text of the label may be retrieved by means
of a "CALL CPGETC ('CTM',CVAL)". The text of the label may
be changed by means of a "CALL CPSETC ('CTM',CVAL)"; this
should only be done during a call with IFLG = 1, 3, 5, or 7;
if it is done for one of the two values 5 and 7, it should also be done
for the other.  When CPCHHL is called, the parameter 'ZDV' will have
been set to the value of the high or low being labelled; its value
may be retrieved and used by CPCHHL.
.SH ACCESS
To use CPCHHL, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
conpack, 
cpback, cpchcf, cpchcl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
