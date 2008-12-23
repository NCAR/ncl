.TH CPMPXY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPMPXY - 
Maps Conpack output from a rectangular
coordinate system superimposed on the data grid to some
other coordinate system.
.SH SYNOPSIS
CALL CPMPXY (IMAP, XINP, YINP, XOTP, YOTP)
.SH DESCRIPTION 
.IP IMAP 12
(INTEGER, input) is zero if the object of the call is 
to ask CPMPXY about its mapping capabilities, greater than 
zero if the object of the call is to do a forward mapping, 
and less than zero if the object of the call is to do an 
inverse mapping. When IMAP is non-zero, its absolute value 
matches the current value of the parameter 'MAP' and 
identifies the mapping to be used.
.IP XINP 12
(REAL, input) is used in one of three ways:
.RS
.IP \(bu 4
When IMAP is zero, the value INT(XINP) is the index of a 
mapping about which CPMPXY is being asked to supply 
information.
.IP \(bu 4
When IMAP is greater than zero, XINP is the X coordinate of 
a point on the contour plot. If 'XC1' = 'XCM' (the default 
situation), then XINP will lie in the range from 1 to M, 
where M is the first dimension of the array being contoured 
(equal to the value of the parameter 'ZDM'); in this case, 
the X coordinate will have the same range as the first 
index of the data array. If the user sets 'XC1' unequal to 
\&'XCM', then XINP will lie in the range from 'XC1' 
(corresponding to an index value of 1) to 'XCM' 
(corresponding to an index value of M).
.IP \(bu
When IMAP is less than zero, XINP is the X coordinate of a 
point on the contour plot, in a coordinate system 
consistent with the current window, as specified by 
arguments 5 through 8 of the last call to the SPPS routine 
SET or by the equivalent call to GKS.
.RE
.IP YINP 12
(REAL, input/output) is used in one of three ways:
.RS
.IP \(bu
When IMAP is zero, CPMPXY is expected to return one of the 
following values of YINP: YINP = 0. indicates that neither 
the forward nor the inverse transformation is defined. YINP 
= 1. indicates that the forward transformation is defined, 
but the inverse is not. YINP = 2. indicates that the 
forward transformation is not defined, but the inverse is. 
YINP = 3. indicates that both the forward and the inverse 
transformations are defined.
.IP \(bu
When IMAP is greater than zero, YINP is the Y coordinate of 
a point on the contour plot. If 'YC1' = 'YCN' (the default 
situation), then YINP will lie in the range from 1 to N, 
where N is the second dimension of the array being 
contoured (equal to the value of the parameter 'ZDN'); in 
this case, the Y coordinate will have the same range as the 
second index of the data array. If the user sets 'YC1' 
unequal to 'YCN', then YINP will lie in the range from 
\&'YC1' (corresponding to an index value of 1) to 'YCN' 
(corresponding to an index value of N).
.IP \(bu
When IMAP is less than zero, YINP is the Y coordinate of a 
point on the contour plot, in a coordinate system 
consistent with the current window, as specified by 
arguments 5 through 8 of the last call to the SPPS routine 
SET or by the equivalent call to GKS.
.RE
.IP "XOTP and YOTP" 12
(REAL, output) are used in one 
of two ways:
.RS
.IP \(bu
If IMAP is greater than zero, XOTP and YOTP are the X and Y 
coordinates of a point on the contour plot, in a coordinate 
system consistent with the current window, as specified by 
arguments 5 through 8 of the last call to the SPPS routine 
SET or by the equivalent call to GKS.
.IP \(bu
When IMAP is less than zero, XOTP and YOTP are the X and Y 
coordinates of a point on the contour plot. If 'XC1' = 
\&'XCM' (the default situation), then XOTP will lie in the 
range from 1 to M, where M is the first dimension of the 
array being contoured (equal to the value of the parameter 
\&'ZDM'); in this case, the X coordinate will have the same 
range as the first index of the data array. If the user 
sets 'XC1' unequal to 'XCM', then XOTP will lie in the 
range from 'XC1' (corresponding to an index value of 1) to 
\&'XCM' (corresponding to an index value of M). Similarly, if 
\&'YC1' = 'YCN' (the default situation), then YOTP will lie 
in the range from 1 to N, where N is the second dimension 
of the array being contoured (equal to the value of the 
parameter 'ZDN'); in this case, the Y coordinate will have 
the same range as the second index of the data array. If 
the user sets 'YC1' unequal to 'YCN', then YOTP will lie in 
the range from 'YC1' (corresponding to an index value of 1) 
to 'YCN' (corresponding to an index value of N).
.sp
In any case, if the point (XINP,YINP) cannot be mapped for 
any reason, some recognizable impossible value should be 
returned for both of XOTP and YOTP and the internal 
parameter 'ORV' should be given that value, thereby 
allowing Conpack routines that call CPMPXY to determine 
whether or not a point being projected is visible or not. 
The value used for this purpose by the Ezmap routines 
MAPTRA and MAPTRI is 1.E12.
.SH USAGE
CPMPXY is not to be called by the user. It is called by 
Conpack when the parameter 'MAP' is non-zero. Each call is 
intended 1) to inquire whether a given mapping is defined 
by CPMPXY, or 2) to map the X and Y coordinates of a single 
point, whose position is known relative to the data grid, 
to X and Y coordinates in some other coordinate system or 
3) (as of version 3.1.3) to do the inverse mapping. The 
default version of CPMPXY is as follows:
.sp
.RS 5
.nf
SUBROUTINE CPMPXY (IMAP,XINP,YINP,XOTP,YOTP)
  IF (IMAP.EQ.0) THEN
    IF (INT(XINP).GE.1.AND.INT(XINP).LE.3) THEN
      YINP=3.
    ELSE
      YINP=0.
    END IF
  ELSE IF (ABS(IMAP).EQ.1) THEN
    IF (IMAP.GT.0) THEN
      CALL MAPTRA (YINP,XINP,XOTP,YOTP)
    ELSE
      CALL MAPTRI (XINP,YINP,YOTP,XOTP)
    END IF
  ELSE IF (ABS(IMAP).EQ.2) THEN
    IF (IMAP.GT.0) THEN
      XOTP=XINP*COS(.017453292519943*YINP)
      YOTP=XINP*SIN(.017453292519943*YINP)
    ELSE
      XOTP=SQRT(XINP*XINP+YINP*YINP)
      YOTP=57.2957795130823*ATAN2(YINP,XINP)
    END IF
  ELSE
    XOTP=XINP
    YOTP=YINP
  END IF
  RETURN
END
.RE
.sp
When CPMPXY is called with IMAP = 0, it assumes it is being 
asked to return information about its mapping capabilities. 
XINP is assumed to have been given the value REAL(I), where 
I is the index of a mapping about which information is 
desired. CPMPXY sets YINP to indicate whether the mapping 
selected by I is implemented or not and whether its inverse 
is implemented or not. In the case of the default version 
of CPMPXY, mappings 1 through 3 are completely implemented 
(both forward and reverse), so a "3." is returned as the 
value of YINP; other mappings are not implemented at all, 
so a "0." is returned as the value of YINP.
.sp
When CPMPXY is called with IMAP = 1, the incoming X and Y 
coordinates are assumed to represent longitude and 
latitude, respectively; the Ezmap routine MAPTRA is called 
to find the X and Y coordinates of the projection of the 
specified point on the globe, and those coordinates are 
returned as the outgoing X and Y coordinates. When IMAP = 
-1, the incoming X and Y coordinates are assumed to be the X 
and Y coordinates of a projected point; the Ezmap routine 
MAPTRI is called to find the longitude and latitude of the 
original point on the globe, and those values are returned 
as the outgoing X and Y coordinates.
.sp
When IMAP = 2, the incoming X and Y coordinates are assumed 
to represent rho and theta (in degrees) in polar 
coordinates; from these are computed the output X and Y 
coordinates. When IMAP = -2, the incoming X and Y 
coordinates are used to compute rho and theta and those 
values are returned.
.sp
If IMAP is anything else, the input X and Y coordinates are 
simply returned as the output X and Y coordinates.
.sp
A user version of CPMPXY can be made to do any desired 
mapping. It should also be made, when IMAP = 0, to return 
correct information about its own capabilities.
.SH ACCESS
To use CPMPXY, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae, cpsetc,
cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
