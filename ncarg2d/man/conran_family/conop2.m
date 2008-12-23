.TH CONOP2 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CONOP2 - sets option switches and specifys various INTEGER parameters
to be used by the Conran_family utility.
.SH SYNOPSIS
CALL CONOP2 (IOPT, IVAL)
.SH DESCRIPTION
.IP IOPT 12
Character, input -- Selects an internal parameter and sets an option
switch.  The possibilities are:
.sp
.nf
\& 'INT=OFF', 'INT=ALL', 'INT=MAJ', 'INT=MIN', 'INT=LAB', 'INT=DAT'
\& 'LSZ=ON' or 'LSZ=OFF'
\& 'NCP=ON' or 'NCP=OFF'
\& 'SML=ON' or 'SML=OFF'
\& 'SPD=ON' or 'SPD=OFF'
\& 'SSZ=ON' or 'SSZ=OFF'
\& 'STL=ON' or 'STL=OFF'
.fi
.IP IVAL 12
Integer, input -- Sets an internal parameter.
.sp
If IOPT is not 'OFF', then the IVAL argument is
used to override the default setting
of that option.
.sp
If IOPT is 'OFF', the IVAL argument can have any dummy value.
Option settings will be returned to their default values.
.sp
The following options are defined by this subroutine:
.IP INT 8
The parameter to determine the intensities of the contour
lines and other parts of the plot.  If
INT=OFF, all intensities are set to the default
values.  If INT=ALL, all intensities are set
to IVAL.  If INT is set to
one of the other possible options (MAJ, MIN,
LAB or DAT), the intensity level for that
option is set to IVAL.
.sp
If program set: CALL CONOP2('INT=OFF',0)
.sp
All the same:  CALL CONOP2('INT=ALL',IVAL)
.sp
Major lines:  CALL CONOP2('INT=MAJ',IVAL)
.sp
Minor lines:  CALL CONOP2('INT=MIN',IVAL)
.sp
Title and message:  CALL CONOP2('INT=LAB',IVAL)
.sp
Data values:  CALL CONOP2('INT=DAT',IVAL)
.sp
Note: 'INT=DAT' relates to the plotted data
values and the plotted maximums and minimums.
.sp
Note: IVAL is the intensity desired.  IVAL
values range from 0 to 255.
.sp
Example:   CALL CONOP2('INT=ALL',110)
.IP LSZ 8
This parameter determines the label size.
(Does not apply to entry CONRAQ.)
The value of LSZ is the desired
length in NDCs multiplied by 1023.
If LSZ=ON, the label size is set to IVAL.
If LSZ=OFF, the default label size is 9./1023.
NDCs.
.sp
Example: CALL CONOP2('LSZ=ON',12)
.IP NCP 8
The parameter to indicate the number of data points
used for the partial derivative
estimation.  If NCP=OFF, IVAL is set to
4, which is the default value.  If NCP=ON,
the user must specify IVAL greater than or
equal to 2.
.sp
Example:  CALL CONOP2('NCP=ON',2)
.sp
Note: IVAL = number of data points used for
estimation.  Changing this value affects the
contours produced and the size of input array
IWK.
.IP SML 8
The parameter to determine the size of minimum and
maximum contour labels in integer scaled NDC units.
(Does not apply to entry CONRAQ.)
The value of SML is the desired
length in NDCs multiplied by 1023.
If SML=OFF, the default label size is
15./1023. NDCs.
If SML=ON, the user must specify IVAL.
.sp
Example: CALL CONOP2('SML=ON',12)
.IP SPD 8
The parameter for the size of the plotted input data
values in integer scaled NDC units.
If SPD=OFF, the default label size is
8./1023. NDCs.
If SPD=ON, the user must specify IVAL.
.sp
Example: CALL CONOP2('SPD=ON',12)
.IP SSZ 8
The parameter to determine the resolution (number of
steps in each direction).  If SSZ=ON, the
user sets IVAL, or, if SSZ=OFF, the program
will automatically set IVAL to the default
value of 40.
.sp
Warning:  The scratch array (7th argument in the
CONRAN call) must be dimensioned IVAL**2 or larger.
.sp
.nf
Example:

      DIMENSION SCR(10000)
      CALL CONOP2('SSZ=ON',100)
      CALL CONRAN(XD,YD,ZD,NDP,WK,IWK,SCR)
.fi
.sp
Note: IVAL is an integer specifying the density
of the virtual grid.  In most cases, the default
value of 40 produces pleasing contours.  For
coarser but quicker contours, lower the
value.  For smoother contours at
the expense of taking a longer time, raise
the value.
.sp
Warning:  For step sizes greater
than 200 in CONRAN, the arrays PV in common block
CONRA1 and ITLOC in common block CONRA9, must be
expanded to about 10 more than IVAL.
.IP STL 8
The parameter to determine the character size of
the main title in integer scaled NDC units.
If STL=OFF, the default label size is
16./1023. NDCs.
If STL=ON, the user must specify IVAL.
.sp
Example: CALL CONOP2('STL=ON',14)
.sp
Note: When 30 or 40 characters are used for
the title, the default size of 16 integer scaled
NDC units works well.  For longer titles,
a smaller title size is required.
.SH USAGE
CONOP2 is called to set parameters of type INTEGER before
CONRAN, CONRAQ, or CONRAS are called to generate the contour plot.
.SH EXAMPLES
Use the command ncargex to see the following examples: tconaq,
tconan, and tconas.
.SH ACCESS
To use CONOP2 load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.
.SH MESSAGES
See the conran_family man page for a description of all Conran_family error
messages and/or informational messages.
.SH SEE ALSO
Online:
conran_family, conran_family_params, conran, conraq, conras, conop1, conop3,
conop4, conpack, conpack_params
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
