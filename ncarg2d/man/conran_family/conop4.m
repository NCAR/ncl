.TH CONOP4 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CONOP4 - sets option switches and specifys various CHARACTER parameters
to be used by the Conran_family utility.
.SH SYNOPSIS
CONOP4 (IOPT,CHARS,NUMC,IFMT)
.SH DESCRIPTION
.IP IOPT 12
Character, input -- Selects an internal parameter and sets an option
switch.  The possibilities are:
.sp
  'DAS=ON', 'DAS=OFF', 'DAS=GTR', 'DAS=EQU', 'DAS=LSS', and 'DAS=ALL'
.br
  'FMT=ON' or 'FMT=OFF'
.br
  'TLE=ON' or 'TLE=OFF'
.sp
If the value of an IOPT option is 'OFF', then the next
three arguments of the CONOP4 call, CHARS, NUMC, and IFMT
can be given any dummy value.
Option settings will be returned to their default values.
.sp
If an option is other than 'OFF', then the CHARS, NUMC,
and IFMT arguments can be used to override the default
settings of that option.
.IP CHARS 12
Character, input -- A string of up to 64 characters.
.IP NUMC 12
Integer, input -- The number of characters in string CHARS.
.IP IFMT 12
Integer, input -- The maximum number of digits which can be printed by
the output format loaded into CHARS when IOPT='FMT=ON'.  IFMT is not
used with any other value of IOPT.
.sp
The following options are defined by this subroutine:
.IP DAS 8
This parameter determines which contours are
represented by dashed lines.  The dashed line
pattern is set in argument CHARS.  Arguments NUMC and IFMT
are not used.  They can assume any dummy value.
.sp
If IOPT='DAS=OFF', only
solid lines are used.  This is the default.
.sp
All solid lines: CALL CONOP4('DAS=OFF',' ',0,0)
.sp
To assign a specified line pattern to all contours:
.sp
CALL CONOP4('DAS=ALL',CHARS,0,0)
.sp
To assign a specified line pattern to selected contours:
.sp
Contour > DBP:  CALL CONOP4('DAS=GTR',CHARS,0,0)
.sp
Contour = DBP:  CALL CONOP4('DAS=EQU',CHARS,0,0)
.sp
Contour < DBP:  CALL CONOP4('DAS=LSS',CHARS,0,0)
.sp
The breakpoint for changing the line pattern, parameter DBP,
is set by a call to entry CONOP3.  DBP = 0. is the default.
See parameter DBP in "man conop3".
.sp
If DAS has a value other than OFF, CHARS is a dashed line pattern
described by a ten character
string with a dollar sign ($) for solid and a
single quote (') for blank.  Recall that in
FORTRAN 77, in a quoted string a single quote
is represented by two single quotes ('').
.sp
Example:  CALL CONOP4('DAS=GTR','$$$$$''$$$$',0,0)
.IP FMT 8
A parameter which specifies a format for output of
the data values.
.sp
If FMT=ON, the user must specify values for
CHARS, NUMC, and IFMT.
CHARS is an output print format.  It has beginning and ending
parenthesis as in (F8.2).
NUMC is the number of characters in CHARS.  It is 6 for (F8.2).
IFMT is the maximum number of digits which can be printed by this
format.  It is 8 for an F8.2.
.sp
.nf
Example:
CHARS = '(F8.2)'
NUMC  = 6
IFMT = 8
CALL CONOP4('FMT=ON',CHARS,NUMC,IFMT)
.fi
.sp
Warning:  IFMT cannot be greater than 10;
however, CONRAN will not test for a valid
format.  Thus, a format which results in IFMT > 10
will cause an undetected error.
.sp
If FMT=OFF, the default values are:
CHARS='(G10.3)', NUMC=7, and IFMT=10, respectively.
.sp
To return to default settings use:
.br
CALL CONOP4('FMT=OFF',' ',0,0)
.IP TLE 8
The parameter to place a title at the top of the plot.
CHARS contains the title, NUMC is the number of characters
in the title and IFMT is not used.
.sp
Example: CALL CONOP4('TLE=ON','VECTOR REVIEW',13,0)
.SH USAGE
CONOP4 is called to set parameters of type CHARACTER before
entries CONRAN, CONRAS, or CONRAQ are called to generate the
contour plots.
.SH EXAMPLES
Use the ncargex command to see pertinent examples tconaq, tconas
and tconan.
.SH ACCESS
To use CONOP4 load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.
.SH MESSAGES
See the conran_family man page for a description of all Conran_family
error messages and/or informational messages.
.SH SEE ALSO
Online:
conran_family, conran_family_params, conran, conraq, conras, conop1, conop2,
conop3, conpack, conpack_params
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
