.TH PWRITX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PWRITX - draws text and other characters using polylines.
.SH UTILITY
This routine is part of the Pwrite_family utility in NCAR Graphics. To see
the overview man page for this utility, type "man pwrite_family".
.SH STATUS
PWRITX is obsolete.  It has been replaced by the PLCHHQ, PLCHMQ, and
PLCHLQ entries of the Plotchar utility.
.sp
PWRITX continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use Plotchar.
.SH SYNOPSIS
CALL PWRITX (X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR)
.SH DESCRIPTION 
.IP X 12
(an input parameter of type REAL) defining the X world coordinate of the
text string to be drawn using polylines.
.IP Y 12
(an input parameter of type REAL) defining the Y world coordinate of the
text string to be drawn using polylines.
.IP IDPC 12
(an input string of type CHARACTER) which are the characters to be
drawn.  There may also be embedded function codes describing how
certain characters are to be drawn.
.IP NCHAR 12
(an input parameter of type INTEGER) giving the number of characters
in the IDPC string.  NCHAR includes a count of all characters in the
string including the function codes.
.IP JSIZE 12
(an input parameter of type INTEGER) giving the width of the characters
to be drawn.
.nf

        0  -  1.0 * the size of the digitized
                    characters in the database
        1  -  1.5 * digitized size
        2  -  2.0 * digitized size
        3  -  3.0 * digitized size

        If greater than 3:

        Principal characters =          JSIZE
        Indexical characters =  (13/21)*JSIZE
     Cartographic characters =  ( 9/21)*JSIZE

.fi
One can relate these sizes to a viewport in normalized device coordinates
(each axis runs 0. to 1.) by dividing by 1023.  That is a character of
width .05 would take up 5% of the possible picture width meaning that less
than 20 characters could fit on a line.
.sp
Note that there is an important difference in how text sizes are
handled in GKS versus PWRITX.  In GKS, text size is given in terms
of character heights.  In PWRITX, text size (JSIZE) is given in
character width.
.IP JOR 12
(an input parameter of type INTEGER) giving the orientation at which
the IDPC string is to be drawn.  JOR is measured counter-clockwise
from the positive X axis.
.IP JCTR 12
(an input parameter of type INTEGER) giving the centering option for
the IDPC character string.
.nf

    0, (X,Y) is the center of the entire IDPC string.
   -1, (X,Y) is the center of the left edge of the
             first character.
    1  (X,Y) is the center of the right edge of the
             last character.
.fi
.SH ACCESS 
To use PWRITX, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
pwrite_family, pwrity,
plotchar, plchhq, plchmq, plchlq
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
