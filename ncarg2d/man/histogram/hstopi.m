.TH HSTOPI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
HSTOPI - Specifies various INTEGER parameters to be used by
the Histogram utility.
.SH SYNOPSIS
CALL HSTOPI (STRING, PARAM1, PARAM2, ICOL, LCOL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_hstopi (char *string, int param1, int param2, 
.br
int *icol, int lcol)
.SH DESCRIPTION
.IP STRING 12
Character, input -- Selects an internal parameter.  The
possibilities are:
.sp
   \'COL=ON\' or \'COL=OFF\'
.br
   \'CLA=ON\' or \'CLA=OFF\'
.sp
If an option is turned \'ON\', then the remaining HSTOPI
arguments can be used to override the default settings
of that option.
.sp
If the option is \'OFF\', then the remaining arguments
of the HSTOPI call can be any dummy values.
Option settings will be returned to their default values.
.sp
The following options are defined by this subroutine:
.RS
.IP COL 12
This option turns the use of color OFF, or ON.  If it
is ON, then eight color indices are assigned to the
various parts of the histogram as described by the
INTEGER array ICOL.  Arguments ICOL and LCOL must be
set, PARAM1 and PARAM2 can have dummy values.
.IP CLA 12
This option allows the size and orientation of class
labels to be altered.  If \'CLA=ON\', the size of the
label characters is set using argument PARAM1, and
the orientation is set by argument PARAM2.  Arguments
ICOL and LCOL can have dummy values.  If \'CLA=OFF\',
defaults are used which cause medium sized characters
to be written in the horizontal direction.
.RE
.IP PARAM1 12
Integer, input -- Specifies the character height of
class labels.
.sp
When \'CLA=ON\'; 1 = small, 2 = medium, 3 = large;
default is 2 when \'CLA=OFF\'.
.IP PARAM2 12
Integer, input -- Specifies the character orientation of
class labels.
.sp
If \'CLA=ON\', labels can vary from 0 (horizontal) to 90 (vertical)
degrees.  Vertical is the default when \'CLA=OFF\'.
.IP ICOL 12
Integer, input -- Assigns a set of RGB color indices to
the eight components of a histogram graphic when \'COL=ON\'.
.sp
ICOL(1) = color index used for area fill of
.br
                histogram bars of dataset 1.
.br
ICOL(2) = color index used for area fill of
.br
                histogram bars of dataset 2.
.br
                (When two datasets are compared
.br
                using parameter IFLAG = 3).
.br
ICOL(3) = color index used for bar outlines.
.br
ICOL(4) = color index used for drawing axes.
.br
ICOL(5) = color index used for drawing a median
.br
                line.
.br
ICOL(6) = color index used for text output
.br
                (labels.)
.br
ICOL(7) = color index used for titles.
.br
ICOL(8) = color index used for drawing the
.br
                perimeter box.
.sp
The default color index is 1 for all (when \'COL=OFF\').
.sp
If \'COL=ON\', the color indices and their associated
colors are as follows. (These may be changed by
specifying an RGB color table prior to your call to
HSTOPI.  See the man page for GKS routine gscr.)
.sp
Color Index      Color
.sp
     0           BLACK
.br
     1           WHITE
.br
     2           RED
.br
     3           GREEN
.br
     4           BLUE
.br
     5           CYAN
.br
     6           MAGENTA
.br
     7           YELLOW
.br
     8           ORANGE
.IP LCOL 12
Integer, input -- Specifies the length of array ICOL.
LCOL must be set to 8.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
HSTOPI is called to set parameters of type INTEGER before
entry HISTGR is called to generate the histogram.  Options
are size and orientation of class labels, and setting the
colors for various components of the graphic.
.sp
For a complete list of parameters available
in this utility, see the histogram_params man page.
.SH EXAMPLES
Use the command "ncargex thstgr" to generate a three frame example
of histogram options.  The following code causes the second
frame to have an array of colors assigned to the graphic
components (argument ICOL.)
.sp
.nf
      REAL     RGB(3,15)
      INTEGER  COLORS(15)
C
C  Define the RGB triples of 15 colors in a data statement.
C  (Code omitted for brevity.  See the data statement in thstgr.f)
C
C  Assign colors to the color indices.
C
      DO 100 I = 1,15
      CALL GSCR(1,I,RGB(1,I),RGB(2,I),RGB(3,I))
  100 CONTINUE
C
C  Assign color indices to the 8 components of the graphic.
C
         COLORS(1) = 8
         COLORS(2) = 3
         COLORS(3) = 14
         COLORS(4) = 11
         COLORS(5) = 6
         COLORS(6) = 13
         COLORS(7) = 14
         COLORS(8) = 5
      CALL HSTOPI(\'COL=ON\',3,0,COLORS,8)
.fi
.sp
Example  "ncargex thstmv" shows three
examples of histograms with missing values in the input data.
.SH ACCESS
To use HSTOPI or c_hstopi, load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.  
.SH MESSAGES
See the histogram man page for a description of all Histogram error
messages and/or informational messages.
.SH SEE ALSO
Online:
histogram, histogram_params, histgr, hstopc, hstopl, hstopr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
