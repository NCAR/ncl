.TH HSTOPC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
HSTOPC - Specifies various CHARACTER variables to be used by
the Histogram utility.
.SH SYNOPSIS
CALL HSTOPC (STRING,STRNG2,NUMBER,ILCH)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_hstopc (char *string, char *strng2, int number, 
.br
int ilch)
.SH DESCRIPTION
.IP STRING 12
Character, input -- Selects an internal parameter.  The
possibilities are:
.sp
  \'FOR=ON\' or \'FOR=OFF\'
.br
  \'TIT=ON\' or \'TIT=OFF\'
.br
  \'LAB=ON\' or \'LAB=OFF\'
.br
  \'PTI=ON\' or \'PTI=OFF\'
.br
  \'FQN=ON\' or \'FQN=OFF\'
.br
  \'CHR=ON, or \'CHR=OFF\'
.sp
If an option is turned \'ON\' then the STRNG2, NUMBER,
and ILCH arguments can be used to override the default
settings of that option.
.sp
If the value of a STRING option is \'OFF\' then the next
three arguments of the HSTOPC call can be any dummy value.
Option settings will be returned to their default values.
.sp
The following options are defined by this subroutine:
.RS
.IP FOR 8
Format for class labels.  The \'FOR=OFF\' default is
 \'(G10.3)\'. Although class values are real numbers,
integer formats are allowed, in which case HISTGR will
convert from real to integer before plotting labels.
.IP TIT 8
A main title of up to 96 characters.  Only 45 characters
are written per line so up to 3 lines may be written.
The \'TIT=OFF\' default is no title.
.IP LAB 8
A label for the class interval (histogram bar) axis.
The \'LAB=OFF\' default value is \'CLASS INTERVALS\' when
the HSTOPL option \'MID=OFF\' is selected, and \'CLASS
MIDVALUES\' when \'MID=ON\'.
.sp
In order to delete this axis label, select \'LAB=ON\' for
STRING and \'NOLABEL\' for STRNG2.
.IP PTI 8
The percent axis label.  Default value when \'PTI=OFF\'
is \'PERCENT OCCURRENCE\' when IFLAG = 0, or 1, and
\'PERCENT of MAXIMUM\' when IFLAG = 2, or 3.
.IP FQN 8
The frequency axis label.  The \'FQN=OFF\' default value
is \'FREQUENCY\'.
.sp
In order to delete this axis label, select \'FQN=ON\' for
STRING and \'NOLABEL\' for STRNG2.
.IP CHR 8
A concatenated string of alphanumeric class interval
labels.  The default value is a set of internally
computed numeric class labels.
.RE
.IP STRNG2 12
Character, input -- A string of up to 45 characters.
.sp
 \'FOR=ON\', STRNG2 is a format for the class labels.
.sp
 \'TIT=ON\', STRNG2 is a histogram main title.
.sp
 \'LAB=ON\', STRNG2 is a label for the class interval axis.
.sp
 \'PTI=ON\', STRNG2 is a label for the percentage axis.
.sp
 \'FQN=ON\', STRNG2 is a label for the frequency axis.
.sp
 \'CHR=ON\', STRNG2 is a concatenated string of class labels.
.sp
The length of this character string will be NUMBER*ILCH,
where NUMBER is the number of class intervals and ILCH
is the number of characters in the interval label.
.IP NUMBER 12
Integer, input -- It only applies to the following
options:
.sp
 \'FOR=ON\', NUMBER specifies the maximum number of class
intervals (histogram bars) that will be labeled.  The
default values (\'FOR=OFF\') are 9 labels for vertical bars
and 15 labels for horizontal bars.
.sp
 \'CHR=ON\', NUMBER must be set to NCLASS, an argument of the
next call to be made to routine HISTGR.
.sp
NUMBER is not used under any other option setting.
.sp
Calls to HSTOPC with either \'FOR=ON\' or \'CHR=ON\' may be
performed in any order; the parameters set by NUMBER
are mutually exclusive.
.IP ILCH 12
Integer, input -- It only applies in the following
instance.

If \'CHR=ON\', ILCH specifys the number of characters in each
label of a class interval (histogram bar).   ILCH
cannot be greater than 15.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
HSTOPC is called to set parameters of type CHARACTER before
entry HISTGR is called to generate the histogram.  Options
are main and axes titles, class interval (bar) labels,
and class interval formats.
.sp
For a complete list of parameters available
in this utility, see the histogram_params man page.
.SH EXAMPLES
Use the command "ncargex thstgr" to generate a three frame example
of histogram options.  The following code causes the second
frame to have a class interval axis of 12 intervals, each with
a 3 character label that specifies a month of the year.
.sp
PARAMETER (NCLASS=12, ILCH=3)
.br
CHARACTER*55 MON
.br
MON=\'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC\'
.br
CALL HSTOPC(\'CHR=ON\',MON,12,3)
.sp
Example  "ncargex thstmv" shows three
examples of histograms with missing values in the input data.
.SH ACCESS
To use HSTOPC or c_hstopc, load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.  
.SH MESSAGES
See the histogram man page for a description of all Histogram error
messages and/or informational messages.
.SH SEE ALSO
Online:
histogram, histogram_params, histgr, hstopi, hstopl, hstopr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
