.TH Ngmisc_params 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Ngmisc_params - This document briefly describes all Ngmisc
internal parameters.
.SH DESCRIPTION 
Parameter descriptions, in alphabetical order, of all Ngmisc
internal parameters follow. Each entry includes the
name of a parameter, its FORTRAN type, its default value, and
a short description of the parameter.
.IP "\'CA\'   -   Integer   -   1"
This parameter controls the type of line cap used in PostScript
output from NCAR GKS.  If the value is 0, butt caps are used;
if the value is 1, round caps are used; if the value is 2,
projecting square caps are used.  
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
.IP "\'CO\'   -   Integer   -   25"
A positive integer specifying a scale factor for the integer coordinate values
in PostScript output from NCAR GKS.  This value will apply to the
first PostScript workstation opened with GOPWK after specifying the
value for \'CO\'.  The default coordinates in PostScript
user space represent units of 1/72 of an inch; for an 8 1/2 inch by 
11 inch page in portrait mode, the default PostScript user coordinate
space is  0  to  612  in X and  0  to  792 in Y.  The value of \'CO\' will
multiply the PostScript user coordinates by the specified amount.
Specifying larger values for CO will increase the accuracy of coordinate
positions at the expense of larger files (more space is needed to
represent the larger integer coordinate values); smaller numbers reduce 
the accuracy of coordinate positions, but reduce file sizes.
.IP "\'ER\'   -   Integer   -   10"
Specifies the maximum number of error messages that will be issued
by NCAR GKS before it terminates execution.  ER must be larger than zero.
.IP "\'FI\'   -   Real   -   0.0005"
A real number in the range 0. to 1. (normalized device coordinate
space) that specifies the spacing between fill lines used in software
fill in PostScript output from NCAR GKS.  
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the "WO" parameter (see below).  
.IP "\'FU\'   -   Integer   -   0"
Controls whether the background color is applied to
the entirety of a page, or just the viewport area, in PostScript output
from NCAR GKS.  If the value is 0, then the background color applies
only to the viewport area; if the value is non-zero, then the background
color covers the entire page.  
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
.IP "\'HA\'   -   Real   -   0.01"
A real number in the range 0. to 1. (normalized device coordinate
space) that specifies the spacing between hatch lines used in hatch-pattern 
filled areas in PostScript output from NCAR GKS. 
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
.IP "\'JO\'   -   Integer   -   1"
Controls the type of line join used in PostScript
output from NCAR GKS.  If the value is 0, miter joins are used;
if the value is 1, round joins are used; if the value is 2,
bevel joins are used.  
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
.IP "\'LX\'   -   Integer   -   36"
Specifies the left X coordinate value to be used in
positioning PostScript output from NCAR GKS on the output page.  This
value is in the default PostScript user space where one unit corresponds
to 1/72 inch.  
This value must be specified before opening the
PostScript workstation to which it will apply.  
See descriptions for the parameters \'LY\', \'UX\', and \'UY\'.
.IP "\'LY\'   -   Integer   -   126"
Specifies the lower Y coordinate value to be used in
positioning PostScript output from NCAR GKS on the output page.  This
value is in the default PostScript user space where one unit corresponds
to 1/72 inch.  
This value must be specified before opening the
PostScript workstation to which it will apply.  
See descriptions for the parameters \'LX\', \'UX\', and \'UY\'.
.IP "\'ME\'   -   Character   -   depends on the workstation type" 
A character string that
specifies the metafile name that will be used by the next NCAR GKS
GOPWK call that opens a GKS CGM or PostScript workstation.
.IP "\'MI\'   -   Real   -   10."
Controls the miter limit of line joins in PostScript
output from NCAR GKS.  This applies only if line joins are specified as
mitered joins (see the description of the parameter \'JO\' above).  
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
.IP "\'NO\'   -   Real   -   1.0"
A positive real number specifying a scale factor for the nominal
linewidth in PostScript output from NCAR GKS. 
Setting the value larger than 1.0 will result in all line thicknesses
being fatter by that amount; setting the value smaller than 1.0 will
result in all line thicknesses being narrower by that amount.
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
.IP "\'PA\'   -   Integer   -   1300"
Specifies the maximum size of paths used in
PostScript output from NCAR GKS.  This value controls when 
software fill will be used in place of hardware fill.
Larger values result in significant file size and interpretation time
savings.  
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
The default value is compatible with the minimum 
value specified in the Adobe PostScript specifications that is to
apply to all PostScript interpreters.  A specific interpreter may
allow for significantly larger paths.  See the documentation for
your local PostScript interpreter to determine this value.
.IP "\'PI\'   -   Character   -   blanks"
A character string that will be used for the picture name of the current
picture in the PICTURE NAME element of an ncgm file.  The first picture 
must be named before the ncgm workstation is opened and subsequent 
pictures must be named immediately after creating the previous picture.  
Not all pictures need be named and any picture that is not assigned a
specific name is assigned blanks in the PICTURE NAME field of the ncgm.
.IP "\'SE\'   -   Integer   -   1"
Specifies whether the segments created in WISS in NCAR GKS will be
erased when WISS is closed.  A value of "1" will cause the segments
to be erased and a value of "0" will cause them to be saved for use
after job termination.
.IP "\'ST\'   -   Integer   -   200"
Specifies the maximum size of the operand stack used in
PostScript output from NCAR GKS.  The primary effect of setting
this value controls how many points
can be included in a locally-defined macro for polylines and filled
areas.  Larger values result in space savings.  
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
The default value is compatible with the minimum value specified in the Adobe 
PostScript specifications that is to apply to all PostScript interpreters.
.IP "\'UX\'   -   Integer   -   576"
Specifies the upper X coordinate value to be used in
positioning PostScript output from NCAR GKS on the output page.  This
value is in the default PostScript user space where one unit corresponds
to 1/72 inch.  
This value must be specified before opening the
PostScript workstation to which it will apply.  
See descriptions for the
parameters "LX", "LY", and "UY".
.IP "\'UY\'   -   Integer   -   666"
An integer that specifies the upper Y coordinate value to be used in
positioning PostScript output from NCAR GKS on the output page.  This
value is in the default PostScript user space where one unit corresponds
to 1/72 inch.  
This value must be specified before opening the
PostScript workstation to which it will apply.  
See descriptions for the parameters "LX", "LY", and "UX".
.IP "\'WO\'   -   Integer   -   none"
An integer specifying the NCAR GKS workstation ID that will be used
for subsequent parameters that apply to a specific workstation.
.SH SEE ALSO
Online:
nggetc,
nggeti,
nggetr,
ngsetc,
ngseti,
ngsetr,
ngpswk,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
