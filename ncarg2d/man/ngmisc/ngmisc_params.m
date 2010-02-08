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
.IP "\'CL\'   -   Integer   -   1"
Specifies whether clipping to NDC space is on in NCAR GKS.  A 
value of "1" will turn clipping on and a value of "0" will turn 
it off.  This flag
controls whether filled areas and polylines are clipped if they
go out of the world coordinate rectangle that gets mapped onto
NDC space.  This clipping is not to be confused with the clipping
that is controlled by the GKS GSCLIP function - that function
controls whether clipping is to be done to the viewport of the
current normalization transformation, not to NDC space itself
(unless the viewport of the current normalization transformation
is the entirety of NDC space).
.IP "\'CM\'   -   Integer     -   1" 
If CM equals 1, then the RGB color model is used for PostScript output;
if CM equals 0, then the CMYK color model is used for PostScript output.	
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
.IP "\'CT\'   -   Integer   -    0"
Specifies whether NGDOTS will draw filled dots or circles.  If the value
of CT is 0, then filled dots are drawn.  If the value of CT is not 0, then
circles are drawn.
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
.IP "\'IG\'   -   Integer   -   0"
A flag to indicate whether the clip rectangles in segment copies 
should be transformed by using the current segment transformation.
The GKS standard says that the clip rectangles should not be so
transformed and this is the default.
.IP "\'JO\'   -   Integer   -   1"
Controls the type of line join used in PostScript
output from NCAR GKS.  If the value is 0, miter joins are used;
if the value is 1, round joins are used; if the value is 2,
bevel joins are used.  
This value applies to the specific
workstation specified in a previous call to NGSETI to set a value
for the \'WO\' parameter (see below).  
.IP "\'LB\'   -   Integer   -   1" 
Specifies the secondary logo color (this is
applicable only for logo type 5, in which case the secondary color
is used for the text string "UCAR").
.IP "\'LC\'   -   Integer   -   1"
Specifies the logo color (except in the
case of an NCAR logo being plotted to a PostScript workstation,
in which case you will get the full-color logo).
.IP "\'LT\'   -   Integer   -   1"
Specify the logo type.  See the documentation
for NGLOGO for a list of the logo types, or execute "ncargex miex01"
for an example plot that lists the logo types.
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
.IP "\'MC\'   -   Integer     -   depends on the workstation type" 
The X11 workstation indicated by the integer should use a "mixed" X color
model.  (-1) indicates the next X11 workstation created.
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
.IP "\'OS\'   -   Real   -  0.07" 
Specifies the logo size in normalized device
coordinates (a number between 0. and 1.).  For example, a size
specification of 0.1 would give a logo size one-tenth the maximum
plot height.
.IP "\'OX\'   -   Real   -  0.93" 
Specify the logo X-coordinate position in
normalized device coordinates.
.IP "\'OY\'   -   Real   -  0.05" 
Specify the logo Y-coordinate position in
normalized device coordinates.
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
.IP "\'PC\'   -   Integer     -   depends on the workstation type" 
The X11 workstation indicated by the integer should use a "private" X color
model.  (-1) indicates the next X11 workstation created.
.IP "\'PE\'   -   Integer   -   20"
Percentage error allowed in matching requested colors in X Window output
where the window shares a single color map with all the other current
X windows.  The value "0" is special and is the same as the value "100"
meaning that any color may be selected.
In the default environment all X11 windows share a single
color map and when a request is made to allocate a new color, and there
are no more colors left, then the nearest color (to within the stated
percentage) is selected.  The "nearest color" is the one in the current
color table that is closest using the normal distance metric in the
RGB color cube.  The percentage error is measured as a percentage of
lenght of a diagonal of the RGB color cube.
.IP "\'PH\'   -   Integer     -   792"
Specification, in points, of the PDF output page height.
.IP "\'PI\'   -   Character   -   blanks"
A character string that will be used for the picture name of the current
picture in the PICTURE NAME element of an ncgm file.  The first picture 
must be named before the ncgm workstation is opened and subsequent 
pictures must be named immediately after creating the previous picture.  
Not all pictures need be named and any picture that is not assigned a
specific name is assigned blanks in the PICTURE NAME field of the ncgm.
.IP "\'PW\'   -   Integer     -   612"
Specification, in points, of the PDF output page width.
.IP "\'SC\'   -   Integer     -   depends on the workstation type" 
The X11 workstation indicated by the integer should use a "shared" X color
model.  (-1) indicates the next X11 workstation created.
.IP "\'SH\'   -   Integer     -   612"
Specification, in points, of the PS output page height.
.IP "\'SE\'   -   Character   -   'GSEG'"
Specifies a root name for segments created by NCAR GKS.  In the 
default case, the segment name used will be constructed from GSEG by
appending an integer constructed from the user id, the process id, and
the segment number.  By default, the segments are stored in the directory
specified by the environment variable TMPDIR (usually set internally
at install time to /tmp).  If SE is changed away from the default setting,
then the segment name will be cunstructed using the value of SE as a
root and appending the segment number.
.IP "\'SW\'   -   Integer     -   792"
Specification, in points, of the PS output page width.
.IP "\'SS\'   -   Integer   -   1"
Specifies whether segments should be deleted when WISS is closed.
The default is to delete segments.  Setting SS to
a non-default value will cause segments to be saved.
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
http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
