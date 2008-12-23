.TH NGPSWK 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGPSWK - returns the workstation type, suitable for use as the
third argument in a call to the NCAR GKS routine GOPWK, for the PostScript
workstation whose attributes are specified in the function arguments.
.SH SYNOPSIS
CALL NGPSWK(PSTYPE, ORIENT, COLOR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngpswk(char *pstype, char *orient, char *color)
.SH DESCRIPTION 
.IP PSTYPE 12
(an input variable of type CHARACTER) specifying the type of the PostScript
file.  It can be one of:
.RS
.IP 'PS' 9
regular PostScript
.IP 'EPS' 9
Encapsulated PostScript (can have only a single picture).
.IP 'EPSI' 9
Encapsulated PostScript Interchange format (an EPS file containing a 
preview bitmap).
.RE
.IP ORIENT 12
(an input variable of type CHARACTER) specifying the orientation of the
output plot.  It can be one of:
.RS
.IP 'PORTRAIT' 16
portrait orientation (long side of page is vertical).
.IP 'LANDSCAPE' 16
landscape orientation (long side of the page is horizontal).
.RE
.IP COLOR 12
(an input variable of type CHARACTER) specifying whether the plot will be
color or monochrome.  It can be one of:
.RS
.IP 'MONOCHROME' 19
the plot with be plotted with all graphics objects being plotted using 
the foreground color.
.IP 'COLOR' 19
the plot will be plotted using color, if available, or intensities 
obtained from the NTSC color to black-and-white intensity mapping 
if gray-scale is available.
.RE
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
In specifying the arguments, only enough characters need be entered so 
that the values can be differentiated.  For example, to specify color, 
using 'C' as the third argument would be sufficient.  Either upper 
case or lower case is accepted.
.SH EXAMPLES
The invocation:
.nf

        IWK = NGPSWK('EPS', 'LANDSCAPE', 'COLOR')

.fi
would return the value "27" for IWK.  This is the workstation type that
would be used as the third argument in a call to the GKS function GOPWK
for a workstation that would produce Encapsulated color Postscript in
landscape mode.
.SH ACCESS
To use NGPSWK or c_ngpswk, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
Warning messages will be issued if one calls NGPSWK with unrecognizable
arguments.
The value 'PS' is used if the first argument is unrecognized; 
the value 'PORTRAIT' is used if the second argument is 
unrecognized; the value 'COLOR' is used if the third argument is unrecognized.
.SH SEE ALSO
Online:
gopwk(3NCARG),
ncarg_gks(3NCARG),
ncarg_cbind(3NCARG)
.sp
Online URL:  http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
