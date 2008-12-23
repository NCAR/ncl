.TH NGPICT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGPICT - Effects a break in the picture drawing sequence in a FORTRAN
code using GKS.  The actions taken depend on whether the designated
workstation is a metafile or an output/input workstation.  An option
is provided for prompting the user when an output/input workstation is
ready and waiting after a pause.
.SH SYNOPSIS
CALL NGPICT(WKID,ACTION)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngpict(int wkid, int action)
.SH DESCRIPTION 
.IP WKID 12
(an input parameter of type INTEGER) which designates the workstation
identifier of a workstation as it was specified in a GOPWK call.
.IP ACTION 12
(an input parameter of type INTEGER) the value of which specifies the
action to be taken on the workstation WKID.
Legal values are:
.RS
.IP 0 
Execute an UPDATE WORKSTATION on WKID.
.IP 1
Execute an UPDATE WORKSTATION followed by a CLEAR WORKSTATION.
.IP 2
Execute an UPDATE WORKSTATION followed by a pause waiting for a 
mouse click or a key click.
.IP 3 
Execute an UPDATE WORKSTATION followed by a pause followed by a 
CLEAR WORKSTATION after the pause has been terminated by a mouse
click or a key click.
.IP 4
Same as 3 except a "<READY>" prompt is issued in the lower left
corner of the window after the UPDATE WORKSTATION.
.RE
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
If WKID designates a metafile, then a CLEAR WORKSTATION is done which 
inserts an END PICTURE into the metafile.  The result produced in this
case would be the same as that resulting from a call to FRAME.  The 
only valid actions for a metafile workstation are 0 and 1.
.sp
NGPICT is designed to be used when more precise control over individual
workstations is desired than that
offered by FRAME.  This might be desirable, for example, when an 
application is controlling several simultaneously active workstations.
To use NGPICT one should be opening GKS workstations with GOPWK rather 
than with OPNGKS.
.SH EXAMPLES
If WKID=2 designates a metafile workstation, then
.nf

        CALL NGPICT(2,1)

.fi
would duplicate the action of a FRAME call.
.sp
If WKID=2 designates an X11 workstation, then
.nf

        CALL NGPICT(2,4)

.fi
would cause a pause in the X11 window and wait for a key click or a
mouse click.  A "<READY>" prompt, indicating that the window is waiting
for a mouse click or a key click,  would appear at the lower left of
the X11 window after the window has been updated.  After a mouse click
in the X11 window, that window will be cleared before program execution
continues.
.SH ACCESS
To use NGPICT or c_ngpict, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
Non-fatal errors are reported for values of ACTION out of range and
for values of ACTION inappropriately applied to a metafile.  Also,
a non-fatal error is reported if NGPICT is called while a segment is
open.
.SH SEE ALSO
Online:
frame(3NCARG),
gflash(3NCARG),
gopwk(3NCARG),
opngks(3NCARG),
gclrwk(3NCARG),
gupwk(3NCARG),
ncarg_cbind(3NCARG)
.sp
Online URL:  http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
