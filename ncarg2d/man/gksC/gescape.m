.\"
.\"	$Id: gescape.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GESCAPE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gescape (Escape) - provides a standard way of implementing nonstandard 
GKS features.  NCAR GKS has only two user-accessible escape functions
defined: one for changing the name of a metafile and another for
pausing in an X window.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gescape(Gint func_id, const Gescape_in_data *in_data, Gstore *store_data,Gescape_out_data **out_data);
.SH DESCRIPTION
.IP func_id 12
(Input) - 
A function identifier specifying the requested activity.  The legal
values for func_id are "-1391" for changing a metafile name and "-1396"
for effecting a pause in an X window.
.IP in_data.escape_r1.size 12
(size_t, Input) - 
Size of the input data record array (for input data record, see below).
.IP in_data.escape_r1.data 12
(void *, Input) - Input data record.  For calls to gescape
with func_id equal to -1391 the input data record should be a character string
containing the desired metafile name left justified and blank filled; for 
calls to gescape with func_id equal to -1396 the input data record should be 
a character string containing the workstation identifier encoded as a five 
character number.
.IP store_data 12
(void *, Input) - Storage for output data.  Not currently in use for NCAR GKS.
.IP out_data.escape_r1.size 12
(size_t, Output) - Size of the output data record array.  Not currently in use
for NCAR GKS.
.IP out_data.escape_r1.data 12
(void *, Output) -  Output data record.    Not currently in use for NCAR GKS.
.SH USAGE
The sizes of the data records must always be at least "1" in value.
.SH EXAMPLES
gescape can be used to dynamically change the name of
an output metafile and to do so one should use the calls
gopen_gks and gopen_ws instead of c_opngks.  If you are using c_opngks,
see the man page for setusv(3NCARG) for changing the name of the metafile.
.sp
To change the name of the output metafile inside your program,
you should make a call similar to the following:
.nf
      
       int str_len = 12;
       Gescape_in_data in_data;
       gopen_gks("stdout",0);
       in_data.escape_r1.data = (Gdata *)malloc(str_len*sizeof(char));
       strcpy(in_data.escape_r1.data, "new.cgm.name" );
       in_data.escape_r1.size = str_len;
       gescape(-1391,&in_data,NULL,NULL);

.fi
The call to gescape to change the metafile name must always occur just
before the call to gopen_ws that opens a CGM workstation.  Setting the
environment variable NCARG_GKS_OUTPUT overrides any attempt to 
change the name of an output metafile via a gescape call.
.sp
See the "User's Guide for NCAR GKS-0A Graphics" for a more complete
example of changing metafile names from within a code.
.sp
Here is an example of a piece of code that will create an X window, draw a
line, and then pause waiting for a mouse click or a key click.
.nf
   
       Gescape_in_data in_data;
       int str_len = 5;

       gopen_gks("stdout",0);
       gopen_ws(3,NULL,8);
       gactivate_ws(3);
       c_line(0.,0.,1.,1.);
       c_sflush();
       in_data.escape_r1.data = (Gdata *)malloc(str_len*sizeof(char));
       strcpy(in_data.escape_r1.data, "    3" );
       in_data.escape_r1.size = str_len;
       gescape(-1396,&in_data,NULL,NULL);

       gdeactivate_ws(3);
       gclose_ws(3);
       gclose_gks();


.fi
.sp
The functions c_frame and c_ngpict are in general much easier to use and more
flexible than the direct ESCAPE call for pausing in an X window.  It is
suggested that those functions be used.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR setusv(3NCARG),
.BR frame(3NCARG),
.BR ngpict(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
