.\"
.\"	$Id: gqchup.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQCHUP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCHUP (Inquire character up vector) - retrieves the current value of
the character up vector.
.SH SYNOPSIS
CALL GQCHUP (ERRIND, CHUX, CHUY)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_char_up_vec(Gint *err_ind, Gvec *char_up_vec);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP CHUX 12
(Real, Output) -  
Gives the X world coordinate of a the up vector.
.IP CHUY 12
(Real, Output) - 
Gives the Y world coordinate of a the up vector.
.SH USAGE
The up vector is that vector formed by the two coordinate points
(0.,0.) and (CHUX,CHUY).  See GSCHUP for more details on the meaning
of the character up vector.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchxp, plotchar, ginq_char_up_vec
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
