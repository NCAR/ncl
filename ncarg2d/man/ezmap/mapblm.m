.TH MAPBLM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPBLM -
Draws geographical outlines masked against an existing area map.  Note that
this routine uses whichever old outline dataset is selected by the value of
the internal parameter 'OU'; to access the new map database "Earth..1", which
was created in 1998, one must call instead the EZMAPB routine MPLNDM.
.SH SYNOPSIS
CALL MAPBLM (IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapblm (int *iama, float *xcra, float *ycra,
.br
int mcra, int *iaai, int *iagi, int mnog, 
.br
int (*ulpr)(float *xcra, float *ycra, int *ncs, 
.br
int *iaai, int *iagi, int *nai))
.SH DESCRIPTION 
.IP IAMA 12 
(an input/output array, dimensioned as specified in a call to the
AREAS routine ARINAM, of type INTEGER) is the array containing the area
map against which boundary lines are to be masked. The
area map must have been initialized by a call to ARINAM; it should contain
the edges required to create a desired effect.
For example, an area map might be created that defines a
region of interest, within which user data is available and within which
boundary lines are to be drawn. For more details, see
the reference document for the package named AREAS.
.IP "XCRA and YCRA" 12 
(scratch arrays, dimensioned at least MCRA, of type REAL)
are to be used by MAPBLM in calls to the AREAS routine ARDRLN; they will
eventually be used in calls to the user-provided line-processing routine
ULPR.
.IP MCRA 12 
(an input expression of type INTEGER) is the dimension of the arrays
XCRA and YCRA.
.IP "IAAI and IAGI" 12 
(scratch arrays, dimensioned at least NOGI, of type
INTEGER) are to be used by MAPBLM in calls to the AREAS routine ARDRLN;
they will eventually be used in calls to the user-provided
line-processing routine ULPR. The mnemonics stand for "Integer Array of Area
Identifiers" and "Integer Array of Group Identifiers", respectively.
.IP NOGI 12 
(an input expression of type INTEGER) is the dimension of the arrays
IAAI and IAGI. The mnemonic stands for "Number Of Group Identifiers (of
edges in the area map)", which determines the required dimension of IAAI
and IAGI.
.IP ULPR 12 
is the name of the user-supplied line-processing routine. It must be
declared EXTERNAL in the routine that calls MAPBLM, so that the compiler
and loader will know that it is the name of a routine to be called
instead of a variable. The user routine ULPR will be called once for each
piece of a boundary line resulting from the masking process; it
may decide to draw (or to not draw) each such piece. ULPR will be called
using a FORTRAN statement like
.sp
.RS 17 
CALL ULPR (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
.RE 
.IP "" 12 
where XCRA and YCRA are real arrays holding the normalized device
coordinates of NCRA points defining a polyline which is part of some
boundary line and IAAI and IAGI are integer arrays holding NGPS
area-identifier/group-identifier pairs for the area within which that
piece of the line lies. In writing ULPR, the user may rely upon a SET
call's having been done which makes it possible to use normalized device
coordinates in calls to routines like CURVE, CURVED, GPL, etc. For more
details, see the reference document for the package named AREAS and, in
particular, the description of the subroutine ARDRLN.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The statement
.RS 5
.sp
CALL MAPBLM (IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
.RE
.sp
does the same thing as the statement
.RS 5
.sp
CALL MAPLOT
.RE
.sp
except that the boundary lines are drawn using calls to MAPITM and MAPIQM,
which does the masking of the lines against the area map defined by the
arguments in the call and passes the pieces resulting from the masking
process to a user-provided line-drawing routine.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
cpex10.
.SH ACCESS
To use MAPBLM or c_mapblm, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
ezmap, 
ezmap_params, 
mapaci,
mapbla,
mapdrw,
mapeod, 
mapfst,
mapgci,
mapgrd,
mapgrm,
mapgtc,  
mapgti,  
mapgtl,  
mapgtr,  
mapint,
mapiq,
mapiqa,
mapiqd,
mapiqm,
mapit,
mapita,
mapitd,
mapitm,
maplbl,
maplmb,
maplot,
mappos,   
maproj,  
maprs,
maprst,  
mapsav,  
mapset,  
mapstc,  
mapsti,  
mapstl,  
mapstr,  
maptra,
maptri,
maptrn,
mapusr, 
mapvec,
mpchln,
mpfnme,
mpgetc,
mpgeti,  
mpgetl,
mpgetr,  
mpglty,
mpiaty,
mpifnb,
mpilnb,
mpiola,
mpiosa,
mpipai,
mpipan,
mpipar,
mpisci,
mplnam,
mplndm,
mplndr,
mplnri,
mpname,
mprset,
mpsetc,  
mpseti,  
mpsetl,  
mpsetr,  
supmap,
supcon,
ncarg_cbind
.sp
Hardcopy: 
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
