.TH MAPGCI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPGCI - Returns a set of points on the shortest great circle route
between two user-specified points on the globe.
.SH SYNOPSIS
CALL MAPGCI(ALAT,ALON,BLAT,BLON,NOPI,RLTI,RLNI)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapgci( float alat, float alon, float blat, 
.br
float blon, int nopi, float *rlti, float *rlni)
.SH DESCRIPTION 
.IP "ALAT and ALON" 12
(input expressions, of type REAL) are the latitude and longitude of
the point at the beginning of a great circle route.
.IP "BLAT and BLON" 12
(input expressions, of type REAL) are the latitude and longitude of
the point at the end of a great circle route.
.IP NOPI 12
(an input expression, of type INTEGER) is the number of equally-spaced
points to be interpolated along the great circle route.
.IP "RLTI and RLNI" 12
(output arrays of type REAL, each dimensioned at least NOPI) are arrays
containing the latitudes and
longitudes of points interpolated along the great circle route.
Each lat/lon pair defines one of the points; they appear in order of
increasing distance along the great circle route.  The positions of
the first point and the last point are not returned in these arrays;
only the interpolated points are.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The statement:
.RS 5
.sp
CALL MAPGCI (ALAT,ALON,BLAT,BLON,NOPI,RLTI,RLNI)
.RE
.sp
defines the positions of two points, A and B, on the globe and the number
of equally-spaced points, NOPI, to be interpolated along the great circle
route from A to B.  The latitudes and longitudes of the interpolated points
are returned to the caller in the arrays RLTI and RLNI.  If the points A
and B are exactly opposite one another on the globe, the code does not
fail, but the direction of the great circle route will be somewhat
unpredictable (since, in that case, there is more than one great circle
route joining the two points).
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
cmpgci.
.SH ACCESS
To use MAPGCI, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_mapgci, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ezmap, 
ezmap_params, 
mapaci,
mapbla,
mapdrw,
mapeod, 
mapfst,
mapgrd,
mapgrm,
mapgtc,  
mapgti,  
mapgtl,  
mapgtr,  
mapint,
mapiq,
mapiqa,
mapiqm,
mapit,
mapita,
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
mpgetc,
mpgeti,  
mpgetl,
mpgetr,  
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
