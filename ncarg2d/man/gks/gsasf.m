.\"
.\"	$Id: gsasf.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSASF 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSASF - (Set aspect source flags) - Sets the aspect source flags that
determine whether primitive attributes are to be selected from individual
settings or from "bundle tables".
.SH SYNOPSIS
CALL GSASF (LASF)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_asfs(const Gasfs *list_asf);
.SH DESCRIPTION 
.IP LASF 12
(Integer array, input) - An array of settings for the thirteen attributes:
.RS
.IP " 1"
Linetype
.IP " 2"
Linewidth scale factor
.IP " 3"
Polyline color index
.IP " 4"
Marker type
.IP " 5"
Marker size scale factor
.IP " 6"
Polymarker color index
.IP " 7"
Text font and precision
.IP " 8"
Character expansion factor
.IP " 9"
Character spacing
.IP "10"
Text color index
.IP "11"
Fill area interior style
.IP "12"
Fill area style index
.IP "13"
Fill area color index
.RE
.sp
Each value of LASF is either "0" (for "bundled") or "1" (for "individual").
.SH USAGE
In NCAR Graphics all attribute aspect source flags are defaulted to 
"individual" (all values for LASF are "1").  
It is advised that for dependable results when using NCAR Graphics 
these values remain in their default settings.  Some GKS packages
default these values to "bundled", so, if one expects to use such
a package, it would be precautionary to call GSASF to set all aspect
source flags to "1" before calling other GKS functions.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online:
gqasf, 
gsln, gslwsc, gsplci, gsmk, gsmksc, gspmci, gstxfp, gschxp,
gstxsp, gstxci, gsfais, gsfasi, gsfaci,
gqln, gqlwsc, gqplci, gqmk, gqmksc, gqpmci, gqtxfp, gqchxp,
gqtxsp, gqtxci, gqfais, gqfasi, gqfaci,, gset_asfs
.sp
Hardcopy:  
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
