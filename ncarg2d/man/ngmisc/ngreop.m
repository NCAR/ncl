.TH NGREOP 3NCARG "October 1996" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGREOP - reopen an existing NCAR Graphics metafile for appending.
.SH SYNOPSIS
CALL NGREOP(WKID, CONID, ITYPE, FNAME, IOPT, IAT, RAT,
.br
            NCOLRS, NSTART, CTAB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.br
#include <ncarg/gks.h>
.sp
void c_ngsrat(int wkid, int conid, int itype, char *fname, 
.br
              int iopt, int *iat, float *rat, int ncolrs, 
.br
              int nstart, Gcolr_rep *ctab)
.SH DESCRIPTION 
.IP WKID 12
(an input variable of type INTEGER) specifying the workstation identifier 
that the reopened metafile will subsequently be known by.  This does not 
have to be the same as the worksttion identifier used to create the 
original metafile.
.IP CONID 12
(an input variable of type INTEGER) specifying the connection identifier.
.IP ITYPE 12
(an input variable of type INTEGER) specifying the workstation type.  
Currently the only valid value is "1".  This subroutine may be augmented
to accommodate PostScript files in the future.
.IP FNAME 12
(an input variable of type CHARACTER) specifying the filename of the 
metafile being opened for appending.
.IP IOPT 12
(an input variable of type INTEGER) specifying the desired action:
.IP "              = 0" 20
reestablish the color table only (using the color table in argument CTAB
described below).  The attributes of the reopened workstation will be set
to default values and may well be out of sync with the current GKS attributes.
.IP "              = 1" 20
reestablish the color table and GKS state (the GKS state as supplied
in arguments IAT and RAT described below).  The state values will not be
flushed to the metafile.
.IP "              = 2" 20
reestablish the color table and GKS state and flush the GKS state 
values to the metafile.
.IP "              = 3" 20
reestablish the color table and flush the current GKS state values to the
metafile (not the values in IAT and RAT).  values to the metafile.
.IP "                 " 12
.sp
If IOPT equals 1 or 2, then IAT and RAT must be supplied, otherwise not.
.IP IAT 12
(an input array of type INTEGER dimensioned for 14) that contains 
GKS integer state variables as follows:
.IP " " 15
IAT( 1) = Clip indicator
.IP " " 15
IAT( 2) = Line type
.IP " " 15
IAT( 3) = Polyline color index
.IP " " 15
IAT( 4) = Marker type
.IP " " 15
IAT( 5) = Polymarker color index
.IP " " 15
IAT( 6) = Text font
.IP " " 15
IAT( 7) = Text precision
.IP " " 15
IAT( 8) = Text color index
.IP " " 15
IAT( 9) = Text path
.IP " " 15
IAT(10) = Text horizontal alignment
.IP " " 15
IAT(11) = Text vertical alignment
.IP " " 15
IAT(12) = Fill area interior style
.IP " " 15
IAT(13) = Fill are style index
.IP " " 15
IAT(14) = Fill area color index
.IP RAT 12
(an input array of type REAL dimensioned for 7) that contains REAL GKS 
attribute settings as follows:
.IP " " 15
RAT( 1) = Linewidth scale factor
.IP " " 15
RAT( 2) = Marker scale factor
.IP " " 15
RAT( 3) = Character expansion factor
.IP " " 15
RAT( 4) = Character spacing
.IP " " 15
RAT( 5) = Character height in world coordinates
.IP " " 15
RAT( 6) = Character up vector, X component in world coordinates
.IP " " 15
RAT( 7) = Character up vector, Y component in world coordinates
.IP NCOLRS 12
(an input variable of type INTEGER) that specifies the number of colors 
in the color table supplied in argument CTAB, described below.  
NCOLRS can be 0 .
.IP NSTART 12
(an input variable of type INTEGER) that specifies the color index 
associated with the first color in the color table CTAB 
(all other color indices are filled in in sequence).  For example, if
NCOLRS = 3 and NSTART = 4, then the color values defined in CTAB would 
be used to define color indices 4, 5, and 6.
.IP CTAB 12
(in the FORTRAN version of this routine, a two-dimensional input array of type REAL dimensioned CTAB(3,NCOLRS); in the C version of this routine, a one-dimensional input array of type Gcolr_rep dimensioned ctab[ncolrs])
that specifies a color table used to initialize the reopened metafile.
This color table does not
necessarily have to agree with the color table in effect when the 
original metafile was created.
.SH USAGE
The most common usage of NGREOP would be in conjunction with usage
of NGSRAT and NGMFTC.  NGMFTC would be used to temporarily close
a metafile and NGSRAT used to save the state of the GKS primitive
attributes at the time of the close.  To reopen the temporarily
closed metafile you would call NGREOP with the attribute settings
previously saved and a setting of IOPT of 2.  NGMFTC can be used
to temporarily close a metafile any time after it has been opened,
even in the middle of a picture.
.sp
NGREOP only reopens the metafile and does not activate it.  It will
be necessary to call GACWK before sending graphics primitives to the
reopened metafile.
.sp
NGREOP can also be used to reopen a previously created metafile -
either created in an independent job step, or in the same job step.
.SH EXAMPLES
The following sequence:
.nf

        CALL NGMFTC(1)
        CALL NGSRAT(2, IAT, RAT)

          ... do stuff

        CALL NGREOP(CALL NGREOP(1, 2, 1, 'gmeta1', 2, IAT, RAT, 
                    NCOLS, 0, CTAB)

.fi

would temporarily close the metafile with workstation ID of 1 (in this
case a metafile with name "gmeta1") and save the GKS state variables 
at the time of the close.  Then the call to NGREOP would reopen the
metafile for appending.
.sp
Use the ncargex command to see the following relevant example: 
pgkex27.
.SH ACCESS
To use NGREOP or c_ngreop, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
NGREOP issues the same messages as those issued by GOPWK.
.SH SEE ALSO
Online:
ngmftc(3NCARG),
ngsrat(3NCARG),
.sp
Online URL:  http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
