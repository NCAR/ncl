C
C $Id: mdgcog.f,v 1.3 2005-06-22 21:36:43 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE MDGCOG (CLAT,CLON,CRAD,ALAT,ALON,NPTS)
C
C This routine returns, in the arrays ALAT and ALON, the latitudes and
C longitudes of NPTS points on the surface of the globe, all of them at
C the great-circle distance CRAD from the point (CLAT,CLON), defining a
C circle.  The last point returned is a copy of the first.
C
        DOUBLE PRECISION CLAT,CLON,CRAD,ALAT(*),ALON(*)
        INTEGER          NPTS
C
C Declare common block containing math constants.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
C
C Declare local variables.
C
        DOUBLE PRECISION RLAT,RLON,UCRD,VCRD,WCRD
        INTEGER          IPNT
C
C Points on a circle around the North Pole are rotated to lie around
C the point at latitude CLAT and longitude CLON.
C
        RLAT=DTOR*(90.-CRAD)
C
        DO 101 IPNT=1,NPTS
          RLON=DTOR*360.D0*(DBLE(IPNT-1)/DBLE(NPTS-1))
          UCRD=COS(RLAT)*COS(RLON)
          VCRD=COS(RLAT)*SIN(RLON)
          WCRD=SIN(RLAT)
          CALL MDRITD (2,90.D0-CLAT,UCRD,VCRD,WCRD)
          CALL MDRITD (3,      CLON,UCRD,VCRD,WCRD)
          ALAT(IPNT)=RTOD*ASIN(WCRD)
          ALON(IPNT)=RTOD*ATAN2(VCRD,UCRD)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
