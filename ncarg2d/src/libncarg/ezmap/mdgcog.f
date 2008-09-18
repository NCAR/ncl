C
C $Id: mdgcog.f,v 1.8 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
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
          ALAT(IPNT)=RTOD*ASIN(MAX(-1.D0,MIN(+1.D0,WCRD)))
          ALON(IPNT)=RTOD*ATAN2(VCRD,UCRD)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
