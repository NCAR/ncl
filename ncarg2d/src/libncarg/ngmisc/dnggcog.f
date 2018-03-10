C
C $Id: nggcog.f,v 1.4 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C This is a modified version of nggcog.f that was generated using
C nag_apt to convert from single to double precision.
C
      SUBROUTINE DNGGCOG (CLAT,CLON,CRAD,ALAT,ALON,NPTS)
      DOUBLE PRECISION CLAT
      DOUBLE PRECISION CLON
      DOUBLE PRECISION CRAD
      DOUBLE PRECISION ALAT
      DOUBLE PRECISION ALON
      DOUBLE PRECISION DTOR
      DOUBLE PRECISION RTOD
      DOUBLE PRECISION RLAT
      DOUBLE PRECISION RLON
      DOUBLE PRECISION UCRD
      DOUBLE PRECISION VCRD
      DOUBLE PRECISION WCRD
C
C This routine returns, in the arrays ALAT and ALON, the latitudes and
C longitudes of NPTS points on the surface of the globe, all of them at
C the great-circle distance CRAD from the point (CLAT,CLON), defining a
C circle.  The last point returned is a copy of the first.
C
        DIMENSION ALAT(*),ALON(*)
C
C Define multiplicative constants to convert from degrees to radians
C and from radians to degrees.
C
        DATA DTOR / .017453292519943D0 /
        DATA RTOD / 57.2957795130823D0 /
C
C Points on a circle around the North Pole are rotated to lie around
C the point at latitude CLAT and longitude CLON.
C
        RLAT=DTOR*(90.D0-CRAD)
C
        DO 101 IPNT=1,NPTS
          RLON=DTOR*360.D0*(DBLE(IPNT-1)/DBLE(NPTS-1))
          UCRD=COS(RLAT)*COS(RLON)
          VCRD=COS(RLAT)*SIN(RLON)
          WCRD=SIN(RLAT)
          CALL DNGRITD (2,90.D0-CLAT,UCRD,VCRD,WCRD)
          CALL DNGRITD (3,      CLON,UCRD,VCRD,WCRD)
          ALAT(IPNT)=RTOD*ASIN(WCRD)
          ALON(IPNT)=RTOD*ATAN2(VCRD,UCRD)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
