C
C $Id: nggcog.f,v 1.1 1994-04-13 22:37:24 kennison Exp $
C
      SUBROUTINE NGGCOG (CLAT,CLON,CRAD,ALAT,ALON,NPTS)
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
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Points on a circle around the North Pole are rotated to lie around
C the point at latitude CLAT and longitude CLON.
C
        RLAT=DTOR*(90.-CRAD)
C
        DO 101 IPNT=1,NPTS
          RLON=DTOR*360.*(REAL(IPNT-1)/REAL(NPTS-1))
          UCRD=COS(RLAT)*COS(RLON)
          VCRD=COS(RLAT)*SIN(RLON)
          WCRD=SIN(RLAT)
          CALL NGRITD (2,90.-CLAT,UCRD,VCRD,WCRD)
          CALL NGRITD (3,    CLON,UCRD,VCRD,WCRD)
          ALAT(IPNT)=RTOD*ASIN(WCRD)
          ALON(IPNT)=RTOD*ATAN2(VCRD,UCRD)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
