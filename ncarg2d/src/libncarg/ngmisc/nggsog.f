C
C $Id: nggsog.f,v 1.4 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGGSOG (SLAT,SLON,SRAD,ALAT,ALON)
C
C This routine returns, in the arrays ALAT and ALON, the latitudes and
C longitudes of 6 points on the surface of the globe, defining a five-
C pointed star.  The last point returned is a copy of the first.  The
C star is centered at the point with latitude SLAT and longitude SLON
C and has a radius of approximately SRAD.  SLAT, SLON, and SRAD are all
C measured in degrees.
C
        DIMENSION ALAT(6),ALON(6)
C
C The array ANGL defines the directions to the points of the star.
C
        DIMENSION ANGL(6)
C
        DATA ANGL / 162. , 18. , -126. , 90. , -54. , 162. /
C
C Define multiplicative constants to convert from degrees to radians
C and from radians to degrees.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Get the latitudes and longitudes for the caller.
C
        DO 101 IPNT=1,6
          UCRD=1.
          VCRD=TAN(DTOR*SRAD)*COS(DTOR*ANGL(IPNT))
          WCRD=TAN(DTOR*SRAD)*SIN(DTOR*ANGL(IPNT))
          TEMP=SQRT(UCRD*UCRD+VCRD*VCRD+WCRD*WCRD)
          UCRD=UCRD/TEMP
          VCRD=VCRD/TEMP
          WCRD=WCRD/TEMP
          CALL NGRITD (2,-SLAT,UCRD,VCRD,WCRD)
          CALL NGRITD (3, SLON,UCRD,VCRD,WCRD)
          ALAT(IPNT)=RTOD*ASIN(WCRD)
          ALON(IPNT)=RTOD*ATAN2(VCRD,UCRD)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
