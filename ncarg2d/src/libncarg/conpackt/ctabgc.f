C
C $Id: ctabgc.f,v 1.1 2004-01-14 23:56:32 kennison Exp $
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
      FUNCTION CTABGC (ALAT,ALON,BLAT,BLON,CLAT,CLON)
C
C (CTABGC = ConpackT, Angle Between Great Circles)
C
C This function, given the latitudes and longitudes of points A, B, and
C C on the globe, returns the absolute value of the angle, in degrees,
C between the great circle from A to B and the great circle from A to C.
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
C Get XYZ coordinates for B and C.
C
      BVOX=COS(DTOR*BLAT)*COS(DTOR*BLON)
      BVOY=COS(DTOR*BLAT)*SIN(DTOR*BLON)
      BVOZ=SIN(DTOR*BLAT)
C
      CVOX=COS(DTOR*CLAT)*COS(DTOR*CLON)
      CVOY=COS(DTOR*CLAT)*SIN(DTOR*CLON)
      CVOZ=SIN(DTOR*CLAT)
C
C Rotate about the Z axis so as to put A on the prime meridian.
C
      CALL NGRITD (3,-ALON,BVOX,BVOY,BVOZ)
      CALL NGRITD (3,-ALON,CVOX,CVOY,CVOZ)
C
C Rotate about the Y axis so as to put A on the equator.
C
      CALL NGRITD (2,ALAT,BVOX,BVOY,BVOZ)
      CALL NGRITD (2,ALAT,CVOX,CVOY,CVOZ)
C
C Rotate about the X axis so as to put B on the equator.
C
      IF (BVOZ.NE.0..OR.BVOY.NE.0.) THEN
        ANGL=-RTOD*ATAN2(BVOZ,BVOY)
      ELSE
        ANGL=0.
      END IF
C
      CALL NGRITD (1,ANGL,CVOX,CVOY,CVOZ)
C
C Set the value of the function accordingly.
C
      IF (CVOZ.NE.0..OR.CVOY.NE.0.) THEN
        CTABGC=ABS(RTOD*ATAN2(CVOZ,CVOY))
      ELSE
        CTABGC=0.
      END IF
C
C Done.
C
      RETURN
C
      END
