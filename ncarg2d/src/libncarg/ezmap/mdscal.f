C
C $Id: mdscal.f,v 1.1 2001-09-19 17:10:36 kennison Exp $
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
      DOUBLE PRECISION FUNCTION MDSCAL (XCOP,YCOP,XCOQ,YCOQ)
C
        REAL XCOP,YCOP,XCOQ,YCOQ
C
C If (XCOP,YCOP) and (XCOQ,YCOQ) are the fractional coordinates of two
C points that are the projections (in a manner specified by the current
C state of EZMAP's internal parameters) of two points, P and Q, on the
C surface of the globe, then the value of
C
C     MDSCAL(XCOP,YCOP,XCOQ,YCOQ)
C
C is the angular distance, in degrees, between P and Q, divided by the
C distance, in the fractional coordinate system, between the points that
C represent P and Q on the map.  This may be used to estimate the scale
C of the map in a particular location and a particular direction.
C
C Note that the scale of a map is a unitless quantity representing the
C ratio of a distance on the map to a distance on the globe.  Because
C EZMAP has no way of knowing at what size a map will be displayed, it
C cannot compute an actual scale.
C
C Note also that this routine calls MDGDDP to compute the angular
C distance from A to B and that that routine assumes a spherical earth.
C
        DOUBLE PRECISION MDGDDP,PLAT,PLON,QLAT,QLON
C
C Pre-set value in case we take an error exit.
C
        MDSCAL=0.D0
C
C Find the latitude and longitude of P.
C
        CALL MDPTRI (DBLE(CFUX(XCOP)),DBLE(CFUY(YCOP)),PLAT,PLON)
C
C Find the latitude and longitude of Q.
C
        CALL MDPTRI (DBLE(CFUX(XCOQ)),DBLE(CFUY(YCOQ)),QLAT,QLON)
C
C Compute the angular distance from P to Q and divide by the distance
C between the points on the map in the fractional system.  If either
C point is not projectable, a zero is returned for the scale.
C
        IF (PLAT.NE.1.D12.AND.QLAT.NE.1.D12)
     +    MDSCAL=MDGDDP(PLAT,PLON,QLAT,QLON)/
     +                         DBLE(SQRT((XCOP-XCOQ)**2+(YCOP-YCOQ)**2))
C
C Done.
C
        RETURN
C
      END
