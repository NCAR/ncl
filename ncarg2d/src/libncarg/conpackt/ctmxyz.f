C
C $Id: ctmxyz.f,v 1.2 2004-03-19 22:51:56 kennison Exp $
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
      SUBROUTINE CTMXYZ (IMAP,XINP,YINP,ZINP,XOTP,YOTP)
C
C Define the constant required to convert an angle from radians to
C degrees.
C
      DATA RTOD / 57.2957795130823 /
C
C If IMAP = 1, treat XINP, YINP, and ZINP as the coordinates of a point
C on the unit sphere.  Compute the latitude and longitude of that point
C and then use EZMAP to find its projection on a map.
C
      IF (IMAP.EQ.1) THEN
C
        RLAT=RTOD*ASIN(ZINP/SQRT(XINP*XINP+YINP*YINP+ZINP*ZINP))
C
        IF (XINP.EQ.0..AND.YINP.EQ.0.) THEN
          RLON=0.
        ELSE
          RLON=RTOD*ATAN2(YINP,XINP)
        END IF
C
        CALL MAPTRA (RLAT,RLON,XOTP,YOTP)
C
C If IMAP = -1, use EZMAP to see if a point on a map is the projection
C of some point on the globe.  (If not, 1.E12s are returned in XOTP and
C YOTP.)
C
      ELSE IF (IMAP.EQ.-1) THEN
C
        CALL MAPTRI (XINP,YINP,XOTP,YOTP)
C
C If IMAP = 2, call TDPACK to project the point (XINP,YINP,ZINP) into
C the projection plane.
C
      ELSE IF (IMAP.EQ.2) THEN
C
        CALL TDPRPT (XINP,YINP,ZINP,XOTP,YOTP)
C
C In all other cases, just do the identity mapping.
C
      ELSE
C
        XOTP=XINP
        YOTP=YINP
C
      END IF
C
      RETURN
C
      END
