C
C $Id: ctmxyz.f,v 1.1 2003-05-28 15:44:32 kennison Exp $
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
      DATA RTOD / 57.2957795130823 /
      IF (IMAP.EQ.1) THEN
        RLAT=RTOD*ASIN(ZINP/SQRT(XINP*XINP+YINP*YINP+ZINP*ZINP))
        IF (XINP.EQ.0..AND.YINP.EQ.0.) THEN
          RLON=0.
        ELSE
          RLON=RTOD*ATAN2(YINP,XINP)
        END IF
        CALL MAPTRA (RLAT,RLON,XOTP,YOTP)
      ELSE IF (IMAP.EQ.-1) THEN
        CALL MAPTRI (XINP,YINP,XOTP,YOTP)
      ELSE
        XOTP=XINP
        YOTP=YINP
      END IF
      RETURN
      END
