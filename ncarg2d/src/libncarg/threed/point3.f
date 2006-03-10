C
C $Id: point3.f,v 1.5 2006-03-10 15:31:41 kennison Exp $
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
      SUBROUTINE POINT3 (U,V,W)
      DIMENSION VIEW(4),WIND(4)
C
C Save the current SET parameters.
C
      CALL GETSET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +             WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
C
C DEFINE NORMALIZATION TRANS TO BE USED WITH POLYMARKER
C
      CALL SET(0.0, 1.0, 0.0, 1.0, 1.0, 1024.0, 1.0, 1024.0, 1)
C
C SET MARKER TYPE TO 1
C
      CALL GSMK (1)
      CALL TRN32T (U,V,W,X,Y,ZDUM,2)
      PX = X
      PY = Y
      CALL GPM (1,PX,PY)
C
C Restore original SET call.
C
      CALL SET(VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     -         WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
C
      RETURN
      END
