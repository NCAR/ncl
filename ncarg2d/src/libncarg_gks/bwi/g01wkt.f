C
C	$Id: g01wkt.f,v 1.4 2000-08-22 15:09:39 haley Exp $
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
      SUBROUTINE G01WKT
C
C  Process workstation transformation.
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01wsl.h'
      include 'g01ins.h'
C
C  Window or viewport?
C
      GOTO (10,20)  MCODES-70
C
C  Workstation window.
C
   10 CONTINUE
C
C  (Error check of rectangle definition above WSI is assumed)
C   Set 'REQUESTED' and 'CURRENT' window in WSL.
C
      CWINDO(1) = RX(1)
      CWINDO(2) = RX(2)
      CWINDO(3) = RY(1)
      CWINDO(4) = RY(2)
      RWINDO(1) = RX(1)
      RWINDO(2) = RX(2)
      RWINDO(3) = RY(1)
      RWINDO(4) = RY(2)
      GOTO 70
C
C  Workstation viewport.
C
   20 CONTINUE
C
C  (Rectangle definition check above WSI is assumed)
C   check limits and store in WSL.
C
      IF (RX(1).LT.0. .OR. RX(2).GT.32767. .OR.
     +    RY(1).LT.0. .OR. RY(2).GT.32767.) THEN
C
C  Viewport definition out of bounds.
C
        RERR = 54
        GOTO 70
      ELSE
C
C  Set 'REQUESTED' and 'CURRENT' viewport in WSL.
C
        RWKVP(1) = RX(1)
        RWKVP(2) = RX(2)
        RWKVP(3) = RY(1)
        RWKVP(4) = RY(2)
        CWKVP(1) = RX(1)
        CWKVP(2) = RX(2)
        CWKVP(3) = RY(1)
        CWKVP(4) = RY(2)
      END IF
C
   70 CONTINUE
C
      RETURN
      END
