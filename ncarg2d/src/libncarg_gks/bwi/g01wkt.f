C
C	$Id: g01wkt.f,v 1.5 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
