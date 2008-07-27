C
C $Id: bcfcrv.f,v 1.5 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BCFCRV(BXI,BYI,NO,XO,YO)
C
C  This subroutine takes the four Bezier specification points in
C  BXI and BYI and returns an array of points ((XO(L),YO(L),L=1,NO)
C  along the Bezier curve using the cubic parametric equations for
C  the Bezier curve.  This subroutine is invoked only when the
C  subdivision algorithm is being overridden by user request.
C
C
      DIMENSION BXI(4),BYI(4),XO(NO),YO(NO)
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL PCBLDA
C
C  Compute the cubic polynomial coefficients.
C
      DX = BXI(1)
      CX = 3.*(BXI(2)-BXI(1))
      BX = 3.*(BXI(3)-BXI(2))-CX
      AX = BXI(4)-BXI(1)-CX-BX
C
      DY = BYI(1)
      CY = 3.*(BYI(2)-BYI(1))
      BY = 3.*(BYI(3)-BYI(2))-CY
      AY = BYI(4)-BYI(1)-CY-BY
C
C  Compute the points along the curve.
C
      XO(1) = BXI(1)
      YO(1) = BYI(1)
C
      AINC = 1./(NO-1)
      DO 10 I=2,NO
        T = REAL(I-1)*AINC
        XO(I) = ((AX*T+BX)*T+CX)*T+DX
        YO(I) = ((AY*T+BY)*T+CY)*T+DY
   10 CONTINUE
C
      RETURN
      END
