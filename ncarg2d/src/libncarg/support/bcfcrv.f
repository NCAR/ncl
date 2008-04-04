C
C $Id: bcfcrv.f,v 1.4 2008-04-04 21:02:55 kennison Exp $
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
