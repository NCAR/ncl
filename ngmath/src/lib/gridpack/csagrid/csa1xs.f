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
      SUBROUTINE CSA1XS (NI,XI,YI,WTS,KNOTS,SSMTH,NDERIV,NO,XO,YO,
     +                   NWRK,WORK,IER)
C
      DIMENSION XI(NI),YI(NI),WTS(*),XO(NO),YO(NO),WORK(NWRK)
C
C  Check on the number of knots.
C
      IF (KNOTS .LT. 4) THEN
        CALL CFAERR (202,' CSA1XS - must have at least four knots',39)       
        IER = 202
        RETURN
      ENDIF
C
C  Check on the size of the workspace.
C
      IF (NWRK .LT. KNOTS*(KNOTS+3)) THEN
        CALL CFAERR (203,' CSA1XS - workspace too small',29)
        IER = 203
        RETURN
      ENDIF
C
C  Calculate the min and max for the knots as the minimum value of
C  the input data and output data and the maximum of the input and
C  output data.
C
      XMN = XI(1)
      XMX = XI(1)
      DO 10 I=2,NI
        XMN = MIN(XMN,XI(I))
        XMX = MAX(XMX,XI(I))
   10 CONTINUE
      XMN = MIN(XMN,XO(1))
      XMX = MAX(XMX,XO(NO))
C
C  Find the coefficients.
C
      CALL SPLCW(1,XI,1,YI,WTS,NI,XMN,XMX,KNOTS,SSMTH,WORK,KNOTS,       
     +           WORK(KNOTS+1),NWRK-KNOTS,IERR)
      IF (IERR .NE. 0) RETURN
C
C  Calculate the approximated values.
C
      DO 20 I=1,NO
        YO(I) = SPLDE(1,XO(I),NDERIV,WORK,XMN,XMX,KNOTS,IER)
        IF (IERR .NE. 0) RETURN
   20 CONTINUE
C
      RETURN
      END
