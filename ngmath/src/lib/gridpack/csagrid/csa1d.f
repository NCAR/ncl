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
      SUBROUTINE CSA1D(NI,XI,YI,KNOTS,NO,XO,YO,NWRK,WORK,IER)
      DOUBLE PRECISION XI
      DOUBLE PRECISION YI
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION WORK
      DOUBLE PRECISION WTS
      DOUBLE PRECISION SSMTH
C
      DIMENSION XI(NI),YI(NI),XO(NO),YO(NO),WORK(NWRK)
C
C  Check on the number of knots.
C
      IF (KNOTS.LT.4) THEN
          CALL CFAERR(202,' CSA1D - must have at least four knots',38)
          IER = 202
          RETURN
      END IF
C
C  Check on the size of the workspace.
C
      IF (NWRK.LT.KNOTS* (KNOTS+3)) THEN
          CALL CFAERR(203,' CSA1D - workspace too small',28)
          IER = 203
          RETURN
      END IF
C
C  Call the expanded entry.
C
      WTS = -1.D0
      SSMTH = 0.D0
      NDERIV = 0
      CALL CSA1XD(NI,XI,YI,WTS,KNOTS,SSMTH,NDERIV,NO,XO,YO,NWRK,WORK,
     +            IER)
C
      RETURN
      END
