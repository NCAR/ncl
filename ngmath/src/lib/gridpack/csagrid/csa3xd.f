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
      SUBROUTINE CSA3XD(NI,XI,UI,WTS,KNOTS,SSMTH,NDERIV,NXO,NYO,NZO,XO,
     +                  YO,ZO,UO,NWRK,WORK,IER)
      DOUBLE PRECISION XI
      DOUBLE PRECISION UI
      DOUBLE PRECISION WTS
      DOUBLE PRECISION SSMTH
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION ZO
      DOUBLE PRECISION UO
      DOUBLE PRECISION WORK
      DOUBLE PRECISION XMN
      DOUBLE PRECISION XMX
      DOUBLE PRECISION XV
      DOUBLE PRECISION SPLDED
C
      DIMENSION XI(3,NI),UI(NI),WTS(*),KNOTS(3),XO(NXO),YO(NYO),ZO(NZO),
     +          UO(NXO,NYO,NZO),WORK(NWRK),NDERIV(3)
      DIMENSION XMN(3),XMX(3),XV(3)
C
C  Check on the number of knots.
C
      NTOT = KNOTS(1)*KNOTS(2)*KNOTS(3)
C
      DO 20 I = 1,3
          IF (KNOTS(I).LT.4) THEN
              CALL CFAERR(202,
     +' CSA3XD - must have at least four knots in every coordinate direc
     +tion',68)
              IER = 202
              RETURN
          END IF
   20 CONTINUE
C
C  Check on the size of the workspace.
C
      IF (NWRK.LT.NTOT* (NTOT+3)) THEN
          CALL CFAERR(203,' CSA3XD - workspace too small',29)
          IER = 202
          RETURN
      END IF
C
C  Calculate the min and max for the knots as the minimum value of
C  the input data and output data and the maximum of the input and
C  output data.
C
      DO 30 J = 1,3
          XMN(J) = XI(J,1)
          XMX(J) = XI(J,1)
          DO 10 I = 2,NI
              XMN(J) = MIN(XMN(J),XI(J,I))
              XMX(J) = MAX(XMX(J),XI(J,I))
   10     CONTINUE
   30 CONTINUE
      XMN(1) = MIN(XMN(1),XO(1))
      XMX(1) = MAX(XMX(1),XO(NXO))
      XMN(2) = MIN(XMN(2),YO(1))
      XMX(2) = MAX(XMX(2),YO(NYO))
      XMN(3) = MIN(XMN(3),ZO(1))
      XMX(3) = MAX(XMX(3),ZO(NZO))
C
C  Find the coefficients.
C
      CALL SPLCWD(3,XI,3,UI,WTS,NI,XMN,XMX,KNOTS,SSMTH,WORK,NTOT,
     +            WORK(NTOT+1),NWRK-NTOT,IERR)
      IF (IERR.NE.0) RETURN
C
C  Calculate the approximated values (coefficients are stored at
C  the beginnig of WORK).
C
      DO 60 I = 1,NXO
          XV(1) = XO(I)
          DO 40 J = 1,NYO
              XV(2) = YO(J)
              DO 50 K = 1,NZO
                  XV(3) = ZO(K)
                  UO(I,J,K) = SPLDED(3,XV,NDERIV,WORK,XMN,XMX,KNOTS,IER)
                  IF (IERR.NE.0) RETURN
   50         CONTINUE
   40     CONTINUE
   60 CONTINUE
C
      RETURN
      END
