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
      SUBROUTINE CSA2LD(NI,XI,UI,KNOTS,NO,XO,YO,UO,NWRK,WORK,IER)
      DOUBLE PRECISION XI
      DOUBLE PRECISION UI
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION UO
      DOUBLE PRECISION WORK
      DOUBLE PRECISION SMTH
      DOUBLE PRECISION WTS
      DOUBLE PRECISION SSMTH
C
      DIMENSION XI(2,NI),UI(NI),KNOTS(2),XO(NO),YO(NO),UO(NO),
     +          WORK(NWRK),NDERIV(2)
      DATA NDERIV/0,0/
C
      NTOT = KNOTS(1)*KNOTS(2)
      DO 20 I = 1,2
          IF (KNOTS(I).LT.4) THEN
              CALL CFAERR(202,
     +' CSA2LD - must have at least four knots in every coordinate direc
     +tion',69)
              IER = 202
              RETURN
          END IF
   20 CONTINUE
C
C  Check on the size of the workspace.
C
      IF (NWRK.LT.NTOT* (NTOT+3)) THEN
          CALL CFAERR(203,' CSA2LD - workspace too small',29)
          IER = 203
          RETURN
      END IF
C
      SMTH = 0.D0
      WTS = -1.D0
      CALL CSA2LXD(NI,XI,UI,WTS,KNOTS,SSMTH,NDERIV,NO,XO,YO,UO,NWRK,
     +             WORK,IER)
C
      RETURN
      END
