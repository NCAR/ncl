CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: costmi.f,v 1.2 2006-11-21 01:10:17 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DCOSTMI(N,WSAVE,LENSAV,IER)
      DOUBLE PRECISION PI
      DOUBLE PRECISION DT
      DOUBLE PRECISION FK
      INTEGER N,LENSAV,IER
      DOUBLE PRECISION WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV.LT.2*N+INT(LOG(DBLE(N)))+4) THEN
          IER = 2
          CALL DXERFFT('COSTMI',3)
          GO TO 300
      END IF
C
      IF (N.LE.3) RETURN
      NM1 = N - 1
      NP1 = N + 1
      NS2 = N/2
      PI = 4.D0*ATAN(1.D0)
      DT = PI/DBLE(NM1)
      FK = 0.D0
      DO 101 K = 2,NS2
          KC = NP1 - K
          FK = FK + 1.D0
          WSAVE(K) = 2.D0*SIN(FK*DT)
          WSAVE(KC) = 2.D0*COS(FK*DT)
  101 CONTINUE
      LNSV = NM1 + INT(LOG(DBLE(NM1))) + 4
      CALL DRFFTMI(NM1,WSAVE(N+1),LNSV,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('COSTMI',-5)
      END IF
  300 CONTINUE
      RETURN
      END
