CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: cosqmi.f,v 1.1 2006-10-27 16:34:09 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQMI(N,WSAVE,LENSAV,IER)
      DOUBLE PRECISION PIH
      DOUBLE PRECISION DT
      DOUBLE PRECISION FK
      INTEGER N,LENSAV,IER
      DOUBLE PRECISION WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV.LT.2*N+INT(LOG(DBLE(N)))+4) THEN
          IER = 2
          CALL XERFFT('COSQMI',3)
          GO TO 300
      END IF
C
      PIH = 2.D0*ATAN(1.D0)
      DT = PIH/DBLE(N)
      FK = 0.D0
      DO 101 K = 1,N
          FK = FK + 1.D0
          WSAVE(K) = COS(FK*DT)
  101 CONTINUE
      LNSV = N + INT(LOG(DBLE(N))) + 4
      CALL RFFTMI(N,WSAVE(N+1),LNSV,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL XERFFT('COSQMI',-5)
      END IF
  300 CONTINUE
      RETURN
      END
