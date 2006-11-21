CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: cosqb1.f,v 1.2 2006-11-21 01:10:16 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DCOSQB1(N,INC,X,WSAVE,WORK,IER)
      DOUBLE PRECISION X
      DOUBLE PRECISION WSAVE
      DOUBLE PRECISION WORK
      DOUBLE PRECISION XIM1
      DIMENSION X(INC,*),WSAVE(*),WORK(*)

      IER = 0
      NS2 = (N+1)/2
      NP2 = N + 2
      DO 101 I = 3,N,2
          XIM1 = X(1,I-1) + X(1,I)
          X(1,I) = .5D0* (X(1,I-1)-X(1,I))
          X(1,I-1) = .5D0*XIM1
  101 CONTINUE
      X(1,1) = .5D0*X(1,1)
      MODN = MOD(N,2)
      IF (MODN.NE.0) GO TO 302
      X(1,N) = .5D0*X(1,N)
  302 LENX = INC* (N-1) + 1
      LNSV = N + INT(LOG(DBLE(N))) + 4
      LNWK = N
C
      CALL DRFFT1B(N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('COSQB1',-5)
          GO TO 400
      END IF
C
      DO 102 K = 2,NS2
          KC = NP2 - K
          WORK(K) = WSAVE(K-1)*X(1,KC) + WSAVE(KC-1)*X(1,K)
          WORK(KC) = WSAVE(K-1)*X(1,K) - WSAVE(KC-1)*X(1,KC)
  102 CONTINUE
      IF (MODN.NE.0) GO TO 305
      X(1,NS2+1) = WSAVE(NS2)* (X(1,NS2+1)+X(1,NS2+1))
  305 DO 103 K = 2,NS2
          KC = NP2 - K
          X(1,K) = WORK(K) + WORK(KC)
          X(1,KC) = WORK(K) - WORK(KC)
  103 CONTINUE
      X(1,1) = X(1,1) + X(1,1)
  400 RETURN
      END
