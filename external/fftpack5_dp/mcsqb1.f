CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: mcsqb1.f,v 1.2 2006-11-21 01:10:17 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DMCSQB1(LOT,JUMP,N,INC,X,WSAVE,WORK,IER)
      DOUBLE PRECISION X
      DOUBLE PRECISION WSAVE
      DOUBLE PRECISION WORK
      DOUBLE PRECISION XIM1
      DIMENSION X(INC,*),WSAVE(*),WORK(LOT,*)

      IER = 0
      LJ = (LOT-1)*JUMP + 1
      NS2 = (N+1)/2
      NP2 = N + 2
      DO 101 I = 3,N,2
          DO 201 M = 1,LJ,JUMP
              XIM1 = X(M,I-1) + X(M,I)
              X(M,I) = .5D0* (X(M,I-1)-X(M,I))
              X(M,I-1) = .5D0*XIM1
  201     CONTINUE
  101 CONTINUE
      DO 301 M = 1,LJ,JUMP
          X(M,1) = .5D0*X(M,1)
  301 CONTINUE
      MODN = MOD(N,2)
      IF (MODN.NE.0) GO TO 302
      DO 303 M = 1,LJ,JUMP
          X(M,N) = .5D0*X(M,N)
  303 CONTINUE
  302 CONTINUE
      LENX = (LOT-1)*JUMP + INC* (N-1) + 1
      LNSV = N + INT(LOG(DBLE(N))) + 4
      LNWK = LOT*N
C
      CALL DRFFTMB(LOT,JUMP,N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('MCSQB1',-5)
          GO TO 400
      END IF
C
      DO 102 K = 2,NS2
          KC = NP2 - K
          M1 = 0
          DO 202 M = 1,LJ,JUMP
              M1 = M1 + 1
              WORK(M1,K) = WSAVE(K-1)*X(M,KC) + WSAVE(KC-1)*X(M,K)
              WORK(M1,KC) = WSAVE(K-1)*X(M,K) - WSAVE(KC-1)*X(M,KC)
  202     CONTINUE
  102 CONTINUE
      IF (MODN.NE.0) GO TO 305
      DO 304 M = 1,LJ,JUMP
          X(M,NS2+1) = WSAVE(NS2)* (X(M,NS2+1)+X(M,NS2+1))
  304 CONTINUE
  305 DO 103 K = 2,NS2
          KC = NP2 - K
          M1 = 0
          DO 203 M = 1,LJ,JUMP
              M1 = M1 + 1
              X(M,K) = WORK(M1,K) + WORK(M1,KC)
              X(M,KC) = WORK(M1,K) - WORK(M1,KC)
  203     CONTINUE
  103 CONTINUE
      DO 104 M = 1,LJ,JUMP
          X(M,1) = X(M,1) + X(M,1)
  104 CONTINUE
  400 CONTINUE
      RETURN
      END
