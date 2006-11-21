CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: sinqmf.f,v 1.2 2006-11-21 01:10:19 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DSINQMF(LOT,JUMP,N,INC,X,LENX,WSAVE,LENSAV,WORK,
     +                   LENWRK,IER)
      DOUBLE PRECISION XHOLD
      INTEGER LOT,JUMP,N,INC,LENX,LENSAV,LENWRK,IER
      DOUBLE PRECISION X(INC,*),WSAVE(LENSAV),WORK(LENWRK)
      LOGICAL DXERCON
C
      IER = 0
C
      IF (LENX.LT. (LOT-1)*JUMP+INC* (N-1)+1) THEN
          IER = 1
          CALL DXERFFT('SINQMF',6)
          GO TO 300
      ELSE IF (LENSAV.LT.2*N+INT(LOG(DBLE(N)))+4) THEN
          IER = 2
          CALL DXERFFT('SINQMF',8)
          GO TO 300
      ELSE IF (LENWRK.LT.LOT*N) THEN
          IER = 3
          CALL DXERFFT('SINQMF',10)
          GO TO 300
      ELSE IF (.NOT.DXERCON(INC,JUMP,N,LOT)) THEN
          IER = 4
          CALL DXERFFT('SINQMF',-1)
          GO TO 300
      END IF
C
      IF (N.EQ.1) RETURN
      NS2 = N/2
      LJ = (LOT-1)*JUMP + 1
      DO 101 K = 1,NS2
          KC = N - K
          DO 201 M = 1,LJ,JUMP
              XHOLD = X(M,K)
              X(M,K) = X(M,KC+1)
              X(M,KC+1) = XHOLD
  201     CONTINUE
  101 CONTINUE
      CALL DCOSQMF(LOT,JUMP,N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('SINQMF',-5)
          GO TO 300
      END IF
      DO 102 K = 2,N,2
          DO 202 M = 1,LJ,JUMP
              X(M,K) = -X(M,K)
  202     CONTINUE
  102 CONTINUE
  300 CONTINUE
      RETURN
      END
