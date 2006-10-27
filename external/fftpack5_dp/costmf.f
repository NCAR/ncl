CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: costmf.f,v 1.1 2006-10-27 16:34:09 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSTMF(LOT,JUMP,N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,
     +                  IER)
      INTEGER LOT,JUMP,N,INC,LENX,LENSAV,LENWRK,IER
      DOUBLE PRECISION X(INC,*),WSAVE(LENSAV),WORK(LENWRK)
      LOGICAL XERCON
C
      IER = 0
C
      IF (LENX.LT. (LOT-1)*JUMP+INC* (N-1)+1) THEN
          IER = 1
          CALL XERFFT('COSTMF',6)
          GO TO 100
      ELSE IF (LENSAV.LT.2*N+INT(LOG(DBLE(N)))+4) THEN
          IER = 2
          CALL XERFFT('COSTMF',8)
          GO TO 100
      ELSE IF (LENWRK.LT.LOT* (N+1)) THEN
          IER = 3
          CALL XERFFT('COSTMF',10)
          GO TO 100
      ELSE IF (.NOT.XERCON(INC,JUMP,N,LOT)) THEN
          IER = 4
          CALL XERFFT('COSTMF',-1)
          GO TO 100
      END IF
C
      IW1 = LOT + LOT + 1
      CALL MCSTF1(LOT,JUMP,N,INC,X,WSAVE,WORK,WORK(IW1),IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL XERFFT('COSTMF',-5)
      END IF
C
  100 CONTINUE
      RETURN
      END
