CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: sintmb.f,v 1.2 2006-11-21 01:10:20 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DSINTMB(LOT,JUMP,N,INC,X,LENX,WSAVE,LENSAV,WORK,
     +                   LENWRK,IER)
      INTEGER LOT,JUMP,N,INC,LENX,LENSAV,LENWRK,IER
      DOUBLE PRECISION X(INC,*),WSAVE(LENSAV),WORK(LENWRK)
      LOGICAL DXERCON
C
      IER = 0
C
      IF (LENX.LT. (LOT-1)*JUMP+INC* (N-1)+1) THEN
          IER = 1
          CALL DXERFFT('SINTMB',6)
          GO TO 100
      ELSE IF (LENSAV.LT.N/2+N+INT(LOG(DBLE(N)))+4) THEN
          IER = 2
          CALL DXERFFT('SINTMB',8)
          GO TO 100
      ELSE IF (LENWRK.LT.LOT* (2*N+4)) THEN
          IER = 3
          CALL DXERFFT('SINTMB',10)
          GO TO 100
      ELSE IF (.NOT.DXERCON(INC,JUMP,N,LOT)) THEN
          IER = 4
          CALL DXERFFT('SINTMB',-1)
          GO TO 100
      END IF
C
      IW1 = LOT + LOT + 1
      IW2 = IW1 + LOT* (N+1)
      CALL DMSNTB1(LOT,JUMP,N,INC,X,WSAVE,WORK,WORK(IW1),WORK(IW2),IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('SINTMB',-5)
      END IF
C
  100 CONTINUE
      RETURN
      END
