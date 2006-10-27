CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: costmf.f,v 1.1 2006-10-27 16:16:29 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSTMF (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COSTMF', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN
        IER = 2
        CALL XERFFT ('COSTMF', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. LOT*(N+1)) THEN
        IER = 3
        CALL XERFFT ('COSTMF', 10)
        GO TO 100
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('COSTMF', -1)
        GO TO 100
      ENDIF
C
      IW1 = LOT+LOT+1
      CALL MCSTF1(LOT,JUMP,N,INC,X,WSAVE,WORK,WORK(IW1),IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSTMF',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
