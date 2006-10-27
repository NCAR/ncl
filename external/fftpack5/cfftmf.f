CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: cfftmf.f,v 1.1 2006-10-27 16:16:26 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFTMF (LOT, JUMP, N, INC, C, LENC, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  LOT, JUMP, N, INC, LENC, LENSAV, LENWRK, IER
      COMPLEX  C(LENC)
      REAL     WSAVE(LENSAV)     ,WORK(LENWRK)
      LOGICAL  XERCON
C
      IER = 0
C
      IF (LENC .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('CFFTMF ', 6)
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) + 4) THEN
        IER = 2
        CALL XERFFT ('CFFTMF ', 8)
      ELSEIF (LENWRK .LT. 2*LOT*N) THEN
        IER = 3
        CALL XERFFT ('CFFTMF ', 10)
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('CFFTMF ', -1)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      IW1 = N+N+1
      CALL CMFM1F (LOT,JUMP,N,INC,C,WORK,WSAVE,WSAVE(IW1),
     1                           WSAVE(IW1+1))
      RETURN
      END
