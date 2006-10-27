CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: rfft1f.f,v 1.1 2006-10-27 16:16:32 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFT1F ( N, INC, R, LENR, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  N, INC, LENR, LENSAV, LENWRK, IER
      REAL     R(LENR), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
C
      IF (LENR .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('RFFT1F ', 6)
      ELSEIF (LENSAV .LT. N + INT(LOG(REAL(N))) +4) THEN
        IER = 2
        CALL XERFFT ('RFFT1F ', 8)
      ELSEIF (LENWRK .LT. N) THEN
        IER = 3
        CALL XERFFT ('RFFT1F ', 10)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL RFFTF1 (N,INC,R,WORK,WSAVE,WSAVE(N+1))
      RETURN
      END
