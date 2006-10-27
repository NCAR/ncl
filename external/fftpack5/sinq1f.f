CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: sinq1f.f,v 1.1 2006-10-27 16:16:33 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINQ1F ( N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
C
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINQ1F', 6)
        GO TO 300
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN
        IER = 2
        CALL XERFFT ('SINQ1F', 8)
        GO TO 300
      ELSEIF (LENWRK .LT. N) THEN
        IER = 3
        CALL XERFFT ('SINQ1F', 10)
        GO TO 300
      ENDIF
C
      IF (N .EQ. 1) RETURN
      NS2 = N/2
      DO 101 K=1,NS2
         KC = N-K
         XHOLD = X(1,K)
         X(1,K) = X(1,KC+1)
         X(1,KC+1) = XHOLD
  101 CONTINUE
      CALL COSQ1F (N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINQ1F',-5)
        GO TO 300
      ENDIF
      DO 102 K=2,N,2
         X(1,K) = -X(1,K)
  102 CONTINUE
  300 RETURN
      END
