CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: cfft2b.f,v 1.1 2006-10-27 16:16:26 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFT2B (LDIM, L, M, C, WSAVE, LENSAV,
     1                     WORK, LENWRK, IER)
      INTEGER L, M, LDIM, LENSAV, LENWRK, IER
      COMPLEX C(LDIM,M)
      REAL WSAVE(LENSAV), WORK(LENWRK)
C
C Initialize error return
C
      IER = 0
C
      IF (L .GT. LDIM) THEN
        IER = 5
        CALL XERFFT ('CFFT2B', -2)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*L + INT(LOG(REAL(L))) + 
     1                    2*M + INT(LOG(REAL(M))) +8) THEN
        IER = 2
        CALL XERFFT ('CFFT2B', 6)
        GO TO 100
      ELSEIF (LENWRK .LT. 2*L*M) THEN
        IER = 3
        CALL XERFFT ('CFFT2B', 8)
        GO TO 100
      ENDIF
C
C Transform X lines of C array
      IW = 2*L+INT(LOG(REAL(L))*LOG(2.)) + 3
      CALL CFFTMB(L, 1, M, LDIM, C, (L-1) + LDIM*(M-1) +1,
     1     WSAVE(IW), 2*M + INT(LOG(REAL(M))) + 4, 
     2     WORK, 2*L*M, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2B',-5)
        GO TO 100
      ENDIF
C
C Transform Y lines of C array
      IW = 1
      CALL CFFTMB (M, LDIM, L, 1, C, (M-1)*LDIM + L,
     1     WSAVE(IW), 2*L + INT(LOG(REAL(L))) + 4, 
     2     WORK, 2*M*L, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2B',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
