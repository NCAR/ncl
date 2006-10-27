CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: cfft2i.f,v 1.1 2006-10-27 16:16:26 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFT2I (L, M, WSAVE, LENSAV, IER)
      INTEGER L, M, IER
      REAL WSAVE(LENSAV)
C
C Initialize error return
C
      IER = 0
C
      IF (LENSAV .LT. 2*L + INT(LOG(REAL(L))) + 
     1                    2*M + INT(LOG(REAL(M))) +8) THEN
        IER = 2
        CALL XERFFT ('CFFT2I', 4)
        GO TO 100
      ENDIF
C
      CALL CFFTMI (L, WSAVE(1), 2*L + INT(LOG(REAL(L))) + 4, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2I',-5)
        GO TO 100
      ENDIF
      CALL CFFTMI (M, WSAVE(2*L+INT(LOG(REAL(L))*LOG(2.)) + 3), 
     1            2*M + INT(LOG(REAL(M))) + 4, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2I',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
