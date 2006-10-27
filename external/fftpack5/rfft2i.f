CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: rfft2i.f,v 1.1 2006-10-27 16:16:32 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFT2I (L, M, WSAVE, LENSAV, IER)
      INTEGER L, M, LENSAV, IER
      INTEGER LWSAV,MWSAV
      REAL WSAVE(LENSAV)
C
C Initialize IER
C
      IER = 0
C
C Verify LENSAV
C
      LWSAV =   L+INT(LOG(REAL(L)))+4
      MWSAV =   2*M+INT(LOG(REAL(M)))+4
      IF (LENSAV .LT. LWSAV+MWSAV) THEN
        IER = 2
        CALL XERFFT ('RFFT2I', 4)
        GO TO 100
      ENDIF
C
      CALL RFFTMI (L, WSAVE(1), L + INT(LOG(REAL(L))) + 4, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('RFFT2I',-5)
        GO TO 100
      ENDIF
      CALL CFFTMI (M, WSAVE(L+INT(LOG(REAL(L)))+5), 
     1            2*M+INT(LOG(REAL(M)))+4,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('RFFT2I',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
