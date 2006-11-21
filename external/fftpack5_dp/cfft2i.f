CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: cfft2i.f,v 1.2 2006-11-21 01:10:16 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DCFFT2I(L,M,WSAVE,LENSAV,IER)
      INTEGER L,M,IER
      DOUBLE PRECISION WSAVE(LENSAV)
C
C Initialize error return
C
      IER = 0
C
      IF (LENSAV.LT.2*L+INT(LOG(DBLE(L)))+2*M+INT(LOG(DBLE(M)))+8) THEN
          IER = 2
          CALL DXERFFT('CFFT2I',4)
          GO TO 100
      END IF
C
      CALL DCFFTMI(L,WSAVE(1),2*L+INT(LOG(DBLE(L)))+4,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('CFFT2I',-5)
          GO TO 100
      END IF
      CALL DCFFTMI(M,WSAVE(2*L+INT(LOG(DBLE(L))*LOG(2.D0))+3),
     +            2*M+INT(LOG(DBLE(M)))+4,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('CFFT2I',-5)
      END IF
C
  100 CONTINUE
      RETURN
      END
