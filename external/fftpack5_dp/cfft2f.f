CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: cfft2f.f,v 1.2 2006-11-21 01:10:16 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DCFFT2F(LDIM,L,M,C,WSAVE,LENSAV,WORK,LENWRK,IER)
      INTEGER L,M,LDIM,LENSAV,LENWRK,IER
      DOUBLE COMPLEX C(LDIM,M)
      DOUBLE PRECISION WSAVE(LENSAV),WORK(LENWRK)
C
C Initialize error return
C
      IER = 0
C
      IF (L.GT.LDIM) THEN
          IER = 5
          CALL DXERFFT('CFFT2F',-2)
          GO TO 100
      ELSE IF (LENSAV.LT.2*L+INT(LOG(DBLE(L)))+2*M+INT(LOG(DBLE(M)))+
     +         8) THEN
          IER = 2
          CALL DXERFFT('CFFT2F',6)
          GO TO 100
      ELSE IF (LENWRK.LT.2*L*M) THEN
          IER = 3
          CALL DXERFFT('CFFT2F',8)
          GO TO 100
      END IF
C
C Transform X lines of C array
      IW = 2*L + INT(LOG(DBLE(L))*LOG(2.D0)) + 3
      CALL DCFFTMF(L,1,M,LDIM,C, (L-1)+LDIM* (M-1)+1,WSAVE(IW),
     +            2*M+INT(LOG(DBLE(M)))+4,WORK,2*L*M,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('CFFT2F',-5)
          GO TO 100
      END IF
C
C Transform Y lines of C array
      IW = 1
      CALL DCFFTMF(M,LDIM,L,1,C, (M-1)*LDIM+L,WSAVE(IW),
     +            2*L+INT(LOG(DBLE(L)))+4,WORK,2*M*L,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('CFFT2F',-5)
      END IF
C
  100 CONTINUE
      RETURN
      END
