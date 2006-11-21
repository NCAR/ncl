CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: rfft2f.f,v 1.2 2006-11-21 01:10:19 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DRFFT2F(LDIM,L,M,R,WSAVE,LENSAV,WORK,LENWRK,IER)
      INTEGER LDIM,L,M,LENSAV,LENWRK,IER
      DOUBLE PRECISION R(LDIM,M),WSAVE(LENSAV),WORK(LENWRK)
C
C
C Initialize IER
C
      IER = 0
C
C Verify LENSAV
C
      LWSAV = L + INT(LOG(DBLE(L))) + 4
      MWSAV = 2*M + INT(LOG(DBLE(M))) + 4
      IF (LENSAV.LT.LWSAV+MWSAV) THEN
          IER = 2
          CALL DXERFFT('RFFT2F',6)
          GO TO 100
      END IF
C
C Verify LENWRK
C
      IF (LENWRK.LT.2* (L/2+1)*M) THEN
          IER = 3
          CALL DXERFFT('RFFT2F',8)
          GO TO 100
      END IF
C
C Verify LDIM is as big as L
C
      IF (LDIM.LT.2* (L/2+1)) THEN
          IER = 5
          CALL DXERFFT('RFFT2F',-6)
          GO TO 100
      END IF
C
C Transform first dimension of array
C
      CALL DRFFTMF(M,LDIM,L,1,R,M*LDIM,WSAVE(1),L+INT(LOG(DBLE(L)))+4,
     +            WORK,2* (L/2+1)*M,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('RFFT2F',-5)
          GO TO 100
      END IF
C
C reshuffle to add in nyquist imaginary components
C
      DO J = 1,M
          IF (MOD(L,2).EQ.0) R(L+2,J) = 0.0D0
          DO I = L,2,-1
              R(I+1,J) = R(I,J)
          END DO
          R(2,J) = 0.0D0
      END DO
C
C transform second dimension of array
C
      CALL DCFFTMF(L/2+1,1,M,LDIM/2,R,M*LDIM/2,
     +            WSAVE(L+INT(LOG(DBLE(L)))+5),2*M+INT(LOG(DBLE(M)))+4,
     +            WORK,2* (L/2+1)*M,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('RFFT2F',-5)
          GO TO 100
      END IF
C
  100 CONTINUE
C
      RETURN
      END
