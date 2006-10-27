CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: rfft2b.f,v 1.1 2006-10-27 16:16:32 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFT2B (LDIM, L, M, R, WSAVE, LENSAV, WORK, 
     1  LENWRK, IER)
      INTEGER LDIM, L, M, LENSAV, LENWRK, IER
      REAL    R(LDIM,M), WSAVE(LENSAV), WORK(LENWRK)
C
C
C Initialize IER
C
      IER = 0
C
C Verify LENSAV
C
      LWSAV =   L + INT(LOG (REAL(L))) +4
      MWSAV =   2*M + INT(LOG (REAL(M))) +4
      IF (LENSAV .LT. LWSAV+MWSAV) THEN
        IER = 2
        CALL XERFFT ('RFFT2B', 6)
        GO TO 100
      ENDIF
C
C Verify LENWRK
C
      IF (LENWRK .LT. 2*(L/2+1)*M) THEN
        IER = 3
        CALL XERFFT ('RFFT2B', 8)
        GO TO 100
      ENDIF
C
C Verify LDIM is as big as L
C
      IF (LDIM .LT. 2*(L/2+1)) THEN
        IER = 5
        CALL XERFFT ('RFFT2B', -6)
        GO TO 100
      ENDIF
C
C transform second dimension of array
C
      CALL CFFTMB(L/2+1,1,M,LDIM/2,R,M*LDIM/2,
     .     WSAVE(L+INT(LOG(REAL(L)))+5), 
     .     2*M+INT(LOG(REAL(M)))+4,WORK,2*(L/2+1)*M,IER1)
      IF(IER1.NE.0) THEN
         IER=20
         CALL XERFFT('RFFT2B',-5)
         GO TO 100
      ENDIF
C
C reshuffle 
C
      DO J=1,M
         DO I=2,L
            R(I,J)=R(I+1,J)
         ENDDO
      ENDDO
C
C Transform first dimension of array
C
      CALL RFFTMB(M,LDIM,L,1,R,M*LDIM,WSAVE(1),
     .     L+INT(LOG(REAL(L)))+4,WORK,2*(L/2+1)*M,IER1)
      IF(IER1.NE.0) THEN
         IER=20
         CALL XERFFT('RFFT2F',-5)
         GO TO 100
      ENDIF
C
  100 CONTINUE
C
      RETURN
      END


