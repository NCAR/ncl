CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: sintb1.f,v 1.2 2006-11-21 01:10:20 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DSINTB1(N,INC,X,WSAVE,XH,WORK,IER)
      DOUBLE PRECISION WORK
      DOUBLE PRECISION SRT3S2
      DOUBLE PRECISION XHOLD
      DOUBLE PRECISION T1
      DOUBLE PRECISION T2
      DOUBLE PRECISION FNP1S4
      DOUBLE PRECISION X(INC,*),WSAVE(*),XH(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DSUM

      IER = 0
      IF (N-2) 200,102,103
  102 SRT3S2 = SQRT(3.D0)/2.D0
      XHOLD = SRT3S2* (X(1,1)+X(1,2))
      X(1,2) = SRT3S2* (X(1,1)-X(1,2))
      X(1,1) = XHOLD
      GO TO 200
  103 NP1 = N + 1
      NS2 = N/2
      DO 104 K = 1,NS2
          KC = NP1 - K
          T1 = X(1,K) - X(1,KC)
          T2 = WSAVE(K)* (X(1,K)+X(1,KC))
          XH(K+1) = T1 + T2
          XH(KC+1) = T2 - T1
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN.EQ.0) GO TO 124
      XH(NS2+2) = 4.D0*X(1,NS2+1)
  124 XH(1) = 0.D0
      LNXH = NP1
      LNSV = NP1 + INT(LOG(DBLE(NP1))) + 4
      LNWK = NP1
C
      CALL DRFFT1F(NP1,1,XH,LNXH,WSAVE(NS2+1),LNSV,WORK,LNWK,IER1)
      IF (IER1.NE.0) THEN
          IER = 20
          CALL DXERFFT('SINTB1',-5)
          GO TO 200
      END IF
C
      IF (MOD(NP1,2).NE.0) GO TO 30
      XH(NP1) = XH(NP1) + XH(NP1)
   30 FNP1S4 = DBLE(NP1)/4.D0
      X(1,1) = FNP1S4*XH(1)
      DSUM = X(1,1)
      DO 105 I = 3,N,2
          X(1,I-1) = FNP1S4*XH(I)
          DSUM = DSUM + FNP1S4*XH(I-1)
          X(1,I) = DSUM
  105 CONTINUE
      IF (MODN.NE.0) GO TO 200
      X(1,N) = FNP1S4*XH(N+1)
C
  200 CONTINUE
      RETURN
      END
