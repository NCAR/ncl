CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: rfftb1.f,v 1.2 2006-11-21 01:10:19 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DRFFTB1(N,IN,C,CH,WA,FAC)
      DOUBLE PRECISION HALF
      DOUBLE PRECISION HALFM
      DOUBLE PRECISION CH(*),C(IN,*),WA(N),FAC(15)
C
      NF = FAC(2)
      NA = 0
      DO 10 K1 = 1,NF
          IP = FAC(K1+2)
          NA = 1 - NA
          IF (IP.LE.5) GO TO 10
          IF (K1.EQ.NF) GO TO 10
          NA = 1 - NA
   10 CONTINUE
      HALF = .5D0
      HALFM = -.5D0
      MODN = MOD(N,2)
      NL = N - 2
      IF (MODN.NE.0) NL = N - 1
      IF (NA.EQ.0) GO TO 120
      CH(1) = C(1,1)
      CH(N) = C(1,N)
      DO 118 J = 2,NL,2
          CH(J) = HALF*C(1,J)
          CH(J+1) = HALFM*C(1,J+1)
  118 CONTINUE
      GO TO 124
  120 DO 122 J = 2,NL,2
          C(1,J) = HALF*C(1,J)
          C(1,J+1) = HALFM*C(1,J+1)
  122 CONTINUE
  124 L1 = 1
      IW = 1
      DO 116 K1 = 1,NF
          IP = FAC(K1+2)
          L2 = IP*L1
          IDO = N/L2
          IDL1 = IDO*L1
          IF (IP.NE.4) GO TO 103
          IX2 = IW + IDO
          IX3 = IX2 + IDO
          IF (NA.NE.0) GO TO 101
          CALL DR1F4KB(IDO,L1,C,IN,CH,1,WA(IW),WA(IX2),WA(IX3))
          GO TO 102
  101     CALL DR1F4KB(IDO,L1,CH,1,C,IN,WA(IW),WA(IX2),WA(IX3))
  102     NA = 1 - NA
          GO TO 115
  103     IF (IP.NE.2) GO TO 106
          IF (NA.NE.0) GO TO 104
          CALL DR1F2KB(IDO,L1,C,IN,CH,1,WA(IW))
          GO TO 105
  104     CALL DR1F2KB(IDO,L1,CH,1,C,IN,WA(IW))
  105     NA = 1 - NA
          GO TO 115
  106     IF (IP.NE.3) GO TO 109
          IX2 = IW + IDO
          IF (NA.NE.0) GO TO 107
C rav    CALL DRIF3KB (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2))
          CALL DR1F3KB(IDO,L1,C,IN,CH,1,WA(IW),WA(IX2))
          GO TO 108
  107     CALL DR1F3KB(IDO,L1,CH,1,C,IN,WA(IW),WA(IX2))
  108     NA = 1 - NA
          GO TO 115
  109     IF (IP.NE.5) GO TO 112
          IX2 = IW + IDO
          IX3 = IX2 + IDO
          IX4 = IX3 + IDO
          IF (NA.NE.0) GO TO 110
          CALL DR1F5KB(IDO,L1,C,IN,CH,1,WA(IW),WA(IX2),WA(IX3),WA(IX4))
          GO TO 111
  110     CALL DR1F5KB(IDO,L1,CH,1,C,IN,WA(IW),WA(IX2),WA(IX3),WA(IX4))
  111     NA = 1 - NA
          GO TO 115
  112     IF (NA.NE.0) GO TO 113
C rav    CALL DRIFGKB (IDO,IP,L1,IDL1,C,C,C,IN,CH,CH,1,WA(IW))
          CALL DR1FGKB(IDO,IP,L1,IDL1,C,C,C,IN,CH,CH,1,WA(IW))
          GO TO 114
C rav 113    CALL DRIFGKB (IDO,IP,L1,IDL1,CH,CH,CH,1,C,C,IN,WA(IW))
  113     CALL DR1FGKB(IDO,IP,L1,IDL1,CH,CH,CH,1,C,C,IN,WA(IW))
  114     IF (IDO.EQ.1) NA = 1 - NA
  115     L1 = L2
          IW = IW + (IP-1)*IDO
  116 CONTINUE
      RETURN
      END
