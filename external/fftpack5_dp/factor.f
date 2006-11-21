CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: factor.f,v 1.2 2006-11-21 01:10:17 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DFACTOR(N,NF,FAC)
      DOUBLE PRECISION FAC(*)
      INTEGER NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/
C
      NL = N
      NF = 0
      J = 0
  101 J = J + 1
      IF (J-4) 102,102,103
  102 NTRY = NTRYH(J)
      GO TO 104
  103 NTRY = NTRY + 2
  104 NQ = NL/NTRY
      NR = NL - NTRY*NQ
      IF (NR) 101,105,101
  105 NF = NF + 1
      FAC(NF) = NTRY
      NL = NQ
      IF (NL.NE.1) GO TO 104
      RETURN
      END
