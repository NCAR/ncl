CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: c1fm1b.f,v 1.2 2006-11-21 01:10:15 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DC1FM1B(N,INC,C,CH,WA,FNF,FAC)
      DOUBLE PRECISION FNF
      DOUBLE COMPLEX C(*)
      DOUBLE PRECISION CH(*),WA(*),FAC(*)
C
C FFTPACK 5.0 auxiliary routine
C
      INC2 = INC + INC
      NF = FNF
      NA = 0
      L1 = 1
      IW = 1
      DO 125 K1 = 1,NF
          IP = FAC(K1)
          L2 = IP*L1
          IDO = N/L2
          LID = L1*IDO
          NBR = 1 + NA + 2*MIN(IP-2,4)
          GO TO (52,62,53,63,54,64,55,65,56,66) NBR
   52     CALL DC1F2KB(IDO,L1,NA,C,INC2,CH,2,WA(IW))
          GO TO 120
   62     CALL DC1F2KB(IDO,L1,NA,CH,2,C,INC2,WA(IW))
          GO TO 120
   53     CALL DC1F3KB(IDO,L1,NA,C,INC2,CH,2,WA(IW))
          GO TO 120
   63     CALL DC1F3KB(IDO,L1,NA,CH,2,C,INC2,WA(IW))
          GO TO 120
   54     CALL DC1F4KB(IDO,L1,NA,C,INC2,CH,2,WA(IW))
          GO TO 120
   64     CALL DC1F4KB(IDO,L1,NA,CH,2,C,INC2,WA(IW))
          GO TO 120
   55     CALL DC1F5KB(IDO,L1,NA,C,INC2,CH,2,WA(IW))
          GO TO 120
   65     CALL DC1F5KB(IDO,L1,NA,CH,2,C,INC2,WA(IW))
          GO TO 120
   56     CALL DC1FGKB(IDO,IP,L1,LID,NA,C,C,INC2,CH,CH,2,WA(IW))
          GO TO 120
   66     CALL DC1FGKB(IDO,IP,L1,LID,NA,CH,CH,2,C,C,INC2,WA(IW))
  120     L1 = L2
          IW = IW + (IP-1)* (IDO+IDO)
          IF (IP.LE.5) NA = 1 - NA
  125 CONTINUE
      RETURN
      END
