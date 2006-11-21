CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: mcfti1.f,v 1.2 2006-11-21 01:10:17 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DMCFTI1(N,WA,FNF,FAC)
      DOUBLE PRECISION FNF
      DOUBLE PRECISION WA(*),FAC(*)
C
      CALL DFACTOR(N,NF,FAC)
      FNF = NF
      IW = 1
      L1 = 1
      DO 110 K1 = 1,NF
          IP = FAC(K1)
          L2 = L1*IP
          IDO = N/L2
          CALL DTABLES(IDO,IP,WA(IW))
          IW = IW + (IP-1)* (IDO+IDO)
          L1 = L2
  110 CONTINUE
      RETURN
      END
