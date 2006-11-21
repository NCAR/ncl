CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: tables.f,v 1.2 2006-11-21 01:10:20 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DTABLES(IDO,IP,WA)
      DOUBLE PRECISION TPI
      DOUBLE PRECISION ARGZ
      DOUBLE PRECISION ARG1
      DOUBLE PRECISION ARG2
      DOUBLE PRECISION ARG3
      DOUBLE PRECISION ARG4
      DOUBLE PRECISION WA(IDO,IP-1,2)
C
      TPI = 8.D0*ATAN(1.D0)
      ARGZ = TPI/DBLE(IP)
      ARG1 = TPI/DBLE(IDO*IP)
      DO 110 J = 2,IP
          ARG2 = DBLE(J-1)*ARG1
          DO 100 I = 1,IDO
              ARG3 = DBLE(I-1)*ARG2
              WA(I,J-1,1) = COS(ARG3)
              WA(I,J-1,2) = SIN(ARG3)
  100     CONTINUE
          IF (IP.LE.5) GO TO 110
          ARG4 = DBLE(J-1)*ARGZ
          WA(1,J-1,1) = COS(ARG4)
          WA(1,J-1,2) = SIN(ARG4)
  110 CONTINUE
      RETURN
      END
