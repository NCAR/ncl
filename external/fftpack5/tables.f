CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: tables.f,v 1.1 2006-10-27 16:16:34 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE TABLES (IDO,IP,WA)
      REAL  WA(IDO,IP-1,2)
C
      TPI = 8.*ATAN(1.)
      ARGZ = TPI/REAL(IP)
      ARG1 = TPI/REAL(IDO*IP)
      DO 110 J=2,IP
         ARG2 = REAL(J-1)*ARG1
         DO 100 I=1,IDO
            ARG3 = REAL(I-1)*ARG2 
            WA(I,J-1,1) = COS(ARG3)
            WA(I,J-1,2) = SIN(ARG3)
  100    CONTINUE
         IF (IP .LE. 5) GO TO 110
         ARG4 = REAL(J-1)*ARGZ
         WA(1,J-1,1) = COS(ARG4)
         WA(1,J-1,2) = SIN(ARG4)
  110 CONTINUE
      RETURN
      END
