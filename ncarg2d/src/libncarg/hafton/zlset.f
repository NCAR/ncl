C
C	$Id: zlset.f,v 1.3 2000-08-22 15:04:45 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE ZLSET (Z,MX,NX,NY,ZL,NLEVL)
      SAVE
C
      DIMENSION       Z(MX,NY)   ,ZL(NLEVL)
C
      COMMON /HAFTO2/ GLO        ,HA         ,NOPTN      ,ALPHA      ,
     1                NSPV       ,SP         ,ICNST
C
      BIG = R1MACH(2)
C
C ZLSET PUTS THE INTENSITY LEVEL BREAK POINTS IN ZL.
C ALL ARGUMENTS ARE AS IN HAFTON.
C
      LX = NX
      LY = NY
      NLEV = NLEVL
      NOPT = IABS(NOPTN)
      RALPH = 1./ALPHA
      ICNST = 0
      IF (GLO.NE.0. .OR. HA.NE.0.) GO TO 106
C
C FIND RANGE IF NOT KNOWN.
C
      GLO = BIG
      HA = -GLO
      IF (NSPV .NE. 0) GO TO 103
      DO 102 J=1,LY
         DO 101 I=1,LX
            ZZ = Z(I,J)
            GLO = AMIN1(ZZ,GLO)
            HA = AMAX1(ZZ,HA)
  101    CONTINUE
  102 CONTINUE
      GO TO 106
  103 DO 105 J=1,LY
         DO 104 I=1,LX
            ZZ = Z(I,J)
            IF (ZZ .EQ. SP) GO TO 104
            GLO = AMIN1(ZZ,GLO)
            HA = AMAX1(ZZ,HA)
  104    CONTINUE
  105 CONTINUE
C
C FILL ZL
C
  106 DELZ = HA-GLO
      IF (DELZ .EQ. 0.) GO TO 115
      DZ = DELZ/FLOAT(NLEV)
      NLEVM1 = NLEV-1
      DO 114 K=1,NLEVM1
         ZNORM = FLOAT(K)/FLOAT(NLEV)
         GO TO (107,108,109,110,111),NOPT
C
C NOPT=1
C
  107    ZL(K) = GLO+FLOAT(K)*DZ
         GO TO 114
C
C NOPT=2
C
  108    ONORM = (1.-(1.-ZNORM)**ALPHA)**RALPH
         GO TO 113
C
C NOPT=3
C
  109    ONORM = 1.-(1.-ZNORM**ALPHA)**RALPH
         GO TO 113
C
C NOPT=4
C
  110    ONORM = .5*(1.-(ABS(ZNORM+ZNORM-1.))**ALPHA)**RALPH
         GO TO 112
C
C NOPT=5
C
  111    ZNORM2 = ZNORM+ZNORM
         IF (ZNORM .GT. .5) ZNORM2 = 2.-ZNORM2
         ONORM = .5*(1.-(1.-ABS(ZNORM2)**ALPHA)**RALPH)
  112    IF (ZNORM .GT. .5) ONORM = 1.-ONORM
  113    ZL(K) = GLO+DELZ*ONORM
  114 CONTINUE
      ZL(NLEV) = BIG
      RETURN
  115 ICNST = 1
      RETURN
      END
