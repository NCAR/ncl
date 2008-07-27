C
C	$Id: zlset.f,v 1.5 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
      NOPT = ABS(NOPTN)
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
            GLO = MIN(ZZ,GLO)
            HA = MAX(ZZ,HA)
  101    CONTINUE
  102 CONTINUE
      GO TO 106
  103 DO 105 J=1,LY
         DO 104 I=1,LX
            ZZ = Z(I,J)
            IF (ZZ .EQ. SP) GO TO 104
            GLO = MIN(ZZ,GLO)
            HA = MAX(ZZ,HA)
  104    CONTINUE
  105 CONTINUE
C
C FILL ZL
C
  106 DELZ = HA-GLO
      IF (DELZ .EQ. 0.) GO TO 115
      DZ = DELZ/REAL(NLEV)
      NLEVM1 = NLEV-1
      DO 114 K=1,NLEVM1
         ZNORM = REAL(K)/REAL(NLEV)
         GO TO (107,108,109,110,111),NOPT
C
C NOPT=1
C
  107    ZL(K) = GLO+REAL(K)*DZ
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
