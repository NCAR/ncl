C
C	$Id: ezisos.f,v 1.2 2000-07-12 16:24:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
C
C The subroutine EZISOS.
C --- ---------- -------
C
      SUBROUTINE EZISOS (T,MU,MV,MW,EYE,SLAB,TISO)
C
C Arguments are as described in ISOSRF.
C
      DIMENSION T(MU,MV,MW),EYE(3)
C
      DATA ANG,PI / .35 , 3.14159265358979 /
C
C Pick types of lines to draw.
C
      NU = MU
      NV = MV
      NW = MW
      TVAL = TISO
      MAX = MAX0(NU,NV,NW)+2
      ATU = NU/2
      ATV = NV/2
      ATW = NW/2
      EYEU = EYE(1)
      EYEV = EYE(2)
      EYEW = EYE(3)
      RU = EYEU-ATU
      RV = EYEV-ATV
      RW = EYEW-ATW
      RU2 = RU*RU
      RV2 = RV*RV
      RW2 = RW*RW
      DU = SQRT(RV2+RW2)
      DV = SQRT(RU2+RW2)
      DW = SQRT(RU2+RV2)
      DR = 1./SQRT(RU2+RV2+RW2)
C
C Compute the arc cosine.
C
      TU = DU*DR
      ANGU = ATAN(ABS(SQRT(1.-TU*TU)/TU))
      IF (TU .LE. 0.) ANGU = PI-ANGU
      TV = DV*DR
      ANGV = ATAN(ABS(SQRT(1.-TV*TV)/TV))
      IF (TV .LE. 0.) ANGV = PI-ANGV
      TW = DW*DR
      ANGW = ATAN(ABS(SQRT(1.-TW*TW)/TW))
      IF (TW .LE. 0.) ANGW = PI-ANGW
C
C Breakpoint is about 20 degrees or about .35 radians.
C
      IFLAG = 0
      IF (ANGU .GT. ANG) IFLAG = IFLAG+4
      IF (ANGV .GT. ANG) IFLAG = IFLAG+2
      IF (ANGW .GT. ANG) IFLAG = IFLAG+1
C
C Find sign of IFLAG.
C
      ICNT = 0
      IF (ABS(RU) .LE. ATU) GO TO  30
      IU = 1
      IF (EYEU .GT. ATU) IU = NU
      DO  20 IW=1,NW
         DO  10 IV=1,NV
            IF (T(IU,IV,IW) .GT. TVAL) ICNT = ICNT-2
            ICNT = ICNT+1
   10    CONTINUE
   20 CONTINUE
   30 IF (ABS(RV) .LE. ATV) GO TO  60
      IV = 1
      IF (EYEV .GT. ATV) IV = NV
      DO  50 IW=1,NW
         DO  40 IU=1,NU
            IF (T(IU,IV,IW) .GT. TVAL) ICNT = ICNT-2
            ICNT = ICNT+1
   40    CONTINUE
   50 CONTINUE
   60 IF (ABS(RW) .LE. ATW) GO TO  90
      IW = 1
      IF (EYEW .GT. ATW) IW = NW
      DO  80 IV=1,NV
         DO  70 IU=1,NU
            IF (T(IU,IV,IW) .GT. TVAL) ICNT = ICNT-2
            ICNT = ICNT+1
   70    CONTINUE
   80 CONTINUE
   90 IFLAG = ISIGN(IFLAG,ICNT)
      CALL ISOSRF (T,NU,NU,NV,NV,NW,EYE,MAX,SLAB,TVAL,IFLAG)
      CALL FRAME
      RETURN
      END
