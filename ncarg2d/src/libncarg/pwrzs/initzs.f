C
C	$Id: initzs.f,v 1.3 2000-07-12 16:25:22 haley Exp $
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
      SUBROUTINE INITZS (IX,IY,IZ,LIN3,ITOP,IENT)
C
      SAVE
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX      ,
     +                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX       ,
     +                EYEY       ,EYEZ
C
      COMMON /PWRZ2S/ X          ,Y          ,Z
      FX(R) = R+FACTX*FLOAT(IX)
      FY(R) = R+FACTY*FLOAT(IY)
C
C
C  DETERMINE INITZS,VISSET,FRSTZ OR VECTZ CALL
C
      GO TO (1000,2000,3000,4000),IENT
 1000 LIN = MAX0(1,MIN0(3,IABS(LIN3)))
      ITO = MAX0(1,MIN0(3,IABS(ITOP)))
C
C SET UP SCALING CONSTANTS
C
      DELMAX = AMAX1(XXMAX-XXMIN,YYMAX-YYMIN,ZZMAX-ZZMIN)
      FACTOR = DELMAX/DELCRT
      FACTX = SIGN(FACTOR,FLOAT(LIN3))
      FACTY = SIGN(FACTOR,FLOAT(ITOP))
C
C SET UP FOR PROPER PLANE
C
      JUMP1 = LIN+(ITO-1)*3
      GO TO (108,101,102,103,108,104,105,106,108),JUMP1
  101 JUMP = 1
      GO TO 107
  102 JUMP = 2
      GO TO 107
  103 JUMP = 3
      GO TO 107
  104 JUMP = 4
      GO TO 107
  105 JUMP = 5
      GO TO 107
  106 JUMP = 6
  107 RETURN
  108 CALL SETER ('INITZS - LINE OR ITOP IMPROPER IN PWRZS CALL' ,1,1)
      LIN3 = 0
      RETURN
C
C **************************** ENTRY VISSET ****************************
C     ENTRY VISSET (IX,IY,IZ)
C
C
C VISSET IS CALLED ONCE FOR EACH END OF THE CHARACTER STRING
C
 2000 IVIS = -1
      ITEMP = 0
      GO TO 110
C
C SEE IF THIS END COULD BE BEHIND THE OBJECT
C
  109 IF (EYEX.GT.XXMAX .AND. XX.GT.XXMAX) ITEMP = ITEMP+1
      IF (EYEY.GT.YYMAX .AND. YY.GT.YYMAX) ITEMP = ITEMP+1
      IF (EYEZ.GT.ZZMAX .AND. ZZ.GT.ZZMAX) ITEMP = ITEMP+1
      IF (EYEX.LT.XXMIN .AND. XX.LT.XXMIN) ITEMP = ITEMP+1
      IF (EYEY.LT.YYMIN .AND. YY.LT.YYMIN) ITEMP = ITEMP+1
      IF (EYEZ.LT.ZZMIN .AND. ZZ.LT.ZZMIN) ITEMP = ITEMP+1
      IF (IZ .EQ. 1) IVISS = ITEMP
C
C IF EITHER END CHARACTER COULD BE HIDDEN, TEST ALL LINE SEGMENTS.
C
      IF (IZ .EQ. 2) IVIS = MIN0(IVISS,ITEMP)
      RETURN
C
C **************************** ENTRY FRSTZ *****************************
C     ENTRY FRSTZ (IX,IY)
C
 3000 IFRST = 1
      GO TO 110
C
C **************************** ENTRY VECTZ *****************************
C     ENTRY VECTZ (IX,IY)
C
 4000 IFRST = 0
C
C PICK CORRECT 3-SPACE PLANE TO DRAW IN
C
  110 GO TO (111,112,113,114,115,116), JUMP
  111 XX = FY(X)
      YY = FX(Y)
      ZZ = Z
      GO TO 117
  112 XX = FY(X)
      YY = Y
      ZZ = FX(Z)
      GO TO 117
  113 XX = FX(X)
      YY = FY(Y)
      ZZ = Z
      GO TO 117
  114 XX = X
      YY = FY(Y)
      ZZ = FX(Z)
      GO TO 117
  115 XX = FX(X)
      YY = Y
      ZZ = FY(Z)
      GO TO 117
  116 XX = X
      YY = FX(Y)
      ZZ = FY(Z)
C
C TRANSLATE TO 2-SPACE
C
  117 CALL TRN32S (XX,YY,ZZ,XT,YT,DUMMY,1)
      IF (IVIS) 109,121,118
  118 IF (IFRST) 119,120,119
C
C IF IN FRONT, DRAW IN ANY CASE.
C
  119 CALL PLOTIT (32*IFIX(XT),32*IFIX(YT),0)
      RETURN
  120 CALL PLOTIT (32*IFIX(XT),32*IFIX(YT),1)
      RETURN
  121 IF (IFRST) 122,123,122
  122 IX1 = XT
      IY1 = YT
      RETURN
  123 IX2 = XT
      IY2 = YT
C
C IF COULD BE HIDDEN, USE HIDDEN LINE PLOTTING ENTRY IN SRFACE
C
      CALL DRAWS (IX1,IY1,IX2,IY2,1,0)
      IX1 = IX2
      IY1 = IY2
      RETURN
      END
