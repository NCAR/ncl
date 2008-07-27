C
C	$Id: initzs.f,v 1.6 2008-07-27 00:17:22 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE INITZS (IX,IY,IZ,LIN3,ITOP,IENT)
C
      SAVE
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX      ,
     +                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX       ,
     +                EYEY       ,EYEZ
C
      COMMON /PWRZ2S/ X          ,Y          ,Z
      FX(R) = R+FACTX*REAL(IX)
      FY(R) = R+FACTY*REAL(IY)
C
C
C  DETERMINE INITZS,VISSET,FRSTZ OR VECTZ CALL
C
      GO TO (1000,2000,3000,4000),IENT
 1000 LIN = MAX(1,MIN(3,ABS(LIN3)))
      ITO = MAX(1,MIN(3,ABS(ITOP)))
C
C SET UP SCALING CONSTANTS
C
      DELMAX = MAX(XXMAX-XXMIN,YYMAX-YYMIN,ZZMAX-ZZMIN)
      FACTOR = DELMAX/DELCRT
      FACTX = SIGN(FACTOR,REAL(LIN3))
      FACTY = SIGN(FACTOR,REAL(ITOP))
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
      IF (IZ .EQ. 2) IVIS = MIN(IVISS,ITEMP)
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
  119 CALL PLOTIT (32*INT(XT),32*INT(YT),0)
      RETURN
  120 CALL PLOTIT (32*INT(XT),32*INT(YT),1)
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
