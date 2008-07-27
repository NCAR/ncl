C
C	$Id: congen.f,v 1.5 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONGEN (XI,YI,IPACK,SCRARR,ICA)
C
C  DRAW A CONTOUR AT THE CURRENT LEVEL
C
C  INPUT
C       XI YI LOWER RIGHT CORNER OF CELL
C       IPACK-FLAG TO ALLOW REDUCTION OF COORDINATE PAIR STORAGE
C             IF REQUIRED
C       SCRARR-SCRATCH ARRAY OF CONTOUR VALUES
C       ICA-ENTERING CASE CONDITIONS IF ANY REQUIRED
C
C
C
C
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONRA3/ IREC
      COMMON /CONRA4/ NCP        ,NCPSZ
      COMMON /CONRA5/ NIT        ,ITIPV
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONR18/ TITLE      ,ICNT   ,ITLSIZ
      COMMON /CONR19/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
     1              LEN      ,IFMT       ,LEND       ,
     2                IFMTD      ,ISIZEP     ,INMIN
      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
      COMMON /CONR20/ NREP       ,NCRT       ,ISIZEL     ,
     1                MINGAP     ,ISIZEM         ,
     2                TENS
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
      LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON
      COMMON /CONR15/ ISTRNG
        CHARACTER*64 ISTRNG
        COMMON /CONR16/ FORM
        CHARACTER*10 FORM
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10 NDASH, IDASH, EDASH
C
C
C
      DIMENSION       SCRARR(1)  ,IXMOV(2)   ,IYMOV(2)
        CHARACTER*64    IHOLD
        CHARACTER*23    IVOUT
      INTEGER         GOOP
C
        SAVE
        DATA NOOP,GOOP/1,0/
C
C  STATEMENT FUNCTIONS FOR MAPPING GRAPHICS OUTPUT
C
C     FX(XXX,YYY) = XXX
C     FY(XXX,YYY) = YYY
C
C  STATEMENT FUNCTION TO MAKE ARRAY ACCESS SEEM LIKE MATRIX ACCESS
C
      SCRTCH(IXX,IYY) = SCRARR(IYY+(IXX-1)*IYMAX)
C
C  DRAW AN ENTIRE CONTOUR LINE WHEN A POTENTIAL START POINT IS
C  PROVIDED
C
C  SAVE STARTING CELL
C
      XCS = XI
      YCS = YI
C
C  TEST IF VALID START POINT
C
      ICASE = ICA
      XC = XI
      YC = YI
      CALL CONCLD (ICASE,NOOP)
C
C  IF NO CONTOUR RETURN
C
      IF (ICASE.EQ.-1) RETURN
      IF (ICASE.EQ.0) RETURN
C
C  IF CONTOUR ALREADY DRAWN RETURN
C
      ILOC = IOR(ISHIFT(JX,ISHFCT),JY)
      IF (NP.EQ.0) GO TO    20
C
C TEST IF CONTOUR FOUND
C
          DO    10 I=1,NP
          IF (ILOC.NE.ICOORD(I)) GO TO    10
          RETURN
   10     CONTINUE
C
C  GET CORRECT OLD CASE
C
   20 IC = IOC
      IF (ICASE.EQ.IOC) IC = NC
C
C  SET UP STRUCTURE TO START IN OTHER DIRECTION FROM HERE IF CONTOUR
C  UNEXPECTLY ENDS IN THIS DIRECTION
C
      IFCASE = IC
      IFOCSE = ICASE
      FXO = XO
      FYO = YO
      LOOP = 1
C
C  SET UP IC TO SIMULATE EXIT FROM A PREVIOUS CELL
C
      IC = MOD(IC+2,4)
C
C  IF EXTRAPOLATING PASS ON
C
      IF (EXTRAP) GO TO    60
C
C  TEST IF CONTOUR EXCEEDED BORDER LIMITS
C  NOTE THAT ICASER CANNOT EQUAL 3 AT THIS POINT
C
      GO TO (   30,   40,   30,   50),ICASE
C
C  EXIT FROM BOTTOM
C
   30 IF (JX.GE.IXMAX) RETURN
      GO TO    60
C
C  EXIT FROM LEFT
C
   40 IF (JY.LE.ITLOC(JX*2 - 1)) RETURN
      GO TO    60
C
C  EXIT FROM RIGHT
C
   50 IF (JY.GE.ITLOC(JX*2 - 1)) RETURN
C
C  SAVE CELL INFO IF COMMING BACK
C
   60 TRT = TR
      BRT = BR
      TLT = TL
      BLT = BL
      IX = JX
      IY = JY
C
C  VALID CONTOUR START FOUND
C
      XX = FX(XO,YO)
      CALL FRSTD (XX,FY(XO,YO))
C
C  DRAW CONTOUR IN THIS CELL
C
   70 XX = FX(XN,YN)
      CALL VECTD (XX,FY(XN,YN))
      XCSTOR = XC
      YCSTOR = YC
      IXSTOR = IX
      IYSTOR = IY
      IOLDC = IC
      IC = ICASE
C
C  ENTER COORDINATE PAIR OF CONTOUR IN LIST
C
      NP = NP+1
      IF (NP.GT.MXXY) GO TO   180
      ICOORD(NP) = ILOC
C
C  BRANCH TO APPROPIATE CODE DEPENDING ON CONTOUR EXIT FROM THE CELL
C
   80 GO TO (   90,  110,  130,  150),IC
C
C  EXIT FORM BOTTOM
C  END CONTOUR IF ON CONVEX HULL
C
   90 IF (EXTRAP) GO TO   100
      IF (IY.LT.ITLOC(IX*2 - 1) .OR. IY-1.GT.ITLOC(IX*2)) GO TO   360
  100 TR = BR
      TL = BL
      XC = XC+STPSZ
C
C  IF ON BORDER END CONTOUR
C
      IX = IX+1
      IF (IX.GT.IXMAX) GO TO   360
      BR = SCRTCH(IX,IY)
      BL = SCRTCH(IX,IY-1)
      ILOC = IOR(ISHIFT(IX,ISHFCT),IY)
C
C  BRANCH IF CONTOUR CLOSED
C
      IF (IX.EQ.JX .AND. IY.EQ.JY) GO TO   170
      CALL CONCLD (ICASE,GOOP)
      IF (ICASE.EQ.-1) GO TO 360
      IF (ICASE.NE.0) GO TO    70
      GO TO   230
C
C  EXIT FROM LEFT SIDE
C  TEST IF IN CONVEX HULL
C
  110 IF (EXTRAP) GO TO   120
      IF (IY-1.LT.ITLOC( (IX-1)*2 - 1 ) .AND. IY-1.LT.ITLOC(IX*2 - 1))
     1    GO TO   360
  120 TR = TL
      BR = BL
      YC = YC-STPSZ
C
C  IF ON BORDER END CONTOUR
C
      IY = IY-1
      IF (IY.LT.2) GO TO   360
      TL = SCRTCH(IX-1,IY-1)
      BL = SCRTCH(IX,IY-1)
C
C  BRANCH IF CONTOUR CLOSED
C
      IF (IX.EQ.JX .AND. IY.EQ.JY) GO TO   170
      ILOC = IOR(ISHIFT(IX,ISHFCT),IY)
      CALL CONCLD (ICASE,GOOP)
      IF (ICASE.EQ.-1) GO TO 360
      IF (ICASE.NE.0) GO TO    70
      GO TO   230
C
C  EXIT FROM TOP
C  END CONTOUR IF OUT OF CONVEX HULL
C
  130 IF (EXTRAP) GO TO   140
      IF (IY.LT.ITLOC( (IX-1)*2 - 1 ) .OR. IY-1.GT.ITLOC( (IX-1)*2 ))
     1    GO TO   360
  140 BR = TR
      BL = TL
      XC = XC-STPSZ
C
C  END CONTOUR IF OUTSIDE OF BORDER
C
      IX = IX-1
      IF (IX.LT.2) GO TO   360
      TR = SCRTCH(IX-1,IY)
      TL = SCRTCH(IX-1,IY-1)
      ILOC = IOR(ISHIFT(IX,ISHFCT),IY)
C
C  BRANCH IF CONTOUR CLOSED
C
      IF (IX.EQ.JX .AND. IY.EQ.JY) GO TO   170
      CALL CONCLD (ICASE,GOOP)
      IF (ICASE.EQ.-1) GO TO 360
      IF (ICASE.NE.0) GO TO    70
      GO TO   230
C
C  EXIT FROM RIGHT SIDE
C  TEST IF ON CONVEX HULL
C
  150 IF (EXTRAP) GO TO   160
      IF (IY.GT.ITLOC( (IX-1)*2 ) .AND. IY.GT.ITLOC(IX*2)) GO TO   360
  160 TL = TR
      BL = BR
      YC = YC+STPSZ
C
C  IF ON BORDER END CONTOUR
C
      IY = IY+1
      IF (IY.GT.IYMAX) GO TO   360
      TR = SCRTCH(IX-1,IY)
      BR = SCRTCH(IX,IY)
      ILOC = IOR(ISHIFT(IX,ISHFCT),IY)
C
C  BRANCH IF CONTOUR CLOSED
C
      IF (IX.EQ.JX .AND. IY.EQ.JY) GO TO   170
      CALL CONCLD (ICASE,GOOP)
      IF (ICASE.EQ.-1) GO TO 360
      IF (ICASE.NE.0) GO TO    70
      GO TO   230
C
C  END THE CONTOUR
C
  170 CALL LASTD
      TR = TRT
      BR = BRT
      TL = TLT
      BL = BLT
      RETURN
C
C  CONTOUR STORAGE EXCEEDED TRY PACKING
C
  180 IF (IPACK.EQ.0) GO TO   200
      NP = 0
      ITEST = IOR(ISHIFT(JX,ISHFCT),JY)
          DO   190 K=1,MXXY
          IF (ICOORD(K).LE.ITEST) GO TO   190
          NP = NP+1
          ICOORD(NP) = ICOORD(K)
  190     CONTINUE
      IF (NP.LT.MXXY) GO TO    80
C
C  FAILURE NO MORE SPACE ABORT THIS CONTOUR LEVEL
C
  200 IHOLD(1:39) = ' CONDRW-CONTOUR STORAGE EXAUSTED LEVEL='
C
C  BLANK FILL THE ENCODE ARRAY
C
        IVOUT = '                       '
      WRITE(IVOUT,'(G13.5)')CONV
        IHOLD(40:62) = IVOUT
      CALL SETER (IHOLD,10,IREC)
      RETURN
C
C  BAD TIME THE CONTOUR EXITED A CORNER OF THE CELL MUST SEARCH FOR
C  NEW CELL
C
  230 IXSTP = IXSTOR
      IYSTP = IYSTOR
      GO TO (  240,  250,  260,  270),IOLDC
C
C PREVIOUS CELL BOTTOM EXIT
C
  240 IXSTP = IXSTP-1
      GO TO   280
C
C  PREVIOUS CELL LEFT EXIT
C
  250 IYSTP = IYSTP+1
      GO TO   280
C
C  PREVIOUS CELL TOP EXIT
C
  260 IXSTP = IXSTP+1
      GO TO   280
C
C  PREVIOUS CELL RIGHT EXIT
C
  270 IYSTP = IYSTP-1
C
C  BRANCH TO CURRENT CELL CASE
C
  280 GO TO (  290,  300,  310,  320),IC
C
C  APPARENT BOTTOM EXIT
C
  290 IXMOV(1) = 0
      IXMOV(2) = 1
      IYMOV(1) = -1
      IYMOV(2) = 1
      GO TO   330
C
C  APPARENT LEFT EXIT
C
  300 IXMOV(1) = 1
      IXMOV(2) = -1
      IYMOV(1) = 0
      IYMOV(2) = -1
      GO TO   330
C
C  APPARENT TOP EXIT
C
  310 IXMOV(1) = 0
      IXMOV(2) = -1
      IYMOV(1) = -1
      IYMOV(2) = 1
      GO TO   330
C
C  APPARENT RIGHT EXIT
C
  320 IXMOV(1) = 1
      IXMOV(2) = -1
      IYMOV(1) = 0
      IYMOV(2) = 1
C
C  SEARCH THE POSSIBLE CELLS
C
  330     DO   350 K=1,2
              DO   340 L=1,2
              XC = XCSTOR + STPSZ*REAL( IXMOV(K) )
              YC = YCSTOR + STPSZ*REAL( IYMOV(L) )
              IX = IXSTOR+IXMOV(K)
              IY = IYSTOR+IYMOV(L)
              ILOC = IOR(ISHIFT(IX,ISHFCT),IY)
C
C  IF BACK TO START END CONTOUR
C
              IF (IX.EQ.JX .AND. IY.EQ.JY) GO TO   170
C
C  IF AT PREVIOUS CELL SKIP PROCESSING
C
              IF (IX.EQ.IXSTP .AND. IY.EQ.IYSTP) GO TO   340
C
C  COMPUTE CELL VALUES
C
              TL = SCRTCH(IX-1,IY-1)
              BL = SCRTCH(IX,IY-1)
              TR = SCRTCH(IX-1,IY)
              BL = SCRTCH(IX,IY)
              ICASE = IC
              CALL CONCLD (ICASE,NOOP)
              IF (ICASE.EQ.-1) GO TO 360
              IF (ICASE.NE.0) GO TO    70
C
C  FAILURE TRY AGAIN
C
  340         CONTINUE
  350     CONTINUE
C
C  NO MORE CONTOUR TRY OTHER END OF LINE
C
  360 IF (LOOP.EQ.0) GO TO   170
      LOOP = 0
      IX = JX
      IY = JY
      TR = TRT
      TL = TLT
      BR = BRT
      BL = BLT
      IC = IFCASE
      ICASE = IC
      IOLDC = IFOCSE
      XC = XI
      YC = YI
      IXSTOR = IX
      IYSTOR = IY
      YCSTOR = YI
      XCSTOR = XI
      XX = FX(FXO,FYO)
      CALL LASTD
      CALL FRSTD (XX,FY(FXO,FYO))
      GO TO (   90,  110,  130,  150),IC
      END
