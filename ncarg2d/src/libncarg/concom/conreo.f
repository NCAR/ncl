C
C	$Id: conreo.f,v 1.5 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONREO (MAJLNS)
C
C THIS ROUTINE PUTS THE MAJOR (LABELED) LEVELS IN THE BEGINNING OF CL
C AND THE MINOR (UNLABELED) LEVELS IN END OF CL.  THE NUMBER OF MAJOR
C LEVELS IS RETURNED IN MAJLNS.  PV IS USED AS A WORK SPACE.  MINGAP IS
C THE NUMBER OF MINOR GAPS (ONE MORE THAN THE NUMBER OF MINOR LEVELS
C BETWEEN MAJOR LEVELS).
C
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONR18/ TITLE      ,ICNT   ,ITLSIZ
      COMMON /CONR20/ NREP       ,NCRT       ,ISIZEL     ,
     1                MINGAP     ,ISIZEM         ,
     2                TENS
      LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10    NDASH, IDASH, EDASH
C
        SAVE
C
      NL = NCL
      IF (NL.LE.4 .OR. MINGAP.LE.1) GO TO   160
      NML = MINGAP-1
      IF (NL.LE.10) NML = 1
C
C CHECK FOR BREAK POINT IN THE LIST OF CONTOURS FOR A MAJOR LINE
C
      NMLP1 = NML+1
          DO    10 I=1,NL
          ISAVE = I
          IF (CL(I).EQ.BPSIZ) GO TO    40
   10     CONTINUE
C
C  NO BREAKPOINT FOUND SO TRY FOR A NICE NUMBER
C
      L = NL/2
      L = ALOG10( ABS( CL(L) ) )+1.
      Q = 10.**L
          DO    30 J=1,3
          Q = Q/10.
              DO    20 I=1,NL
              ISAVE = I
              IF (MOD( ABS( CL(I) + 1.E-9*CL(I) )/Q,REAL(NMLP1) ).LE.
     1            .0001) GO TO    40
   20         CONTINUE
   30     CONTINUE
      ISAVE = NL/2
C
C PUT MAJOR LEVELS IN PV
C
   40 ISTART = MOD(ISAVE,NMLP1)
      IF (ISTART.EQ.0) ISTART = NMLP1
      NMAJL = 0
          DO    50 I=ISTART,NL,NMLP1
          NMAJL = NMAJL+1
          PV(NMAJL) = CL(I)
   50     CONTINUE
      MAJLNS = NMAJL
      L = NMAJL
C
C PUT MINOR LEVELS IN PV
C
      IC = NML/2 + 1
      L = MAJLNS+1
          DO   100 LOOP=1,NML
          IC1 = IC
              DO    90 IWCH=1,2
              IF (LOOP.EQ.1) GO TO    60
              IC1 = IC+(LOOP-1)
              IF (IWCH.EQ.2) IC1 = IC-(LOOP-1)
              IF (IC1.GE.NMLP1) GO TO    90
              IF (IC1.LE.0) GO TO    90
   60             DO    70 K=ISTART,NL,NMLP1
                  IND = K+IC1
                  IF (IND.GT.NL) GO TO    80
                  PV(L) = CL(IND)
                  L = L+1
   70             CONTINUE
   80         IF (LOOP.EQ.1) GO TO   100
   90         CONTINUE
  100     CONTINUE
C
C  IF MAJOR LINES DID NOT START ON THE FIRST ENTRY PICK UP THE MISSING
C  LEVELS
C
      IF (ISTART.EQ.1) GO TO   140
          DO   130 LOOP=1,NML
          IC1 = IC
              DO   120 IWCH=1,2
              IF (LOOP.EQ.1) GO TO   110
              IC1 = IC+(LOOP-1)
              IF (IWCH.EQ.2) IC1 = IC-(LOOP-1)
  110         IF (IC1.GE.ISTART) GO TO   120
              IF (IC1.LE.0) GO TO   120
              PV(L) = CL(IC1)
              L = L+1
              IF (LOOP.EQ.1) GO TO   130
  120         CONTINUE
  130     CONTINUE
C
C PUT REORDERED ARRAY BACK IN ORIGINAL PLACE
C
  140     DO   150 I=1,NL
          CL(I) = PV(I)
  150     CONTINUE
      RETURN
  160 MAJLNS = NL
      RETURN
      END
