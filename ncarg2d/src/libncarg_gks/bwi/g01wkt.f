C
C	$Id: g01wkt.f,v 1.1.1.1 1992-04-17 22:33:59 ncargd Exp $
C
        SUBROUTINE G01WKT
C
C       PROCESS WORKSTATION TRANSFORMATION.
C
      COMMON /GKSIN1/FCODE,CONT,IL1,IL2,ID(128),RL1,RL2,RX(128),
     - RY(128),STRL1,STRL2,RERR
      COMMON /GKSIN2/STR
      INTEGER FCODE, CONT, IL1, IL2, ID, RL1, RL2
      INTEGER STRL1, STRL2, RERR
      REAL  RX, RY
      CHARACTER*80 STR
      COMMON  /G01WSL/  MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN  ,
     +                  MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS   ,
     +                  RWINDO(4)       ,CWINDO(4)      ,
     +                  RWKVP (4)       ,CWKVP (4)      ,
     +                  MOLMAX  ,MOL    ,MCOVFL ,MCSORT ,MCOLI(256),
     +                  SRED(256)       ,SGREEN(256)    ,SBLUE(256),
     +                  MRCREC(4)       ,MRCLIP
        INTEGER         MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN
        INTEGER         MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS
        INTEGER         MOLMAX  ,MOL    ,MCOVFL ,MCSORT ,MCOLI
        REAL            RWINDO          ,CWINDO
        REAL            RWKVP           ,CWKVP
        REAL            SRED            ,SGREEN         ,SBLUE
        INTEGER         MRCREC  ,MRCLIP
      COMMON  /G01INS/  MCODES  ,MCONTS ,
     +                  MVDCFW  ,MCIXFW ,MDCCFW ,MIXFW  ,MINTFW ,
     +                  MDCCRG  ,MXOFF  ,MXSCAL ,MYOFF  ,MYSCAL ,
     +                  MINXVD  ,MAXXVD ,MINYVD ,MAXYVD ,
     +                  MCFRM   ,MCOPCL ,MCOPID ,MCNBYT ,
     +                  MCCBYT  ,MCFPP  ,MSLFMT ,MEFW   ,MCTCHG ,
     +                  MBCCHG
        INTEGER         MCODES  ,MCONTS
        INTEGER         MVDCFW  ,MCIXFW ,MDCCFW ,MIXFW  ,MINTFW
        INTEGER         MDCCRG  ,MXOFF  ,MXSCAL ,MYOFF  ,MYSCAL
        INTEGER         MINXVD  ,MAXXVD ,MINYVD ,MAXYVD
        INTEGER         MCFRM   ,MCOPCL ,MCOPID ,MCNBYT
        INTEGER         MCCBYT  ,MCFPP  ,MSLFMT ,MEFW   ,MCTCHG
        INTEGER         MBCCHG
C
C       WINDOW OR VIEWPORT?
C
        GOTO (10,20)  MCODES-70
C
C          WORKSTATION WINDOW.
C
10      CONTINUE
C
C          (ERROR CHECK OF RECTANGLE DEFINITION ABOVE WSI IS ASSUMED)
C          SET 'REQUESTED' AND 'CURRENT' WINDOW IN WSL.
C
           CWINDO(1) = RX(1)
           CWINDO(2) = RX(2)
           CWINDO(3) = RY(1)
           CWINDO(4) = RY(2)
           RWINDO(1) = RX(1)
           RWINDO(2) = RX(2)
           RWINDO(3) = RY(1)
           RWINDO(4) = RY(2)
           GOTO 70
C
C          WORKSTATION VIEWPORT.
C
20      CONTINUE
C
C          (RECTANGLE DEFINITION CHECK ABOVE WSI IS ASSUMED)
C          CHECK LIMITS AND STORE IN WSL.
C
           IF (RX(1).LT.0. .OR. RX(2).GT.32767. .OR.
     +         RY(1).LT.0. .OR. RY(2).GT.32767.) THEN
C
C             VIEWPORT DEFINITION OUT OF BOUNDS.
C
              RERR = 54
              GOTO 70
           ELSE
C
C             SET 'REQUESTED' AND 'CURRENT' VIEWPORT IN WSL.
C
              RWKVP(1) = RX(1)
              RWKVP(2) = RX(2)
              RWKVP(3) = RY(1)
              RWKVP(4) = RY(2)
              CWKVP(1) = RX(1)
              CWKVP(2) = RX(2)
              CWKVP(3) = RY(1)
              CWKVP(4) = RY(2)
           END IF
C
C
C
70      CONTINUE
        RETURN
C
        END
