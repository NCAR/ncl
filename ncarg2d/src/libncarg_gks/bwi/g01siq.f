C
C	$Id: g01siq.f,v 1.1.1.1 1992-04-17 22:33:59 ncargd Exp $
C
        SUBROUTINE G01SIQ
C
C       WORKSTATION STATE LIST (WSL) INQUIRY.
C             (OPCODE -200 THRU -299)
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
C
        INTEGER  INDX, IPTR, I, NCI
C
C
C        CODE -200 -201 -202
        GOTO  (200, 201, 202) -199-MCODES
C
C        CODE -226
        IF (MCODES .EQ. -226)  GOTO 226
C
C        CODE -256 -257
        GOTO  (256, 257) -255-MCODES
C
C        CODE -290 -291 -292 -293
        GOTO  ( 39,  39,  39,  39) -289-MCODES
C
C       FALL THROUGH MEANS UNDEFINED OPCODE.
C
        RERR = 320
        RETURN
C
C       ERROR 39, "SPECIFIED WORKSTATION IS NEITHER OF CATEGORY OUTPUT
C       NOR OF CATEGORY OUTIN".
C
39      CONTINUE
        RERR = 39
        RETURN
C
C       INQUIRE WORKSTATION DEFERRAL AND UPDATE STATE.
C
200     CONTINUE
        ID(2) = MDEFMO
        ID(3) = MREGMO
        ID(4) = MDEMPT
        ID(5) = MNFRAM
        RETURN
C
C       INQUIRE WORKSTATION STATE.
C
201     CONTINUE
        ID(2) = MSTATE
        RETURN
C
C       INQUIRE WORKSTATION TRANSFORMATION.
C
202     CONTINUE
        ID(2) = MTUS
        RX(1) = RWINDO(1)
        RX(2) = RWINDO(2)
        RX(3) = RWINDO(3)
        RX(4) = RWINDO(4)
        RX(5) = CWINDO(1)
        RX(6) = CWINDO(2)
        RX(7) = CWINDO(3)
        RX(8) = CWINDO(4)
        RY(1) = RWKVP(1)
        RY(2) = RWKVP(2)
        RY(3) = RWKVP(3)
        RY(4) = RWKVP(4)
        RY(5) = CWKVP(1)
        RY(6) = CWKVP(2)
        RY(7) = CWKVP(3)
        RY(8) = CWKVP(4)
        RETURN
C
C       INQUIRE WORKSTATION CONNECTION AND TYPE.
C
226     CONTINUE
        ID(2) = MCONID
        ID(3) = MWTYPE
        RETURN
C
C     INQUIRE COLOR REPRESENTATION.
C
256     CONTINUE
C
C       SEARCH FOR INDEX IN INDEX LIST.
C
        INDX = ID(2)
        IPTR = 0
        DO 2561 I=1,MOL
           IF (INDX.EQ.MCOLI(I))  THEN
              IPTR = I
              GOTO 2562
           END IF
2561    CONTINUE
2562    CONTINUE
        IF (IPTR.NE.0)  THEN
C
C          INDEX WAS FOUND, COPY COLOR COMPONENTS.
           RX(1) = SRED(IPTR)
           RX(2) = SGREEN(IPTR)
           RX(3) = SBLUE(IPTR)
        ELSE
C
           RERR = 94
        END IF
        RETURN
C
C       INQUIRE LIST ELEMENT OF COLOR INDICES.
C
257     CONTINUE
C
C       RETURN THE COUNT BEFORE CHECKING FOR THE ERROR CONDITION
C       (INDEX HAS NOT BEEN DEFINED).
C
        ID(3) = MOL
C
C       CHECK THAT INDEX IS DEFINED.
C
        NCI = ID(2)
        IF (NCI .GT. MOL)  THEN
C
C          UNDEFINED.
           RERR = 502
        ELSE
C
C          DEFINED.
           ID(4) = MCOLI(NCI)
        END IF
        RETURN
C
        END
