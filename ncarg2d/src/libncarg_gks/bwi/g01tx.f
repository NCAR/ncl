C
C	$Id: g01tx.f,v 1.1.1.1 1992-04-17 22:33:59 ncargd Exp $
C
      SUBROUTINE G01TX
C
C     PROCESS TEXT INSTRUCTION.
C
C ************************************************************************
C **                                                                    **
C **     NOTE:  Continuation across character partitions is not         **
C **            implemented.  This limits character strings to          **
C **            lengths less than 32768.                                **
C **                                                                    **
C ************************************************************************
C
C
      COMMON /GKSIN1/FCODE,CONT,IL1,IL2,ID(128),RL1,RL2,RX(128),
     - RY(128),STRL1,STRL2,RERR
      COMMON /GKSIN2/STR
      INTEGER FCODE, CONT, IL1, IL2, ID, RL1, RL2
      INTEGER STRL1, STRL2, RERR
      REAL  RX, RY
      CHARACTER*80 STR
      COMMON  /G01IO/   MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY(256)     ,
     +                  MOBFSZ  ,MOUTBF(720)    ,MBFPOS ,
     +                  MFGLUN  ,MXBITS         ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
        INTEGER         MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY  ,MOBFSZ ,
     +                  MBFPOS  ,MFGLUN ,MOUTBF ,MXBITS ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
      COMMON  /G01CHA/  MFNAME  ,MPNAME
      CHARACTER*80      MFNAME  ,MPNAME
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
      COMMON  /G01ARQ/  MRPLIX  ,MRLTYP ,ARLWSC ,MRPLCI ,
     +                  MRPMIX  ,MRMTYP ,ARMSZS ,MRPMCI ,
     +                  MRTXIX  ,MRTXP  ,MRTXAL(2)      ,MRCHH  ,
     +                  MRCHOV(4)       ,MRTXFO ,MRTXPR ,ARCHXP ,
     +                  ARCHSP  ,MRTXCI ,
     +                  MRFAIX  ,MRPASZ(4)      ,MRPARF(2)      ,
     +                  MRFAIS  ,MRFASI ,MRFACI ,
     +                  MRASF(13)
        INTEGER         MRPLIX  ,MRLTYP ,MRPLCI
        REAL            ARLWSC
        INTEGER         MRPMIX  ,MRMTYP ,MRPMCI
        REAL            ARMSZS
        INTEGER         MRTXIX  ,MRTXP  ,MRTXAL ,MRTXFO
        INTEGER         MRTXPR  ,MRTXCI ,MRCHH  ,MRCHOV
        REAL            ARCHXP  ,ARCHSP
        INTEGER         MRFAIX  ,MRPASZ ,MRPARF ,MRFAIS ,MRFASI
        INTEGER         MRFACI  ,MRASF
        INTEGER         MRAEQV(45)
        REAL            ARAEQV(45)
        EQUIVALENCE     (MRPLIX, MRAEQV, ARAEQV)
      COMMON  /G01AST/  MSPLIX  ,MSLTYP ,ASLWSC ,MSPLCI ,
     +                  MSPMIX  ,MSMTYP ,ASMSZS ,MSPMCI ,
     +                  MSTXIX  ,MSTXP  ,MSTXAL(2)      ,MSCHH  ,
     +                  MSCHOV(4)       ,MSTXFO ,MSTXPR ,ASCHXP ,
     +                  ASCHSP  ,MSTXCI ,
     +                  MSFAIX  ,MSPASZ(4)      ,MSPARF(2)      ,
     +                  MSFAIS  ,MSFASI ,MSFACI ,
     +                  MSASF(13)
        INTEGER         MSPLIX  ,MSLTYP ,MSPLCI
        REAL            ASLWSC
        INTEGER         MSPMIX  ,MSMTYP ,MSPMCI
        REAL            ASMSZS
        INTEGER         MSTXIX  ,MSTXP  ,MSTXAL ,MSTXFO
        INTEGER         MSTXPR  ,MSTXCI ,MSCHH  ,MSCHOV
        REAL            ASCHXP  ,ASCHSP
        INTEGER         MSFAIX  ,MSPASZ ,MSPARF ,MSFAIS ,MSFASI
        INTEGER         MSFACI  ,MSASF
        INTEGER         MSAEQV(45)
        REAL            ASAEQV(45)
        EQUIVALENCE     (MSPLIX, MSAEQV, ASAEQV)
      COMMON  /G01ADC/  VALCHG(37)      ,ANYASF ,AGPEND(4)      ,
     +                  IVPLIX  ,IVLTYP ,IVLWSC ,IVPLCI ,IVPMIX ,
     +                  IVMTYP  ,IVMSZS ,IVPMCI ,IVTXIX ,IVTXP  ,
     +                  IVTXAL  ,IVCHH  ,IVCHOV ,IVTXFO ,IVTXPR ,
     +                  IVCHXP  ,IVCHSP ,IVTXCI ,IVFAIX ,IVPASZ ,
     +                  IVPARF  ,IVFAIS ,IVFASI ,IVFACI ,IVASF  ,
     +                  IP2AEA(26)      ,IL2AEA(26)     ,
     +                  IALTYP  ,IALWSC ,IAPLCI ,IAMTYP ,IAMSZS ,
     +                  IAPMCI  ,IATXFP ,IACHXP ,IACHSP ,IATXCI ,
     +                  IAFAIS  ,IAFASI ,IAFACI ,
     +                  NCGASF  ,NGKASF ,MASMAP(18)
        LOGICAL         VALCHG  ,ANYASF ,AGPEND
        INTEGER         IVPLIX  ,IVLTYP ,IVLWSC ,IVPLCI ,IVPMIX
        INTEGER         IVMTYP  ,IVMSZS ,IVPMCI ,IVTXIX ,IVTXP
        INTEGER         IVTXAL  ,IVCHH  ,IVCHOV ,IVTXFO ,IVTXPR
        INTEGER         IVCHXP  ,IVCHSP ,IVTXCI ,IVFAIX ,IVPASZ
        INTEGER         IVPARF  ,IVFAIS ,IVFASI ,IVFACI ,IVASF
        INTEGER         IP2AEA  ,IL2AEA
        INTEGER         IALTYP  ,IALWSC ,IAPLCI ,IAMTYP ,IAMSZS
        INTEGER         IAPMCI  ,IATXFP ,IACHXP ,IACHSP ,IATXCI
        INTEGER         IAFAIS  ,IAFASI ,IAFACI
        INTEGER         NCGASF  ,NGKASF ,MASMAP
C
        LOGICAL         ASFCHG(13)
        EQUIVALENCE     (VALCHG(25),ASFCHG(1))
C
C
C  Id code parameters for every element, and class codes for each class.
C
      COMMON /G01OPC/ IDNOOP, IDBEGM, IDENDM, IDBEGP, IDBGPB, IDENDP
      COMMON /G01OPC/ IDMVER, IDMELT, IDDREP, IDCSEL, IDVEXT, IDVINT
      COMMON /G01OPC/ IDCREC, IDCLIN, IDPLIN, IDPMRK, IDTEXT, IDPGON
      COMMON /G01OPC/ IDCARY, IDGDP,  IDLBIX, IDLTYP, IDLWID, IDLCLR
      COMMON /G01OPC/ IDMBIX, IDMTYP, IDMSIZ, IDMCLR, IDTBIX, IDTFON
      COMMON /G01OPC/ IDTPRE, IDCHEX, IDCHSP, IDTCLR, IDCHHT, IDCHOR
      COMMON /G01OPC/ IDTXPA, IDTXAL, IDFBIX, IDINTS, IDFCLR, IDHAIX
      COMMON /G01OPC/ IDPTIX, IDFRPT, IDPTBL, IDPTSZ, IDCTBL, IDASFS
      COMMON /G01OPC/ IDESC,  IDMESS, IDAPLD, IDBKGC, IDDSCR, IDFLST
      COMMON /G01OPC/ CLDELM, CLMDES, CLPDES, CLCNTL, CLPRIM, CLPRAT
      COMMON /G01OPC/ CLESCE, CLEXTE
C
C  Parameter data types.
C
      INTEGER         IDNOOP, IDBEGM, IDENDM, IDBEGP, IDBGPB, IDENDP
      INTEGER         IDMVER, IDMELT, IDDREP, IDCSEL, IDVEXT, IDVINT
      INTEGER         IDCREC, IDCLIN, IDPLIN, IDPMRK, IDTEXT, IDPGON
      INTEGER         IDCARY, IDGDP,  IDLBIX, IDLTYP, IDLWID, IDLCLR
      INTEGER         IDMBIX, IDMTYP, IDMSIZ, IDMCLR, IDTBIX, IDTFON
      INTEGER         IDTPRE, IDCHEX, IDCHSP, IDTCLR, IDCHHT, IDCHOR
      INTEGER         IDTXPA, IDTXAL, IDFBIX, IDINTS, IDFCLR, IDHAIX
      INTEGER         IDPTIX, IDFRPT, IDPTBL, IDPTSZ, IDCTBL, IDASFS
      INTEGER         IDESC,  IDMESS, IDAPLD, IDBKGC, IDDSCR, IDFLST
      INTEGER         CLDELM, CLMDES, CLPDES, CLCNTL, CLPRIM, CLPRAT
      INTEGER         CLESCE, CLEXTE
C
C Class code parameters for every element.
C
      INTEGER         CLNOOP, CLBEGM, CLENDM, CLBEGP, CLBGPB, CLENDP
      INTEGER         CLMVER, CLMELT, CLDREP, CLCSEL, CLVEXT, CLVINT
      INTEGER         CLCREC, CLCLIN, CLPLIN, CLPMRK, CLTEXT, CLPGON
      INTEGER         CLCARY, CLGDP,  CLLBIX, CLLTYP, CLLWID, CLLCLR
      INTEGER         CLMBIX, CLMTYP, CLMSIZ, CLMCLR, CLTBIX, CLTFON
      INTEGER         CLTPRE, CLCHEX, CLCHSP, CLTCLR, CLCHHT, CLCHOR
      INTEGER         CLTXPA, CLTXAL, CLFBIX, CLINTS, CLFCLR, CLHAIX
      INTEGER         CLPTIX, CLFRPT, CLPTBL, CLPTSZ, CLCTBL, CLASFS
      INTEGER         CLESC,  CLMESS, CLAPLD, CLBKGC, CLDSCR, CLFLST
C
C  Equivalence all individual class code parameters to the single
C  code for the class in which the element(s) belong.
C
      EQUIVALENCE (CLDELM, CLNOOP,CLBEGM,CLENDM,CLBEGP,CLBGPB,CLENDP)
      EQUIVALENCE (CLMDES, CLMVER,CLMELT,CLDREP,CLDSCR,CLFLST)
      EQUIVALENCE (CLPDES, CLCSEL,CLVEXT,CLBKGC)
      EQUIVALENCE (CLCNTL, CLVINT,CLCREC,CLCLIN)
      EQUIVALENCE (CLPRIM, CLPLIN,CLPMRK,CLTEXT,CLPGON,CLCARY,CLGDP)
      EQUIVALENCE (CLPRAT, CLLBIX,CLLTYP,CLLWID,CLLCLR,CLMBIX,CLMTYP)
      EQUIVALENCE (CLPRAT, CLMSIZ,CLMCLR,CLTBIX,CLTFON,CLTPRE,CLCHEX)
      EQUIVALENCE (CLPRAT, CLCHSP,CLTCLR,CLCHHT,CLCHOR,CLTXPA,CLTXAL)
      EQUIVALENCE (CLPRAT, CLFBIX,CLINTS,CLFCLR,CLHAIX,CLPTIX,CLFRPT)
      EQUIVALENCE (CLPRAT, CLPTBL,CLPTSZ,CLCTBL,CLASFS)
      EQUIVALENCE (CLESCE, CLESC), (CLEXTE, CLMESS,CLAPLD)
      COMMON /GKSENU/ GYES,  GNO,   GCONDI, GALWAY, GACTIV, GINACT
      COMMON /GKSENU/ GEMPTY,GNEMPT,GPEND,  GNPEND, GCLIP , GNCLIP
C
      INTEGER         GYES,  GNO,   GCONDI, GALWAY, GACTIV, GINACT
      INTEGER         GEMPTY,GNEMPT,GPEND,  GNPEND, GCLIP , GNCLIP
C
      INTEGER  NUMO(2), KALL, IPRIM, NBYTES
      INTEGER  GFINAL, G01PBL
      SAVE KALL
C
C
      DATA KALL/0/,  IPRIM/3/,  GFINAL/1/
C
      KALL = KALL+1
      IF  (KALL.EQ.1) THEN
C
C       FIRST CALL OF TEXT ELEMENT.
C
C       SET WSL ENTRY "DISPLAY SURFACE EMPTY" TO "NOT EMPTY"
C
        MDEMPT = GNEMPT
C
C       PROCESS PENDING ATTRIBUTES.
C
        IF (AGPEND(IPRIM)) THEN
C
C          SOME CHANGES ARE PENDING.
C
           IF (VALCHG(IVTXIX))  THEN
C
C             GKS TEXT INDEX, SEND CGM TEXT BUNDLE INDEX.
C
              NBYTES = 1+(MIXFW-1)/8
              CALL GPUTNI (CLTBIX, IDTBIX, NBYTES, RERR)
              CALL GPUTPR (MRTXIX, MIXFW, 1, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              MSTXIX         = MRTXIX
              VALCHG(IVTXIX) = .FALSE.
           END IF
           IF (VALCHG(IVTXFO))  THEN
C
C             FONT COMPONENT OF GKS TEXT FONT/PRECISION, SEND
C             CGM TEXT PRECISION.
C
              NBYTES = 1+(MEFW-1)/8
              CALL GPUTNI (CLTFON, IDTFON, NBYTES, RERR)
              CALL GPUTPR (MRTXFO, MEFW, 1, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              MSTXFO         = MRTXFO
              VALCHG(IVTXFO) = .FALSE.
           END IF
           IF (VALCHG(IVTXPR)) THEN
C
C             PRECISION COMPONENT OF GKS TEXT FONT/PRECISION,
C             SEND CGM TEXT PRECSION.
C
              NBYTES         = 1+(MIXFW-1)/8
              CALL GPUTNI (CLTPRE, IDTPRE, NBYTES, RERR)
              CALL GPUTPR (MRTXPR, MIXFW, 1, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              MSTXPR         = MRTXPR
              VALCHG(IVTXPR) = .FALSE.
           END IF
           IF (VALCHG(IVCHXP)) THEN
C
C             GKS CHARACTER EXPANSION FACTOR, SEND CGM
C             CHARACTER EXPANSION FACTOR.
C
              NBYTES = 1+(2*MCFPP-1)/8
              CALL GPUTNI (CLCHEX, IDCHEX, NBYTES, RERR)
              CALL GFLCNV (ARCHXP, NUMO)
              CALL GPUTPR (NUMO, MCFPP, 2, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              ASCHXP         = ARCHXP
              VALCHG(IVCHXP) = .FALSE.
           END IF
           IF (VALCHG(IVCHSP)) THEN
C
C             GKS CHARACTER SPACING, SEND CGM CHARACTER SPACING.
C
              NBYTES = 1+(2*MCFPP-1)/8
              CALL GPUTNI (CLCHSP, IDCHSP, NBYTES, RERR)
              CALL GFLCNV (ARCHSP, NUMO)
              CALL GPUTPR (NUMO, MCFPP, 2, RERR)
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              ASCHSP         = ARCHSP
              VALCHG(IVCHSP) = .FALSE.
           END IF
           IF (VALCHG(IVTXCI)) THEN
C
C             GKS TEXT COLOR INDEX, SEND CGM TEXT COLOR.
C
              NBYTES = 1+(MCIXFW-1)/8
              CALL GPUTNI (CLTCLR, IDTCLR, NBYTES, RERR)
              CALL GPUTPR (MRTXCI, MCIXFW, 1, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              MSTXCI         = MRTXCI
              VALCHG(IVTXCI) = .FALSE.
           END IF
           IF (VALCHG(IVTXP)) THEN
C
C             GKS TEXT PATH, SEND CGM TEXT PATH.
C
              NBYTES = 1+(MEFW-1)/8
              CALL GPUTNI (CLTXPA, IDTXPA, NBYTES, RERR)
              CALL GPUTPR (MRTXP , MEFW,  1, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              MSTXP         = MRTXP
              VALCHG(IVTXP) = .FALSE.
           END IF
           IF (VALCHG(IVTXAL)) THEN
C
C             GKS TEXT ALIGNMENT, SEND CGM TEXT ALIGNMENT.
C
              NBYTES = 1+(2*MEFW + 4*MCFPP - 1)/8
              CALL GPUTNI (CLTXAL, IDTXAL, NBYTES, RERR)
C
C             PUT IN GKS/CGM DISCRETE ALIGNMENT VALUES.
              CALL GPUTPR (MRTXAL, MEFW, 2, RERR)
C
C             ADD (UNUSED) CGM CONTINUOUS PARAMETERS.
              NUMO(1) = 0
              NUMO(2) = 0
              CALL GPUTPR (NUMO, MCFPP, 2, RERR)
              CALL GPUTPR (NUMO, MCFPP, 2, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              MSTXAL(1)      = MRTXAL(1)
              MSTXAL(2)      = MRTXAL(2)
              VALCHG(IVTXAL) = .FALSE.
           END IF
           IF (VALCHG(IVCHH)) THEN
C
C             GKS CHARACTER HEIGHT (DERIVED FROM INTERFACE VECTOR PAIR),
C             PUT OUT CGM CHARACTER HEIGHT.
C
              NBYTES = 1+(MVDCFW-1)/8
              CALL GPUTNI (CLCHHT, IDCHHT, NBYTES, RERR)
              CALL GPUTPR (MRCHH, MVDCFW,  1, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              MSCHH         = MRCHH
              VALCHG(IVCHH) = .FALSE.
           END IF
           IF (VALCHG(IVCHOV)) THEN
C
C             GKS CHARACTER UP (AS PER INTERFACE VECTOR PAIR),
C             SEND CGM CHARACTER ORIENTATION (VECTOR PAIR).
C
              NBYTES = 1+(4*MVDCFW-1)/8
              CALL GPUTNI (CLCHOR, IDCHOR, NBYTES, RERR)
              CALL GPUTPR (MRCHOV(1) , MVDCFW,  4, RERR)
              IF (RERR .NE. 0) RETURN
C
C             SET SENT VALUE TO REQUESTED, CLEAR CHANGE FLAG.
C
              MSCHOV(1)      = MRCHOV(1)
              MSCHOV(2)      = MRCHOV(2)
              MSCHOV(3)      = MRCHOV(3)
              MSCHOV(4)      = MRCHOV(4)
              VALCHG(IVCHOV) = .FALSE.
           END IF
           IF (ANYASF)  THEN
C
C             SOME ASF HAS CHANGED.
C
              CALL G01SAS (IPRIM, RERR)
              IF (RERR.NE.0)  RETURN
           END IF
C
C          CLEAR AGGREGATE CHANGE VARIABLE.
C
           AGPEND(IPRIM) = .FALSE.
        END IF
C
C       TREAT FIRST CALL, PUT OUT OPCODE, AND POINTS
C
C
C       PUT OUT OPCODE (CLASS AND ID) AND TOTAL LENGTH.
C
        NBYTES = G01PBL (STRL1, 1+(MEFW+2*MVDCFW-1)/8 )
        CALL GPUTNI (CLTEXT, IDTEXT, NBYTES, RERR)
C
C       PUT OUT STARTING POINT.
C
C       TRUNCATE POINT TO LIMITS OF NDC UNIT SQUARE, CONVERT TO VDC,
C       AND STORE IN MPXPY.
C
        MPXPY(1) = MXOFF + IFIX(FLOAT(MXSCAL)*
     -                 (AMAX1(0.,AMIN1(1.0,RX(1)))))
        MPXPY(2) = MYOFF + IFIX(FLOAT(MYSCAL)*
     -                 (AMAX1(0.,AMIN1(1.0,RY(1)))))
        CALL GPUTPR (MPXPY, MVDCFW, 2, RERR)
C
C       PUT OUT FINAL FLAG.
        CALL GPUTPR (GFINAL, MEFW, 1, RERR)
C
C       PUT OUT FIRST (AND PERHAPS ONLY) PART OF CHARACTER STRING
        CALL GPUTPS (STR, STRL1, STRL2, 0, RERR)
        IF (RERR .NE. 0) RETURN
C
C       END OF PROCESSING FOR FIRST CALL OF ELEMENT.
C
      ELSE IF (KALL .GT. 1) THEN
C
C       THIS IS A CONTINUATION CALL FOR THE ELEMENT.
C
        CALL GPUTPS (STR, STRL1, STRL2, 1, RERR)
        IF (RERR .NE. 0) RETURN
      END IF
C
C     IF THERE IS TO BE NO CONTINUATION, RESET THE PARAMETER "KALL".
C
      IF (CONT .EQ. 0) THEN
         KALL = 0
      END IF
C
      RETURN
C
      END
