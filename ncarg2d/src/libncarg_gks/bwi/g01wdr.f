C
C	$Id: g01wdr.f,v 1.1.1.1 1992-04-17 22:33:59 ncargd Exp $
C
        SUBROUTINE G01WDR
C
C***********************************************************************
C***********************************************************************
C**                                                                   **
C**                                                                   **
C**            WORKSTATION DRIVER FOR WORKSTATION TYPE 1              **
C**                       NCAR CGM GENERATOR                          **
C**                                                                   **
C**                                                                   **
C***********************************************************************
C***********************************************************************
C
C
        IMPLICIT INTEGER (A-Z)
        CHARACTER*60 BEGSTR
C
C
C       DEFINITION OF ALL COMMONS USED BY WORKSTATION.
C
      COMMON /GKSIN1/FCODE,CONT,IL1,IL2,ID(128),RL1,RL2,RX(128),
     - RY(128),STRL1,STRL2,RERR
      COMMON /GKSIN2/STR
      INTEGER FCODE, CONT, IL1, IL2, ID, RL1, RL2
      INTEGER STRL1, STRL2, RERR
      REAL  RX, RY
      CHARACTER*80 STR
      COMMON  /G01WDT/  LWTYPE, LWKCAT, MVERSN
        INTEGER         LWTYPE, LWKCAT, MVERSN
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
      COMMON  /G01ADF/  MDPLIX  ,MDLTYP ,ADLWSC ,MDPLCI ,
     +                  MDPMIX  ,MDMTYP ,ADMSZS ,MDPMCI ,
     +                  MDTXIX  ,MDTXP  ,MDTXAL(2)      ,MDCHH  ,
     +                  MDCHOV(4)       ,MDTXFO ,MDTXPR ,ADCHXP ,
     +                  ADCHSP  ,MDTXCI ,
     +                  MDFAIX  ,MDPASZ(4)       ,MDPARF(2)     ,
     +                  MDFAIS  ,MDFASI  ,MDFACI  ,
     +                  MDASF(13)
        INTEGER         MDPLIX  ,MDLTYP ,MDPLCI
        REAL            ADLWSC
        INTEGER         MDPMIX  ,MDMTYP ,MDPMCI
        REAL            ADMSZS
        INTEGER         MDTXIX  ,MDTXP  ,MDTXAL ,MDTXFO
        INTEGER         MDTXPR  ,MDTXCI ,MDCHH  ,MDCHOV
        REAL            ADCHXP  ,ADCHSP
        INTEGER         MDFAIX  ,MDPASZ ,MDPARF ,MDFAIS ,MDFASI
        INTEGER         MDFACI  ,MDASF
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
        INTEGER  G01PBL
        CHARACTER FNAME*80,DNAME*1
C
      DATA BEGSTR
     - /'                                                            '/
C
C     INITIALIZE FOR THIS ENTRY.
C
C       CLEAR WSI ERROR PARAMETER.
C
        RERR = 0
C
C       CHECK FCODE IF LAST INVOCATION INDICATED CONTINUATION.
C
        IF (MCONTS.EQ.1 .AND. FCODE.NE.MCODES)  GOTO 905
C
C       COPY AND SAVE FUNCTION CODE AND CONTINUATION FLAG.
C
        MCODES = FCODE
        MCONTS = CONT
C
C     BRANCH FOR FUNCTION PROCESSING.  TESTING OF CODE ORGANIZED TO
C     MINIMIZE NUMBER OF TESTS, BY TESTING FOR MOST FREQUENTLY EXPECTED
C     FUNCTIONS FIRST.
C
C
C       OUTPUT PRIMITIVES.
C
C        CODE   11   12   13   14   15   16
        GOTO  (110, 120, 130, 140, 150, 160)  MCODES-10
C
C
C       PRIMITIVE ATTRIBUTES.
C
        IF (MCODES.GE.21 .AND. MCODES.LE.43)  THEN
C
C          INVOKE PRIMITIVE ATTRIBUTE PROCESSING ROUTINE AND RETURN.
C
           CALL G01PAT
           RETURN
        END IF
C
C       CONTROL AND ESCAPE FUNCTIONS.
C
C        CODE   -3   -2   -1    0    1
        GOTO  (210, 220, 230, 240, 250,
     +         910, 270, 910, 910, 300)  MCODES+4
C        CODE    2    3    4    5    6
C
C
C       CLIPPING CONTROL FUNCTION(S).
C
        IF (MCODES.EQ.61)  THEN
           CALL G01CLP
           RETURN
        END IF
C
C
C       COLOR TABLE.
C
        IF (MCODES.EQ.56)  THEN
           CALL G01CTB
           RETURN
        END IF
C
C       NEW PICTURE INITIALIZATION.
C
        IF (MCODES .EQ. 91)  THEN
           CALL G01SNP(RERR)
           RETURN
        END IF
C
C       SET PICTURE NAME.
C
        IF (MCODES.EQ.92) THEN
           MPNAME(1:60) = STR(1:60)
           RETURN
        ENDIF
C
C       WORKSTATION TRANSFORMATION.
C
        IF (MCODES.EQ.71 .OR. MCODES.EQ.72)  THEN
           CALL G01WKT
           RETURN
        END IF
C
C       SEGMENTATION FUNCTIONS
C
        IF (MCODES.GE.80 .AND. MCODES.LE.84) THEN
           CALL G01SEG
           RETURN
        ENDIF
C
C       SET FILE NAME FOR OUTPUT METAFILE
C
        IF (MCODES.EQ.90) THEN
           MFNAME = STR
           CALL GTNLEN(MFNAME,ILEN,IER)
           MFNAME(ILEN+1:ILEN+1) = CHAR(0)
           RETURN
        ENDIF
C
C
C       WDT INQUIRY (-100 THRU -199)
C
        IF (MCODES.LE.-100 .AND. MCODES.GE.-199)  THEN
           CALL G01DIQ
           RETURN
        END IF
C
C
C       WSL INQUIRY (-200 THRU -299)
C
        IF (MCODES.LE.-200 .AND. MCODES.GE.-299)  THEN
           CALL G01SIQ
           RETURN
        END IF
C
C
C       FALL THROUGH MEANS UNDEFINED OPCODE.
C
C
        GOTO 910
C
C **********************************************************************
C *                                                                    *
C *          ========== OUTPUT PRIMITIVE PROCESSING ==========         *
C *                                                                    *
C **********************************************************************
C
C
C     POLYLINE.
C
110     CONTINUE
        CALL G01PL
        RETURN
C
C     POLYMARKER.
C
120     CONTINUE
        CALL G01PM
        RETURN
C
C     TEXT.
C
130     CONTINUE
        CALL G01TX
        RETURN
C
C     FILL AREA.
C
140     CONTINUE
        CALL G01FA
        RETURN
C
C     CELL ARRAY.
C
150     CONTINUE
        CALL G01CA
        RETURN
C
C     GDP  (NOT SUPPORTED AT ALL, JUST GENERATES ERROR).
C
160     CONTINUE
        RERR = 104
        RETURN
C
C **********************************************************************
C *                                                                    *
C *         ---------- CONTROL FUNCTION PROCESSING ----------          *
C *                                                                    *
C **********************************************************************
C
C     OPEN WORKSTATION.
C
210     CONTINUE
C
C       CALL ROUTINE TO FLAG MEMORY RESIDENT FILES FOR UNIX ICTRANS USE.
C
        CALL GOPNIC(IDUM)
C
C       MARK WORKSTATION OPEN IN WSL, SET WORKSTATION ID, CONNECTION
C       ID, WORKSTATION TYPE.
C
        MOPEN  = GYES
        MWKID  = ID(1)
        MCONID = ID(2)
        MWTYPE = ID(3)
C
C       INITIALIZE METACODE RECORD NUMBER
C
        MRECNM = 1
C
C       COPY LUN FOR OUTPUT.
C
        MFGLUN = MCONID
C
C       OPEN METACODE OUTPUT UNIT
C
        FNAME = MFNAME
        CALL GTNLEN(FNAME,ILEN,IER)
        FNAME(ILEN+1:ILEN+1) = CHAR(0)
        CALL G01MIO (1, MFGLUN, FNAME(1:ILEN), MOUTBF, 1, RERR)
        IF (RERR.NE.0)  RETURN
C
C       INITIALIZE CURRENT-BUFFER-POSITION POINTER.
C
        MBFPOS = 32
C
C       SET NUMBER OF BITS PER METACODE RECORD.
C
        MXBITS = 11520
C
C       COMPUTE NUMBER OF INTEGER WORDS NEEDED FOR OUTPUT BUFFER
C       (MOUTBF)
C
        MOBFSZ = 1 + (MXBITS-1)/I1MACH(5)
C
C       SET DATA TYPE FOR NEW METACODE
C
        MDTYPE = 3
C
C       INITIALIZE WORKSTATION STATE LIST PARAMETERS.
C
        CALL  G01IWS
C
C       INITIALIZE ATTRIBUTE CONTEXT.
C
        CALL  G01IAC
C
C       INITIALIZE NEW-FRAME, BEGIN-METAFILE, END-METAFILE FLAGS.
C
        MNFFLG = GNO
        MBMFLG = GYES
        MEMFLG = GNO
C
C       GENERATE BEGIN METAFILE INSTRUCTION, 60 BLANK CHARACTERS.
C
        CALL GPUTNI (CLBEGM, IDBEGM, G01PBL(60,0), RERR)
        CALL GPUTPS (BEGSTR, 60, 60, 0, RERR)
        IF (RERR.NE.0)  RETURN
C
C       PUT OUT METAFILE DESCRIPTOR AND FLUSH.
C
        CALL GPUTMD (RERR)
        CALL G01FLB (RERR)
        IF (RERR.NE.0)  RETURN
C
C       INITIALIZE CURRENT FRAME SEQUENCE NUMBER.
C
        MCFRM = 0
        RETURN
C
C     ACTIVATE WORKSTATION.
C
220     CONTINUE
        MSTATE = GACTIV
        RETURN
C
C     DEACTIVATE WORKSTATION.
C
230     CONTINUE
        MSTATE = GINACT
        RETURN
C
C     CLOSE WORKSTATION.
C
240     CONTINUE
C
C       MARK WORKSTATION CLOSED, FLUSH BUFFER, CLOSE THE OUTPUT UNIT
C
        MOPEN = GNO
C       SET PARAMETER TO TELL G01CLW TO CONDITIONALLY FINISH PICTURE.
        ID(2) = GCONDI
        CALL G01CLW
C
C      PUT OUT END METAFILE RECORD.
C
        MNFFLG = GNO
        MEMFLG = GYES
        MBFPOS = 32
        CALL GPUTNI (CLENDM, IDENDM, 0, RERR)
        CALL G01FLB (RERR)
C
C       CALL CENTRAL I/O ROUTINE TO INDICATE 'CLOSE WORKSTATION'.
C
        CALL G01MIO (2, MFGLUN, DNAME, MOUTBF, 1, RERR)
        RETURN
C
C     CLEAR WORKSTATION.
C
250     CONTINUE
        CALL G01CLW
        RETURN
C
C     UPDATE WORKSTATION.
C
270     CONTINUE
C
C       FLUSH OUTPUT BUFFER.
C
        CALL G01FLB (RERR)
        RETURN
C
C     ESCAPE.
C
300     CONTINUE
        CALL G01ESC
        RETURN
C
C
C     NEW CODE ON 1ST WSI CALL AFTER A CALL INDICATING CONTINUATION.
C
905     CONTINUE
        RERR = 325
        RETURN
C
C     UNDEFINED/UNSUPPORTED OPCODE.
C
910     CONTINUE
        RERR = 320
        RETURN
C
        END
