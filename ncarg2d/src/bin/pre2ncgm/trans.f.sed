C
C	$Id: trans.f.sed,v 1.3 2008-07-27 00:59:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM TRANS
C
C  Main program for the NCAR Fortran CGM translator.  This translator
C  will also accept NCAR pre-CGM metafiles.
C
      COMMON/TREROR/ ALLOK, MFRCHK, MTOPER, METRDC, REDERR, TYPCHG
     1             ,INVTYP, MINVLD, TYPERR, FRMEND, ENCINT, IVDCDT
     2             ,GCOERR, GCRERR, GCCERR, FCOERR, FCRERR, FCCERR
     3             ,PLIDXG, PMIDXG, TXIDXG, PGIDXG, INVLMT, CELERR
     4             ,COIERR, COLNRM, UNKNOW, UNKOPC, ENDMTF, VNEROR
     5             ,BADRSZ, DEVOUT, NOVERS, BADFNT, PGMERR, FASERR
     6             ,HINERR, VDWERR, RDWERR, RIXLIM
      INTEGER        ALLOK, MFRCHK, MTOPER, METRDC, REDERR, TYPCHG
     1             ,INVTYP, MINVLD, TYPERR, FRMEND, ENCINT, IVDCDT
     2             ,GCOERR, GCRERR, GCCERR, FCOERR, FCRERR, FCCERR
     3             ,PLIDXG, PMIDXG, TXIDXG, PGIDXG, INVLMT, CELERR
     4             ,COIERR, COLNRM, UNKNOW, UNKOPC, ENDMTF, VNEROR
     5             ,BADRSZ, DEVOUT, NOVERS, BADFNT, PGMERR, FASERR
     6             ,HINERR, VDWERR, RDWERR, RIXLIM
      COMMON /TRFRAM/ METALL, FRINIT, FRCNT
      LOGICAL METALL, FRINIT
      INTEGER  FRCNT
      COMMON /PTRBUFR/ METBIT, MBUFER, MDTYPE, METREC, MRECLN,
     1                MCONTF, MOPRST, LNGFLG, MERGFL
      INTEGER MNWRDS, RECLOC, RECSIZ, DTLOC, DTSIZ, MBITST, MBUFOF,
     1        MINSBD, MERLOC, MERSIZ
#if defined(cray)
      PARAMETER (MNWRDS=180, RECLOC=0, RECSIZ=16, DTLOC=16, DTSIZ=4
#else
      PARAMETER (MNWRDS=360, RECLOC=0, RECSIZ=16, DTLOC=16, DTSIZ=4
#endif
     1          ,MBITST=32, MBUFOF=32, MINSBD=16, MERLOC=23, MERSIZ=1)
      INTEGER METBIT, MBUFER(MNWRDS), MDTYPE, METREC, MRECLN, MOPRST
      LOGICAL MCONTF, LNGFLG, MERGFL
      COMMON/TRINOT/ IPTR, MBUFOT, MAXBYT, DEVUNT, METUNT,
     1          METIPT, ERRUNT, FNTUNT
      INTEGER MAXCNT
      PARAMETER (MAXCNT=200)
      INTEGER IPTR, MBUFOT(MAXCNT), MAXBYT, DEVUNT, METUNT, ERRUNT,
     1        FNTUNT
      LOGICAL METIPT
      COMMON /TRTYPE/ METMIN, METEXT, METPRT, METHED, MIOPCL, MXOPCL,
     1                MIOPID, MXOPID, ASCDEC, ASCHEX, ASCOCT, BINARY,
     2                GAHNOR, GALEFT, GACENT, GARITE, GAVNOR, GATOP ,
     3                GACAP , GAHALF, GABASE, GABOTT, GRIGHT, GLEFT ,
     4                GUP   ,
     5                GDOWN , CINDEX, CDIRCT, PENUP , PENDN , FLAGVL,
     6                SEPRVL, ASCTEK, TYPRGB, TYPBGR,
     7                TYPHLS, TYPMON, SOLID , DASHED, INVSBL, SOLPAT,
     8                ASFIND, ASFBND, ASFLNT, ASFLNW, ASFLNC, ASFMRT,
     9                ASFMRS, ASFMRC, ASFFIS, ASFFHI, ASFFPI, ASFFCO,
     A                ASFFPT, ASFFPW, ASFFPC, ASFTFI, ASFTPR, ASFTCX,
     B                ASFTCS, ASFTCO, ASCFLT, PHOLLO, PSOLID, PPATTR,
     C                PHATCH, PEMPTY, HHORIZ, HVERTI, HPOSLP, HNESLP,
     D                HHOAVE, HPOANE
      INTEGER         METMIN, METEXT, METPRT, METHED, MIOPCL, MXOPCL,
     1                MIOPID, MXOPID, ASCDEC, ASCHEX, ASCOCT, BINARY,
     2                GAHNOR, GALEFT, GACENT, GARITE, GAVNOR, GATOP ,
     3                GACAP , GAHALF, GABASE, GABOTT, GRIGHT, GLEFT ,
     4                GUP   ,
     5                GDOWN , CINDEX, CDIRCT, PENUP , PENDN , FLAGVL,
     6                SEPRVL, ASCTEK, TYPRGB, TYPBGR,
     7                TYPHLS, TYPMON, SOLID , DASHED, INVSBL, SOLPAT,
     8                ASFIND, ASFBND, ASFLNT, ASFLNW, ASFLNC, ASFMRT,
     9                ASFMRS, ASFMRC, ASFFIS, ASFFHI, ASFFPI, ASFFCO,
     A                ASFFPT, ASFFPW, ASFFPC, ASFTFI, ASFTPR, ASFTCX,
     B                ASFTCS, ASFTCO, ASCFLT, PHOLLO, PSOLID, PPATTR,
     C                PHATCH, PEMPTY, HHORIZ, HVERTI, HPOSLP, HNESLP,
     D                HHOAVE, HPOANE
      COMMON /THFLG/NSFLG
C
      INTEGER IOS, STATUS
      CHARACTER*1 STRING(80)
C
C Do calls forcing BLOCKDATAs to be loaded from a binary library.
C
      CALL TRNDAT
      CALL TRNDT2
C
C  Set the machine dependent constants.
C
      CALL TRNMCH
C
      STATUS = ALLOK
C
C  Determine if text and header recors should be suppressed from
C  being written to the output file.
C
      DO 10 I=1,80
        STRING(I) = ' '
   10 CONTINUE
      NSFLG = 0
      IF (IARGCT(IDUM) .GE. 0) THEN
        CALL ARGGET(1,STRING)
        IF (STRING(1).EQ.'-' .AND. (STRING(2).EQ.'n' .OR.
     -                            STRING(2).EQ.'N')) THEN
          NSFLG = 1
        ELSE
          NSFLG = 0
        ENDIF
      ENDIF
C
C  Attempt to open the metafile.
C
      CALL FNDARG(IOS,STATUS)
      IF (STATUS.NE.ALLOK) CALL ALOVER(IOS,STATUS)
C
C  Open the GKS package.
C
      CALL GOPKS(1,IDUM)
      CALL GOPWK(1,METUNT,1)
      CALL GACWK(1)
C
C  Use normalization 0.
C
      CALL GSELNT(0)
C
C  Set clipping off.
C
      CALL GSCLIP(0)
C
C  Read in the first metafile record.
C
      METREC = 1
      CALL METRED(IOS,STATUS)
      IF (STATUS.NE.ALLOK) CALL ALOVER(IOS,STATUS)
C
      FRINIT = .TRUE.
      FRCNT = 0
C
C  Loop until we run out of data, or error.
C
      CALL TRASSN(IOS,STATUS)
      IF (STATUS.NE.ALLOK .AND. STATUS.NE.ENDMTF)
     -                         CALL ALOVER(IOS,STATUS)
C
C  End the plotting and set the default color table.
C
      CALL XMDEF(IOS,STATUS)
      CALL LODCTB(IOS,STATUS)
C
C  Flush the buffer.
C
      CALL PLOTIT(0,0,2)
C
C  Close the GKS package.
C
      CALL GDAWK(1)
      CALL GCLWK(1)
      CALL GCLKS
C
      STOP
      END
