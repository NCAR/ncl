C
C	$Id: twobyt.f,v 1.6 2008-07-27 00:59:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TWOBYT(IOS,STATUS)
C
C  PERFORM THE OPERATION OF RELATIVE PEN POSITION
C
C  OUTPUT
C       IOS-IO STATUS VALID ONLY IF STATUS DEFINED WITH AN I/O ERROR
C       STATUS-THE STATUS FLAG DEFINED BY COMMON TREROR
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
      COMMON /TRINST/ OPCL, OPID, LEN, CONT
      INTEGER OPCL, OPID, LEN
      LOGICAL CONT
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
      COMMON /TRMTYP/ FOURBT, MULTBT, TWOBT, PTSFLG, CURCOL, IPWRIT,
     1                IFRAME, ISETOP, IRANG, IPTS, ICAS, IINT, IORNT,
     2                ISIZE, IFONT, IPAT, ISSIZ, ICENT, ICOLR,
     3                MCENT, MSIZE, MOR, OLDX, OLDY, MCASE, MINTEN,
     4                MFONT, MDASH, MSPOT, MCOLOR, MLIMIT, PTSYM,
     5                SAVX, SAVY
      INTEGER FOURBT, MULTBT, TWOBT, PTSFLG, CURCOL, IPWRIT,
     1                IFRAME, ISETOP, IRANG, IPTS, ICAS, IINT, IORNT,
     2                ISIZE, IFONT, IPAT, ISSIZ, ICENT, ICOLR,
     3                MCENT, MSIZE, MOR, OLDX, OLDY, MCASE, MINTEN,
     4                MFONT, MDASH, MSPOT, MCOLOR(3), MLIMIT(4), PTSYM,
     5                SAVX, SAVY
      COMMON /PGKSCOM/ CLPDAT, CLPFLG, POLIDX, LINTYP, LINWTH, LINCOL,
     1                LINRGB, MARIDX, MARSIZ, MARCOL, MARRGB, TXTIDX,
     2                INTSTL, PATIDX, FILCOL, FILRGB, MARTYP, HORIZ ,
     3                VERT  , PATH  , CHIGHT, XU    , YU    , XB    ,
     4                YB    , TXTCOL, FINDEX, CEXPN , CSPACE, CURIDX,
     5                CURIMP, CURINT, COLMOD, FILIDX, TXTRGB, PROPN ,
     6                FIRSTX, FIRSTY, LASTX , LASTY , TRATIO, CBV   ,
     7                CUV   , CHP   , CVP   , CCSPAC, CHHORZ, CDV   ,
     8                PMRSIZ, CLPX  , CLPY  , CLPP  , DEFREP, DEFLEN,
     9                VDCECR, TRANVN, TXTPRE, HATIDX, FILRPT, ASFSRF,
     A                ASFSDF, MAPMOD, VERSOK, PCBFST, CPGLEN, CLPNUL,
     B                MTDLEN
      INTEGER         CMPMAX, ASFMAX
      PARAMETER      (CMPMAX=256    , ASFMAX=18)
      LOGICAL         CLPFLG, PROPN , CHHORZ, DEFREP, CLPNUL, MAPMOD,
     1                VERSOK, PCBFST
      REAL            LINWTH, MARSIZ, CEXPN , CSPACE, TRATIO, CBV(2),
     1                CUV(2), CDV(2), CCSPAC
      INTEGER         CLPDAT(4)     , POLIDX, LINTYP, LINCOL, LINRGB(3),
     1                MARIDX, MARCOL, MARRGB(3)     , TXTIDX, INTSTL,
     2                PATIDX, FILCOL, FILRGB(3)     , MARTYP, HORIZ ,
     3                VERT  , PATH  , XU    , YU    , XB    , YB    ,
     4                TXTCOL, FINDEX, CURIMP(CMPMAX), CURINT(CMPMAX*3),
     5                CHIGHT, COLMOD, FILIDX, TXTRGB(3)     , CURIDX,
     6                FIRSTX, FIRSTY, LASTX , LASTY , CHP   , CVP   ,
     7                PMRSIZ, CLPX  , CLPY  , CLPP  , DEFLEN, TRANVN,
     8                TXTPRE, HATIDX, FILRPT(2)     , VDCECR(4)     ,
     9                ASFSRF(ASFMAX), ASFSDF(ASFMAX), CPGLEN, MTDLEN
      COMMON /GKSCHR/ MTDESC
      CHARACTER*80    MTDESC
      COMMON /CAPFNT/ CHRSTR, CHREND, CHRWDT, CHRSCH, FNTRHT,
     1                FNTTOP, FNTCAP, FNTHLF, FNTBAS, FNTBOT,
     2                FNTTYP, FNTSCH, CORXST, CORXLN, CORYST,
     3                CORYLN, CORPST, CORPLN, PBEGST, PBEGLN,
     4                PENDST, PENDLN, CORSCH, NEWCLS, CPNTRS,
     5                CPNLST, CSTRKS
      INTEGER         CHRSM1, CHRSM2
      PARAMETER       (CHRSM1=128, CHRSM2=5121)
      INTEGER         CHRSTR, CHREND, CHRWDT,
     1                CHRSCH(11)    , FNTRHT, FNTTOP,
     2                FNTCAP, FNTHLF, FNTBAS, FNTBOT, FNTTYP,
     3                FNTSCH(9)     , CORXST, CORXLN,
     4                CORYST, CORYLN, CORPST, CORPLN,
     5                PBEGST, PBEGLN, PENDST, PENDLN,
     6                CORSCH(10)    ,
     7                NEWCLS(300)   ,
     8                CPNTRS(CHRSM1), CPNLST        ,
     9                CSTRKS(CHRSM2)
C
C  XLCN   --  Bit offset to extract X coordinates from fontcap.
C  YLCN   --  Bit offset to extract Y coordinates from fontcap.
C  PLCN   --  Bit offset to extract PEN control bit from fontcap.
C  FNTMAX --  Number of fonts obtained from FONT LIST (defaults to 1).
C  FNTS   --  An array of pointers associating indices obtained from
C             FONT INDEX elements with the internally supported fonts.
C             Consult the documentation in BLOCKDATA TRNDATX for a
C             list of supported fonts.
C  FLEFT  --  An array of character left values for proportionally
C             spaced fonts.
C  FRIGHT --  An array of character right values for proportionally
C             spaced fonts.
C  MXFLST --  The maximum number of strings accepted in a FONT LIST
C             element.
C  NSFNTS --  The number of distinct fonts supported internally.
C
      PARAMETER (NSFNTS=20,MXFLST=300)
      COMMON /FNTCOM/ XLCN  , YLCN  , PLCN  , FNTMAX,
     1                FNTS  , FLEFT , FRIGHT, SFLENS
      INTEGER         FLEFT(CHRSM1) , FRIGHT(CHRSM1), FNTS(MXFLST)  ,
     1                XLCN, YLCN    , PLCN  , FNTMAX
      CHARACTER*30 SFONTS(NSFNTS)
      COMMON /SUPFNT/ SFONTS
C
C  Size of common CAPFNT in words.
C
      INTEGER WRLEN
      PARAMETER (WRLEN=5600)
C
      INTEGER IOS, STATUS
      INTEGER BIAS32, CSIZE, PSIZE, XC, PEN, YC
      INTEGER STRX, STRY, METCOR
C
C  SAVE PREVIOUS LINE POINT FOR CONNECTED POINTS,
C  SAVX and SAVY need to be in common with POSIT
C  since there may be a mixture of two byte and
C  four byte instructions.
C
      SAVE    /TRMTYP/
C
C   SIZE OF A CHARACTER MARKER FOR POINTS MODE
C
      INTEGER PTSIZ
      DATA PTSIZ/15/
C
C  THERE IS NO NEED FOR RECORD ERROR CHECKS BECAUSE THE 16 BIT PARCEL
C  IS IN THE METAFILE BUFFER
C
C  THE 32 BIT BIAS FOR THE RELATIVE POSITIONING
C
      DATA BIAS32/32/
      DATA CSIZE,PSIZE/6,1/
C
C  Initialize maximum metacode coordinate.
C
      DATA METCOR/32767/
C
C  SET THE ERROR FLAG TO OK
C
      STATUS = ALLOK
C
C  GET THE X COORDINATE
C
      CALL MNINST(CSIZE,IOS,STATUS)
      XC = OPCL
C
C  GET THE PEN STATUS
C
      CALL MNINST(PSIZE,IOS,STATUS)
      PEN = OPCL
C
C  SKIP THE NULL BIT
C
      CALL MNINST(PSIZE,IOS,STATUS)
C
C  GET THE Y COORDINATE
C
      CALL MNINST(CSIZE,IOS,STATUS)
      YC = OPCL
C
C  BIAS AND SET THE ABSOLUTE PEN POSITION
C
      OLDX = (XC-BIAS32) + OLDX
      OLDY = (YC-BIAS32) + OLDY
C
C  TEST IF IN POINTS MODE OR LINE MODE
C
      IF (PTSFLG.EQ.0) THEN
C
C       LINE MODE PERFORM THE PEN POSITION INSTRUCTION
C
        CALL PATDRW(OLDX, OLDY, PEN, IOS, STATUS)
C
      ELSE
C
C       POINTS MODE
C
        STRX = OLDX
        STRY = OLDY
C
C       CHECK IF POINT OR CHARACTER REQUIRED
C
        IF (PTSYM.EQ.0) THEN
C
C         DRAW A POINT
C
          CALL PLOTIT(0,0,2)
          CALL GSMK(1)
          XCOR = REAL(OLDX)/32767.
          YCOR = REAL(OLDY)/32767.
          CALL GPM(1,XCOR,YCOR)
C
        ELSE
C
C         DRAW A SYMBOL
C
          CALL PLOTIT(0,0,2)
          CHIGHT = PTSIZ
          XCOR = REAL(STRX)/32767.
          YCOR = REAL(STRY)/32767.
          CALL WTSTR(XCOR,YCOR,CHAR(PTSYM),CHIGHT,0,0)
        END IF
C
C               IF PEN DOWN THEN DRAW FROM LAST POINT TO CURRENT POINT
C
        IF (PEN.EQ.PENDN) THEN
                CALL PLTIT(SAVX, SAVY, 0, IOS, STATUS)
                CALL PLTIT(STRX, STRY, 1, IOS, STATUS)
        END IF
C
C  SAVE THE CURRENT POSITION
C
        SAVX = STRX
        SAVY = STRY
       END IF
C
      RETURN
      END
