C
C	$Id: pwt.f,v 1.6 2008-07-27 00:59:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PWT(IOS,STATUS)
C
C  START UP THE CHARACTER WRITING SEQUENCE TO THE DEVICE
C
C  OUTPUT
C       IOS-IO STATUS VALID ONLY IF STATUS EQUALS AN I/O ERROR
C       STATUS-THE ERROR STATUS DEFINED BY COMMON TREROR
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
      COMMON /TRINST/ OPCL, OPID, LEN, CONT
      INTEGER OPCL, OPID, LEN
      LOGICAL CONT
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
      COMMON /CAPLIN/ PLAVBL, LDSSTR, LDSSIZ, LDTSTR, LDTSIZ,
     1                LMSSTR, LMSSIZ, LMTSTR, LMTSIZ, LCSSTR,
     2                LCSSIZ, LCTSTR, LCTSIZ, LINFIN, LINFMT,
     3                LWSSTR, LWSSIZ, LWTSTR, LWTSIZ, LWTFIN,
     4                LWTFMT, LWTRNG, LWTSCF, LBSSTR, LBSSIZ,
     5                LBTSTR, LBTSIZ, LPSSTR, LPSSIZ, LPTSTR,
     6                LPTSIZ
      INTEGER         LDSMAX, LDTMAX, LMSMAX, LMTMAX, LCSMAX,
     1                LCTMAX, LVCFMX, LWSMAX, LWTMAX, LWTFMX,
     2                LBSMAX, LBTMAX, LPSMAX, LPTMAX
      PARAMETER   (LDSMAX=30, LDTMAX=15, LMSMAX=30, LMTMAX=15,
     1             LCSMAX=30, LCTMAX=15, LVCFMX=8,  LWSMAX=30,
     2             LWTMAX=15, LWTFMX=8 , LBSMAX=30, LBTMAX=15,
     3             LPSMAX=20, LPTMAX=20)
      INTEGER         LDSSTR(LDSMAX), LDSSIZ, LDTSTR(LDTMAX),
     1                LDTSIZ, LMSSTR(LMSMAX), LMSSIZ,
     2                LMTSTR(LMTMAX), LMTSIZ, LCSSTR(LCSMAX),
     3                LCSSIZ, LCTSTR(LCTMAX), LCTSIZ, LINFIN(8),
     4                LINFMT(LVCFMX,4)      , LWSSTR(LWSMAX),
     5                LWSSIZ, LWTSTR(LWTMAX), LWTSIZ, LWTFIN(8),
     6                LWTFMT(LWTFMX,4)      , LWTRNG(2)     ,
     7                LBSSTR(LBSMAX), LBSSIZ, LBTSTR(LBTMAX),
     8                LBTSIZ, LPSSTR(LPSMAX), LPSSIZ,
     9                LPTSTR(LPTMAX), LPTSIZ
      LOGICAL         PLAVBL
      REAL            LWTSCF,LINRIN(8),LWTRIN(8)
      INTEGER         LENLIN
      PARAMETER   (LENLIN=1+LDSMAX+1+LDTMAX+1+LMSMAX+1+
     1             LMTMAX+1+LCSMAX+1+LCTMAX+1+8+LVCFMX*4+
     2             LWSMAX+1+LWTMAX+1+8+LWTFMX*4+2+1+LBSMAX+
     3             1+LBTMAX+1+LPSMAX+1+LPTMAX+1)
      EQUIVALENCE (LINFIN,LINRIN), (LWTFIN,LWTRIN)
C
      INTEGER IOS, STATUS
      INTEGER K, CCOUNT, CBYTE, NULL, ICNT, STRX, STRY
      CHARACTER*300 TCHAR
      REAL DEGRAD,METCOR
C
C  256 CHARACTERS MAX ALLOWED
C
      INTEGER MXCH
      PARAMETER (MXCH=300)
      INTEGER ACHAR(MXCH)
      DATA NULL /0/
C
C  DEGREES TO RADIANS CONVERSION AND COORDINATE EXPANDER
C
      DATA DEGRAD,METCOR/.01745329,32767.0/
C
C  DEFINE INSTRUCTION COUNT DATA AND CHARACTER BYTE LENGTH
C
      DATA ICNT,CBYTE/8,8/
C
      STATUS = ALLOK
C
C  GET THE CHARACTER COUNT (NO ERROR CHECKING REQUIRED)
C
      CALL MNINST(ICNT,IOS,STATUS)
      CCOUNT = OPCL
      IF (CCOUNT.GT.MXCH) CCOUNT = MXCH
C
C  GET THE CHARACTER STRING
C
      DO 100 K=1,CCOUNT
        CALL MNINST(CBYTE,IOS,STATUS)
        ACHAR(K) = OPCL
        IF (STATUS.NE.ALLOK) RETURN
C
 100  CONTINUE
C
C  TEST IF ANOTHER READ NEEDED TO GET TO A 16 BIT BOUNDARY
C
      IF (0 .NE. MOD(CCOUNT,2)) CALL MNINST(CBYTE,IOS,STATUS)
C
C  GET THE COORDINATE LOCATION FOR THE STRING
C       DO NOT PASS THE COMMON VALUES BECAUSE THEY ARE RESET BY PLTIT
C
      STRX = OLDX
      STRY = OLDY
C
      CALL GSTXFP(MFONT,2)
      TCHAR = ' '
      DO 130 I=1,CCOUNT
      TCHAR(I:I) = CHAR(ACHAR(I))
  130 CONTINUE
      CALL PLOTIT(0,0,2)
      CALL WTSTR(FLOAT(STRX)/32767.,FLOAT(STRY)/32767.,
     -           TCHAR(1:CCOUNT),MSIZE/32,MOR,MCENT-1)
C
      RETURN
      END
