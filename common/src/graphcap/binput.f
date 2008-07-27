C
C	$Id: binput.f,v 1.4 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE BINPUT(IOS,STATUS)
C
C  Write out the binary graphcap file.
C
C  OUTPUT
C       IOS    --  The I/O status word,  this is valid only if STATUS
C                  indicates an error.
C       STATUS --  The error status as defined by COMMON CAPERR.
C
C  All information put to the output file is contained in
C  common blocks.  Following is a list of the common blocks
C  which are written out, and their sizes.  One record is
C  written for each common block.
C
C    COMMON       SIZE
C    ------       ------
C    CAPDEV       LENDEV
C    CAPLIN       LENLIN
C    CAPUSR       LENUSR
C    CAPCOL       LENCOL
C    CAPTXT       LENTXT
C    CAPMAR       LENMAR
C    CAPPLG       LENPLG
C    CAPBND       LENBND
C    CAPSCN       LENSCN
C
      COMMON /CAPIOB/ UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPIO2/ LINE, GRNM
      INTEGER LNMAX
      PARAMETER (LNMAX=80)
      CHARACTER*80 LINE
      CHARACTER*80 GRNM
      INTEGER UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
      COMMON /CAPDEV/ DGISTR, DGISIZ, DGESTR, DGESIZ, DTISTR,
     1                DTISIZ, DCDLLX, DCDLLY, DCDURX, DCDURY,
     3                DCOAVL, CORFMT, CORFIN, BATCH , DHCSIZ,
     4                DHCSTR, CORXOF, CORYOF, DASBIT, CORXSC,
     5                CORYSC, VDWLLX, VDWLLY, VDWURX, VDWURY
      INTEGER         DGIMAX, DGEMAX, DTIMAX, DCFTMX, DHCMAX
      PARAMETER   (DGIMAX=300, DGEMAX=150, DTIMAX=100)
      PARAMETER   (DCFTMX=30 , DHCMAX=50)
      INTEGER         DGISTR(DGIMAX), DGISIZ, DGESTR(DGEMAX),
     1                DGESIZ, DTISTR(DTIMAX), DTISIZ, DCDLLX,
     2                DCDLLY, DCDURX, DCDURY, CORFMT(DCFTMX,4),
     3                CORFIN(8)     , DHCSIZ, DHCSTR(DHCMAX),
     4                CORXOF, CORYOF, DASBIT, VDWLLX, VDWLLY,
     5                VDWURX, VDWURY
      REAL            CORXSC, CORYSC, CORRIN(8)
      LOGICAL         DCOAVL, BATCH
C  Size of the COMMON
      INTEGER         LENDEV
      PARAMETER   (LENDEV=DGIMAX+1+DGEMAX+1+DTIMAX+1+4+1+4*DCFTMX+
     1                  8+2+DHCMAX+9)
      EQUIVALENCE (CORFIN,CORRIN)
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
      COMMON /CAPUSR/ UPRSTR, UPRSIZ
      INTEGER    UPRMAX
      PARAMETER (UPRMAX=80)
      INTEGER    UPRSTR(UPRMAX), UPRSIZ
      INTEGER    LENUSR
      PARAMETER (LENUSR=UPRMAX+1)
      COMMON /CAPCOL/ COLINT, COLIDX, IDXCUR, VDMINT, DMPAVL, COLFMT
     1               ,COLFIN, IDXMAX, MSTSTR, MSTSIZ, MTRSTR, DMPMDL
     2               ,MTRSIZ, DMPIDV, DMPFIN, DMPFMT
      INTEGER         MAPMAX, COLMAX, MSTMAX, MTRMAX, DMPMAX
      PARAMETER   (MAPMAX=256, COLMAX=15, MSTMAX=50, MTRMAX=20)
      PARAMETER   (DMPMAX=50)
      INTEGER         COLINT(MAPMAX*3)      , COLIDX(MAPMAX), IDXCUR,
     1                VDMINT, COLFMT(COLMAX,4)      , COLFIN(8)     ,
     2                IDXMAX, MSTSTR(MSTMAX), MSTSIZ, MTRSTR(MTRMAX),
     3                DMPMDL, MTRSIZ, DMPFIN(8)     , DMPFMT(DMPMAX,4)
      LOGICAL         DMPAVL, DMPIDV
      REAL            COLRIN(8),DMPRIN(8)
      INTEGER LENCOL
      PARAMETER   (LENCOL=MAPMAX*3+MAPMAX+1+1+1+COLMAX*4+8+1+MSTMAX+
     1                    1+MTRMAX+1+1+1+8+DMPMAX*4)
      EQUIVALENCE (COLFIN,COLRIN),(DMPFIN,DMPRIN)
      COMMON/CAPBND/PLBTEC, PLIBTB, PLTBTB, PLWBTB, PLCBTB,
     1              PMBTEC, PMIBTB, PMTBTB, PMSBTB, PMCBTB,
     2              TXBTEC, TXIBTB, TXFBTB, TXPBTB, TCXBTB,
     3              TCSBTB, TXCBTB, FABTEC, FAIBTB, FISBTB,
     4              FASBTB, FACBTB
      INTEGER       TBLSIZ
      PARAMETER (TBLSIZ=6)
      INTEGER PLBTEC,PLIBTB(TBLSIZ),PLTBTB(TBLSIZ),PLCBTB(TBLSIZ),
     1        PMBTEC,PMIBTB(TBLSIZ),PMTBTB(TBLSIZ),PMCBTB(TBLSIZ),
     2        TXBTEC,TXIBTB(TBLSIZ),TXFBTB(TBLSIZ),TXPBTB(TBLSIZ),
     3        TXCBTB(TBLSIZ),FACBTB(TBLSIZ),
     4        FABTEC,FAIBTB(TBLSIZ),FISBTB(TBLSIZ),FASBTB(TBLSIZ)
      REAL PLWBTB(TBLSIZ),PMSBTB(TBLSIZ),TCXBTB(TBLSIZ),TCSBTB(TBLSIZ)
      INTEGER LENBND
      PARAMETER (LENBND=4+18*TBLSIZ)
      COMMON /CAPTXT/ TCSSTR, TCSSIZ, TCTSTR, TCTSIZ, TXTFIN, TXTFMT,
     1                TXSSTR, TXSSIZ, TXTSTR, TXTSIZ
      INTEGER         TCSMAX, TCTMAX, TVCFMX, TXSMAX, TXTMAX
      PARAMETER (TCSMAX=10, TCTMAX=10, TVCFMX=5, TXSMAX=20, TXTMAX=20)
      INTEGER         TCSSTR(TCSMAX), TCSSIZ, TCTSTR(TCTMAX), TCTSIZ,
     1                TXTFIN(5)     , TXTFMT(TVCFMX,4)      ,
     2                TXSSTR(TXSMAX), TXSSIZ, TXTSTR(TXTMAX), TXTSIZ
      INTEGER         LENTXT
      PARAMETER (LENTXT=TCSMAX+1+TCTMAX+1+5+TVCFMX*4+TXSMAX+1+
     1                  TXTMAX+1)
      COMMON /CAPMAR/ MCSSTR, MCSSIZ, MCTSTR, MCTSIZ, MARFIN, MARFMT,
     1                MRSSTR, MRSSIZ, MRTSTR, MRTSIZ, MDOTSZ
      INTEGER         MCSMAX, MCTMAX, MVCFMX, MRSMAX, MRTMAX
      PARAMETER (MCSMAX=30, MCTMAX=15, MVCFMX=5, MRSMAX=20, MRTMAX=10)
      INTEGER         MCSSTR(MCSMAX), MCSSIZ, MCTSTR(MCTMAX), MCTSIZ,
     1                MARFIN(5)     , MARFMT(MVCFMX,4)      ,
     2                MRSSTR(MRSMAX), MRSSIZ, MRTSTR(MRTMAX), MRTSIZ,
     3                MDOTSZ
      INTEGER         LENMAR
      PARAMETER (LENMAR=MCSMAX+1+MCTMAX+1+5+MVCFMX*4+MRSMAX+1+
     1           MRTMAX+1+1)
      COMMON /CAPPLG/ PCSSTR, PCSSIZ, PCTSTR, PCTSIZ, PLSSTR,
     1                PLSSIZ, PLTSTR, PLTSIZ, PBSSTR, PBSSIZ,
     2                PBTSTR, PBTSIZ, PHATSP, PMAXPT, PPSSTR,
     3                PPSSIZ, PPTSTR, PPTSIZ, PLGSIM, PSIMSP,
     4                PSIMTR
      INTEGER         PCSMAX, PCTMAX, PLSMAX, PLTMAX, PBSMAX,
     1                PBTMAX, PPSMAX, PPTMAX
      PARAMETER   (PCSMAX=20, PCTMAX=15, PLSMAX=40, PLTMAX=20)
      PARAMETER   (PBSMAX=30, PBTMAX=15, PPSMAX=20, PPTMAX=20)
      INTEGER         PCSSTR(PCSMAX), PCSSIZ, PCTSTR(PCTMAX), PCTSIZ,
     1                PLSSTR(PLSMAX), PLSSIZ, PLTSTR(PLTMAX), PLTSIZ,
     2                PBSSTR(PBSMAX), PBSSIZ, PBTSTR(PBTMAX), PBTSIZ,
     3                PHATSP, PMAXPT, PPSSTR(PPSMAX), PPSSIZ,
     4                PPTSTR(PPTMAX), PPTSIZ, PSIMSP, PSIMTR
      LOGICAL         PLGSIM
      INTEGER         LENPLG
      PARAMETER   (LENPLG=PCSMAX+1+PCTMAX+1+PLSMAX+1+PLTMAX+1+
     1                    PBSMAX+1+PBTMAX+1+1+1+PPSMAX+1+PPTMAX+
     2                    1+1+1+1)
      COMMON /CAPSPC/ DUMSPC,ENDDSP
      INTEGER     DUMSIZ,DUMSM1
      PARAMETER (DUMSIZ=327, DUMSM1=DUMSIZ-1)
      INTEGER     DUMSPC(DUMSM1),ENDDSP
      COMMON /CAPSCN/ SCSSTR, SCSSIZ, SCTSTR, SCTSIZ, SCNLLX,
     1                SCNLLY, SCNURX, SCNURY, SCNXOF, SCNYOF,
     2                SCNXSC, SCNYSC, SCNFMT, SCNFIN, SCVFMT,
     3                SCVFIN, SCNSIM
      INTEGER         SCSMAX, SCTMAX, SFMMAX, SFNMAX, SCVFMX,
     1                SCVFIX
      PARAMETER  (SCSMAX=50, SCTMAX=50, SFMMAX=10, SFNMAX=8)
      PARAMETER  (SCVFMX=10, SCVFIX=8)
      INTEGER         SCSSTR(SCSMAX), SCSSIZ, SCTSTR(SCTMAX), SCTSIZ,
     2                SCNLLX, SCNLLY, SCNURX, SCNURY, SCNXOF, SCNYOF,
     3                SCNFMT(SFMMAX,4)      , SCNFIN(SFNMAX),
     4                SCVFMT(SCVFMX,4)      , SCVFIN(SCVFIX)
      REAL            SCNXSC, SCNYSC, SCNRIN(SFNMAX), SCVRIN(SCVFIX)
      LOGICAL         SCNSIM
      INTEGER         LENSCN
      PARAMETER  (LENSCN=SCSMAX+1+SCTMAX+1+1+1+1+1+1+1+1+1+
     1                   (SFMMAX*4)+SFNMAX+(SCVFMX*4)+SCVFIX+1)
      EQUIVALENCE (SCNFIN,SCNRIN), (SCVFIN,SCVRIN)
C
      INTEGER IOS, STATUS
      REAL ALOG10
      INTEGER II
C
C  Initialize STATUS to ALLOK.
C
      STATUS = ALLOK
C
C  Set the size of the device color map if it has not been defined.
C
      IF (IDXMAX.EQ.-1) IDXMAX = IDXCUR
C
C  IF necessary, reduce the map size to the maximum allowed
C  in the translator.
C
      IF (IDXMAX .GT. MAPMAX) IDXMAX=MAPMAX
C
C  Write out the workstation DEVICE definitions.
C
      CORFIN(3) = DCFTMX
      CORFIN(4) = 4
      CALL BINWRI(UNIT, LENDEV, DGISTR, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Write out the workstation LINE drawing definitions.
C
      LINFIN(3) = LVCFMX
      LINFIN(4) = 4
      IF (LINFIN(1) .NE. 5) LINFIN(5) = 5
      LWTFIN(3) = LWTFMX
      LWTFIN(4) = 4
      IF (LWTFIN(1) .NE. 5) THEN
C
C  The QMS (flagged by NFLG=1) is treated specially.
C
        IF (NFLG .EQ. 1) THEN
          LWTFIN(5) = 2
        ELSE
          LWTFIN(5) = 5
        ENDIF
      ENDIF
      CALL BINWRI(UNIT, LENLIN, PLAVBL, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Write out the USER interface data.
C
      CALL BINWRI(UNIT, LENUSR, UPRSTR, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Write out the DEVICE color map information.
C
      COLFIN(3) = COLMAX
      COLFIN(4) = 4
C
C  Set the number of characters required to encode a color index,
C  if the encoding is not ASCII real.
C
      IF (COLFIN(1) .NE. 5) COLFIN(5) =
     -                           IFIX(ALOG10(FLOAT(IDXMAX+1))+1)
      DMPFIN(3) = DMPMAX
      DMPFIN(4) = 4
C
C  Set the number of characters required to encode a color intensity,
C  if the encoding is not ASCII real.
C
      IF (DMPFIN(1) .NE. 5) THEN
        IDX3 = MAPMAX*3
        INTMAX = 0
        DO 150 I=1,IDX3
        INTMAX = MAX0(COLINT(I),INTMAX)
  150   CONTINUE
        DMPFIN(5) = IFIX(ALOG10(FLOAT(INTMAX+1))+1)
      ENDIF
      CALL BINWRI(UNIT, LENCOL, COLINT, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Write out the device TEXT data.
C
      CALL BINWRI(UNIT, LENTXT, TCSSTR, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Write out the device MARKER data.
C
      CALL BINWRI(UNIT, LENMAR, MCSSTR, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Write out the device POLYGON data.
C
      CALL BINWRI(UNIT, LENPLG, PCSSTR, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Write out the BUNDLE tables.
C
      CALL BINWRI(UNIT, LENBND, PLBTEC, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Write out the RASTER class data.
C
      SCVFIN(3) = SCVFMX
      SCVFIN(4) = 4
      SCNFIN(3) = SFMMAX
      SCNFIN(4) = 4
      CALL BINWRI(UNIT, LENSCN, SCSSTR, IOS, STATUS)
      IF (STATUS.NE.ALLOK)RETURN
C
C  Zero out the dummy space and put to the output file.
C  Currently the first five locations of the dummy space
C  are defined to support the NCAR color DICOMED--this
C  situation will be cleaned up for Release 3.0 .
C
      DO 1111 II = 1,DUMSIZ
        DUMSPC(II) = 0
 1111 CONTINUE
      CALL BINWRI(UNIT, DUMSIZ, DUMSPC, IOS, STATUS)
C
      RETURN
      END
