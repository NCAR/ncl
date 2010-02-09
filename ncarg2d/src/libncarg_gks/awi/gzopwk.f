C
C	$Id: gzopwk.f,v 1.15 2010-02-09 23:19:57 brownrig Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZOPWK(WKID,CONID,WTYPE)
C
C  OPEN WORKSTATION
C
      INTEGER EOPWK
      PARAMETER (EOPWK=2)
C
      include 'gkscom.h'
C
      INTEGER WKID,CONID,WTYPE
      INTEGER LASF(13)
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(8,EOPWK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation identifier is valid.
C
      CALL GZCKWK(20,EOPWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the connection identifier is valid.
C
      IF (CONID.EQ.5 .OR. CONID.EQ.6) THEN
        ERS = 1
        CALL GERHND(21,EOPWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the workstation type is valid.
C
      CALL GZCKWK(22,EOPWK,IDUM,WTYPE,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation is currently open.
C
      CALL GZCKWK(24,EOPWK,WKID,WTYPE,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if there is room for another open workstation.
C
      IF (NOPWK .GE. MOPWK) THEN
        ERS = 1
        CALL GERHND(26,EOPWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Only one CGM workstation currently allowed.
C
      IF (WTYPE .EQ. GCGM) THEN
        DO 30 I=1,NOPWK
          IF (SWKTP(I) .EQ. GCGM) THEN
            ERS = 1
            CALL GERHND(-112,EOPWK,ERF)
            ERS = 0
            RETURN
          ENDIF
   30   CONTINUE
      ENDIF
C
C  Only one PDF workstation currently allowed.
C
      IF (WTYPE.EQ.GPDFP .OR. WTYPE.EQ.GPDFL) THEN
        DO 50 I=1,NOPWK
          IF (SWKTP(I).EQ.GPDFP .OR. SWKTP(I).EQ.GPDFL) THEN
            ERS = 1
            CALL GERHND(-353,EOPWK,ERF)
            ERS = 0
            RETURN
          ENDIF
   50   CONTINUE
      ENDIF
C
C  Make sure that WISS is not already open if the workstation
C  type is WISS.
C
      IF (WTYPE .EQ. GWSS) THEN
        DO 40 I=1,NOPWK
          IF (SWKTP(I) .EQ. GWSS) THEN
            ERS = 1
            CALL GERHND(28,EOPWK,ERF)
            ERS = 0
            RETURN
          ENDIF
   40   CONTINUE
      WCONID = CONID
      ENDIF
C
C  Set the operating state to WSOP if in state GKOP.
C
      IF (OPS .EQ. GGKOP) THEN
        OPS = GWSOP
      ENDIF
C
C  Pass information across the workstation interface.
C
        FCODE = -3
        CONT  = 0
        CALL GZROI(0)
        IF ((WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX) .OR.
     +       WTYPE.EQ.GPDFP .OR. WTYPE.EQ.GPDFL) THEN
          IL1   = 15
          IL2   = 15
          ID(1) = WKID
          ID(2) = CONID
          ID(3) = WTYPE
C
C  Positioning coordinates for those workstations that can use them.
C
          ID(4) = CLLX
          ID(5) = CLLY
          ID(6) = CURX
          ID(7) = CURY
C
C  Scale factor for PostScript and PDF workstations.
C
          ID(8) = CPSCL
C
C  Color model for PostScript and PDF workstations.
C
          ID(9) = CCMDL
C
C  Flag for suppressing background color for PS and PDF.
C
          ID(10) = CSUPR
C
C  Flag for portrait/landscape mode (0 or non-zero)
C
          ID(11) = CPTLD
C
C  Specification for page width in PDF files.
C
          ID(12) = PDFWTH
C
C  Specification for page height in PDF files.
C
          ID(13) = PDFHGT
C
C  Specification for page width in PS files.
C
          ID(14) = PSWTH
C
C  Specification for page height in PDF files.
C
          ID(15) = PSHGT

        ELSE IF (WTYPE.GE.GCROMIN .AND. WTYPE.LE.GCROMAX) THEN
          IL1   = 9
          IL2   = 9
          ID(1) = WKID
          ID(2) = CONID
          ID(3) = WTYPE
C
C  Positioning coordinates for those workstations that can use them.
C
          ID(4) = CLLX
          ID(5) = CLLY
          ID(6) = CURX
          ID(7) = CURY
C
C  Specification for page size in document-oriented files.
C
          ID(8) = PDFWTH
          ID(9) = PDFHGT
	ELSE IF (WTYPE.EQ.GXWE .OR. WTYPE.EQ.GXWC .OR.
     +           WTYPE.EQ.GPIX) THEN
	  IL1   = 4
	  IL2   = 4
          ID(1) = WKID
          ID(2) = CONID
          ID(3) = WTYPE
C
C  Color model for X workstations.
C
	  ID(4) = COLMOD
        ELSE
          IL1   = 3
          IL2   = 3
          ID(1) = WKID
          ID(2) = CONID
          ID(3) = WTYPE
        ENDIF
C
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EOPWK,ERF)
          ERS = 0
          RETURN
        ENDIF
C
C  Add the workstation identifier to the set of open workstations.
C
      NOPWK = NOPWK+1
      SOPWK(NOPWK) = WKID
      SWKTP(NOPWK) = WTYPE
C
C  Set the file name for an MO workstation back to default; set the
C  positioning coordinates back to defaults; set the PostScript
C  coordinate scale factor back to the default; set the X color model
C  back to default.
C
        GFNAME = 'DEFAULT'
        CLLX = -9999
        CLLY = -9999
        CURX = -9999
        CURY = -9999
        CPSCL = -1
        COLMOD = -1
C
C  For an X window that does not currently exist, obtain the local
C  window ID for all future calls; for an X window that already exists
C  the connection ID is the window ID.  In the case of an X window
C  that does not exist, the window ID is returned from the interface
C  call in IL2 (non-standard usage of this parameter).  All the
C  other drivers will return a -1 in IL2.
C
        IF (WTYPE.EQ.GXWC  .OR. WTYPE.EQ.GDMP  .OR.
     +      WTYPE.EQ.GXWE  .OR. WTYPE.EQ.GPIX  .OR.
     +     (WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX) .OR.
     +      WTYPE.EQ.GPDFP .OR. WTYPE.EQ.GPDFL .OR.
     +     (WTYPE.GE.GCROMIN .AND. WTYPE.LE.GCROMAX) ) THEN
          LXWKID(NOPWK) = IL2
        ENDIF
C
C  Establish the current attribute settings (temporerily turn off
C  processing of error -109 [non-implementation of certain functions
C  on output only workstations] by setting CUFLAG non-zero).
C
        CUFLAG = WKID
        CALL GSCLIP (CCLIP)
C       CALL GSPLI  (CPLI )
        CALL GSLN   (CLN  )
        CALL GSLWSC (CLWSC)
        CALL GSPLCI (CPLCI)
C       CALL GSPMI  (CPMI )
        CALL GSMK   (CMK  )
        CALL GSMKSC (CMKS )
        CALL GSPMCI (CPMCI)
C       CALL GSTXI  (CTXI )
        CALL GSTXFP (CTXFP(1),CTXFP(2))
        CALL GSCHXP (CCHXP)
        CALL GSCHSP (CCHSP)
        CALL GSTXCI (CTXCI)
        CALL GSCHH  (CCHH )
        CALL GSCHUP (CCHUP(1),CCHUP(2))
        CALL GSTXP  (CTXP )
        CALL GSTXAL (CTXAL(1),CTXAL(2))
C       CALL GSFAI  (CFAI )
        CALL GSFAIS (CFAIS)
        CALL GSFASI (CFASI)
        CALL GSFACI (CFACI)
C       CALL GSPA   (CPA  (1),CPA  (2))
C       CALL GSPARF (CPARF(1),CPARF(2))
        LASF( 1) = CLNA
        LASF( 2) = CLWSCA
        LASF( 3) = CPLCIA
        LASF( 4) = CMKA
        LASF( 5) = CMKSA
        LASF( 6) = CPMCIA
        LASF( 7) = CTXFPA
        LASF( 8) = CCHXPA
        LASF( 9) = CCHSPA
        LASF(10) = CTXCIA
        LASF(11) = CFAISA
        LASF(12) = CFASIA
        LASF(13) = CFACIA
        CALL GSASF (LASF)
        CUFLAG = -1
C
      RETURN
      END
