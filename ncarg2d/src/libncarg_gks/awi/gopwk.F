      SUBROUTINE GOPWK(WKID,CONID,WTYPE)
C
C  OPEN WORKSTATION
C
      INTEGER EOPWK
      PARAMETER (EOPWK=2)
C
      include 'gkscom.h'
C
      INTEGER WKID,CONID,WTYPE
      LOGICAL IOPEN
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
C  Check if the specified connection identifier is currently open
C  for CGM or WISS workstation types (CONID is not 
C  relevant for other currently supported workstation types).
C
      IF (WTYPE.EQ.GCGM  .OR. WTYPE.EQ.GWSS) THEN
#if defined(u370) && defined(unix)
C       INQUIRE (CONID,OPENED=IOPEN)
C       IF (IOPEN) THEN
C         ERS = 1
C         CALL GERHND(26,EOPWK,ERF)
C         ERS = 0
C         RETURN
C       ENDIF
#else
        INQUIRE (CONID,OPENED=IOPEN)
        IF (IOPEN) THEN
          ERS = 1
          CALL GERHND(26,EOPWK,ERF)
          ERS = 0
          RETURN
        ENDIF
#endif
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
C  Add the workstation identifier to the set of open workstations.
C
      NOPWK = NOPWK+1
      SOPWK(NOPWK) = WKID
      SWKTP(NOPWK) = WTYPE
C
C  Pass information across the workstation interface.
C
        FCODE = -3
        CONT  = 0
        CALL GZROI(0)
        IF (WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX) THEN
          IL1   = 8
          IL2   = 8
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
C  Scale factor for PostScript workstaitons.
C
          ID(8) = CPSCL
        ELSE
          IL1   = 3
          IL2   = 3
          ID(1) = WKID
          ID(2) = CONID
          ID(3) = WTYPE
        ENDIF
C
        STRL1 = 80
        STRL2 = 80
        STR   = GFNAME
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EOPWK,ERF)
          ERS = 0
          RETURN
        ENDIF
C
C  Set the file name for an MO workstation back to default; set the
C  positioning coordinates back to defaults.
C
        GFNAME = 'DEFAULT'
        CLLX = -1
        CLLY = -1
        CURX = -1
        CURY = -1
C
C  For an X window that does not currently exist, obtain the local 
C  window ID for all future calls; for an X window that already exists
C  the connection ID is the window ID.  In the case of an X window
C  that does not exist, the window ID is returned from the interface
C  call in IL2 (non-standard usage of this parameter).  The
C  PostScript drivers are treated the same as X windows for this
C  purpose.
C
        IF (WTYPE.EQ.GXWC  .OR. WTYPE.EQ.GDMP  .OR. 
     +      WTYPE.EQ.GXWE  .OR. 
     +        (WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX)) THEN
          LXWKID(NOPWK) = IL2
        ENDIF
C
C  Establish the current attribute settings (temporerily turn off
C  processing of error -109 [non-implementation of certain functions
C  on output only workstations] by setting CUFLAG non-zero).
C
        CUFLAG = WKID
        CALL GSCLIP (CCLIP)
        CALL GSPLI  (CPLI )
        CALL GSLN   (CLN  )
        CALL GSLWSC (CLWSC)
        CALL GSPLCI (CPLCI)
        CALL GSPMI  (CPMI )
        CALL GSMK   (CMK  )
        CALL GSMKSC (CMKS )
        CALL GSPMCI (CPMCI)
        CALL GSTXI  (CTXI )
        CALL GSTXFP (CTXFP(1),CTXFP(2))
        CALL GSCHXP (CCHXP)
        CALL GSCHSP (CCHSP)
        CALL GSTXCI (CTXCI)
        CALL GSCHH  (CCHH )
        CALL GSCHUP (CCHUP(1),CCHUP(2))
        CALL GSTXP  (CTXP )
        CALL GSTXAL (CTXAL(1),CTXAL(2))
        CALL GSFAI  (CFAI )
        CALL GSFAIS (CFAIS)
        CALL GSFASI (CFASI)
        CALL GSFACI (CFACI)
        CALL GSPA   (CPA  (1),CPA  (2))
        CALL GSPARF (CPARF(1),CPARF(2))
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
