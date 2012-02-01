C
C	$Id: gzreop.f,v 1.7 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZREOP(LIDR,IDR,ENAM)
C
C  Reopen CGM workstation.
C
      include 'gkscom.h'
C
      INTEGER WKID,CONID,WTYPE,ENAM
      CHARACTER*80 IDR(LIDR)
      INTEGER LASF(13)
C
      INTEGER CIATT(31)
      REAL    CFATT(19)
C
      RERR = 0
C
C  Read in control information
C
C       WKID   -  workstation identifier.
C       CONID  -  connection identifier.
C       WTYPE  -  workstation type.
C       IAFLG  -  flag to indicate if attribute settings are being supplied.
C       NCLRS  -  the number of colors in the supplied color table
C                 (can equal 0).
C       NCXS   -  The color index associated with the first color in the
C                 color table (all other color indices are filled in in
C                 sequence).
C       NOPTN  -  the action to be taken -
C                    = 0 - reestablish the color table only.
C                    = 1 - reestablish the color table and GKS state
C                          (do not issue the state values to the metafile).
C                    = 2 - reestablish the color table and GKS state 
C                          (issue GKS state values to the metafile).
C                    = 3 - reestablish the color table and issue the
C                          current GKS state values to the metafile.
C
      READ(IDR(1),502) WKID,  CONID, WTYPE, IAFLG, NCLRS, NCXS, NOPTN
  502 FORMAT(7I5)
C
C  Check if the workstation identifier is valid.
C
      IF (WKID .LT. 0) THEN
        ERS = 1
        CALL GERHND(-401,ENAM,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the connection identifier is valid.
C
      IF (CONID.EQ.5 .OR. CONID.EQ.6) THEN
        ERS = 1
        CALL GERHND(-402,ENAM,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the workstation type is valid.
C
      IF (WTYPE .NE. GCGM) THEN
        ERS = 1
        CALL GERHND(-403,ENAM,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the workstation is currently open.
C
      IF (NOPWK .GT. 0) THEN
        DO 204 I=1,NOPWK
          IF (SOPWK(I) .EQ. WKID) THEN
            ERS = 1
            CALL GERHND(-404,ENAM,ERF)
            ERS = 0
            RETURN
          ENDIF
  204   CONTINUE
      ENDIF
C
C  Check if there is room for another open workstation.
C
      IF (NOPWK .GE. MOPWK) THEN
        ERS = 1
        CALL GERHND(-405,ENAM,ERF)
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
            CALL GERHND(-406,ENAM,ERF)
            ERS = 0
            RETURN
          ENDIF
   30   CONTINUE
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
C  Pass file name and type information across the workstation interface.
C
      CUFLAG = WKID
      FCODE = -4
      CONT  = 0
      CALL GZROI(0)
      IL1   = 3
      IL2   = 3
      ID(1) = WKID
      ID(2) = CONID
      ID(3) = WTYPE
      CALL GZTOWK
      CUFLAG = -1
      IF (RERR .GT. 0) THEN
        RETURN
      ELSE IF (RERR .EQ. -10) THEN
        NOPICT = 1
      ENDIF
C
C  Re-establish the color table.
C
C    ICNDX is the current color index.
C    IANDX is a pointer into the color table of the IDR array.
C
      ICNDX = NCXS-1
      IANDX = 6
CRLB:      IF (IAFLG .NE. 0) IANDX = 12
      IF (IAFLG .NE. 0) IANDX = 14
C
      ICRM = MOD(NCLRS,4)
      ICDV = NCLRS/4
      DO 50 I=1,ICDV
        DO 60 J=1,4
          ID1 = 18*(J-1)+1
          ID2 = ID1+17
          READ(IDR(IANDX)(ID1:ID2),500) RED,GREEN,BLUE
  500     FORMAT(3F6.3)
          ICNDX = ICNDX+1
          CALL GSCR(WKID,ICNDX,RED,GREEN,BLUE) 
   60   CONTINUE
        IANDX = IANDX+1
   50 CONTINUE
C
      DO 70 J=1,ICRM
        ID1 = 18*(J-1)+1
        ID2 = ID1+17
        READ(IDR(IANDX)(ID1:ID2),500) RED,GREEN,BLUE
        ICNDX = ICNDX+1
        CALL GSCR(WKID,ICNDX,RED,GREEN,BLUE)
   70 CONTINUE
C
C  If the attributes are available, read them in.
C
      IF (IAFLG .NE. 0) THEN
C        --------------------------------------------------
C        Serializing these values was modified to support the presence of
C        32bit ARGB values, which are always *much* larger in magnitude
C        than the original I4 character fields.  Expanded to I10s; note
C        that 2 extra 80-character records are now required, so that
C        subsequent offsets into IDR have changed.     RLB 1/2012
C
C        READ(IDR( 6),503) (CIATT(LL),LL= 1,20)
C        READ(IDR( 7),504) (CIATT(LL),LL=21,31)
C        READ(IDR( 8),505) (CFATT(LL),LL= 1, 5)
C        READ(IDR( 9),505) (CFATT(LL),LL= 6,10)
C        READ(IDR(10),505) (CFATT(LL),LL=11,15)
C        READ(IDR(11),506) (CFATT(LL),LL=16,19)
C        --------------------------------------------------
        READ(IDR( 6),508) (CIATT(LL),LL= 1, 8)
        READ(IDR( 7),508) (CIATT(LL),LL= 9,16)
        READ(IDR( 8),508) (CIATT(LL),LL=17,24)
        READ(IDR( 9),508) (CIATT(LL),LL=25,31)
        READ(IDR(10),505) (CFATT(LL),LL= 1, 5)
        READ(IDR(11),505) (CFATT(LL),LL= 6,10)
        READ(IDR(12),505) (CFATT(LL),LL=11,15)
        READ(IDR(13),506) (CFATT(LL),LL=16,19)
  503   FORMAT(20I4)
  504   FORMAT(11I4)
  505   FORMAT(5E16.7)
  506   FORMAT(4E16.7)
  508   FORMAT(8I10)
      ENDIF
C
      CUFLAG = WKID
      IF (NOPTN.EQ.1 .OR. NOPTN.EQ.2) THEN
        CCLIP    = CIATT( 1)
        CPLI     = CIATT( 2)
        CLN      = CIATT( 3)
        CLWSC    = CFATT( 1)
        CPLCI    = CIATT( 4)
        CPMI     = CIATT( 5)
        CMK      = CIATT( 6)
        CMKS     = CFATT( 2)
        CPMCI    = CIATT( 7)
        CTXI     = CIATT( 8)
        CTXFP(1) = CIATT( 9)
        CTXFP(2) = CIATT(10)
        CCHXP    = CFATT( 3)
        CCHSP    = CFATT( 4)
        CTXCI    = CIATT(11)
        CCHH     = CFATT( 5)
        CCHUP(1) = CFATT( 6)
        CCHUP(2) = CFATT( 7)
        CTXP     = CIATT(12)
        CTXAL(1) = CIATT(13)
        CTXAL(2) = CIATT(14)
        CFAI     = CIATT(15)
        CFAIS    = CIATT(16)
        CFASI    = CIATT(17)
        CFACI    = CIATT(18)
        CPA(1)   = CFATT( 8)
        CPA(2)   = CFATT( 9)
        CPARF(1) = CFATT(10)
        CPARF(2) = CFATT(11)
        CLNA     = CIATT(19)
        CLWSCA   = CIATT(20)
        CPLCIA   = CIATT(21)
        CMKA     = CIATT(22)
        CMKSA    = CIATT(23)
        CPMCIA   = CIATT(24)
        CTXFPA   = CIATT(25)
        CCHXPA   = CIATT(26)
        CCHSPA   = CIATT(27)
        CTXCIA   = CIATT(28)
        CFAISA   = CIATT(29)
        CFASIA   = CIATT(30)
        CFACIA   = CIATT(31)
C
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
      ENDIF
      IF (NOPTN.EQ.2 .OR. NOPTN.EQ.3) THEN
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
C
C  Send out the context.
C
C  Put out new picture initialization if CGM and the picture is empty.
C
        IF (WTYPE .EQ. GCGM) THEN
          IF (NOPICT .LE. 0) THEN
            FCODEO = FCODE
            CONTO  = CONT
            FCODE = 91
            CONT  =  0
            CALL GZROI(0)
            CALL G01WDR(WKID,' ')
            FCODE  = FCODEO
            CONT   = CONTO
            NOPICT = 1
          ENDIF
        ENDIF
C
        CUFLAG = WKID
        FCODE = -6
        CONT  = 0
        CALL GZROI(0)
        IL1   = 0
        IL2   = 0
        CALL GZTOWK
        CUFLAG = -1
        IF (RERR .NE. 0) RETURN
      ENDIF
C
      CUFLAG = -1
C
C  Make sure that the current normalization transformation is in
C  effect.
C
      CALL GSELNT(CNT)
C
      RETURN
      END
