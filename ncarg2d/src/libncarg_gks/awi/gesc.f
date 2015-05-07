C
C	$Id: gesc.f,v 1.40 2010-02-08 06:01:29 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GESC(FCTID,LIDR,IDR,MLODR,LODR,ODR)
C
C  ESCAPE
C
      INTEGER EESC
      PARAMETER (EESC=11)
C
      include 'gkscom.h'
      include 'trstat.h'
C
      INTEGER FCTID, LIDR, IBUF(46), TBUF(720)
      REAL    UBUF(26)
      CHARACTER*(*) IDR(LIDR),ODR(MLODR)
      CHARACTER*137 NAMET
      CHARACTER*5   DNAME
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,EESC,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the input data record is of the proper length.
C
      LEN1 = LEN(IDR(1))
      IF (LEN1 .NE. 80) THEN
        ERS = 1
        CALL GERHND(182,EESC,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the function ID is supported.
C
      IF (FCTID.LT.0 .AND. (FCTID.LT.-1610 .OR. FCTID.GT.-1350)) THEN
        ERS = 1
C       CALL GERHND(180,EESC,ERF)
        ERS = 0
      ENDIF
C
C  Process legal escape function ID'S:
C      -1386  --  Flag for indicating whether the clip rectangle within
C                 a segment will be transformed as per the segment 
C                 transformation. (1 = transform; 0 = do not transform)
C      -1387  --  Temporarily close an NCGM, no matter what the status 
C                 of the file.
C      -1388  --  Set/save/restore attributes.
C      -1389  --  Reopen an NCGM for appending.
C      -1390  --  Returns 'NCAR_GKS0A--VERSION_4.1' in ODR as a check 
C                 to see if the NCAR GKS package is being used.
C      -1391  --  Metafile name
C      -1392  --  FLASH4 support
C      -1393  --  Picture name
C      -1394  --  A root for forming segment file names.  For example,
C                 if the root is 'fseg', then the segment names will
C                 be of the form 'fsegnn' where 'nn' is the segment number.
C      -1395  --  Cause a pause in ctrans processing.
C      -1396  --  Flag a pause in the X driver.
C      -1397  --  Flags whether segments should be deleted or not.
C                 (0 = save; 1 = delete).
C      -1398  --  Maximum number of error messages before abort.
C      -1399  --  Flag for GKS clipping (0 = off; 1 = on).
C
C  Private escape element only for use by gescape (libncarg_gksC)
C      -1450  --  Pass native "C" data to "C" gks drivers.
C                 Causes an error 180 if called here.
C
C  X driver specific escapes (-1400 through -1410):
C      -1400  --  Specify the error allowed in matching a requested
C                 color, expressed as a percentage.  The value "0" is
C                 special--it means you don't care what color is chosen.
C                 This is only used if -1402 is "shared"
C      -1401  --  Used to tell the X driver that the window should have
C                 its own color map, allowing that window to have 256 
C                 colors.  This has been superceded by -1402.  Setting
C                 this element is equivalant to calling -1402 with "private".
C      -1402  --  Used to tell the X driver what color mode to use.  This
C                 is a super-set of -1401 above.  The three modes are
C                 "shared=0", "mixed=2", and "private=1".  "private" is the
C                 same as -1401.  It is possible to switch color modes
C                 during the middle of execution - but only in one direction.
C                 From "shared" you can switch to either of the other two
C                 modes.  From "mixed" you can switch to "private".  If you
C                 are already using "private" you can not switch.  -1400
C                 is only used if -1402 is "shared".
C
C  PostScript/PDF specific escapes (-1532 to -1510):
C
C      -1510  --  Flags beginning of segment copy for PS workstations.
C      -1511  --  Flags end of segment copy for PS workstations.
C      -1512  --  Spacing between fill lines.
C      -1513  --  Spacing between hatch lines.
C      -1514  --  Maximum size of the stack.
C      -1515  --  Maximum number of points in a path.
C      -1516  --  Scale factor for nominal linewidth.
C      -1517  --  Background fills entire page.
C      -1518  --  Line joins.
C      -1519  --  Line caps.
C      -1520  --  Miter limit.
C      -1521  --  Coordinate points for picture positioning.
C      -1522  --  Scale factor for PS coordintaes.
C      -1523  --  Select CMYK color model or RGB model.
C      -1524  --  Flag for suppressing bounding box and background color
C                   0 = put out background and bounding box;
C                   1 = neither background or bounding box;
C                   2 = no background;
C                   3 = no bounding box
C      -1525  --  Indicate portrait (=0) or landscape (=1) mode for PS.
C      -1526  --  Positioning coordinates that can be set between pictures
C                   or in the middle of a picture.
C
C      -1527  --  Produce an NCAR logo.
C      -1528  --  Bounding box coordinates for EPS/EPSI files.
C      -1529  --  Width for PDF MediaBox
C      -1530  --  Height for PDF MediaBox
C      -1531  --  Width for PS PageSize
C      -1532  --  Height for PS PageSize
C
      IF (FCTID .EQ. -1399) THEN
C
C  Value to flag if GKS clipping is on. (0 = no; 1 = yes).
C
        READ(IDR(1), 501) GKSCLP
      ELSE IF (FCTID .EQ. -1398) THEN
C
C  Get maximum number of allowable error messages.
C
        READ (IDR(1),500,ERR=135) MXERMG
        MXERMG = MAX(0,MXERMG)
        RETURN
  135   CONTINUE
        ERS = 1
        CALL GERHND(182,EESC,ERF)
        ERS = 0
        RETURN
      ELSE IF (FCTID .EQ. -1397) THEN
C
C  Save segment flag. (1 = delete; 0 = save).
C
        READ(IDR(1), 501) SEGDEL
      ELSE IF (FCTID .EQ. -1396) THEN
C
C  Decode the workstation ID.
C
        READ (IDR(1),501,ERR=130) IWKID
        GO TO 140
  501   FORMAT(I5)
  130   CONTINUE
        ERS = 1
        CALL GERHND(182,EESC,ERF)
        ERS = 0
        RETURN
  140   CONTINUE
        CUFLAG = IWKID
        FCODE = 6
        CALL GZROI(0)
        IL1 = 1
        IL2 = 1
        ID(1) = FCTID
C
C  Send over the data record if there is one (recall that the
C  string length of STR is divisible by 80).
C
        CONT = 0
        STRL1 = 80
        STRL2 = 80
        STR(1:80) = IDR(1)
        CALL GZTOWK
        IF (RERR .NE. 0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
        CUFLAG = -1
      ELSE IF (FCTID .EQ. -1395) THEN
C
C  Put out a CGM MESSAGE element.  The message string is in IDR.
C
        FCODE = 93
        CONT  = 0
        CALL GZROI(0)
        STRL1 = 80
        STRL2 = 80
        STR(1:80) = IDR(1)
        IL1   = 1
        IL2   = 1
        ID(1) = FCTID
        CALL GZTOWK
        IF (RERR .NE. 0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
      ELSE IF (FCTID .EQ. -1394) THEN
C
C  Root for segment file names.
C
        GSEGRT = ' '
        GSEGRT = IDR(1)
      ELSE IF (FCTID.EQ.-1393) THEN
C
C  Picture name.
C
        FCODE = 92
        CONT = 0
        CALL GZROI(0)
        STRL1 = 80
        STRL2 = 80
        STR(1:80) = IDR(1)
        IF (NOPICT .GT. 0) THEN
          ERS = 1
          RERR = -108
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
        CALL GZTOWK
        IF (RERR .NE. 0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
      ELSE IF (FCTID .EQ. -1392) THEN
C
C  FLASH4 support (segment number is in columns 11-20; segment
C  name is in columns 24-80 of IDR(1) (and in IDR(2) if long enough).
C
C
C  Check if the input data record is dimensioned properly.
C
        IF (LIDR .LE. 0) THEN
           ERS = 1
           CALL GERHND(182,EESC,ERF)
           ERS = 0
           RETURN
        ENDIF
C
C  Check if the segment is already in use.
C
        READ(IDR(1)(11:20),500) ICSEG 
  500   FORMAT(I10)
        DO 210 I=1,NUMSEG
          IF (SEGS(I) .EQ. ICSEG) THEN
            ERS = 1
            CALL GERHND(121,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
  210   CONTINUE
        NUMSEG = NUMSEG+1
        SEGS(NUMSEG) = ICSEG
        SEGNAM(NUMSEG)(1:57) = IDR(1)(24:80)
        IF (LIDR .EQ. 2) THEN
          SEGNAM(NUMSEG)(58:137) = IDR(2)
        ENDIF
C
C  Get the connection ID.
C
        READ(IDR(1)(21:23),510) IFUNIT
  510   FORMAT(I3)
C
C  Determine the segment length.
C
        CALL GTNLEN(SEGNAM(NUMSEG),ILEN,IER)
        NAMET = ' '
        NAMET(1:ILEN) = SEGNAM(NUMSEG)(1:ILEN)
        NAMET(ILEN+1:ILEN+1) = CHAR(0)
        CALL G01MIO(8, IFUNIT, NAMET(1:ILEN+1), IDUM1, IDUM2, IER)
        IF (IER .NE. 0) THEN
          RERR = -105
          GO TO 110
        ENDIF
        CALL G01MIO(5,IFUNIT,DNAME,IDUM1,IDUM2,IER)
        MRECNM = 1
        MOBFSZ = 1+(11520-1)/I1MACH(5)
  100   CONTINUE
        CALL G01MIO(4, IFUNIT, DNAME, TBUF, MOBFSZ, IER)
        IF (IER .NE. 0) THEN
          IF (IER .EQ. -1) THEN
            IF (NUMSEG .GT. 100) THEN
              RERR = 120
              GO TO 110
            ELSE
              SEGLEN(NUMSEG) = MRECNM-1
              CALL G01MIO(2, IFUNIT, DNAME, TBUF, MOBFSZ, IER)
              GO TO 110
            ENDIF
          ELSE
            RERR = IER
            GO TO 110
          ENDIF
        ENDIF
        MRECNM = MRECNM+1
        GO TO 100
  110   CONTINUE 
C
C  Default the segment transformation.
C
        DO 30 I=1,2
          DO 40 J=1,3
            SEGT(NUMSEG,I,J) = 0.
   40     CONTINUE
   30   CONTINUE
        SEGT(NUMSEG,1,1) = 1.
        SEGT(NUMSEG,2,2) = 1.
      ELSE IF (FCTID .EQ. -1391) THEN
C
C  File name for output metafile.
C
        GFNAME = ' '
        DO 250 I=1,LIDR
          JLIM = 80
          IF (I .EQ. 13) JLIM = 64
          DO 260 J=1,JLIM
            INDX = 80*(I-1)+J
            GFNAME(INDX:INDX) = IDR(I)(J:J)
  260     CONTINUE
  250   CONTINUE
C
C  Set flag to indicate that the current picture is empty.
C
        NOPICT = 0
      ELSE IF (FCTID .EQ. -1390) THEN
        ODR(1) = 'NCAR_GKS0A--VERSION_4.0'
      ELSE IF (FCTID .EQ. -1389) THEN
C
C  Reopen NCGM for appending.
C
        GFNAME = ' '
        DO 255 I=1,4
          JLIM = 80
          IF (I .EQ. 4) JLIM = 16
          DO 265 J=1,JLIM
            INDX = 80*(I-1)+J
            GFNAME(INDX:INDX) = IDR(I+1)(J:J)
  265     CONTINUE
  255   CONTINUE
C
C  Set flag to indicate that the current picture is empty (GZREOP
C  will reset this to picture not empty if the reopen is done on
C  a file in mid picture.
C
        NOPICT = 0
        CALL GZREOP(LIDR, IDR, EESC)
        IF (RERR .GT. 0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
C
C  X11 window escapes.
C
      ELSE IF (FCTID.GE.-1410 .AND. FCTID.LE.-1400) THEN
C
C  Decode the workstation ID.
C
        READ (IDR(1),501,ERR=150) IWKID
        GO TO 160
C
  150   CONTINUE
        ERS = 1
        CALL GERHND(182,EESC,ERF)
        ERS = 0
        RETURN
C
  160   CONTINUE
C
C  If setting the X Color Model, and IWKID = -1, then the setting 
C  applies to the next open X workstation.
C
	IF (FCTID.EQ.-1402 .AND. IWKID.EQ.-1) THEN
	  READ(IDR(1)(7:11),501,ERR=150) COLMOD
	  IF (COLMOD.LT.0 .OR. COLMOD.GT.2) THEN
	    GOTO 150
	  ENDIF
	  RETURN
	ENDIF
C
C  Return if not an X11 workstation.
C
        CALL GQWKC(IWKID,IER,ICONID,ITYP)
        IF (ITYP.NE.GXWE .AND. ITYP.NE.GXWC) THEN
          CUFLAG = -1
          RETURN
        ENDIF
C
        CUFLAG = IWKID
        FCODE = 6
        CALL GZROI(0)
        IL1 = 1
        IL2 = 1
        ID(1) = FCTID
C
C  Send over the data record.
C
        IF (LIDR .EQ. 1) THEN
          CONT = 0
          STRL1 = 80
          STRL2 = 80
          STR(1:80) = IDR(1)
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
          CUFLAG = -1
        ELSE IF (LIDR .GT. 1) THEN
          CONT = 1
          STRL1 = 80*LIDR
          STRL2 = 80
          LDRM1 = LIDR-1
          DO 220 I=1,LDRM1
            STR(1:80) = IDR(I)
            IF (I .GT. 1) IL2 = 0
            CALL GZTOWK
            IF (RERR .NE. 0) THEN
              ERS = 1
              CALL GERHND(RERR,EESC,ERF)
              ERS = 0
              RETURN
            ENDIF
  220     CONTINUE
          CONT = 0
          STR(1:80) = IDR(LIDR)
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
          CUFLAG = -1
        ENDIF
      ELSE IF (FCTID .EQ. -1388) THEN
C
C  Save/restore attributes.
C
C  RLB, 1/2012:
C  This escape was modified in conjunction with ngmisc/ngsrat.f to change the width
C  of formatted integer fields from 5 to 10 characters.  The wider fields are needed to
C  properly encode 32bit ARGB values, which are generally large magnitude integers.
C  Note also that now state is encoded across 4 80-character records (IBUF/UBUF),
C  instead of 3.
        READ (IDR(1),501) IOPT
        IF (IOPT.EQ.0 .OR. IOPT.EQ.1) THEN
          CALL GZUSAT(IOPT,IBUF,UBUF)
        ELSE IF (IOPT .EQ. 2) THEN
          CALL GZUSAT(IOPT,IBUF(16),UBUF)
          IBUF(2) = IBUF(16)
          IBUF(3) = IBUF(18)
          IBUF(4) = IBUF(19)
          IBUF(5) = IBUF(21)
          IBUF(6) = IBUF(22)
          IBUF(7) = IBUF(24)
          IBUF(8) = IBUF(25)
          IBUF(9) = IBUF(26)
          IBUF(10) = IBUF(27)
          IBUF(11) = IBUF(28)
          IBUF(12) = IBUF(29)
          IBUF(13) = IBUF(31)
          IBUF(14) = IBUF(32)
          IBUF(15) = IBUF(33)
CRLB:     WRITE(ODR(1),560) (IBUF(LL),LL=2,15)
          WRITE(ODR(1),561) (IBUF(LL),LL=2,8)
          WRITE(ODR(2),561) (IBUF(LL),LL=9,15)
  560     FORMAT(14I5)
  561     FORMAT(7I10)
          WRITE(ODR(3),540) (UBUF(LL),LL=1,5)
          WRITE(ODR(4),550) (UBUF(LL),LL=6,7)
        ELSE IF (IOPT .EQ. 3) THEN
CRLB:     READ (IDR(1),530) (IBUF(LL),LL=1,15)
          READ (IDR(1),531) (IBUF(LL),LL=1,8)
          READ (IDR(2),561) (IBUF(LL),LL=9,15)
  530     FORMAT(15I5)
  531     FORMAT(I5,7I10)
          READ (IDR(3),540) (UBUF(LL),LL=1,5)
  540     FORMAT(5E16.7)
          READ (IDR(4),550) (UBUF(LL),LL=6,7)
  550     FORMAT(2E16.7)
          IBUF(16) = IBUF(2)
          IBUF(17) = 1
          IBUF(18) = IBUF(3)
          IBUF(19) = IBUF(4)
          IBUF(20) = 1
          IBUF(21) = IBUF(5) 
          IBUF(22) = IBUF(6) 
          IBUF(23) = 1
          IBUF(24) = IBUF(7) 
          IBUF(25) = IBUF(8) 
          IBUF(26) = IBUF(9) 
          IBUF(27) = IBUF(10) 
          IBUF(28) = IBUF(11) 
          IBUF(29) = IBUF(12) 
          IBUF(30) = 1
          IBUF(31) = IBUF(13) 
          IBUF(32) = IBUF(14) 
          IBUF(33) = IBUF(15) 
          IBUF(34) = 1
          IBUF(35) = 1
          IBUF(36) = 1
          IBUF(37) = 1
          IBUF(38) = 1
          IBUF(39) = 1
          IBUF(40) = 1
          IBUF(41) = 1
          IBUF(42) = 1
          IBUF(43) = 1
          IBUF(44) = 1
          IBUF(45) = 1
          IBUF(46) = 1
C
          DO 155 I=1,7
            UBUF(7+I) = UBUF(I)
  155     CONTINUE
          UBUF(15) = 1.
          UBUF(16) = 1.
          UBUF(17) = 0.
          UBUF(18) = 0.
          CALL GQCLIP(IER,ICD,UBUF(19)) 
          SCL = 1./SQRT(UBUF(13)*UBUF(13)+UBUF(14)*UBUF(14))
          XP = UBUF(12)*SCL*UBUF(13)
          YP = UBUF(12)*SCL*UBUF(14)
          XBT =  YP
          YBT = -XP
          CALL GZW2NX(1,XP,XTMP)
          CALL GZW2NY(1,YP,YTMP)
          CALL GZW2NX(1,0.,ZXTMP)
          CALL GZW2NY(1,0.,ZYTMP)
          UBUF(23) = XTMP-ZXTMP
          UBUF(24) = YTMP-ZYTMP
          CALL GZW2NX(1,XBT,XTMP)
          CALL GZW2NY(1,YBT,YTMP)
          UBUF(25) = XTMP-ZXTMP
          UBUF(26) = YTMP-ZYTMP
C
          CALL GZUSAT(3,IBUF(16),UBUF(8))
        ENDIF
      ELSE IF (FCTID .EQ. -1387) THEN
C
C  Terminate an NCGM without putting out an END PICTURE or an 
C  END METAFILE.  This is to be used as a way of suspending
C  output to a metafile for possible reopening with GZREOP.
C
        READ (IDR(1),501) IWKID
C
C  Check that GKS is in the proper state.
C
        CALL GZCKST(7,EESC,IER)
        IF (IER .NE. 0) RETURN
C
C  Check if the workstation identifier is valid.
C
        CALL GZCKWK(20,EESC,IWKID,IDUM,IER)
        IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
        CALL GZCKWK(25,EESC,IWKID,IDUM,IER)
        IF (IER .NE. 0) RETURN
C
C  Check if the workstation is currently active.
C
        CALL GZCKWK(29,EESC,IWKID,IDUM,IER)
        IF (IER .NE. 0) RETURN
C
C  Invoke the workstation interface (do this before flagging
C  the workstation as closed).
C
C  Determine the workstation type.
C
        DO 230 I=1,NOPWK
          IF (SOPWK(I) .EQ. IWKID) THEN
            NWKTP = SWKTP(I)
            GO TO 240
          ENDIF
  230   CONTINUE
  240   CONTINUE
C
        IF (NWKTP .NE. GCGM) THEN
          ERS = 1
          CALL GERHND(-400,EESC,ERF)
          ERS = 0
        ENDIF
C
C  Invoke the workstation interface.  Set CUFLAG to indicate that 
C  the interface call should go only to the specifically designated 
C  workstation.

        CUFLAG = IWKID
        FCODE = -5
        CONT  = 0
        CALL GZROI(0)
        IL1  = 1
        IL2  = 1
        ID(1) = IWKID
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
        ENDIF
        CUFLAG = -1
C
C  Remove the workstation identifier from the set of open 
C  workstations in the GKS state list and delete it from
C  the X workstation list.
C
        IF (NOPWK .EQ. 1) THEN
          SOPWK(1) = -1
          SWKTP(1) = -1
          LXWKID(1) = -1
          NOPWK = 0
        ELSE
          DO 201 I=1,NOPWK
            IF (SOPWK(I) .EQ. IWKID) THEN
              IF (I .EQ. NOPWK) THEN
                SOPWK(NOPWK) = -1
                SWKTP(NOPWK) = -1
                LXWKID(I) = -1
                NOPWK = NOPWK-1
              ELSE
                NM1 = NOPWK-1
                DO 202 J=I,NM1
                  SOPWK(J) = SOPWK(J+1)
                  SWKTP(J) = SWKTP(J+1)
                  LXWKID(J) = LXWKID(J+1)
  202           CONTINUE
                SOPWK(NOPWK) = -1
                SWKTP(NOPWK) = -1
                LXWKID(NOPWK) = -1
                NOPWK = NOPWK-1
              ENDIF
            ENDIF
  201     CONTINUE
        ENDIF
C
C  Set GKS to state GKOP if no workstations remain open.
C
        IF (SOPWK(1) .EQ. -1) THEN
          OPS = GGKOP
        ENDIF
      ELSE IF (FCTID .EQ. -1386) THEN
        READ (IDR(1),501,ERR=137) IGSGCP
        GO TO 136
  137   CONTINUE
        ERS = 1
        CALL GERHND(182,EESC,ERF)
        ERS = 0
        RETURN
  136   CONTINUE
C
C
C  PostScript and PDF escapes.
C
      ELSE IF (FCTID.GE.-1532 .AND. FCTID.LE.-1510) THEN
C
C  Decode the workstation ID.
C
        READ (IDR(1),501,ERR=350) IWKID
        GO TO 360
C
  350   CONTINUE
        ERS = 1
        CALL GERHND(182,EESC,ERF)
        ERS = 0
        RETURN
C
  360   CONTINUE
        CUFLAG = IWKID
C
C  If setting coordinates for positioning on the page, store them
C  for use with the next OPEN WORKSTATION.  This call can be made
C  with no workstations open.
C
        IF (FCTID .EQ. -1521) THEN
          CUFLAG = -1
          READ(IDR(1)( 2:10),520) CLLX
          READ(IDR(1)(12:20),520) CLLY
          READ(IDR(1)(22:30),520) CURX
          READ(IDR(1)(32:40),520) CURY
  520     FORMAT(I9)
        ENDIF 
C
C  Coordinate scale factor for next PostScript workstation opened.
C
        IF (FCTID .EQ. -1522) THEN
          CUFLAG = -1
          READ(IDR(1)(1:5),501) CPSCL
          RETURN
        ENDIF 
C
C  Color model selection (1=RGB, 0=CMYK).
C
        IF (FCTID .EQ. -1523) THEN
          READ(IDR(1)(1: 5),501) CCMDL
        ENDIF 
C
C  Suppress flag for background color and/or bounding box.
C
        IF (FCTID .EQ. -1524) THEN
          READ(IDR(1)(1:5),501) CSUPR
          RETURN
        ENDIF 
C
C  Portrait/landscape flag for PS and PDF output.
C
        IF (FCTID .EQ. -1525) THEN
          READ(IDR(1)(1:5),501,ERR=146) IWKID
          GO TO 166 
  146     CONTINUE
          ERS = 1
          CALL GERHND(182,EESC,ERF)
          ERS = 0
          RETURN
  166     CONTINUE
          CUFLAG = IWKID
          READ(IDR(1)(6:10),501) CPTLD
        ENDIF 
C
C  Positioning coordinates for PS that can be set between pictures
C  or in the middle of a picture.
C
        IF (FCTID .EQ. -1526) THEN
          READ(IDR(1)(1:5),501,ERR=145) IWKID
          GO TO 165 
  145     CONTINUE
          ERS = 1
          CALL GERHND(182,EESC,ERF)
          ERS = 0
          RETURN
  165     CONTINUE
          CUFLAG = IWKID
        ENDIF 
C
C
C  Width for PDF MediaBox in 1/72" units.
C
        IF (FCTID .EQ. -1529) THEN
          READ(IDR(1)(1:5),501) PDFWTH
          CUFLAG = -1
          RETURN
        ENDIF 
C
C  Height for PDF MediaBox in 1/72" units.
C
        IF (FCTID .EQ. -1530) THEN
          READ(IDR(1)(1:5),501) PDFHGT
          CUFLAG = -1
          RETURN
        ENDIF 
C
C
C  Width for PS paper size width in 1/72" units.
C
        IF (FCTID .EQ. -1531) THEN
          READ(IDR(1)(1:5),501) PSWTH
          CUFLAG = -1
          RETURN
        ENDIF 
C
C  Height for PS paper size hight in 1/72" units.
C
        IF (FCTID .EQ. -1532) THEN
          READ(IDR(1)(1:5),501) PSHGT
          CUFLAG = -1
          RETURN
        ENDIF 
C
C  Return if not a PostScript or PDF workstation, unless FCTID = -1521
C  or FCTID = -1525, or FCTID = -1526.
C
        CALL GQWKC(IWKID,IER,ICONID,ITYP)
        IF (ITYP.NE.GPDFP .AND. ITYP.NE.GPDFL) THEN
          IF (ITYP.GT.GPSMAX .OR. ITYP.LT.GPSMIN) THEN
            CUFLAG = -1
            IF (FCTID.NE.-1521 .AND. FCTID.NE.-1525 .AND.
     +          FCTID.NE.-1526) RETURN
          ENDIF
        ENDIF
C
  167   CONTINUE
        FCODE = 6
        CALL GZROI(0)
        IL1 = 1
        IL2 = 1
        ID(1) = FCTID
C
C  Send over the data record.
C
        IF (LIDR .EQ. 1) THEN
          CONT = 0
          STRL1 = 80
          STRL2 = 80
          STR(1:80) = IDR(1)
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
          CUFLAG = -1
        ELSE IF (LIDR .GT. 1) THEN
          CONT = 1
          STRL1 = 80*LIDR
          STRL2 = 80
          LDRM1 = LIDR-1
          DO 320 I=1,LDRM1
            STR(1:80) = IDR(I)
            IF (I .GT. 1) IL2 = 0
            CALL GZTOWK
            IF (RERR .NE. 0) THEN
              ERS = 1
              CALL GERHND(RERR,EESC,ERF)
              ERS = 0
              RETURN
            ENDIF
  320     CONTINUE
          CONT = 0
          STR(1:80) = IDR(LIDR)
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
          CUFLAG = -1
        ENDIF
C
C  Positioning coordinates for PS that can be set between pictures
C  or in the middle of a picture.
C
        IF (FCTID .EQ. -1528) THEN
          READ(IDR(1)(1:5),501,ERR=176) IWKID
          GO TO 175 
  176     CONTINUE
          ERS = 1
          CALL GERHND(182,EESC,ERF)
          ERS = 0
          RETURN
  175     CONTINUE
          CUFLAG = IWKID
        ENDIF 
C
C  Return if not a PostScript or PDF workstation, unless FCTID = -1521
C  or FCTID = -1525, or FCTID = -1526, or FCTID = -1528.
C
        CALL GQWKC(IWKID,IER,ICONID,ITYP)
        IF (ITYP.NE.GPDFP .AND. ITYP.NE.GPDFL) THEN
          IF (ITYP.GT.GPSMAX .OR. ITYP.LT.GPSMIN) THEN
            CUFLAG = -1
            IF (FCTID.NE.-1521 .AND. FCTID.NE.-1525 .AND.
     +        FCTID.NE.-1526 .AND. FCTID.NE.-1528) RETURN
          ENDIF
        ENDIF
C
        FCODE = 6
        CALL GZROI(0)
        IL1 = 1
        IL2 = 1
        ID(1) = FCTID
C
C  Send over the data record.
C
        IF (LIDR .EQ. 1) THEN
          CONT = 0
          STRL1 = 80
          STRL2 = 80
          STR(1:80) = IDR(1)
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
          CUFLAG = -1
        ELSE IF (LIDR .GT. 1) THEN
          CONT = 1
          STRL1 = 80*LIDR
          STRL2 = 80
          LDRM1 = LIDR-1
          DO 321 I=1,LDRM1
            STR(1:80) = IDR(I)
            IF (I .GT. 1) IL2 = 0
            CALL GZTOWK
            IF (RERR .NE. 0) THEN
              ERS = 1
              CALL GERHND(RERR,EESC,ERF)
              ERS = 0
              RETURN
            ENDIF
  321     CONTINUE
          CONT = 0
          STR(1:80) = IDR(LIDR)
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
          CUFLAG = -1
        ENDIF
C
C  Sending "native" C data to a C driver (NOT FROM FORTRAN!)
C
      ELSE IF (FCTID .EQ. -1450) THEN
        ERS = 1
        CALL GERHND(180,EESC,ERF)
        ERS = 0
        RETURN
      ELSE
C
C  Send ESCAPE element.
C
        FCODE = 6
        CALL GZROI(0)
        IL1 = 1
        IL2 = 1
        RL1 = 0
        RL2 = 0
        IC1 = 0
        IC2 = 0
        ID(1) = FCTID
C
C  Send over the data record if there is one (recall that the
C  string length of STR is divisible by 80).
C
        IF (LIDR .GE. 1) THEN
          IF (LIDR .EQ. 1) THEN
            CONT = 0
            STRL1 = 80
            STRL2 = 80
            STR(1:80) = IDR(1)
            CALL GZTOWK
            IF (RERR .NE. 0) THEN
              ERS = 1
              CALL GERHND(RERR,EESC,ERF)
              ERS = 0
              RETURN
            ENDIF
          ELSE
C
C  Send over the data record 80 characters at a time.
C
            CONT = 1
            STRL1 = 80*LIDR
            STRL2 = 80
            LDRM1 = LIDR-1
            DO 200 I=1,LDRM1
              STR(1:80) = IDR(I)
              IF (I .GT. 1) IL2 = 0
              CALL GZTOWK
              IF (RERR .NE. 0) THEN
                ERS = 1
                CALL GERHND(RERR,EESC,ERF)
                ERS = 0
                RETURN
              ENDIF
  200       CONTINUE
            CONT = 0
            STR(1:80) = IDR(LIDR)
            CALL GZTOWK
            IF (RERR .NE. 0) THEN
              ERS = 1
              CALL GERHND(RERR,EESC,ERF)
              ERS = 0
              RETURN
            ENDIF
          ENDIF
        ELSE
          CONT = 0
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
