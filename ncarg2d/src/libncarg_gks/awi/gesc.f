C
C	$Id: gesc.f,v 1.8 1994-04-28 23:28:12 fred Exp $
C
      SUBROUTINE GESC(FCTID,LIDR,IDR,MLODR,LODR,ODR)
C
C  ESCAPE
C
      INTEGER EESC
      PARAMETER (EESC=11)
C
      include 'gkscom.h'
C
      INTEGER FCTID,LIDR,TBUF(720)
      CHARACTER*(*) IDR(LIDR),ODR(MLODR)
      CHARACTER*57  NAMET
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
      IF (FCTID.LT.0.AND.FCTID.NE.-1391 .AND. FCTID.NE.-1392 .AND.
     -                   FCTID.NE.-1393 .AND. FCTID.NE.-1394 .AND.
     -                   FCTID.NE.-1395 .AND. FCTID.NE.-1396 .AND.
     -                   FCTID.NE.-1397) THEN
        ERS = 1
C       CALL GERHND(180,EESC,ERF)
        ERS = 0
      ENDIF
C
C  Process legal escape function ID'S:
C      -1391  --  Metafile name
C      -1392  --  FLASH4 support
C      -1393  --  Picture name
C      -1394  --  Currently undefined.
C      -1395  --  Cause a pause in ctrans processing.
C      -1396  --  Flag a pause in the X driver.
C      -1397  --  Color table identifier for use by NCAR Interactive.
C
C  PostScript specific escapes.
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
C      -1522  --  Scale factor for PS coordintaes
C
      IF (FCTID .EQ. -1396) THEN
C
C  Decode the workstation ID.
C
        READ (IDR,501,ERR=130) IWKID
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
C     ELSE IF (FCTID.EQ.-1394) THEN
C
C  This ESCAPE function supports the interaction of issuing
C  picture initialization and the GFLASH package.
C
C       IF (IDR(1)(1:1) .EQ. '1') THEN
C         MODEF = 1
C       ELSE IF (IDR(1)(1:1) .EQ. '0') THEN
C         MODEF = 0
C       ELSE IF (IDR(1)(1:1) .EQ. '2') THEN
C
C  Put out new picture initialization if the picture is empty.
C
C         READ(IDR(1)(2:9),230) IDWK 
C 230     FORMAT(I8)
C         IF (NOPICT .LE. 0) THEN
C           CALL G01SNP(RERR)
C           NOPICT = 1
C         ENDIF
C       ENDIF
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
C  FLASH4 support.
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
C  Check if the segment name is already in use.
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
        SEGNAM(NUMSEG) = IDR(1)(24:80)
C
C  Get the connection ID.
C
        READ(IDR(1)(21:23),510) IFUNIT
  510   FORMAT(I3)
C
C  Determine the segment length.
C
        CALL GTNLEN(IDR(1)(24:80),ILEN,IER)
        NAMET = ' '
        NAMET(1:ILEN) = IDR(1)(24:24+ILEN-1)
        NAMET(ILEN+1:ILEN+1) = CHAR(0)
        CALL G01MIO(1, IFUNIT, NAMET(1:ILEN+1), IDUM1, IDUM2, IER)
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
          IF (I .EQ. 4) JLIM = 16
          DO 260 J=1,JLIM
            INDX = 80*(I-1)+J
            GFNAME(INDX:INDX) = IDR(I)(J:J)
  260     CONTINUE
  250   CONTINUE
C
C  Set flag to indicate that the current picture is empty.
C
        NOPICT = 0
C
C  PostScript escapes.
C
      ELSE IF (FCTID.GE.-1530 .AND. FCTID.LE.-1510) THEN
C
C  Decode the workstation ID.
C
        READ (IDR,501,ERR=150) IWKID
        GO TO 160
C
  150   CONTINUE
        ERS = 1
        CALL GERHND(182,EESC,ERF)
        ERS = 0
        RETURN
C
  160   CONTINUE
        CUFLAG = IWKID
C
C  If setting coordinates for positioning on the page, store them
C  for use with the next OPEN WORKSTATION.  This call can be made
C  with no workstations open.
C
        IF (FCTID .EQ. -1521) THEN
          READ(IDR(1)( 2:10),520) CLLX
          READ(IDR(1)(12:20),520) CLLY
          READ(IDR(1)(22:30),520) CURX
          READ(IDR(1)(32:40),520) CURY
  520     FORMAT(I9)
          RETURN
        ENDIF 
C
C  Coordinate scale factor for next PostScript workstation opened.
C
        IF (FCTID .EQ. -1522) THEN
          READ(IDR(1)(1:5),520) CPSCL
          RETURN
        ENDIF 
C
C  Return if not a PostScript workstation.
C
        CALL GQWKC(IWKID,IER,ICONID,ITYP)
        IF (ITYP.GT.GPSMAX .OR. ITYP.LT.GPSMIN) RETURN 
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

        ENDIF
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
