C
C	$Id: g01seg.f,v 1.2 1993-01-09 02:06:43 fred Exp $
C
      SUBROUTINE G01SEG
C
C  This subroutine processes the copy segment function for CGM
C  output.
C
C     For FCODE = 80  --  create segment
C               = 81  --  close segment
C               = 82  --  copy segment to workstation
C               = 83  --  initialize from pre-existing segment
C               = 84  --  delete segment
C
C-------------------------------------------------------------
C
C
C       DEFINITION OF ALL COMMONS.
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01io.h'
C
      CHARACTER*1 DNAME
C     CHARACTER CURNAM*7,NLIST(100)*80,FNAME*80,DNAME*1
C     CHARACTER*80 IDR(1),ODR(1)
C     INTEGER SEGID(100),IFLEN(100)
      DIMENSION TBUF(720)
C
      SAVE
C
C     DATA LINDX/0/
C
      ICODE = FCODE
      IF (ICODE .NE. 82) THEN
        PRINT * , 'G01SEG -- internal error, see consultant'
        STOP
      ENDIF
C     GO TO (280,281,282,283,284) ICODE-79
C
C 280 CONTINUE
C     IF (IL2 .GT. 0) THEN
C       IFUNIT = ID(1)
C       INAME  = ID(2)
C     ENDIF
C
C  Check if flash buffer connection ID is the same as that
C  for the metafile output.
C
C     IF (IFUNIT .EQ. MFGLUN) THEN
C       RERR = -106
C       GO TO 110
C     ENDIF
C
C  Create file name with prefix GNFB, and store the name.
C
C     WRITE(CURNAM(1:6),500) INAME
C     IF(INAME .LT. 10) CURNAM(5:5) = '0'
C
C  Flush the output buffer.
C
C     IER = 0
C     CALL G01FLB(IER)
C     IF (IER .NE. 0) THEN
C       RERR = 303
C       GO TO 110
C     ENDIF
C
C  Save output unit, and record information for later restore.
C
C     IUNOLD = MFGLUN
C     MRCOLD = MRECNM
C
C  Change the output unit.
C
C     MFGLUN = IFUNIT
C     CURNAM(7:7) = CHAR(0)
C     CALL G01MIO(1, MFGLUN, CURNAM(1:6), IDUM1, IDUM2, IER)
C     IF (IER .NE. 0) THEN
C       RERR = -105
C       GO TO 110
C     ENDIF
C     CALL G01MIO(5,MFGLUN,DNAME,IDUM1,IDUM2,IER)
C     MRECNM = 1
C
C  Send out all attribute settings.
C
C     CALL G01DMP
C     RETURN
C
C 281 CONTINUE
C
C  Flush the output buffer.
C
C     CALL G01FLB(IER)
C     IF (IER .NE. 0) THEN
C       RERR = 303
C       GO TO 110
C     ENDIF
C
C  Store the current FLASH buffer name and the number of records.
C
C     LINDX = LINDX+1
C     SEGID(LINDX) = INAME
C     NLIST(LINDX) = ' '
C     NLIST(LINDX) = CURNAM
C     IFLEN(LINDX) = MRECNM-1
C
C  Close the FLASH buffer; restore the default metafile output.
C
C     CALL G01MIO(2, MFGLUN, DNAME, IDUM1, IDUM2, IER)
C     MFGLUN = IUNOLD
C     MRECNM = MRCOLD
C
C  Write current attributes to metafile output.
C
C     IDR(1) = '2'
C     IFID = -1394
C     CALL GESC(IFID,1,IDR,1,IDUM,ODR)
C     CALL G01DMP
C     RETURN
C
C  Copy segment to metafile.
C
C 282 CONTINUE
      IF (IL2 .GT. 0) THEN
        IFUNIT = ID(1)
        INAME  = ID(2)
        MAXREC = ID(3)
      ENDIF
C
C  Flush the output buffer.
C
      IER = 0
      CALL G01FLB(IER)
      IF (IER .NE. 0) THEN
        RERR = 303
        GO TO 110
      ENDIF
C
C  Open the segment for reading.
C
      CALL GTNLEN(STR,ILEN,IER)
      STR(ILEN+1:ILEN+1) = CHAR(0)
      CALL G01MIO(1, IFUNIT, STR(1:ILEN), IDUM1, IDUM2, IER)
      IF (IER .NE. 0) THEN
        RERR = -105
        GO TO 110
      ENDIF
      MRCOLD = MRECNM
      CALL G01MIO(5, IFUNIT, DNAME, IDUM1, IDUM2, IER)
      MRECNM = 1
C
      DO 30 I=1,MAXREC
      MRECNM = I
      CALL G01MIO(4, IFUNIT, DNAME, TBUF, MOBFSZ, IER)
      MRECNM = MRCOLD+I-1
      CALL G01MIO(3,MFGLUN,DNAME,TBUF,MOBFSZ,IER)
      IF (IER .NE. 0) THEN
        RERR = 303
        GO TO 110
      ENDIF
   30 CONTINUE
C
C  Close the segment; flush the output buffer.
C
      CALL G01MIO(2, IFUNIT, DNAME, IDUM1, IDUM2, IER)
      MRECNM = MRCOLD+MAXREC
      CALL G01FLB(IERR)
      IF (IER .NE. 0) THEN
        RERR = 303
        GO TO 110
      ENDIF
C
C  Re-establish original attribute settings.
C
      CALL G01DMP
      RETURN
C
C 283 CONTINUE
C
C  Decode file name
C
C     FNAME = ' '
C     FNAME(1:57) = STR(24:80)
C     CALL GTNLEN(FNAME,ILEN,IER)
C     FNAME(ILEN+1:ILEN+1) = CHAR(0)
C
C  Decode segment ID and
C  Check to see if the segment ID is already in use.
C
C     READ(STR(11:20),501) INAME
C     DO 40 I=1,LINDX
C     IF (INAME .EQ. SEGID(I)) GO TO 50
C  40 CONTINUE
C     GO TO 60
C  50 CONTINUE
C     RERR = 121
C     GO TO 110
C
C  60 CONTINUE
C
C  Decode connection ID.
C
C     READ(STR(21:23),502) IFUNIT
C     MODEF = 4
C     FNAME(ILEN+1:ILEN+1) = CHAR(0)
C     CALL G01MIO(1, IFUNIT, FNAME(1:ILEN), IDUM1, IDUM2, IER)
C     IF (IER .NE. 0) THEN
C       RERR = -105
C       GO TO 110
C     ENDIF
C     MRCOLD = MRECNM
C     CALL G01MIO(5,IFUNIT,DNAME,IDUM1,IDUM2,IER)
C     MRECNM = 1
C 100 CONTINUE
C     CALL G01MIO(4, IFUNIT, DNAME, TBUF, MOBFSZ, IER)
C     IF (IER .NE. 0) THEN
C       IF (IER .EQ. -1) THEN
C         LINDX = LINDX+1
C         IF (LINDX .GT. 100) THEN
C           RERR = 120
C           GO TO 110
C         ELSE
C           IL1 = 1
C           ID(1) = INAME
C           SEGID(LINDX) = INAME
C           NLIST(LINDX) = FNAME
C           IFLEN(LINDX) = MRECNM-1
C           CALL G01MIO(2, IFUNIT, DNAME, TBUF, MOBFSZ, IER)
C           MRECNM = MRCOLD
C           GO TO 110
C         ENDIF
C       ELSE
C         RERR = IER
C         GO TO 110
C       ENDIF
C     ENDIF
C     MRECNM = MRECNM+1
C     GO TO 100
C
  110 CONTINUE
      MRECNM = MRCOLD
      RETURN
C
C 284 CONTINUE
C
C  Delete the named segment.
C
C     INAME = ID(1)
C     DO 70 I=1,LINDX
C     IF (SEGID(I) .EQ. INAME) THEN
C       DO 80 J=I+1,LINDX
C       SEGID(J-1) = SEGID(J)
C       NLIST(J-1) = NLIST(J)
C       IFLEN(J-1) = IFLEN(J)
C  80   CONTINUE
C       SEGID(LINDX) = 0
C       NLIST(LINDX) = ' '
C       IFLEN(LINDX) = 0
C       LINDX = LINDX-1
C       GO TO 90
C     ENDIF
C  70 CONTINUE
C  90 CONTINUE
C     RETURN
C
C 500 FORMAT('GNFB',I2)
C 501 FORMAT(I10)
C 502 FORMAT(I3)
C
      END
