C
C	$Id: g01seg.f,v 1.1.1.1 1992-04-17 22:33:59 ncargd Exp $
C
      SUBROUTINE G01SEG
C
C  This subroutine processes segmentation calls by creating
C  segments on disk.
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
      COMMON /GKSIN1/FCODE,CONT,IL1,IL2,ID(128),RL1,RL2,RX(128),
     - RY(128),STRL1,STRL2,RERR
      COMMON /GKSIN2/STR
      INTEGER FCODE, CONT, IL1, IL2, ID, RL1, RL2
      INTEGER STRL1, STRL2, RERR
      REAL  RX, RY
      CHARACTER*80 STR
      COMMON  /G01IO/   MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY(256)     ,
     +                  MOBFSZ  ,MOUTBF(720)    ,MBFPOS ,
     +                  MFGLUN  ,MXBITS         ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
        INTEGER         MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY  ,MOBFSZ ,
     +                  MBFPOS  ,MFGLUN ,MOUTBF ,MXBITS ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
      COMMON  /G01CHA/  MFNAME  ,MPNAME
      CHARACTER*80      MFNAME  ,MPNAME
C
      CHARACTER CURNAM*7,NLIST(100)*80,FNAME*80,DNAME*1
      CHARACTER*80 IDR(1),ODR(1)
      INTEGER SEGID(100),IFLEN(100)
      DIMENSION TBUF(720)
C
      SAVE
C
      DATA LINDX/0/
C
      ICODE = FCODE
      GO TO (280,281,282,283,284) ICODE-79
C
  280 CONTINUE
      IF (IL2 .GT. 0) THEN
        IFUNIT = ID(1)
        INAME  = ID(2)
      ENDIF
C
C  Check if flash buffer connection ID is the same as that
C  for the metafile output.
C
      IF (IFUNIT .EQ. MFGLUN) THEN
        RERR = -106
        GO TO 110
      ENDIF
C
C  Create file name with prefix GNFB, and store the name.
C
      WRITE(CURNAM(1:6),500) INAME
      IF(INAME .LT. 10) CURNAM(5:5) = '0'
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
C  Save output unit, and record information for later restore.
C
      IUNOLD = MFGLUN
      MRCOLD = MRECNM
C
C  Change the output unit.
C
      MFGLUN = IFUNIT
      CURNAM(7:7) = CHAR(0)
      CALL G01MIO(1, MFGLUN, CURNAM(1:6), IDUM1, IDUM2, IER)
      IF (IER .NE. 0) THEN
        RERR = -105
        GO TO 110
      ENDIF
      CALL G01MIO(5,MFGLUN,DNAME,IDUM1,IDUM2,IER)
      MRECNM = 1
C
C  Send out all attribute settings.
C
      CALL G01DMP
      RETURN
C
  281 CONTINUE
C
C  Flush the output buffer.
C
      CALL G01FLB(IER)
      IF (IER .NE. 0) THEN
        RERR = 303
        GO TO 110
      ENDIF
C
C  Store the current FLASH buffer name and the number of records.
C
      LINDX = LINDX+1
      SEGID(LINDX) = INAME
      NLIST(LINDX) = ' '
      NLIST(LINDX) = CURNAM
      IFLEN(LINDX) = MRECNM-1
C
C  Close the FLASH buffer; restore the default metafile output.
C
      CALL G01MIO(2, MFGLUN, DNAME, IDUM1, IDUM2, IER)
      MFGLUN = IUNOLD
      MRECNM = MRCOLD
C
C  Write current attributes to metafile output.
C
      IDR(1) = '2'
      IFID = -1394
      CALL GESC(IFID,1,IDR,1,IDUM,ODR)
      CALL G01DMP
      RETURN
C
  282 CONTINUE
      IF (IL2 .GT. 0) THEN
        IFUNIT = ID(1)
        INAME  = ID(2)
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
C  Check that buffer ID is in the list.
C
      IF (LINDX .LT. 1) THEN
        RERR = 122
        GO TO 110
      ELSE IF (LINDX .GT. 100) THEN
        RERR = 120
        GO TO 110
      ELSE
        DO 10 I=1,LINDX
        IF (INAME .EQ. SEGID(I)) THEN
          FNAME = NLIST(I)
          MAXREC = IFLEN(I)
          GO TO 20
        ENDIF
   10   CONTINUE
        RERR = 120
        GO TO 110
      ENDIF
   20 CONTINUE
C
C  Open the FLASH buffer, copy to metafile output unit.
C
      CALL GTNLEN(FNAME,ILEN,IER)
      FNAME(ILEN+1:ILEN+1) = CHAR(0)
      CALL G01MIO(1, IFUNIT, FNAME(1:ILEN), IDUM1, IDUM2, IER)
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
      IF (IER .EQ. -1) THEN
       RERR = 302
       GO TO 110
      ENDIF
      MRECNM = MRCOLD+I-1
      CALL G01MIO(3,MFGLUN,DNAME,TBUF,MOBFSZ,IER)
      IF (IER .NE. 0) THEN
        RERR = 303
        GO TO 110
      ENDIF
   30 CONTINUE
C
C  Close the FLASH buffer; flush the output buffer.
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
  283 CONTINUE
C
C  Decode file name
C
      FNAME = ' '
      FNAME(1:57) = STR(24:80)
      CALL GTNLEN(FNAME,ILEN,IER)
      FNAME(ILEN+1:ILEN+1) = CHAR(0)
C
C  Decode segment ID and
C  Check to see if the segment ID is already in use.
C
      READ(STR(11:20),501) INAME
      DO 40 I=1,LINDX
      IF (INAME .EQ. SEGID(I)) GO TO 50
   40 CONTINUE
      GO TO 60
   50 CONTINUE
      RERR = 121
      GO TO 110
C
   60 CONTINUE
C
C  Decode connection ID.
C
      READ(STR(21:23),502) IFUNIT
      MODEF = 4
      CALL G01MIO(1, IFUNIT, FNAME(1:ILEN), IDUM1, IDUM2, IER)
      IF (IER .NE. 0) THEN
        RERR = -105
        GO TO 110
      ENDIF
      MRCOLD = MRECNM
      CALL G01MIO(5,IFUNIT,DNAME,IDUM1,IDUM2,IER)
      MRECNM = 1
  100 CONTINUE
      CALL G01MIO(4, IFUNIT, DNAME, TBUF, MOBFSZ, IER)
      IF (IER .NE. 0) THEN
        IF (IER .EQ. -1) THEN
          LINDX = LINDX+1
          IF (LINDX .GT. 100) THEN
            RERR = 120
            GO TO 110
          ELSE
            IL1 = 1
            ID(1) = INAME
            SEGID(LINDX) = INAME
            NLIST(LINDX) = FNAME
            IFLEN(LINDX) = MRECNM-1
            CALL G01MIO(2, IFUNIT, DNAME, TBUF, MOBFSZ, IER)
            MRECNM = MRCOLD
            GO TO 110
          ENDIF
        ELSE
          RERR = IER
          GO TO 110
        ENDIF
      ENDIF
      MRECNM = MRECNM+1
      GO TO 100
C
  110 CONTINUE
      MRECNM = MRCOLD
      RETURN
C
  284 CONTINUE
C
C  Delete the named segment.
C
      INAME = ID(1)
      DO 70 I=1,LINDX
      IF (SEGID(I) .EQ. INAME) THEN
        DO 80 J=I+1,LINDX
        SEGID(J-1) = SEGID(J)
        NLIST(J-1) = NLIST(J)
        IFLEN(J-1) = IFLEN(J)
   80   CONTINUE
        SEGID(LINDX) = 0
        NLIST(LINDX) = ' '
        IFLEN(LINDX) = 0
        LINDX = LINDX-1
        GO TO 90
      ENDIF
   70 CONTINUE
   90 CONTINUE
      RETURN
C
  500 FORMAT('GNFB',I2)
  501 FORMAT(I10)
  502 FORMAT(I3)
C
      END
