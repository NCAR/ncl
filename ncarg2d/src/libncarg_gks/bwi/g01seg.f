C
C	$Id: g01seg.f,v 1.9 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE G01SEG
C
C  This subroutine processes the copy segment function for CGM
C  output.
C
C     For FCODE = 82  --  copy segment to workstation
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
      INTEGER TBUF(720)
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
C
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
      CALL G01MIO(8, IFUNIT, STR(1:ILEN), IDUM1, IDUM2, IER)
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
      CALL G01DMP(1)
      RETURN
  110 CONTINUE
      MRECNM = MRCOLD
      RETURN
C
      END
