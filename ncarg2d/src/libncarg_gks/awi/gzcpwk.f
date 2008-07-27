C
C	$Id: gzcpwk.f,v 1.7 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZCPWK(WKID)
C
C  Copy the segment (whose name is stored in common variable
C  STR) to the workstation WKID.
C
C  The part of the code that interprets the segments is comprised
C  of the following subroutines:
C
C    GASPAR
C    GATELM 
C    GCELCD
C    GCELDC
C    GCELDR
C    GELMS
C    GESCFN
C    GFILAT
C    GKSPOP
C    GMARAT
C    GNINST
C    GENNEG
C    GNPART
C    GOPDEC
C    GPOLAT
C    GPUTPT
C    GSEGDT
C    GSEGRD
C    GTFLT
C    GTXTAT
C    GTXDRW
C    GXLATE
C    GXMDEF
C    GXOPDF
C    GZROI
C    GZW2GK
C
      include 'gkscom.h'
C
      INTEGER WKID
      INTEGER ICNTX(31)
      REAL    RCNTX(19)
      CHARACTER*80 IDR,ODR
C
      PARAMETER (MXCOL=256)
C
C  Open input segment for reading.
C
      ILEN = 0
      DO 10 I=1,80
      IF (STR(I:I).EQ.' ' .OR. STR(I:I).EQ.CHAR(0)) THEN
        ILEN = I-1
        GO TO 20
      ENDIF
   10 CONTINUE
   20 CONTINUE
      STR(ILEN+1:ILEN+1) = CHAR(0)
      CALL G01MIO(8, WCONID, STR(1:ILEN), IDUM1, IDUM2, IER)
      IF (IER .NE. 0) THEN
        RERR = -105
        RETURN
      ENDIF
C
C  Save the GKS attribute context.
C
      CALL GZSRAT(0,ICNTX,RCNTX)
C
C  Activate WKID if it is not active so that the output primitives
C  will be written to it.
C
      ISFLG = 0
      CALL GQWKS(WKID,IER,ISTATE)
      IF (ISTATE .EQ. 0) THEN
        ISFLG = 1
        CALL GACWK(WKID)
      ENDIF
C
C  Send a flag to PostScript workstations to indicate the beginning
C  of a segment copy so that the color setting can be retained and
C  reset after the copy.
C
      WRITE(IDR,500) WKID
  500 FORMAT(I5)
      CALL GESC(-1510,1,IDR,1,1,ODR)
C
C  Copy the segment.  Set CUFLAG to inidicate that the interface 
C  calls should go only to the designated workstation.
C
      CUFLAG = WKID
      CALL GZW2GK(WKID,WCONID,IER)
      CUFLAG = -1
C
C  Tell PostScript workstations that we are done copying the segment.
C
      CALL GESC(-1511,1,IDR,1,1,ODR)
      IF (IER .NE. 0) RETURN
C
C  Restore the attribute context.
C
      CALL GZSRAT(1,ICNTX,RCNTX)
C
C  Close the input segment.
C
      CALL G01MIO(2, WCONID, STR(1:ILEN), IDUM1, IDUM2, IER)
      IF (IER .NE. 0) RETURN
C
C  Deactive the workstation if it was not active upon invocation.
C
      IF (ISFLG .EQ. 1) CALL GDAWK(WKID)
C
      RETURN
      END 
