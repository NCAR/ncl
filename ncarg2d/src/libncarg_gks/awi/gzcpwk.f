C
C	$Id: gzcpwk.f,v 1.2 1993-03-19 01:27:38 fred Exp $
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
      CALL G01MIO(1, WCONID, STR(1:ILEN), IDUM1, IDUM2, IER)
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
C  Copy the segment.
C
      CALL GZW2GK(WKID,WCONID,IER)
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
