C
C	$Id: gzw2gk.f,v 1.1 1993-01-09 02:04:36 fred Exp $
C
      SUBROUTINE GZW2GK(WKID,CONID,IER)
C
C  Copy the segment which has been attached to Fortran unit
C  CONID to the workstation WKID by way of parsing the CGM
C  elements in the segment and making the appropriate GKS
C  calls.
C
      include 'gkscom.h'
C
      INTEGER WKID,CONID
      INTEGER IOS,STATUS
      REAL TWIND(4),TVIEW(4)
C
      EXTERNAL GSEGDT
      include 'trbufr.h'
C
C  Use normalization transformation 1 set to the identity (we use 1
C  so that the window can be changed for clipping).
C
      CALL GQCNTN(IER,NTOLD)
      CALL GQNT(NTOLD,IER,TWIND,TVIEW)
      CALL GSWN(1,0.,1.,0.,1.)
      CALL GSVP(1,0.,1.,0.,1.)
      CALL GSELNT(1)
C
C  Read in the first record.
C
      MCONID = CONID
      METREC = 1
      CALL GSEGRD(IOS,STATUS)
      IF (STATUS .NE. 0) THEN
        IER = 302
        RETURN
      ENDIF
C
C  Set the default operand lengths before reading any instructions.
C
      CALL GXOPDF
C
C  Set defaults.
C
      CALL GXMDEF
C
C  Loop until we run out of data, or error.
C
      CALL GXLATE(WKID,IOS,STATUS)
      IF (STATUS .EQ. -1) THEN
C
C  Normal exit, restore normalization transformation.
C
        IF (NTOLD .NE. 0) THEN
          CALL GSWN(NTOLD,TWIND(1),TWIND(2),TWIND(3),TWIND(4))
          CALL GSVP(NTOLD,TVIEW(1),TVIEW(2),TVIEW(3),TVIEW(4))
        ENDIF
        CALL GSELNT(NTOLD)
        IER = 0
      ELSE
        IER = STATUS
      ENDIF
C
      RETURN
      END
