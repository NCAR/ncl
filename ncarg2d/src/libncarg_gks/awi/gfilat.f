C
C	$Id: gfilat.f,v 1.4 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE GFILAT(IOS,STATUS)
C
C  Process FILL AREA attributes.
C
C  VALID ATTRIBUTES:
C    FILL AREA INDEX
C    INTERIOR STYLE
C    FILL COLOR
C    HATCH INDEX
C    PATTERN INDEX (Currently does not produce any CGM elements)
C    FILL REFERENCE POINT (Currently does not produce any CGM elements)
C    PATTERN TABLE (Currently does not produce any CGM elements)
C    PATTERN SIZE  (Currently does not produce any CGM elements)
C
      include 'trinst.h'
      include 'trstat.h'
      include 'trpars.h'
      include 'trcode.h'
C
      INTEGER IOS, STATUS
C
      IF (OPID.EQ.ATELFI) THEN
C
C  Set the fill area bundle index.
C
        CALL GOPDEC(FILIDX,MIXCPR,1,IOS,STATUS)
        CALL GSFAI(FILIDX)
      ELSE IF (OPID.EQ.ATELIS) THEN
C
C  Set the interior style.
C
        CALL GOPDEC(INTSTL,MIXCPR,1,IOS,STATUS)
        CALL GSFAIS(INTSTL)
      ELSE IF (OPID.EQ.ATELFC) THEN
C
C  Set the fill area color.
C
        CALL GOPDEC(FILCOL,MCICPR,1,IOS,STATUS)
        CALL GSFACI(FILCOL)
      ELSE IF (OPID.EQ.ATELHI) THEN
C
C  Set the hatch index.
C
        CALL GOPDEC(HATIDX,MIXCPR,1,IOS,STATUS)
        CALL GSFASI(HATIDX)
      ELSE IF (OPID.EQ.ATELPI) THEN
C
C  Set the pattern index.
C
        CALL GOPDEC(PATIDX,MIXCPR,1,IOS,STATUS)
        CALL GSFASI(PATIDX)
      ELSE IF (OPID.EQ.ATELFR) THEN
C
C  Set the fill area reference point.
C
        CALL GOPDEC(FILRPT,MOPLEN,2,IOS,STATUS)
        CALL GSPARF(REAL(FILRPT(1))/32767.,REAL(FILRPT(2))/32767.)
      ELSE IF (OPID .EQ. ATELPS) THEN
C
C  Pattern size.
C
        IVDCL = MOPLEN
        CALL GSKPOP(IVDCL,4,IOS,STATUS)
      ELSE IF (OPID .EQ. ATELPT) THEN
C
C  Pattern table.
C
        ISKP = LEN
        CALL GSKPOP(8,ISKP,IOS,STATUS)
      END IF
C
      RETURN
      END
