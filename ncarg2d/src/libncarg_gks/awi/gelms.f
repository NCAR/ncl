C
C	$Id: gelms.f,v 1.1 1993-01-09 01:58:19 fred Exp $
C
        SUBROUTINE GELMS(IOS,STATUS)
C
C  This routine invokes the processors for the valid 
C  graphical elements.
C
C  Supported graphical elements: 
C       POLYLINE
C       POLYMARKER
C       TEXT
C       POLYGON
C       CELL ARRAY
C       GENERALIZED DRAWING PRIMITIVE
C
      include 'trinst.h'
      include 'trcode.h'
C
      INTEGER IOS, STATUS
C
      IF (OPID .EQ. GPELPL) THEN
C
C  POLYLINE
C
        CALL GPUTPT(11)
C
      ELSE IF (OPID .EQ. GPELPM) THEN
C
C  POLYMARKER
C
        CALL GPUTPT(12)
C
      ELSE IF (OPID .EQ. GPELPG) THEN
C
C  POLYGON
C
        CALL GPUTPT(14)
C
      ELSE IF (OPID .EQ. GPELTX) THEN 
C
C  TEXT
C
        CALL GTXDRW(IOS,STATUS)
C
      ELSE IF (OPID .EQ. GPELCA) THEN
C
C  CELL ARRAY
C
        CALL GCELDR(IOS,STATUS)
C
      ELSE IF (OPID.EQ.GPELGP) THEN
C
C  GENERALIZED DRAWING PRIMITIVE (Currently not supported).
C
        CALL GSKPOP(8,LEN,IOS,STATUS)
      END IF
C
      RETURN
      END
