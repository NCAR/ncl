C
C	$Id: gmarat.f,v 1.1 1993-01-09 01:58:58 fred Exp $
C
        SUBROUTINE GMARAT(IOS,STATUS)
C
C  Set the marker attributes.
C
C  Valid attributes:
C    POLYMARKER INDEX
C    MARKER TYPE
C    MARKER SIZE 
C    MARKER COLOR
C
      include 'trinst.h'
      include 'trstat.h'
      include 'trpars.h'
      include 'trcode.h'
C
      INTEGER IOS, STATUS
C
      IF (OPID .EQ. ATELMB) THEN
C
C  POLYMARKER BUNDLE INDEX
C
        CALL GOPDEC(MARIDX,MIXCPR,1,IOS,STATUS)
        CALL GSPMI(MARIDX)
      ELSE IF (OPID .EQ. ATELMT) THEN
C
C  MARKER TYPE
C
        CALL GOPDEC(MARTYP,MIXCPR,1,IOS,STATUS)
        CALL GSMK(MARTYP)
      ELSE IF (OPID .EQ. ATELMZ) THEN
C
C  MARKER SIZE
C
        CALL GTFLT(MARSIZ,MFLCPR,IOS,STATUS)
        CALL GSMKSC(MARSIZ)
      ELSE IF (OPID .EQ. ATELMC) THEN
C
C  Set the marker color
C
        CALL GOPDEC(MARCOL,MCICPR,1,IOS,STATUS)
        CALL GSPMCI(MARCOL)
      ENDIF
C
      RETURN
      END
