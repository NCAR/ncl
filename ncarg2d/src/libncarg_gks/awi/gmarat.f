C
C	$Id: gmarat.f,v 1.5 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
      include 'gkscom.h'
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
C
C  Scale the marker size by the current transformation matrix.
C  Take a vector (0,MARSIZ), transform it with the
C  current transformation matrix, then find the length of the
C  transformed vector.
C
        YSCALE = SQRT(CURTM(1,2)*CURTM(1,2)+CURTM(2,2)*CURTM(2,2))
        HS = MARSIZ*YSCALE
        IF (HS .LT. 0.00004) HS = 0.00004
        CALL GSMKSC(HS)
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
