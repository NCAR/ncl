C
C	$Id: gpolat.f,v 1.4 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE GPOLAT(IOS,STATUS)
C
C  Set POLYLINE attributes.
C
C  Valid attributes:
C    POLYLINE INDEX
C    LINE TYPE
C    LINE WIDTH
C    LINE COLOR
C
      include 'trcode.h'
      include 'trinst.h'
      include 'trstat.h'
      include 'trpars.h'
C
      INTEGER IOS, STATUS
C
      IF (OPID .EQ. ATELLI) THEN
C
C  POLYLINE BUNDLE INDEX
C
        CALL GOPDEC(POLIDX,MIXCPR,1,IOS,STATUS)
        CALL GSPLI(POLIDX)
      ELSE IF (OPID .EQ. ATELLT) THEN
C
C  LINE TYPE
C
        CALL GOPDEC(LINTYP,MIXCPR,1,IOS,STATUS)
        CALL GSLN(LINTYP)
      ELSE IF (OPID .EQ. ATELLW) THEN
C
C  LINE WIDTH
C
        CALL GTFLT(LINWTH,MFLCPR,IOS,STATUS)
        CALL GSLWSC(LINWTH)
      ELSE IF (OPID .EQ. ATELLC) THEN
C
C  LINE COLOR
C
        CALL GOPDEC(LINCOL,MCICPR,1,IOS,STATUS)
        CALL GSPLCI(LINCOL)
      ENDIF
C
      RETURN
      END
