C
C	$Id: gatelm.f,v 1.4 2008-07-27 00:20:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE GATELM(IOS,STATUS)
C
C  Set the attributes. 
C
C  Supported attribute sets:
C       POLYLINES
C       POLYMARKERS
C       TEXT AND CHARACTERS
C       FILL OPTIONS
C       COLOR TABLE
C       ASPECT SOURCE FLAGS
C
      include 'trinst.h'
      include 'trcode.h'
C
      INTEGER IOS, STATUS
C
      STATUS = 0
C
      IF (OPID.GE.ATELLI .AND. OPID.LT.ATELMB) THEN
C
C  POLYLINE set
C
        CALL GPOLAT(IOS,STATUS)
C
      ELSE IF (OPID.GE.ATELMB .AND. OPID.LT.ATELTI) THEN
C
C  MARKER set
C
        CALL GMARAT(IOS,STATUS)
C
      ELSE IF (OPID.GE.ATELTI .AND. OPID.LT.ATELFI) THEN
C
C  TEXT set
C
        CALL GTXTAT(IOS,STATUS)
C
      ELSE IF (OPID.GE.ATELFI .AND. OPID.LT.ATELCB) THEN
C
C  FILL attribute set.
C
        CALL GFILAT(IOS,STATUS)
C
      ELSE IF (OPID .EQ. ATELCB) THEN
C
C  Color table attributes (skip them, they are not supposed to
C  be in a segment).
C
        CALL GSKPOP(8,LEN,IOS,STATUS)
C
      ELSE IF (OPID .EQ. ATELAS) THEN
C
C  Set the aspect source flags.
C
         CALL GASPAR(IOS,STATUS)
      END IF
C
      RETURN
      END
