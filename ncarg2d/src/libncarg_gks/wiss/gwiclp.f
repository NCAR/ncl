C
C	$Id: gwiclp.f,v 1.6 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWICLP(IFORCE)
C
C  Process clipping parameters.  If the current picture is empty,
C  then putting a clip indicator or clip rectangle into the metafile
C  is suppressed.  Setting IFORCE to 1 forces a clip indicator and
C  clip rectangle into the metafile, and this is done when the picture
C  becomes non-empty.  In all other cases the clip indicator or
C  rectangle is put into the metafile depending upon whether a
C  change has been detected from the previous setting.
C
      include 'gksin.h'
      include 'gwienu.h'
      include 'gwiwsl.h'
      include 'gwiins.h'
      include 'gwiopc.h'
C
      INTEGER  NBYTES, I, ICRLOC(4)
      LOGICAL  CHANGE
C
      SAVE
C
C  Handle the case where a clip indicator and rectangle is forced
C  into the metafile.
C
      IF (IFORCE .NE. 0) GO TO 100
C
C  If the clipping indicator has changed, store it in the WSL.  If
C  the picture is not empty, send the clip indicator.
C
      IF (ID(1).NE.MRCLIP)  THEN
        MRCLIP = ID(1)
          IF (MDEMPT .EQ. GNEMPT) THEN
C
C  Put out opcode (class and id) and length.
C
          NBYTES = 1+(MEFW-1)/8
          CALL GWPTNI (CLCLIN, IDCLIN, NBYTES, RERR)
          IF (RERR.NE.0)  RETURN
C
C  Put out clipping indicator parameter.
C
          CALL GWPTPR (MRCLIP, MEFW,     1, RERR)
          IF (RERR.NE.0)  RETURN
        ENDIF
      END IF
C
C  Normalize the clipping rectangle and store it.  Send the rectangle
C  if the picture is not empty.
C
C  Normalize NDC limits of rectangle (assume bounds check above WSI), 
C  store as rectangle corner points.
C
      ICRLOC(1) = MXOFF + MXSCAL*RX(1)
      ICRLOC(3) = MXOFF + MXSCAL*RX(2)
      ICRLOC(2) = MYOFF + MYSCAL*RY(1)
      ICRLOC(4) = MYOFF + MYSCAL*RY(2)
      CHANGE = .FALSE.
      DO 10 I=1,4
        IF (ICRLOC(I).NE.MRCREC(I))  THEN
          CHANGE = .TRUE.
          MRCREC(I) = ICRLOC(I)
        END IF
   10 CONTINUE
      IF (CHANGE)  THEN
        IF (MDEMPT .EQ. GNEMPT) THEN
C
C  Total byte length, based on VDC bit precision.
C
          NBYTES = 1 + (4*MVDCFW-1)/8
C
C  Put out opcode (class and id) and length.
C
          CALL GWPTNI (CLCREC, IDCREC, NBYTES, RERR)
          IF (RERR.NE.0)  RETURN
C
C  Put out clipping rectangle corner points.
C
          CALL GWPTPR (MRCREC, MVDCFW,     4, RERR)
        ENDIF
      END IF
      RETURN
C
  100 CONTINUE
C
C  Put out opcode (class and id) and length.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GWPTNI (CLCLIN, IDCLIN, NBYTES, RERR)
      IF (RERR.NE.0)  RETURN
C
C  Put out clipping indicator parameter.
C
      CALL GWPTPR (MRCLIP, MEFW,     1, RERR)
      IF (RERR.NE.0)  RETURN
C
C  Total byte length, based on VDC bit precision.
C
      NBYTES = 1 + (4*MVDCFW-1)/8
C
C  Put out opcode (class and id) and length.
C
      CALL GWPTNI (CLCREC, IDCREC, NBYTES, RERR)
      IF (RERR.NE.0)  RETURN
C
C  Put out clipping rectangle corner points.
C
      CALL GWPTPR (MRCREC, MVDCFW,     4, RERR)
      RETURN
C
      END
