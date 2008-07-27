C
C $Id: plotif.f,v 1.11 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PLOTIF (FX,FY,IP)
C
C Move the pen to the point (FX,FY), in the fractional cooordinate
C system.  If IP is zero, do a pen-up move.  If IP is one, do a pen-down
C move.  If IP is two, flush the buffer.
C
C The variable IU(5), in the labelled common block IUTLCM, specifies
C the size of the pen-move buffer (between 2 and 50).
C
      COMMON /IUTLCM/ IU(100)
      SAVE /IUTLCM/
C
C The common block VCTSEQ contains variables implementing the buffering
C of pen moves.
C
      COMMON /VCTSEQ/ NQ,QX(50),QY(50),NF,IF(25)
      SAVE /VCTSEQ/
C
C In the common block PLTCM are recorded the coordinates of the last
C pen position, in the metacode coordinate system, for MXMY.
C
      COMMON /PLTCM/ JX,JY
      SAVE /PLTCM/
C
C VP and WD hold viewport and window parameters obtained, when needed,
C from GKS.
C
      DIMENSION VP(4),WD(4)
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL UTILBD
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PLOTIF - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for out-of-range values of the pen parameter.
C
      IF (IP.LT.0.OR.IP.GT.2) THEN
        CALL SETER ('PLOTIF - ILLEGAL VALUE FOR IPEN',2,1)
        RETURN
      END IF
C
C If a buffer flush is requested, jump.
C
      IF (IP.EQ.2) GO TO 101
C
C A pen-up move to the current point (whether the pen is up or down) is
C simply ignored, since it cannot possibly have any effect but to put
C extra pen-up moves in the buffer.
C
      IF (IP.EQ.0.AND.FX.EQ.QX(NQ).AND.FY.EQ.QY(NQ)) RETURN
C
C Set JX and JY for a possible call to MXMY.
C
      JX=KFMX(FX)
      JY=KFMY(FY)
C
C If the current move is a pen-down move, or if the last one was, bump
C the pointer into the coordinate arrays and, if the current move is
C a pen-up move, make a new entry in the array IF, which records the
C positions of the pen-up moves.  Note that we never get two pen-up
C moves in a row, which means that IF need be dimensioned only half as
C large as QX and QY.
C
      IF (IP.NE.0.OR.IF(NF).NE.NQ) THEN
        NQ=NQ+1
        IF (IP.EQ.0) THEN
          NF=NF+1
          IF(NF)=NQ
        END IF
      END IF
C
C Save the coordinates of the point, in the fractional coordinate
C system.
C
      QX(NQ)=FX
      QY(NQ)=FY
C
C If the point-coordinate buffer is full, dump the buffers; otherwise,
C return.
C
      IF (NQ.LT.IU(5)) RETURN
C
C Dump the buffers.  If NQ is one, there's nothing to dump.  All that's
C there is a single pen-up move.
C
  101 IF (NQ.LE.1) RETURN
C
C Get NT, the number of the current transformation, and, if it is not
C zero, modify the current transformation so that we can use fractional
C coordinates (normalized device coordinates, in GKS terms).
C
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('PLOTIF - ERROR EXIT FROM GQCNTN',3,1)
        RETURN
      END IF
      IF (NT.NE.0) THEN
        CALL GQNT (NT,IE,WD,VP)
        IF (IE.NE.0) THEN
          CALL SETER ('PLOTIF - ERROR EXIT FROM GQNT',4,1)
          RETURN
        END IF
        CALL GSWN (NT,VP(1),VP(2),VP(3),VP(4))
      END IF
C
C Dump out a series of polylines, each one defined by a pen-up move and
C a series of pen-down moves.
C
      DO 102 I=1,NF-1
        CALL GPL (IF(I+1)-IF(I),QX(IF(I)),QY(IF(I)))
  102 CONTINUE
      IF (IF(NF).NE.NQ) CALL GPL (NQ-IF(NF)+1,QX(IF(NF)),QY(IF(NF)))
C
C Put the current transformation back the way it was.
C
      IF (NT.NE.0) THEN
        CALL GSWN (NT,WD(1),WD(2),WD(3),WD(4))
      END IF
C
C Move the last pen position to the beginning of the buffer and pretend
C there was a pen-up move to that position.
C
      QX(1)=QX(NQ)
      QY(1)=QY(NQ)
      NQ=1
      IF(1)=1
      NF=1
C
C Done.
C
      RETURN
C
      END
