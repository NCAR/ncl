C
C	$Id: gwipat.f,v 1.5 2008-07-27 00:21:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWIPAT
C
C  Process primitive attributes.
C
      include 'gksin.h'
      include 'gwiins.h'
      include 'gwiadc.h'
C
      REAL   UP, BASE, DMAX, FACTOR
C
      SAVE
C
        GOTO (210, 220, 230, 240, 250, 260, 270, 280,
     +        290, 300, 310, 320, 330, 340, 350, 360,
     +        370, 380, 390, 400, 410, 420, 430) MCODES-20
C
C  POLYLINE INDEX
C
  210 CONTINUE
      CALL GWPDVI (ID(1), IVPLIX, 1)
      RETURN
C
C  LINETYPE
C
  220 CONTINUE
      CALL GWPDVI (ID, IVLTYP, 1)
      RETURN
C
C  LINEWIDTH SCALE FACTOR
C
  230 CONTINUE
      CALL GWPDVR (RX, IVLWSC, 1)
      RETURN
C
C  POLYLINE COLOR INDEX
C
  240 CONTINUE
      CALL GWPDVI (IC, IVPLCI, 1)
      RETURN
C
C  POLYMARKER INDEX
C
  250 CONTINUE
      CALL GWPDVI (ID, IVPMIX, 2)
      RETURN
C
C  MARKER TYPE
C
  260 CONTINUE
      CALL GWPDVI (ID, IVMTYP, 2)
      RETURN
C
C  MARKER SIZE SCALE FACTOR
C
  270 CONTINUE
      CALL GWPDVR (RX, IVMSZS, 2)
      RETURN
C
C  POLYMARKER COLOR INDEX
C
  280 CONTINUE
      CALL GWPDVI (IC, IVPMCI, 2)
      RETURN
C
C  TEXT INDEX
C
  290 CONTINUE
      CALL GWPDVI (ID, IVTXIX, 3)
      RETURN
C
C  TEXT FONT AND PRECISION
C
  300 CONTINUE
C
C  Font.
C    Convert GKS font index to pointer to font list.
C
      ID(1) = ABS(ID(1))
      CALL GWPDVI (ID(1), IVTXFO, 3)
C
C  Precision.
C
      CALL GWPDVI (ID(2), IVTXPR, 3)
      RETURN
C
C  CHARACTER EXPANSION FACTOR
C
  310 CONTINUE
      CALL GWPDVR (RX, IVCHXP, 3)
      RETURN
C
C  CHARACTER SPACING
C
  320 CONTINUE
      CALL GWPDVR (RX, IVCHSP, 3)
      RETURN
C
C  TEXT COLOR INDEX
C
  330 CONTINUE
      CALL GWPDVI (IC, IVTXCI, 3)
      RETURN
C
C  CHARACTER ORIENTATION VECTORS
C
  340 CONTINUE
C
C  Extract and process height.
C
      UP   = SQRT (RX(1)**2+RY(1)**2)
      BASE = SQRT (RX(2)**2+RY(2)**2)
      DMAX = MAX(UP,BASE)
      IF (DMAX .GT. 1.0)  THEN
C
C  A vector is longer than 1.0 NDC, scale down both vectors equally.
C
        FACTOR = 1.0/DMAX
        RX(1) = FACTOR*RX(1)
        RY(1) = FACTOR*RY(1)
        RX(2) = FACTOR*RX(2)
        RY(2) = FACTOR*RY(2)
      END IF
C
C  Fix height and process it.
C
      RTMP = 0.5+UP*REAL(MYSCAL)
      ID(1) = MAXYVD
      IF (RTMP .LT. REAL(MAXYVD)) ID(1) = INT(RTMP)
      CALL GWPDVI (ID, IVCHH, 3)
C
C  Convert NDC vectors to VDC and process.
C
      ID(1) = MIN (MAXYVD, INT (0.5 + MXSCAL*RX(1)))
      ID(2) = MIN (MAXYVD, INT (0.5 + MYSCAL*RY(1)))
      ID(3) = MIN (MAXYVD, INT (0.5 + MXSCAL*RX(2)))
      ID(4) = MIN (MAXYVD, INT (0.5 + MYSCAL*RY(2)))
      CALL GWPDVI (ID, IVCHOV, 3)
      RETURN
C
C  TEXT PATH
C
  350 CONTINUE
      CALL GWPDVI (ID, IVTXP, 3)
      RETURN
C
C  TEXT ALIGNMENT
C
  360 CONTINUE
      CALL GWPDVI (ID, IVTXAL, 3)
      RETURN
C
C  FILL AREA INDEX
C
  370 CONTINUE
      CALL GWPDVI (ID, IVFAIX, 4)
      RETURN
C
C  FILL AREA INTERIOR STYLE
C
  380 CONTINUE
      CALL GWPDVI (ID, IVFAIS, 4)
      RETURN
C
C  FILL AREA STYLE INDEX
C
  390 CONTINUE
      CALL GWPDVI (ID, IVFASI, 4)
      RETURN
C
C  FILL AREA COLOR INDEX
C
  400 CONTINUE
      CALL GWPDVI (IC, IVFACI, 4)
      RETURN
C
C  PATTERN SIZE (Currently not supported).
C
  410 CONTINUE
C
C  Truncate DX,DY to limits of NDC unit square,
C  convert to VDC, store as height and width vectors.
C
C     ID(4) = 0
C     ID(3) = INT(REAL(MXSCAL)*(MAX(0.,MIN(1.0,RX(1)))))
C     ID(2) = INT(REAL(MYSCAL)*(MAX(0.,MIN(1.0,RY(1)))))
C     ID(1) = 0
C     CALL GWPDVI (ID, IVPASZ, 4)
      RETURN
C
C  PATTERN REFERENCE POINT (currently not supported).
C
  420 CONTINUE
C
C  Truncate X,Y to limits of NDC unit square and convert to VDC.
C
C     ID(1) = INT(REAL(MXSCAL)*(MAX(0.,MIN(1.0,RX(1)))))
C     ID(2) = INT(REAL(MYSCAL)*(MAX(0.,MIN(1.0,RX(2)))))
C     CALL GWPDVI (ID, IVPARF, 4)
      RETURN
C
C  ASPECT SOURCE FLAGS
C
  430 CONTINUE
      CALL GWPASF
C
      RETURN
      END
