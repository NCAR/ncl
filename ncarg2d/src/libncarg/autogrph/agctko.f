C
C $Id: agctko.f,v 1.6 2008-07-27 00:14:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGCTKO (XBGA,YBGA,XDCA,YDCA,CFAX,CFAY,CSFA,JAOR,NMMT,
     +                   QMDP,WMML,WMMR,FNLL,FNLR,MM12,MM34,XMMT,YMMT)
C
      DIMENSION XMMT(4),YMMT(4)
C
C The routine AGCTKO is used to compute the x and y offsets to the end-
C points of the left-of-label and right-of-label portions of the major
C and minor tick marks.  See AGAXIS for definitions of the arguments.
C
C A note about WMML and WMMR:  Each is a positive number, of the form
C (E) or (1+E), where E (=EPSILON) is .LT. 1. and is expressed as a
C fraction of the smaller side of the curve window.  If the form (E) is
C used, it implies just a tick of length E; if the form (1+E) is used,
C it implies a tick long enough to reach the edge of the curve window,
C plus the length E.
C
C If the tick-mark count NMMT .EQ. 0 or the tick-mark dash pattern QMDP
C .EQ. 0 or both the left-of-axis and right-of-axis tick-mark lengths
C WMML and WMMR .EQ. 0, then no tick marks are to be drawn.
C
      IF (NMMT.EQ.0.OR.QMDP.EQ.0..OR.(WMML.EQ.0..AND.WMMR.EQ.0.))
     *                                                         GO TO 115
C
C Compute the distances of the tick mark ends from the axis as fractions
C of the axis length, using only the (EPSILON) portion of WMML and WMMR.
C
      FMML=-CSFA*MOD(WMML,1.)
      FMMR=+CSFA*MOD(WMMR,1.)
C
C If the labels overlap the axis and the (EPSILON) form was used for
C WMML or WMMR, move the tick mark to the end of the label.
C
      IF (FNLL*FNLR.GE.0.) GO TO 101
C
      IF (WMML.LT.1.) FMML=FMML+FNLL
C
      IF (WMMR.LT.1.) FMMR=FMMR+FNLR
C
C Compute the x and y offsets to the ends of the tick mark.
C
  101 XMML=+CFAX*FMML*YDCA
      YMML=-CFAY*FMML*XDCA
      XMMR=+CFAX*FMMR*YDCA
      YMMR=-CFAY*FMMR*XDCA
C
C If the (1+EPSILON) form was used for WMML or WMMR, adjust XMML, YMML,
C XMMR, and YMMR as implied by the current axis orientation.
C
      IF (WMML.LT.1.) GO TO 107
C
      GO TO (102,103,104,105) , JAOR
C
C Axis at 0 degrees (left to right).
C
  102 YMML=YMML+1.-YBGA
      GO TO 106
C
C Axis at 90 degrees (bottom to top).
C
  103 XMML=XMML-XBGA
      GO TO 106
C
C Axis at 180 degrees (right to left).
C
  104 YMML=YMML-YBGA
      GO TO 106
C
C Axis at 270 degrees (top to bottom).
C
  105 XMML=XMML+1.-XBGA
C
  106 FMML=(XMML+YMML)/(CFAX*YDCA-CFAY*XDCA)
C
  107 IF (WMMR.LT.1.) GO TO 113
C
      GO TO (108,109,110,111) , JAOR
C
C Axis at 0 degrees (left to right).
C
  108 YMMR=YMMR-YBGA
      GO TO 112
C
C Axis at 90 degrees (bottom to top).
C
  109 XMMR=XMMR+1.-XBGA
      GO TO 112
C
C Axis at 180 degrees (right to left).
C
  110 YMMR=YMMR+1.-YBGA
      GO TO 112
C
C Axis at 270 degrees (top to bottom).
C
  111 XMMR=XMMR-XBGA
C
  112 FMMR=(XMMR+YMMR)/(CFAX*YDCA-CFAY*XDCA)
C
C Now split the tick mark into two portions - one to the left, and one
C to the right, of the numeric label space.
C
  113 XMMT(1)=XMML
      YMMT(1)=YMML
      XMMT(2)=XMMR
      YMMT(2)=YMMR
      MM12=1
      MM34=0
      IF (FMMR.LE.FNLL.OR.FNLL.GE.FNLR) RETURN
C
      MM12=0
      IF (FMML.GE.FNLL) GO TO 114
      MM12=1
      XMMT(2)=+CFAX*(FNLL-.005*CSFA)*YDCA
      YMMT(2)=-CFAY*(FNLL-.005*CSFA)*XDCA
C
  114 IF (FMMR.LE.FNLR) RETURN
C
      MM34=1
      XMMT(4)=XMMR
      YMMT(4)=YMMR
      XMMT(3)=XMML
      YMMT(3)=YMML
C
      IF (FMML.GE.FNLR) RETURN
      XMMT(3)=+CFAX*(FNLR+.005*CSFA)*YDCA
      YMMT(3)=-CFAY*(FNLR+.005*CSFA)*XDCA
      RETURN
C
C No ticks to be drawn - zero the flags MM12 and MM34 to indicate this.
C
  115 MM12=0
      MM34=0
      RETURN
C
      END
