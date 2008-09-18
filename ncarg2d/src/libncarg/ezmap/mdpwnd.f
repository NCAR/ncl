C
C $Id: mdpwnd.f,v 1.8 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPWND (XNXT,YNXT,IFST)
C
        DOUBLE PRECISION XNXT,YNXT
        INTEGER          IFST
C
C This is a windowing routine for line draws.  Code using it should
C declare the common block MAPWNC and put into it minimum and maximum
C values of X and Y that together define a window at the edges of which
C lines are to be clipped.  Once that has been done, each call to
C MDPWND with IFST = 0 declares a point (XNXT,YNXT) at which a line
C is to begin and each call to MDPWND with IFST = 1 declares a point
C (XNXT,YNXT) at which a line is to continue.
C
C This version of MDPWND just draws the windowed line segments, using
C calls to LINE.  Each (XNXT,YNXT) is expected to be a point in the
C user coordinate system.  Likewise, the values of WXMN, WXMX, WYMN,
C and WYMX are expected to be in the user coordinate system.  Also,
C the mapping from the user system to the fractional system must not
C be logarithmic in either direction.
C
C Declare the common block that holds the clipping window parameters.
C
        COMMON /MAPWNC/  WXMN,WXMX,WYMN,WYMX
        DOUBLE PRECISION WXMN,WXMX,WYMN,WYMX
        SAVE   /MAPWNC/
C
C Declare local variables.
C
        INTEGER          LPOW,MPOW,NPOW
C
        DOUBLE PRECISION XDIF,XLST,XPE1,XPE2,XPEW,YDIF,YLST,YPE1,YPE2,
     +                   YPEW,YPT1,YPT2
C
C Certain quantities need to be saved from call to call.  LPOW is a
C "last-point-outside-window" flag.  (XLST,YLST) is the last point
C (from the previous call to MDPWND).
C
        SAVE LPOW,XLST,YLST
C
C Compute a "next-point-outside-window" flag.  The value of this flag
C is between -4 and +4, depending on where the next point is relative
C to the window, as shown in the following diagram:
C
C                      |      |
C                   -2 |  +1  | +4
C            WYMX -----+------+-----
C                   -3 |   0  | +3
C            WYMN -----+------+-----
C                   -4 |  -1  | +2
C                      |      |
C                    WXMN    WXMX
C
C Ultimately, we combine the values of this flag for two consecutive
C points in such a way as to get an integer between 1 and 81, telling
C us what combination of inside/outside we have to deal with.
C
        NPOW=INT(3.D0*(SIGN(.51D0,XNXT-WXMN)+SIGN(.51D0,XNXT-WXMX))+
     +                (SIGN(.51D0,YNXT-WYMN)+SIGN(.51D0,YNXT-WYMX)))
C
C If the next point is not the first point of a line, there is work to
C be done.
C
        IF (IFST.NE.0) THEN
C
C There are various possible cases, depending on whether the last point
C was inside or outside the window and whether the next point is inside
C or outside the window.
C
          IF (LPOW.EQ.0) THEN
            IF (NPOW.NE.0) GO TO 101
            CALL LINE (REAL(XLST),REAL(YLST),REAL(XNXT),REAL(YNXT))
            IF (ICFELL('MDPWND',1).NE.0) RETURN
            GO TO 115
          ELSE
            IF (NPOW.EQ.0) GO TO 103
            GO TO 105
          END IF
C
C Last point inside, next point outside.
C
  101     XPEW=XLST
          YPEW=YLST
          XDIF=XNXT-XLST
          YDIF=YNXT-YLST
C
          IF (ABS(XDIF).GT..000001D0*(WXMX-WXMN)) THEN
            XPEW=WXMN
            IF (XDIF.GE.0.D0) XPEW=WXMX
            YPEW=YLST+(XPEW-XLST)*YDIF/XDIF
            IF (YPEW.GE.WYMN.AND.YPEW.LE.WYMX) GO TO 102
          END IF
C
          IF (ABS(YDIF).GT..000001D0*(WYMX-WYMN)) THEN
            YPEW=WYMN
            IF (YDIF.GE.0.D0) YPEW=WYMX
            XPEW=XLST+(YPEW-YLST)*XDIF/YDIF
          END IF
C
  102     CALL LINE (REAL(XLST),REAL(YLST),REAL(XPEW),REAL(YPEW))
          IF (ICFELL('MDPWND',2).NE.0) RETURN
C
          GO TO 115
C
C Last point outside, next point inside.
C
  103     XPEW=XNXT
          YPEW=YNXT
          XDIF=XLST-XNXT
          YDIF=YLST-YNXT
C
          IF (ABS(XDIF).GT..000001D0*(WXMX-WXMN)) THEN
            XPEW=WXMN
            IF (XDIF.GE.0.D0) XPEW=WXMX
            YPEW=YNXT+(XPEW-XNXT)*YDIF/XDIF
            IF (YPEW.GE.WYMN.AND.YPEW.LE.WYMX) GO TO 104
          END IF
C
          IF (ABS(YDIF).GT..000001D0*(WYMX-WYMN)) THEN
            YPEW=WYMN
            IF (YDIF.GE.0.D0) YPEW=WYMX
            XPEW=XNXT+(YPEW-YNXT)*XDIF/YDIF
          END IF
C
  104     CALL LINE (REAL(XPEW),REAL(YPEW),REAL(XNXT),REAL(YNXT))
          IF (ICFELL('MDPWND',3).NE.0) RETURN
C
          GO TO 115
C
C Last point outside, next point outside.  Check whether or not part of
C the line joining them lies in the window.
C
  105     MPOW=9*LPOW+NPOW+41
C
          GO TO ( 115,115,115,115,115,106,115,106,106,
     +            115,115,115,107,115,106,107,106,106,
     +            115,115,115,107,115,115,107,107,115,
     +            115,109,109,115,115,106,115,106,106,
     +            115,115,115,115,115,115,115,115,115,
     +            108,108,115,108,115,115,107,107,115,
     +            115,109,109,115,115,109,115,115,115,
     +            108,108,109,108,115,109,115,115,115,
     +            108,108,115,108,115,115,115,115,115 ) , MPOW
C
  106     XPE1=WXMN
          YPT1=WYMN
          XPE2=WXMX
          YPT2=WYMX
          GO TO 110
C
  107     XPE1=WXMN
          YPT1=WYMX
          XPE2=WXMX
          YPT2=WYMN
          GO TO 110
C
  108     XPE1=WXMX
          YPT1=WYMX
          XPE2=WXMN
          YPT2=WYMN
          GO TO 110
C
  109     XPE1=WXMX
          YPT1=WYMN
          XPE2=WXMN
          YPT2=WYMX
C
  110     XDIF=XNXT-XLST
          YDIF=YNXT-YLST
C
          IF (ABS(XDIF).LE..000001D0*(WXMX-WXMN)) GO TO 112
          YPE1=YLST+(XPE1-XLST)*YDIF/XDIF
          YPE2=YLST+(XPE2-XLST)*YDIF/XDIF
C
          IF (ABS(YDIF).LE..000001D0*(WYMX-WYMN)) THEN
            IF (YPE1.LT.WYMN.OR.YPE1.GT.WYMX) GO TO 115
            IF (YPE2.LT.WYMN.OR.YPE2.GT.WYMX) GO TO 115
            GO TO 114
          END IF
C
          IF (YPE1.GE.WYMN.AND.YPE1.LE.WYMX) GO TO 111
          YPE1=YPT1
          XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
          IF (XPE1.LT.WXMN.OR.XPE1.GT.WXMX) GO TO 115
C
  111     IF (YPE2.GE.WYMN.AND.YPE2.LE.WYMX) GO TO 114
          GO TO 113
C
  112     YPE1=YPT1
          XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
          IF (XPE1.LT.WXMN.OR.XPE1.GT.WXMX) GO TO 115
C
  113     YPE2=YPT2
          XPE2=XLST+(YPE2-YLST)*XDIF/YDIF
          IF (XPE2.LT.WXMN.OR.XPE2.GT.WXMX) GO TO 115
C
  114     CALL LINE (REAL(XPE1),REAL(YPE1),REAL(XPE2),REAL(YPE2))
          IF (ICFELL('MDPWND',4).NE.0) RETURN
C
        END IF
C
C Processing of the next point is done.  It becomes the last point and
C we return to the user for a new next point.
C
  115   LPOW=NPOW
        XLST=XNXT
        YLST=YNXT
C
        RETURN
C
      END
