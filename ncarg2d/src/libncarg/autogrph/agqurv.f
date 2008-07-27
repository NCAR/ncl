C
C $Id: agqurv.f,v 1.10 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGQURV (XVEC,IIEX,YVEC,IIEY,NEXY,SVAL)
C
      DIMENSION XVEC(1),YVEC(1)
C
C AGQURV plots the curve defined by the points ((X(I),Y(I)),I=1,NEXY),
C where
C
C    X(I)=XVEC(1+(I-1)*IIEX) (unless IIEX=0, in which case X(I)=I), and
C    Y(I)=YVEC(1+(I-1)*IIEY) (unless IIEY=0, in which case Y(I)=I).
C
C If, for some I, X(I)=SVAL or Y(I)=SVAL, curve line segments having
C (X(I),Y(I)) as an endpoint are omitted.
C
C The curve drawn is windowed.  Portions of the curve which would fall
C outside the current curve window, as defined by the last SET call,
C are not drawn.
C
C Check first whether the number of curve points is properly specified.
C
      IF (NEXY.LE.0) GO TO 901
C
C Initialization.  Pretend that the last point was point number zero.
C Set the indices for the x and y vectors accordingly.  Clear the line-
C drawn-to-last-point and last-point-outside-window flags.
C
      INDP=0
      INDX=1-IIEX
      INDY=1-IIEY
      LDLP=0
      LPOW=0
C
C Initialization.  Retrieve the current curve window, user window, and
C x/y linear/logarithmic flags.
C
      CALL GETSET (XLCW,XRCW,YBCW,YTCW,XLUW,XRUW,YBUW,YTUW,LTYP)
C
C Initialization.  Set linear/log flag and linear-window limits for
C x-axis values.
C
      IF (LTYP.EQ.1.OR.LTYP.EQ.2) THEN
        LLUX=0
        XLLW=XLUW
        XRLW=XRUW
      ELSE
        LLUX=1
        XLLW=ALOG10(XLUW)
        XRLW=ALOG10(XRUW)
      END IF
C
C Initialization.  Set linear/log flag and linear-window limits for
C y-axis values.
C
      IF (LTYP.EQ.1.OR.LTYP.EQ.3) THEN
        LLUY=0
        YBLW=YBUW
        YTLW=YTUW
      ELSE
        LLUY=1
        YBLW=ALOG10(YBUW)
        YTLW=ALOG10(YTUW)
      END IF
C
C Initialization.  Call SET, if necessary, to define a linear mapping.
C (This greatly simplifies the windowing code.)
C
      IF (LTYP.NE.1) THEN
        CALL PLOTIT (0,0,2)
        CALL SET (XLCW,XRCW,YBCW,YTCW,XLLW,XRLW,YBLW,YTLW,1)
      END IF
C
C Initialization.  Compute mimimum and maximum values of x which are
C slightly outside the linear window.  (Note:  XLLW and XRLW will not
C be used after this.)
C
      IF (XLLW.GT.XRLW) THEN
        TEMP=XLLW
        XLLW=XRLW
        XRLW=TEMP
      END IF
      XEPS=.000001*(XRLW-XLLW)
      XMIN=XLLW-XEPS
      XMAX=XRLW+XEPS
C
C Initialization.  Compute minimum and maximum values of y which are
C slightly outside the linear window.  (Note:  YBLW and YTLW will not
C be used after this.)
C
      IF (YBLW.GT.YTLW) THEN
        TEMP=YBLW
        YBLW=YTLW
        YTLW=TEMP
      END IF
      YEPS=.000001*(YTLW-YBLW)
      YMIN=YBLW-YEPS
      YMAX=YTLW+YEPS
C
C Beginning of loop through points.  Update indices and determine the
C user-space coordinates of the next point.
C
  101 IF (INDP.EQ.NEXY) GO TO 120
      INDP=INDP+1
C
      INDX=INDX+IIEX
      XNXT=XVEC(INDX)
      IF (IIEX.EQ.0) XNXT=REAL(INDP)
      IF (LLUX.NE.0.AND.XNXT.LE.0.) XNXT=SVAL
C
      INDY=INDY+IIEY
      YNXT=YVEC(INDY)
      IF (IIEY.EQ.0) YNXT=REAL(INDP)
      IF (LLUY.NE.0.AND.YNXT.LE.0.) YNXT=SVAL
C
C Check whether (XNXT,YNXT) is a special-value point.  Handle that case.
C
      IF (XNXT.EQ.SVAL.OR.YNXT.EQ.SVAL) THEN
        LPOW=0
        IF (LDLP.EQ.0) GO TO 101
        IF (LDLP.EQ.1) CALL VECTD (XLST,YLST)
        CALL LASTD
        LDLP=0
        GO TO 101
      END IF
C
C If user space is not linear/linear, modify XNXT and YNXT accordingly.
C
      IF (LLUX.NE.0) XNXT=ALOG10(XNXT)
      IF (LLUY.NE.0) YNXT=ALOG10(YNXT)
C
C Set the next-point-outside-window flag to a value between -4 and +4,
C inclusive.  A non-zero value indicates that the next point is outside
C the window and indicates which of eight possible areas it falls in.
C
      NPOW=INT(3.*(SIGN(.51,XNXT-XMIN)+SIGN(.51,XNXT-XMAX))+
     +            (SIGN(.51,YNXT-YMIN)+SIGN(.51,YNXT-YMAX)))
C
C There are now various possible cases, depending on whether the line-
C drawn-to-last-point flag is set or not, whether the next point is in
C the window or not, and whether the last point was in the window, not
C in the window, or non-existent (point 0 or a special-value point).
C
      IF (LDLP.EQ.0) GO TO 102
      IF (NPOW.NE.0) GO TO 103
C
C Line drawn to last point, next point inside, last point inside.
C
      CALL VECTD (XNXT,YNXT)
      LDLP=LDLP+1
      GO TO 119
C
  102 IF (NPOW.NE.0) GO TO 109
      IF (LPOW.NE.0) GO TO 105
C
C No line drawn to last point, next point inside, no last point.
C
      CALL FRSTD (XNXT,YNXT)
      LDLP=1
      GO TO 119
C
C Line drawn to last point, next point outside, last point inside.
C
  103 XPIW=XLST
      YPIW=YLST
      XPOW=XNXT
      YPOW=YNXT
      JUMP=1
      GO TO 107
  104 CALL VECTD (XPEW,YPEW)
      CALL LASTD
      LDLP=0
      GO TO 119
C
C No line drawn to last point, next point inside, last point outside.
C
  105 XPIW=XNXT
      YPIW=YNXT
      XPOW=XLST
      YPOW=YLST
      JUMP=2
      GO TO 107
  106 CALL FRSTD (XPEW,YPEW)
      CALL VECTD (XNXT,YNXT)
      LDLP=2
      GO TO 119
C
C The following local procedure, given a point (XPIW,YPIW) inside the
C window and a point (XPOW,YPOW) outside the window, finds the point of
C intersection (XPEW,YPEW) of a line joining them with the window edge.
C
  107 XPEW=XPIW
      YPEW=YPIW
      XDIF=XPOW-XPIW
      YDIF=YPOW-YPIW
C
      IF (ABS(XDIF).GT.XEPS) THEN
        XPEW=XMIN
        IF (XDIF.GE.0.) XPEW=XMAX
        YPEW=YPIW+(XPEW-XPIW)*YDIF/XDIF
        IF (YPEW.GE.YMIN.AND.YPEW.LE.YMAX) GO TO 108
      END IF
C
      IF (ABS(YDIF).GT.YEPS) THEN
        YPEW=YMIN
        IF (YDIF.GE.0.) YPEW=YMAX
        XPEW=XPIW+(YPEW-YPIW)*XDIF/YDIF
      END IF
C
  108 GO TO (104,106) , JUMP
C
C No line drawn to last point, next point outside.  Jump if no last
C point.
C
  109 IF (LPOW.EQ.0) GO TO 119
C
C No line drawn to last point, next point outside, last point outside.
C Check whether a portion of the line joining them lies in the window.
C
      MPOW=9*LPOW+NPOW+41
C
      GO TO (119,119,119,119,119,110,119,110,110,
     +       119,119,119,111,119,110,111,110,110,
     +       119,119,119,111,119,119,111,111,119,
     +       119,113,113,119,119,110,119,110,110,
     +       119,119,119,119,119,119,119,119,119,
     +       112,112,119,112,119,119,111,111,119,
     +       119,113,113,119,119,113,119,119,119,
     +       112,112,113,112,119,113,119,119,119,
     +       112,112,119,112,119,119,119,119,119) , MPOW
C
  110 XPE1=XMIN
      YPT1=YMIN
      XPE2=XMAX
      YPT2=YMAX
      GO TO 114
C
  111 XPE1=XMIN
      YPT1=YMAX
      XPE2=XMAX
      YPT2=YMIN
      GO TO 114
C
  112 XPE1=XMAX
      YPT1=YMAX
      XPE2=XMIN
      YPT2=YMIN
      GO TO 114
C
  113 XPE1=XMAX
      YPT1=YMIN
      XPE2=XMIN
      YPT2=YMAX
C
  114 XDIF=XNXT-XLST
      YDIF=YNXT-YLST
C
      IF (ABS(XDIF).LE.XEPS) GO TO 116
      YPE1=YLST+(XPE1-XLST)*YDIF/XDIF
      YPE2=YLST+(XPE2-XLST)*YDIF/XDIF
C
      IF (ABS(YDIF).LE.YEPS) GO TO 118
      IF (YPE1.GE.YMIN.AND.YPE1.LE.YMAX) GO TO 115
      YPE1=YPT1
      XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
      IF (XPE1.LT.XMIN.OR.XPE1.GT.XMAX) GO TO 119
C
  115 IF (YPE2.GE.YMIN.AND.YPE2.LE.YMAX) GO TO 118
      GO TO 117
C
  116 YPE1=YPT1
      XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
      IF (XPE1.LT.XMIN.OR.XPE1.GT.XMAX) GO TO 119
C
  117 YPE2=YPT2
      XPE2=XLST+(YPE2-YLST)*XDIF/YDIF
      IF (XPE2.LT.XMIN.OR.XPE2.GT.XMAX) GO TO 119
C
  118 CALL FRSTD (XPE1,YPE1)
      CALL VECTD (XPE2,YPE2)
      CALL LASTD
C
C Processing of next point is done.  It becomes the last point and we
C go back for a new next point.
C
  119 LPOW=NPOW
      XLST=XNXT
      YLST=YNXT
      GO TO 101
C
C Last point was final point.  Finish up.
C
  120 IF (LDLP.NE.0) THEN
        IF (LDLP.EQ.1) CALL VECTD (XLST,YLST)
        CALL LASTD
      END IF
C
C Restore logarithmic mapping, if appropriate.
C
      IF (LTYP.NE.1) THEN
        CALL PLOTIT (0,0,2)
        CALL SET (XLCW,XRCW,YBCW,YTCW,XLUW,XRUW,YBUW,YTUW,LTYP)
      END IF
C
C Return to caller.
C
      RETURN
C
C Error exit.
C
  901 CALL SETER ('AGQURV - NUMBER OF POINTS IS LESS THAN OR EQUAL TO ZE
     +RO',7,2)
C
      END
