C
C $Id: cpwlam.f,v 1.3 1994-05-18 16:17:45 kennison Exp $
C
      SUBROUTINE CPWLAM (XNXT,YNXT,IFST,IAMA,IGID,IAIL,IAIR)
C
      DIMENSION IAMA(*)
C
C This is a windowing routine for line draws.  Code using it should
C declare the common block WDCOMN and put into it minimum and maximum
C values of X and Y that together define a window at the edges of which
C lines are to be clipped.  Once that has been done, each call to
C CPWLAM with IFST = 0 declares a point (XNXT,YNXT) at which a line
C is to begin and each call to CPWLAM with IFST = 1 declares a point
C (XNXT,YNXT) at which a line is to continue.
C
C This version of CPWLAM puts the windowed line segments into the area
C map IAMA, using group identifier IGID, left area identifier IAIL, and
C right area identifier IAIR.  Each (XNXT,YNXT) is expected to be a
C point in the fractional coordinate system.  Likewise, the values of
C XMIN, XMAX, YMIN, and YMAX are expected to be in the fractional
C coordinate system.
C
C Declare the common block that holds the clipping window parameters.
C
      COMMON /CPWCMN/ XMIN,XMAX,YMIN,YMAX
C
C Declare some arrays to be used for passing point coordinates to
C AREDAM.
C
      DIMENSION XCRA(2),YCRA(2)
C
C Certain quantities need to be saved from call to call.  LPOW is a
C "last-point-outside-window" flag.  (XLST,YLST) is the last point
C (from the previous call to CPWLAM).
C
      SAVE LPOW,XLST,YLST
C
C Compute a "next-point-outside-window" flag.  The value of this flag
C is between -4 and +4, depending on where the next point is relative
C to the window, as shown in the following diagram:
C
C                      |      |
C                   -2 |  +1  | +4
C            YMAX -----+------+-----
C                   -3 |   0  | +3
C            YMIN -----+------+-----
C                   -4 |  -1  | +2
C                      |      |
C                    XMIN    XMAX
C
C Ultimately, we combine the values of this flag for two consecutive
C points in such a way as to get an integer between 1 and 81, telling
C us what combination of inside/outside we have to deal with.
C
      NPOW=IFIX(3.*(SIGN(.51,XNXT-XMIN)+SIGN(.51,XNXT-XMAX))+
     +             (SIGN(.51,YNXT-YMIN)+SIGN(.51,YNXT-YMAX)))
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
          XCRA(1)=CFUX(XLST)
          IF (ICFELL('CPWLAM',1).NE.0) RETURN
          YCRA(1)=CFUY(YLST)
          IF (ICFELL('CPWLAM',2).NE.0) RETURN
          XCRA(2)=CFUX(XNXT)
          IF (ICFELL('CPWLAM',3).NE.0) RETURN
          YCRA(2)=CFUY(YNXT)
          IF (ICFELL('CPWLAM',4).NE.0) RETURN
          CALL AREDAM (IAMA,XCRA,YCRA,2,IGID,IAIL,IAIR)
          IF (ICFELL('CPWLAM',5).NE.0) RETURN
          GO TO 115
        ELSE
          IF (NPOW.EQ.0) GO TO 103
          GO TO 105
        END IF
C
C Last point inside, next point outside.
C
  101   XPEW=XLST
        YPEW=YLST
        XDIF=XNXT-XLST
        YDIF=YNXT-YLST
C
        IF (ABS(XDIF).GT..000001*(XMAX-XMIN)) THEN
          XPEW=XMIN
          IF (XDIF.GE.0.) XPEW=XMAX
          YPEW=YLST+(XPEW-XLST)*YDIF/XDIF
          IF (YPEW.GE.YMIN.AND.YPEW.LE.YMAX) GO TO 102
        END IF
C
        IF (ABS(YDIF).GT..000001*(YMAX-YMIN)) THEN
          YPEW=YMIN
          IF (YDIF.GE.0.) YPEW=YMAX
          XPEW=XLST+(YPEW-YLST)*XDIF/YDIF
        END IF
C
  102   XCRA(1)=CFUX(XLST)
        IF (ICFELL('CPWLAM',6).NE.0) RETURN
        YCRA(1)=CFUY(YLST)
        IF (ICFELL('CPWLAM',7).NE.0) RETURN
        XCRA(2)=CFUX(XPEW)
        IF (ICFELL('CPWLAM',8).NE.0) RETURN
        YCRA(2)=CFUY(YPEW)
        IF (ICFELL('CPWLAM',9).NE.0) RETURN
        CALL AREDAM (IAMA,XCRA,YCRA,2,IGID,IAIL,IAIR)
        IF (ICFELL('CPWLAM',10).NE.0) RETURN
C
        GO TO 115
C
C Last point outside, next point inside.
C
  103   XPEW=XNXT
        YPEW=YNXT
        XDIF=XLST-XNXT
        YDIF=YLST-YNXT
C
        IF (ABS(XDIF).GT..000001*(XMAX-XMIN)) THEN
          XPEW=XMIN
          IF (XDIF.GE.0.) XPEW=XMAX
          YPEW=YNXT+(XPEW-XNXT)*YDIF/XDIF
          IF (YPEW.GE.YMIN.AND.YPEW.LE.YMAX) GO TO 104
        END IF
C
        IF (ABS(YDIF).GT..000001*(YMAX-YMIN)) THEN
          YPEW=YMIN
          IF (YDIF.GE.0.) YPEW=YMAX
          XPEW=XNXT+(YPEW-YNXT)*XDIF/YDIF
        END IF
C
  104   XCRA(1)=CFUX(XPEW)
        IF (ICFELL('CPWLAM',11).NE.0) RETURN
        YCRA(1)=CFUY(YPEW)
        IF (ICFELL('CPWLAM',12).NE.0) RETURN
        XCRA(2)=CFUX(XNXT)
        IF (ICFELL('CPWLAM',13).NE.0) RETURN
        YCRA(2)=CFUY(YNXT)
        IF (ICFELL('CPWLAM',14).NE.0) RETURN
        CALL AREDAM (IAMA,XCRA,YCRA,2,IGID,IAIL,IAIR)
        IF (ICFELL('CPWLAM',15).NE.0) RETURN
C
        GO TO 115
C
C Last point outside, next point outside.  Check whether or not part of
C the line joining them lies in the window.
C
  105   MPOW=9*LPOW+NPOW+41
C
        GO TO ( 115,115,115,115,115,106,115,106,106,
     +          115,115,115,107,115,106,107,106,106,
     +          115,115,115,107,115,115,107,107,115,
     +          115,109,109,115,115,106,115,106,106,
     +          115,115,115,115,115,115,115,115,115,
     +          108,108,115,108,115,115,107,107,115,
     +          115,109,109,115,115,109,115,115,115,
     +          108,108,109,108,115,109,115,115,115,
     +          108,108,115,108,115,115,115,115,115 ) , MPOW
C
  106   XPE1=XMIN
        YPT1=YMIN
        XPE2=XMAX
        YPT2=YMAX
        GO TO 110
C
  107   XPE1=XMIN
        YPT1=YMAX
        XPE2=XMAX
        YPT2=YMIN
        GO TO 110
C
  108   XPE1=XMAX
        YPT1=YMAX
        XPE2=XMIN
        YPT2=YMIN
        GO TO 110
C
  109   XPE1=XMAX
        YPT1=YMIN
        XPE2=XMIN
        YPT2=YMAX
C
  110   XDIF=XNXT-XLST
        YDIF=YNXT-YLST
C
        IF (ABS(XDIF).LE..000001*(XMAX-XMIN)) GO TO 112
        YPE1=YLST+(XPE1-XLST)*YDIF/XDIF
        YPE2=YLST+(XPE2-XLST)*YDIF/XDIF
C
        IF (ABS(YDIF).LE..000001*(YMAX-YMIN)) THEN
          IF (YPE1.LT.YMIN.OR.YPE1.GT.YMAX) GO TO 115
          IF (YPE2.LT.YMIN.OR.YPE2.GT.YMAX) GO TO 115
          GO TO 114
        END IF
C
        IF (YPE1.GE.YMIN.AND.YPE1.LE.YMAX) GO TO 111
        YPE1=YPT1
        XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
        IF (XPE1.LT.XMIN.OR.XPE1.GT.XMAX) GO TO 115
C
  111   IF (YPE2.GE.YMIN.AND.YPE2.LE.YMAX) GO TO 114
        GO TO 113
C
  112   YPE1=YPT1
        XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
        IF (XPE1.LT.XMIN.OR.XPE1.GT.XMAX) GO TO 115
C
  113   YPE2=YPT2
        XPE2=XLST+(YPE2-YLST)*XDIF/YDIF
        IF (XPE2.LT.XMIN.OR.XPE2.GT.XMAX) GO TO 115
C
  114   XCRA(1)=CFUX(XPE1)
        IF (ICFELL('CPWLAM',16).NE.0) RETURN
        YCRA(1)=CFUY(YPE1)
        IF (ICFELL('CPWLAM',17).NE.0) RETURN
        XCRA(2)=CFUX(XPE2)
        IF (ICFELL('CPWLAM',18).NE.0) RETURN
        YCRA(2)=CFUY(YPE2)
        IF (ICFELL('CPWLAM',19).NE.0) RETURN
        CALL AREDAM (IAMA,XCRA,YCRA,2,IGID,IAIL,IAIR)
        IF (ICFELL('CPWLAM',20).NE.0) RETURN
C
      END IF
C
C Processing of the next point is done.  It becomes the last point and
C we return to the user for a new next point.
C
  115 LPOW=NPOW
      XLST=XNXT
      YLST=YNXT
C
      RETURN
C
      END
