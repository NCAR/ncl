C
C $Id: ctwlam.f,v 1.1 2003-05-28 15:44:35 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE CTWLAM (XNXT,YNXT,IFST,IAMA,IGID,IAIL,IAIR)
C
      DIMENSION IAMA(*)
C
C This is a windowing routine for line draws.  Code using it should
C declare the common block WDCOMN and put into it minimum and maximum
C values of X and Y that together define a window at the edges of which
C lines are to be clipped.  Once that has been done, each call to
C CTWLAM with IFST = 0 declares a point (XNXT,YNXT) at which a line
C is to begin and each call to CTWLAM with IFST = 1 declares a point
C (XNXT,YNXT) at which a line is to continue.
C
C This version of CTWLAM puts the windowed line segments into the area
C map IAMA, using group identifier IGID, left area identifier IAIL, and
C right area identifier IAIR.  Each (XNXT,YNXT) is expected to be a
C point in the fractional coordinate system.  Likewise, the values of
C XWMN, XWMX, YWMN, and YWMX are expected to be in the fractional
C coordinate system.
C
C Declare the common block that holds the clipping window parameters.
C
      COMMON /CTWCMN/ XWMN,XWMX,YWMN,YWMX
C
C Declare some arrays to be used for passing point coordinates to
C AREDAM.
C
      DIMENSION XCRA(2),YCRA(2)
C
C Certain quantities need to be saved from call to call.  LPOW is a
C "last-point-outside-window" flag.  (XLST,YLST) is the last point
C (from the previous call to CTWLAM).
C
      SAVE LPOW,XLST,YLST
C
C Compute a "next-point-outside-window" flag.  The value of this flag
C is between -4 and +4, depending on where the next point is relative
C to the window, as shown in the following diagram:
C
C                      |      |
C                   -2 |  +1  | +4
C            YWMX -----+------+-----
C                   -3 |   0  | +3
C            YWMN -----+------+-----
C                   -4 |  -1  | +2
C                      |      |
C                    XWMN    XWMX
C
C Ultimately, we combine the values of this flag for two consecutive
C points in such a way as to get an integer between 1 and 81, telling
C us what combination of inside/outside we have to deal with.
C
      NPOW=IFIX(3.*(SIGN(.51,XNXT-XWMN)+SIGN(.51,XNXT-XWMX))+
     +             (SIGN(.51,YNXT-YWMN)+SIGN(.51,YNXT-YWMX)))
C
C If the next point is not the first point of a line, there is work to
C be done.
C
      IF (IFST.NE.0) THEN
C
C The left and right area identifiers passed to AREDAM must be defined
C to be consistent with the user coordinate system, rather than the
C fractional system.
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
        IF ((XWDL.LT.XWDR.AND.YWDB.LT.YWDT).OR.(XWDL.GT.XWDR.AND.YWDB.GT
     +.YWDT)) THEN
          JAIL=IAIL
          JAIR=IAIR
        ELSE
          JAIL=IAIR
          JAIR=IAIL
        END IF
C
C There are various possible cases, depending on whether the last point
C was inside or outside the window and whether the next point is inside
C or outside the window.
C
        IF (LPOW.EQ.0) THEN
          IF (NPOW.NE.0) GO TO 101
          XCRA(1)=CFUX(XLST)
          IF (ICFELL('CTWLAM',1).NE.0) RETURN
          YCRA(1)=CFUY(YLST)
          IF (ICFELL('CTWLAM',2).NE.0) RETURN
          XCRA(2)=CFUX(XNXT)
          IF (ICFELL('CTWLAM',3).NE.0) RETURN
          YCRA(2)=CFUY(YNXT)
          IF (ICFELL('CTWLAM',4).NE.0) RETURN
          CALL AREDAM (IAMA,XCRA,YCRA,2,IGID,JAIL,JAIR)
          IF (ICFELL('CTWLAM',5).NE.0) RETURN
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
        IF (ABS(XDIF).GT..000001*(XWMX-XWMN)) THEN
          XPEW=XWMN
          IF (XDIF.GE.0.) XPEW=XWMX
          YPEW=YLST+(XPEW-XLST)*YDIF/XDIF
          IF (YPEW.GE.YWMN.AND.YPEW.LE.YWMX) GO TO 102
        END IF
C
        IF (ABS(YDIF).GT..000001*(YWMX-YWMN)) THEN
          YPEW=YWMN
          IF (YDIF.GE.0.) YPEW=YWMX
          XPEW=XLST+(YPEW-YLST)*XDIF/YDIF
        END IF
C
  102   XCRA(1)=CFUX(XLST)
        IF (ICFELL('CTWLAM',6).NE.0) RETURN
        YCRA(1)=CFUY(YLST)
        IF (ICFELL('CTWLAM',7).NE.0) RETURN
        XCRA(2)=CFUX(XPEW)
        IF (ICFELL('CTWLAM',8).NE.0) RETURN
        YCRA(2)=CFUY(YPEW)
        IF (ICFELL('CTWLAM',9).NE.0) RETURN
        CALL AREDAM (IAMA,XCRA,YCRA,2,IGID,JAIL,JAIR)
        IF (ICFELL('CTWLAM',10).NE.0) RETURN
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
        IF (ABS(XDIF).GT..000001*(XWMX-XWMN)) THEN
          XPEW=XWMN
          IF (XDIF.GE.0.) XPEW=XWMX
          YPEW=YNXT+(XPEW-XNXT)*YDIF/XDIF
          IF (YPEW.GE.YWMN.AND.YPEW.LE.YWMX) GO TO 104
        END IF
C
        IF (ABS(YDIF).GT..000001*(YWMX-YWMN)) THEN
          YPEW=YWMN
          IF (YDIF.GE.0.) YPEW=YWMX
          XPEW=XNXT+(YPEW-YNXT)*XDIF/YDIF
        END IF
C
  104   XCRA(1)=CFUX(XPEW)
        IF (ICFELL('CTWLAM',11).NE.0) RETURN
        YCRA(1)=CFUY(YPEW)
        IF (ICFELL('CTWLAM',12).NE.0) RETURN
        XCRA(2)=CFUX(XNXT)
        IF (ICFELL('CTWLAM',13).NE.0) RETURN
        YCRA(2)=CFUY(YNXT)
        IF (ICFELL('CTWLAM',14).NE.0) RETURN
        CALL AREDAM (IAMA,XCRA,YCRA,2,IGID,JAIL,JAIR)
        IF (ICFELL('CTWLAM',15).NE.0) RETURN
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
  106   XPE1=XWMN
        YPT1=YWMN
        XPE2=XWMX
        YPT2=YWMX
        GO TO 110
C
  107   XPE1=XWMN
        YPT1=YWMX
        XPE2=XWMX
        YPT2=YWMN
        GO TO 110
C
  108   XPE1=XWMX
        YPT1=YWMX
        XPE2=XWMN
        YPT2=YWMN
        GO TO 110
C
  109   XPE1=XWMX
        YPT1=YWMN
        XPE2=XWMN
        YPT2=YWMX
C
  110   XDIF=XNXT-XLST
        YDIF=YNXT-YLST
C
        IF (ABS(XDIF).LE..000001*(XWMX-XWMN)) GO TO 112
        YPE1=YLST+(XPE1-XLST)*YDIF/XDIF
        YPE2=YLST+(XPE2-XLST)*YDIF/XDIF
C
        IF (ABS(YDIF).LE..000001*(YWMX-YWMN)) THEN
          IF (YPE1.LT.YWMN.OR.YPE1.GT.YWMX) GO TO 115
          IF (YPE2.LT.YWMN.OR.YPE2.GT.YWMX) GO TO 115
          GO TO 114
        END IF
C
        IF (YPE1.GE.YWMN.AND.YPE1.LE.YWMX) GO TO 111
        YPE1=YPT1
        XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
        IF (XPE1.LT.XWMN.OR.XPE1.GT.XWMX) GO TO 115
C
  111   IF (YPE2.GE.YWMN.AND.YPE2.LE.YWMX) GO TO 114
        GO TO 113
C
  112   YPE1=YPT1
        XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
        IF (XPE1.LT.XWMN.OR.XPE1.GT.XWMX) GO TO 115
C
  113   YPE2=YPT2
        XPE2=XLST+(YPE2-YLST)*XDIF/YDIF
        IF (XPE2.LT.XWMN.OR.XPE2.GT.XWMX) GO TO 115
C
  114   XCRA(1)=CFUX(XPE1)
        IF (ICFELL('CTWLAM',16).NE.0) RETURN
        YCRA(1)=CFUY(YPE1)
        IF (ICFELL('CTWLAM',17).NE.0) RETURN
        XCRA(2)=CFUX(XPE2)
        IF (ICFELL('CTWLAM',18).NE.0) RETURN
        YCRA(2)=CFUY(YPE2)
        IF (ICFELL('CTWLAM',19).NE.0) RETURN
        CALL AREDAM (IAMA,XCRA,YCRA,2,IGID,JAIL,JAIR)
        IF (ICFELL('CTWLAM',20).NE.0) RETURN
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
