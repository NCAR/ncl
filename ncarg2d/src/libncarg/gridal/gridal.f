C
C $Id: gridal.f,v 1.10 2008-07-27 00:17:14 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,IGPH,XINT,YINT)
C
C Declare the common block containing real and integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ILTY,IORX,NCFX,NCFY,RCWX,
     +                  RCWY,RDCX,RDCY,RMJX,RMJY,RMNX,RMNY,RWAX,RWLB,
     +                  RWMJ,RWMN
        SAVE   /GAREIN/
C
C Declare the common block containing character parameters.
C
        COMMON /GACHAR/ FNLX,FNLY
        CHARACTER*10    FNLX,FNLY
        SAVE   /GACHAR/
C
C Declare an array in which to receive the "clipping rectangle".
C
        DIMENSION CLPR(4)
C
C Declare local variables to use in encoding labels.
C
        CHARACTER*10 FNLB
        CHARACTER*24 LABL
C
C Declare some local variables double-precision.  (They are used to
C create labels.)
C
        DOUBLE PRECISION DLBL,EPSI,OPEP,VEPS,VLBL
C
C Initialize the values of EPSI and OPEP so that they will be recomputed
C by the code itself.
C
        SAVE EPSI,OPEP
C
        DATA EPSI,OPEP / 0.D0 , 1.D0 /
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL GABLDT
C
C Check for an uncleared prior error.
C
        IF (ICFELL('GRIDAL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If it has not been done yet, compute the constants "epsilon" and
C "1+epsilon"; the latter is to be used multiplicatively in rounding
C to get rid of strings of nines in labels.
C
        IF (EPSI.EQ.0.D0) THEN
C
          NSDR=0
C
  101     NSDR=NSDR+1
          CALL GAGTRN (NSDR,TMP1,TMP2,TMP3)
          IF (TMP2.NE.1..AND.TMP2.NE.TMP3.AND.NSDR.LT.100) GO TO 101
C
          EPSI=10.D0**(1-NSDR)
          OPEP=1.D0+EPSI
C
        END IF
C
C Pick up the current definition of the window and the viewport and
C the current x/y linear/log flag.
C
        CALL GETSET (VPLX,VPRX,VPBY,VPTY,WDLX,WDRX,WDBY,WDTY,LILO)
        IF (ICFELL('GRIDAL',2).NE.0) RETURN
C
C Set minimum and maximum values of X and Y.
C
        XMIN=MIN(WDLX,WDRX)
        XMAX=MAX(WDLX,WDRX)
C
        YMIN=MIN(WDBY,WDTY)
        YMAX=MAX(WDBY,WDTY)
C
C Set the linear/log and mirror image flags for the two axes.
C
        ILGX=(LILO-1)/2
C
        IF (WDLX.LT.WDRX) THEN
          IMIX=0
        ELSE
          IMIX=1
        END IF
C
        ILGY=MOD(LILO-1,2)
C
        IF (WDBY.LT.WDTY) THEN
          IMIY=0
        ELSE
          IMIY=1
        END IF
C
C Compute the width and height of the plotter window, in plotter units.
C
        CALL GETSI (IP2X,IP2Y)
        IF (ICFELL('GRIDAL',3).NE.0) RETURN
        WPLO=2.**IP2X-1.
        HPLO=2.**IP2Y-1.
C
C Compute the number of major and minor divisions of each axis, imposing
C limits on the input values in order to keep the code from blowing up.
C
        IF (ILGX.EQ.0) THEN
          NMJX=MAX(1,MIN(10000,MJRX))
          NMNX=MAX(1,MIN(10000,MNRX))
        ELSE
          IPTX=MAX(1,MIN(100,MJRX))
          FPTX=REAL(IPTX)
          NMJX=INT(1.0001*ABS(ALOG10(WDRX/WDLX))/FPTX)
          IF (MNRX.LE.10) THEN
            NMNX=9
          ELSE
            NMNX=1
          END IF
        END IF
C
        IF (ILGY.EQ.0) THEN
          NMJY=MAX(1,MIN(10000,MJRY))
          NMNY=MAX(1,MIN(10000,MNRY))
        ELSE
          IPTY=MAX(1,MIN(100,MJRY))
          FPTY=REAL(IPTY)
          NMJY=INT(1.0001*ABS(ALOG10(WDTY/WDBY))/FPTY)
          IF (MNRY.LE.10) THEN
            NMNY=9
          ELSE
            NMNY=1
          END IF
        END IF
C
C Save the current state of the clipping indicator and then turn it off.
C
        CALL GQCLIP (IGER,ICLP,CLPR)
        IF (IGER.NE.0) THEN
          CALL SETER ('GRIDAL - ERROR EXIT FROM GQCLIP',4,1)
          RETURN
        END IF
        CALL GSCLIP (0)
C
C The following loop runs through the types of items to be drawn.  ITEM
C = 1 implies minor ticks, 2 implies major ticks, 3 implies the axes,
C and 4 implies the labels.
C
        DO 104 ITEM=1,4
C
C Set the color index and line width for the type of item being drawn.
C
          IF (ITEM.EQ.1) THEN
            IF (ICMN.GE.0) THEN
              CALL PLOTIF (0.,0.,2)
              IF (ICFELL('GRIDAL',5).NE.0) RETURN
              CALL GQPLCI (IGER,ICS1)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQPLCI',6,1)
                RETURN
              END IF
              CALL GSPLCI (ICMN)
            END IF
            IF (RWMN.GT.0.) THEN
              CALL PLOTIF (0.,0.,2)
              IF (ICFELL('GRIDAL',7).NE.0) RETURN
              CALL GQLWSC (IGER,SLWS)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQLWSC',8,1)
                RETURN
              END IF
              CALL GSLWSC (RWMN)
            END IF
          ELSE IF (ITEM.EQ.2) THEN
            IF (ICMJ.GE.0) THEN
              CALL PLOTIF (0.,0.,2)
              IF (ICFELL('GRIDAL',9).NE.0) RETURN
              CALL GQPLCI (IGER,ICS1)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQPLCI',10,1)
                RETURN
              END IF
              CALL GSPLCI (ICMJ)
            END IF
            IF (RWMJ.GT.0.) THEN
              CALL PLOTIF (0.,0.,2)
              IF (ICFELL('GRIDAL',11).NE.0) RETURN
              CALL GQLWSC (IGER,SLWS)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQLWSC',12,1)
                RETURN
              END IF
              CALL GSLWSC (RWMJ)
            END IF
          ELSE IF (ITEM.EQ.3) THEN
            IF (ICAX.GE.0) THEN
              CALL PLOTIF (0.,0.,2)
              IF (ICFELL('GRIDAL',13).NE.0) RETURN
              CALL GQPLCI (IGER,ICS1)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQPLCI',14,1)
                RETURN
              END IF
              CALL GSPLCI (ICAX)
            END IF
            IF (RWAX.GT.0.) THEN
              CALL PLOTIF (0.,0.,2)
              IF (ICFELL('GRIDAL',15).NE.0) RETURN
              CALL GQLWSC (IGER,SLWS)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQLWSC',16,1)
                RETURN
              END IF
              CALL GSLWSC (RWAX)
            END IF
          ELSE IF (ITEM.EQ.4) THEN
            IF (ICLB.GE.0) THEN
              CALL GQPLCI (IGER,ICS1)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQPLCI',17,1)
                RETURN
              END IF
              CALL GSPLCI (ICLB)
              CALL GQTXCI (IGER,ICS2)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQTXCI',18,1)
                RETURN
              END IF
              CALL GSTXCI (ICLB)
            END IF
            IF (RWLB.GT.0.) THEN
              CALL PLOTIF (0.,0.,2)
              IF (ICFELL('GRIDAL',19).NE.0) RETURN
              CALL GQLWSC (IGER,SLWS)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQLWSC',20,1)
                RETURN
              END IF
              CALL GSLWSC (RWLB)
            END IF
          END IF
C
C The next loop runs through the four axes.  IAXS = 1 implies the left
C axis, 2 the bottom axis, 3 the right axis, and 4 the top axis.
C
          DO 103 IAXS=1,4
C
C On the first pass through the loop, set up the required parameters
C to do the left axis.
C
            IF (IAXS.EQ.1) THEN
C
C If the left axis isn't being done at all, or if the type of item
C being drawn now isn't present on the left axis, skip it.
C
              IF (IYLB.LT.0) GO TO 103
              IF (ITEM.EQ.1.AND.NMNY.LE.1) GO TO 103
              IF (ITEM.EQ.4.AND.IYLB.LE.0) GO TO 103
C
C Set the linear/log flag.
C
              ILGF=ILGY
C
C Set the mirror-image flag to indicate whether the axis is being drawn
C in the direction from smaller user values to larger user values (0)
C or in the reverse direction (1).
C
              IMIF=1-IMIY
C
C Set the parameters determining the number of major and minor divisions
C of the axis.
C
              NMJD=NMJY
              NMND=NMNY
              IF (ITEM.NE.1) NMND=1
C
C Determine the fractional coordinates of the first point on the axis
C and the x and y increment required to get from each major tick to
C the next.
C
              IF (MOD(IGPH,4)-1.LE.0) THEN
                QMJX=VPLX
              ELSE
                QMJX=CUFX(MAX(XMIN,MIN(XMAX,XINT)))
                IF (ICFELL('GRIDAL',21).NE.0) RETURN
              END IF
              DMJX=0.
              QMJY=VPTY
              DMJY=VPBY-VPTY
              IF (ITEM.NE.3) THEN
                IF (ILGF.EQ.0) THEN
                  DMJY=DMJY/REAL(NMJD)
                ELSE
                  FPTN=FPTY
                  DMJY=DMJY*FPTN/ABS(ALOG10(WDTY/WDBY))
                  IF (IMIF.NE.0) QMJY=VPBY-REAL(NMJD)*DMJY
                END IF
              END IF
C
C Set the coordinates of the points at which we should not draw major
C ticks because they would overlap intersecting axes.
C
              AMJX=QMJX
              AMJY=2.
              BMJX=QMJX
              BMJY=-1.
              IF ((MOD(IGPH,4)-1.LT.0.OR.RMJY.GT.0.).AND.IXLB.GE.0) THEN
                IF (IGPH/4-1.LE.0) THEN
                  AMJY=VPTY
                  BMJY=VPBY
                ELSE
                  AMJY=CUFY(MAX(YMIN,MIN(YMAX,YINT)))
                  IF (ICFELL('GRIDAL',22).NE.0) RETURN
                END IF
              END IF
C
C Compute tick-mark offset parameters.
C
              IF (MOD(IGPH,4)-1.LT.0) THEN
                TMJX=VPRX-VPLX
                TMNX=VPRX-VPLX
              ELSE
                IF (RMJY.GT.-1..AND.RMJY.LT.+1.) THEN
                  TMJX=RMJY
                ELSE
                  TMJX=REAL(INT(RMJY))/WPLO
                END IF
                IF (RMNY.GT.-1..AND.RMNY.LT.+1.) THEN
                  TMNX=RMNY
                ELSE
                  TMNX=REAL(INT(RMNY))/WPLO
                END IF
              END IF
              TMJY=0.
              TMNY=0.
C
C If numeric labels are being done, compute the value of the first one,
C the increment required to get from one to the next, and all other
C local variables required to encode and write the labels.
C
              IF (ITEM.EQ.4) THEN
C
                VLBL=DBLE(WDTY)
                IF (ILGF.EQ.0) THEN
                  DLBL=DBLE(WDBY-WDTY)/DBLE(NMJD)
                  VEPS=EPSI*DBLE(ABS(WDTY-WDBY))
                ELSE
                  DLBL=10.D0**IPTY
                  IF (IMIF.NE.0) DLBL=1.D0/DLBL
                  VEPS=0.D0
                END IF
C
                IF (RDCX.EQ.0.) THEN
                  DLBX=-20./WPLO
                ELSE IF (RDCX.EQ.1.) THEN
                  DLBX=+20./WPLO
                  IF (MOD(IGPH,4)-1.LE.0) DLBX=DLBX+VPRX-VPLX
                ELSE IF (RDCX.LE.-1..OR.RDCX.GE.+1.) THEN
                  DLBX=REAL(INT(-RDCX))/WPLO
                ELSE
                  DLBX=-RDCX
                END IF
                DLBY=0.
C
                FNLB=FNLY
C
                RCHW=RCWY
                IF (RCHW.LE.0..OR.RCHW.GE.1.) THEN
                  ICHW=INT(MAX(0.,RCHW))
                  IF (ICHW.LE.3) ICHW=(8+4*MOD(ICHW,2))*(1+ICHW/2)
                  RCHW=REAL(ICHW)/WPLO
                END IF
                ICHW=MAX(4,INT(RCHW*WPLO))
C
                IORI=0
                ICEN=INT(-SIGN(1.,DLBX))
C
                NCFR=NCFY
                IF (NCFR.NE.0) THEN
                  MLBL=1
                  NLBL=NCFR
                END IF
C
              END IF
C
C On the second pass through the loop, set up the required parameters
C to do the bottom axis.
C
            ELSE IF (IAXS.EQ.2) THEN
C
              IF (IXLB.LT.0) GO TO 103
              IF (ITEM.EQ.1.AND.NMNX.LE.1) GO TO 103
              IF (ITEM.EQ.4.AND.IXLB.LE.0) GO TO 103
C
              ILGF=ILGX
C
              IMIF=IMIX
C
              NMJD=NMJX
              NMND=NMNX
              IF (ITEM.NE.1) NMND=1
C
              QMJX=VPLX
              DMJX=VPRX-VPLX
              IF (ITEM.NE.3) THEN
                IF (ILGF.EQ.0) THEN
                  DMJX=DMJX/REAL(NMJD)
                ELSE
                  FPTN=FPTX
                  DMJX=DMJX*FPTN/ABS(ALOG10(WDRX/WDLX))
                  IF (IMIF.NE.0) QMJX=VPRX-REAL(NMJD)*DMJX
                END IF
              END IF
              IF (IGPH/4-1.LE.0) THEN
                QMJY=VPBY
              ELSE
                QMJY=CUFY(MAX(YMIN,MIN(YMAX,YINT)))
                IF (ICFELL('GRIDAL',23).NE.0) RETURN
              END IF
              DMJY=0.
C
              AMJX=-1.
              AMJY=QMJY
              BMJX=2.
              BMJY=QMJY
              IF ((IGPH/4-1.LT.0.OR.RMJX.GT.0.).AND.IYLB.GE.0) THEN
                IF (MOD(IGPH,4)-1.LE.0) THEN
                  AMJX=VPLX
                  BMJX=VPRX
                ELSE
                  AMJX=CUFX(MAX(XMIN,MIN(XMAX,XINT)))
                  IF (ICFELL('GRIDAL',24).NE.0) RETURN
                END IF
              END IF
C
              TMJX=0.
              TMNX=0.
              IF (IGPH/4-1.LT.0) THEN
                TMJY=VPTY-VPBY
                TMNY=VPTY-VPBY
              ELSE
                IF (RMJX.GT.-1..AND.RMJX.LT.+1.) THEN
                  TMJY=RMJX
                ELSE
                  TMJY=REAL(INT(RMJX))/WPLO
                END IF
                IF (RMNX.GT.-1..AND.RMNX.LT.+1.) THEN
                  TMNY=RMNX
                ELSE
                  TMNY=REAL(INT(RMNX))/WPLO
                END IF
              END IF
C
              IF (ITEM.EQ.4) THEN
C
                VLBL=DBLE(WDLX)
                IF (ILGF.EQ.0) THEN
                  DLBL=DBLE(WDRX-WDLX)/DBLE(NMJD)
                  VEPS=EPSI*DBLE(ABS(WDRX-WDLX))
                ELSE
                  DLBL=10.D0**IPTX
                  IF (IMIF.NE.0) DLBL=1.D0/DLBL
                  VEPS=0.D0
                END IF
C
                DLBX=0.
                IF (RDCY.EQ.0.) THEN
                  DLBY=-20./HPLO
                ELSE IF (RDCY.EQ.1.) THEN
                  DLBY=+20./HPLO
                  IF (IGPH/4-1.LE.0) DLBY=DLBY+VPTY-VPBY
                ELSE IF (RDCY.LE.-1..OR.RDCY.GE.+1.) THEN
                  DLBY=REAL(INT(-RDCY))/HPLO
                ELSE
                  DLBY=-RDCY
                END IF
C
                FNLB=FNLX
C
                RCHW=RCWX
                IF (RCHW.LE.0..OR.RCHW.GE.1.) THEN
                  ICHW=INT(MAX(0.,RCHW))
                  IF (ICHW.LE.3) ICHW=(8+4*MOD(ICHW,2))*(1+ICHW/2)
                  RCHW=REAL(ICHW)/WPLO
                END IF
                ICHW=MAX(4,INT(RCHW*WPLO))
C
                IF (IORX.EQ.0) THEN
                  IORI=0
                  ICEN=0
                  DLBY=DLBY+SIGN(RCHW,DLBY)
                ELSE
                  IORI=90
                  ICEN=INT(-SIGN(1.,DLBY))
                END IF
C
                NCFR=NCFX
                IF (NCFR.NE.0) THEN
                  MLBL=1
                  NLBL=NCFR
                END IF
C
              END IF
C
C On the third pass through the loop, set up the required parameters
C to do the right axis.
C
            ELSE IF (IAXS.EQ.3) THEN
C
              IF (IYLB.LT.0) GO TO 103
              IF (ITEM.EQ.1.AND.NMNY.LE.1) GO TO 103
              IF (ITEM.EQ.4) GO TO 103
              IF ((ITEM.EQ.1.OR.ITEM.EQ.2).AND.
     +                                     MOD(IGPH,4)-1.NE.0) GO TO 103
C
              ILGF=ILGY
C
              IMIF=IMIY
C
              NMJD=NMJY
              NMND=NMNY
              IF (ITEM.NE.1) NMND=1
C
              IF (MOD(IGPH,4)-1.LE.0) THEN
                QMJX=VPRX
              ELSE
                QMJX=CUFX(MAX(XMIN,MIN(XMAX,XINT)))
                IF (ICFELL('GRIDAL',25).NE.0) RETURN
              END IF
              DMJX=0.
              QMJY=VPBY
              DMJY=VPTY-VPBY
              IF (ITEM.NE.3) THEN
                IF (ILGF.EQ.0) THEN
                  DMJY=DMJY/REAL(NMJD)
                ELSE
                  FPTN=FPTY
                  DMJY=DMJY*FPTN/ABS(ALOG10(WDTY/WDBY))
                  IF (IMIF.NE.0) QMJY=VPTY-REAL(NMJD)*DMJY
                END IF
              END IF
C
              AMJX=QMJX
              AMJY=-1.
              BMJX=QMJX
              BMJY=2.
              IF (RMJY.GT.0..AND.IXLB.GE.0) THEN
                IF (IGPH/4-1.LE.0) THEN
                  AMJY=VPBY
                  BMJY=VPTY
                ELSE
                  AMJY=CUFY(MAX(YMIN,MIN(YMAX,YINT)))
                  IF (ICFELL('GRIDAL',26).NE.0) RETURN
                END IF
              END IF
C
              IF (RMJY.GT.-1..AND.RMJY.LT.+1.) THEN
                TMJX=-RMJY
              ELSE
                TMJX=-REAL(INT(RMJY))/WPLO
              END IF
              IF (RMNY.GT.-1..AND.RMNY.LT.+1.) THEN
                TMNX=-RMNY
              ELSE
                TMNX=-REAL(INT(RMNY))/WPLO
              END IF
              TMJY=0.
              TMNY=0.
C
C On the fourth pass through the loop, set up the required parameters
C to do the top axis.
C
            ELSE IF (IAXS.EQ.4) THEN
C
              IF (IXLB.LT.0) GO TO 103
              IF (ITEM.EQ.1.AND.NMNX.LE.1) GO TO 103
              IF ((ITEM.EQ.1.OR.ITEM.EQ.2).AND.IGPH/4-1.NE.0) GO TO 103
              IF (ITEM.EQ.4) GO TO 103
C
              ILGF=ILGX
C
              IMIF=1-IMIX
C
              NMJD=NMJX
              NMND=NMNX
              IF (ITEM.NE.1) NMND=1
C
              QMJX=VPRX
              DMJX=VPLX-VPRX
              IF (ITEM.NE.3) THEN
                IF (ILGF.EQ.0) THEN
                  DMJX=DMJX/REAL(NMJD)
                ELSE
                  FPTN=FPTX
                  DMJX=DMJX*FPTN/ABS(ALOG10(WDRX/WDLX))
                  IF (IMIF.NE.0) QMJX=VPLX-REAL(NMJD)*DMJX
                END IF
              END IF
              IF (IGPH/4-1.LE.0) THEN
                QMJY=VPTY
              ELSE
                QMJY=CUFY(MAX(YMIN,MIN(YMAX,YINT)))
                IF (ICFELL('GRIDAL',27).NE.0) RETURN
              END IF
              DMJY=0.
C
              AMJX=2.
              AMJY=QMJY
              BMJX=-1.
              BMJY=QMJY
              IF (RMJX.GT.0..AND.IYLB.GE.0) THEN
                IF (MOD(IGPH,4)-1.LE.0) THEN
                  AMJX=VPRX
                  BMJX=VPLX
                ELSE
                  AMJX=CUFX(MAX(XMIN,MIN(XMAX,XINT)))
                  IF (ICFELL('GRIDAL',28).NE.0) RETURN
                END IF
              END IF
C
              TMJX=0.
              TMNX=0.
              IF (RMJX.GT.-1..AND.RMJX.LT.+1.) THEN
                TMJY=-RMJX
              ELSE
                TMJY=-REAL(INT(RMJX))/WPLO
              END IF
              IF (RMNX.GT.-1..AND.RMNX.LT.+1.) THEN
                TMNY=-RMNX
              ELSE
                TMNY=-REAL(INT(RMNX))/WPLO
              END IF
C
            END IF
C
C See if the item being drawn requires looping through the tick mark
C positions along the axis.
C
            IF (ITEM.NE.3) THEN
C
C Initialize the counter which controls whether we draw major ticks or
C minor ticks and the flag which determines in which direction we draw
C the ticks.
C
              IMND=0
              IFLP=0
C
C Loop through the positions at which tick marks and/or labels need to
C be drawn.
C
              DO 102 IMRK=1,NMJD*NMND+1
                IF (IMND.EQ.0) THEN
                  PMJX=QMJX
                  PMJY=QMJY
                  QMJX=QMJX+DMJX
                  QMJY=QMJY+DMJY
                  IF (ITEM.EQ.2) THEN
                    IF (ABS(PMJX-AMJX+PMJY-AMJY).GT..0001.AND.
     +                  ABS(PMJX-BMJX+PMJY-BMJY).GT..0001) THEN
                      IFLP=1-IFLP
                      IF (IFLP.EQ.0) THEN
                        CALL PLOTIF (PMJX,     PMJY     ,0)
                        IF (ICFELL('GRIDAL',29).NE.0) RETURN
                        CALL PLOTIF (PMJX+TMJX,PMJY+TMJY,1)
                        IF (ICFELL('GRIDAL',30).NE.0) RETURN
                      ELSE
                        CALL PLOTIF (PMJX+TMJX,PMJY+TMJY,0)
                        IF (ICFELL('GRIDAL',31).NE.0) RETURN
                        CALL PLOTIF (PMJX,     PMJY     ,1)
                        IF (ICFELL('GRIDAL',32).NE.0) RETURN
                      END IF
                    END IF
                  END IF
                  IF (ITEM.EQ.4) THEN
                    IF (FNLB(2:2).NE.'I'.AND.FNLB(2:2).NE.'i') THEN
                      VNCD=REAL(VLBL*OPEP)
                      IF (ABS(VLBL).LT.VEPS) VNCD=0.
                      WRITE (LABL,FNLB) VNCD
                    ELSE
                      ILBL=NINT(VLBL)
                      WRITE (LABL,FNLB) ILBL
                    END IF
                    IF (NCFR.EQ.0) CALL GALBEX (LABL,MLBL,NLBL)
                    IF (ILTY.EQ.0) THEN
                      XDUM=CFUX(PMJX+DLBX)
                      IF (ICFELL('GRIDAL',33).NE.0) RETURN
                      YDUM=CFUY(PMJY+DLBY)
                      IF (ICFELL('GRIDAL',34).NE.0) RETURN
                      CALL WTSTR  (XDUM,YDUM,LABL(MLBL:NLBL),
     +                             ICHW,IORI,ICEN)
                      IF (ICFELL('GRIDAL',35).NE.0) RETURN
                    ELSE IF (ILTY.EQ.1) THEN
                      XDUM=CFUX(PMJX+DLBX)
                      IF (ICFELL('GRIDAL',36).NE.0) RETURN
                      YDUM=CFUY(PMJY+DLBY)
                      IF (ICFELL('GRIDAL',37).NE.0) RETURN
                      CALL PLCHHQ (XDUM,YDUM,LABL(MLBL:NLBL),
     +                             RCHW,REAL(IORI),REAL(ICEN))
                      IF (ICFELL('GRIDAL',38).NE.0) RETURN
                    ELSE
                      XDUM=CFUX(PMJX+DLBX)
                      IF (ICFELL('GRIDAL',36).NE.0) RETURN
                      YDUM=CFUY(PMJY+DLBY)
                      IF (ICFELL('GRIDAL',37).NE.0) RETURN
                      CALL GAPLCH (XDUM,YDUM,LABL(MLBL:NLBL),
     +                             RCHW,REAL(IORI),REAL(ICEN))
                      IF (ICFELL('GRIDAL',38).NE.0) RETURN
                    END IF
                    IF (ILGF.EQ.0) THEN
                      VLBL=VLBL+DLBL
                    ELSE
                      VLBL=VLBL*DLBL
                    END IF
                  END IF
                ELSE
                  IF (ILGF.EQ.0) THEN
                    PMNX=PMJX+(QMJX-PMJX)*REAL(IMND)/REAL(NMND)
                    PMNY=PMJY+(QMJY-PMJY)*REAL(IMND)/REAL(NMND)
                  ELSE
                    IF (IMIF.EQ.0) THEN
                      PMNX=PMJX+(QMJX-PMJX)*ALOG10(REAL(IMND+1))/FPTN
                      PMNY=PMJY+(QMJY-PMJY)*ALOG10(REAL(IMND+1))/FPTN
                    ELSE
                      PMNX=QMJX+(PMJX-QMJX)*ALOG10(REAL(NMND-IMND+1))
     +                                                             /FPTN
                      PMNY=QMJY+(PMJY-QMJY)*ALOG10(REAL(NMND-IMND+1))
     +                                                             /FPTN
                    END IF
                  END IF
                  IFLP=1-IFLP
                  IF (IFLP.EQ.0) THEN
                    CALL PLOTIF (PMNX,     PMNY     ,0)
                    IF (ICFELL('GRIDAL',39).NE.0) RETURN
                    CALL PLOTIF (PMNX+TMNX,PMNY+TMNY,1)
                    IF (ICFELL('GRIDAL',40).NE.0) RETURN
                  ELSE
                    CALL PLOTIF (PMNX+TMNX,PMNY+TMNY,0)
                    IF (ICFELL('GRIDAL',41).NE.0) RETURN
                    CALL PLOTIF (PMNX,     PMNY     ,1)
                    IF (ICFELL('GRIDAL',42).NE.0) RETURN
                  END IF
                END IF
                IMND=MOD(IMND+1,NMND)
  102         CONTINUE
            ELSE
C
C Draw the axis.
C
              CALL PLOTIF (QMJX,QMJY,0)
              IF (ICFELL('GRIDAL',43).NE.0) RETURN
              CALL PLOTIF (QMJX+DMJX,QMJY+DMJY,1)
              IF (ICFELL('GRIDAL',44).NE.0) RETURN
C
            END IF
C
  103     CONTINUE
C
C Reset the polyline and text color indices.
C
          IF ((ITEM.EQ.1.AND.ICMN.GE.0).OR.
     +        (ITEM.EQ.2.AND.ICMJ.GE.0).OR.
     +        (ITEM.EQ.3.AND.ICAX.GE.0).OR.
     +        (ITEM.EQ.4.AND.ICLB.GE.0)) THEN
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('GRIDAL',45).NE.0) RETURN
            CALL GSPLCI (ICS1)
          END IF
C
          IF (ITEM.EQ.4.AND.ICLB.GE.0) THEN
            CALL GSTXCI (ICS2)
          END IF
C
C Reset the line width scale factor.
C
          IF ((ITEM.EQ.1.AND.RWMN.GT.0.).OR.
     +        (ITEM.EQ.2.AND.RWMJ.GT.0.).OR.
     +        (ITEM.EQ.3.AND.RWAX.GT.0.).OR.
     +        (ITEM.EQ.4.AND.RWLB.GT.0.)) THEN
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('GRIDAL',46).NE.0) RETURN
            CALL GSLWSC (SLWS)
          END IF
C
  104   CONTINUE
C
C Flush the SPPS pen-move buffer.
C
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('GRIDAL',47).NE.0) RETURN
C
C Restore the original state of the clipping indicator.
C
        CALL GSCLIP (ICLP)
C
C Done.
C
        RETURN
C
      END
