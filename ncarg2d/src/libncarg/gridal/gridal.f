C
C	$Id: gridal.f,v 1.1.1.1 1992-04-17 22:31:19 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   G R I D A L
C-----------------------------------------------------------------------
C
      SUBROUTINE GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,IGPH,XINT,YINT)
C
C Declare the common block containing real or integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ICWX,ICWY,IDCX,IDCY,IORX,
     +                  IMJX,IMJY,IMNX,IMNY,NCFX,NCFY
        SAVE   /GAREIN/
C
C Declare the common block containing character parameters.
C
        COMMON /GACHAR/ FNLX,FNLY
        CHARACTER*10    FNLX,FNLY
        SAVE   /GACHAR/
C
C Declare the block data "routine" external.  This should force it to
C be loaded.
C
        EXTERNAL GABLDT
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
C  The following is for gathering statistics on library use at NCAR.
C
        CALL Q8QST4 ('GRAPHX','GRIDAL','GRIDAL','VERSION 01')
C
C Compute constants "epsilon" and "1+epsilon", the latter to be used
C multiplicatively in rounding to get rid of strings of nines in labels.
C
        EPSI=10.**(2-IFIX(ALOG10(FLOAT(I1MACH(10)))*FLOAT(I1MACH(11))))
        OPEP=1.+EPSI
C
C Pick up the current definition of the window and the viewport and
C the current x/y linear/log flag.
C
        CALL GETSET (VPLX,VPRX,VPBY,VPTY,WDLX,WDRX,WDBY,WDTY,LILO)
C
C Set minimum and maximum values of X and Y.
C
        XMIN=AMIN1(WDLX,WDRX)
        XMAX=AMAX1(WDLX,WDRX)
C
        YMIN=AMIN1(WDBY,WDTY)
        YMAX=AMAX1(WDBY,WDTY)
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
        WPLO=2.**IP2X-1.
        HPLO=2.**IP2Y-1.
C
C Compute the number of major and minor divisions of each axis, imposing
C limits on the input values in order to keep the code from blowing up.
C
        IF (ILGX.EQ.0) THEN
          NMJX=MAX0(1,MIN0(10000,MJRX))
          NMNX=MAX0(1,MIN0(10000,MNRX))
        ELSE
          IPTX=MAX0(1,MIN0(100,MJRX))
          FPTX=FLOAT(IPTX)
          NMJX=IFIX(OPEP*ABS(ALOG10(WDRX/WDLX))/FPTX)
          IF (MNRX.LE.10) THEN
            NMNX=9
          ELSE
            NMNX=1
          END IF
        END IF
C
        IF (ILGY.EQ.0) THEN
          NMJY=MAX0(1,MIN0(10000,MJRY))
          NMNY=MAX0(1,MIN0(10000,MNRY))
        ELSE
          IPTY=MAX0(1,MIN0(100,MJRY))
          FPTY=FLOAT(IPTY)
          NMJY=IFIX(OPEP*ABS(ALOG10(WDTY/WDBY))/FPTY)
          IF (MNRY.LE.10) THEN
            NMNY=9
          ELSE
            NMNY=1
          END IF
        END IF
C
C Save the current state of the clipping indicator and then turn it off.
C
        CALL GQCLIP (IERR,ICLP,CLPR)
        CALL GSCLIP (0)
C
C The following loop runs through the types of items to be drawn.  ITEM
C = 1 implies minor ticks, 2 implies major ticks, 3 implies the axes,
C and 4 implies the labels.
C
        DO 103 ITEM=1,4
C
C Set the color index for the type of item being drawn.
C
          IF (ITEM.EQ.1) THEN
            IF (ICMN.GE.0) THEN
              CALL PLOTIF (0.,0.,2)
              CALL GQPLCI (IGER,ICSV)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQPLCI',1,2)
                STOP
              END IF
              CALL GSPLCI (ICMN)
            END IF
          ELSE IF (ITEM.EQ.2) THEN
            IF (ICMJ.GE.0) THEN
              CALL PLOTIF (0.,0.,2)
              CALL GQPLCI (IGER,ICSV)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQPLCI',2,2)
                STOP
              END IF
              CALL GSPLCI (ICMJ)
            END IF
          ELSE IF (ITEM.EQ.3) THEN
            IF (ICAX.GE.0) THEN
              CALL PLOTIF (0.,0.,2)
              CALL GQPLCI (IGER,ICSV)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQPLCI',3,2)
                STOP
              END IF
              CALL GSPLCI (ICAX)
            END IF
          ELSE IF (ITEM.EQ.4) THEN
            IF (ICLB.GE.0) THEN
              CALL GQTXCI (IGER,ICSV)
              IF (IGER.NE.0) THEN
                CALL SETER ('GRIDAL - ERROR EXIT FROM GQTXCI',4,2)
                STOP
              END IF
              CALL GSTXCI (ICLB)
            END IF
          END IF
C
C The next loop runs through the four axes.  IAXS = 1 implies the left
C axis, 2 the bottom axis, 3 the right axis, and 4 the top axis.
C
          DO 102 IAXS=1,4
C
C On the first pass through the loop, set up the required parameters
C to do the left axis.
C
            IF (IAXS.EQ.1) THEN
C
C If the left axis isn't being done at all, or if the type of item
C being drawn now isn't present on the left axis, skip it.
C
              IF (IYLB.LT.0) GO TO 102
              IF (ITEM.EQ.1.AND.NMNY.LE.1) GO TO 102
              IF (ITEM.EQ.4.AND.IYLB.LE.0) GO TO 102
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
                QMJX=CUFX(AMAX1(XMIN,AMIN1(XMAX,XINT)))
              END IF
              DMJX=0.
              QMJY=VPTY
              DMJY=VPBY-VPTY
              IF (ITEM.NE.3) THEN
                IF (ILGF.EQ.0) THEN
                  DMJY=DMJY/FLOAT(NMJD)
                ELSE
                  FPTN=FPTY
                  DMJY=DMJY*FPTN/ABS(ALOG10(WDTY/WDBY))
                  IF (IMIF.NE.0) QMJY=VPBY-FLOAT(NMJD)*DMJY
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
              IF ((MOD(IGPH,4)-1.LT.0.OR.IMJY.GT.0).AND.IXLB.GE.0) THEN
                IF (IGPH/4-1.LE.0) THEN
                  AMJY=VPTY
                  BMJY=VPBY
                ELSE
                  AMJY=CUFY(AMAX1(YMIN,AMIN1(YMAX,YINT)))
                END IF
              END IF
C
C Compute tick-mark offset parameters.
C
              IF (MOD(IGPH,4)-1.LT.0) THEN
                TMJX=VPRX-VPLX
                TMNX=VPRX-VPLX
              ELSE
                TMJX=FLOAT(IMJY)/WPLO
                TMNX=FLOAT(IMNY)/WPLO
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
                VLBL=WDTY
                IF (ILGF.EQ.0) THEN
                  DLBL=(WDBY-WDTY)/FLOAT(NMJD)
                  VEPS=EPSI*ABS(WDTY-WDBY)
                ELSE
                  DLBL=10.**IPTY
                  IF (IMIF.NE.0) DLBL=1./DLBL
                  VEPS=0.
                END IF
C
                IF (IDCX.EQ.0) THEN
                  DLBX=-20./WPLO
                ELSE IF (IDCX.EQ.1) THEN
                  DLBX=+20./WPLO
                  IF (MOD(IGPH,4)-1.LE.0) DLBX=DLBX+VPRX-VPLX
                ELSE
                  DLBX=FLOAT(-IDCX)/WPLO
                END IF
                DLBY=0.
C
                FNLB=FNLY
C
                ICHW=ICWX
                IORI=0
                ICEN=IFIX(-SIGN(1.,DLBX))
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
              IF (IXLB.LT.0) GO TO 102
              IF (ITEM.EQ.1.AND.NMNX.LE.1) GO TO 102
              IF (ITEM.EQ.4.AND.IXLB.LE.0) GO TO 102
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
                  DMJX=DMJX/FLOAT(NMJD)
                ELSE
                  FPTN=FPTX
                  DMJX=DMJX*FPTN/ABS(ALOG10(WDRX/WDLX))
                  IF (IMIF.NE.0) QMJX=VPRX-FLOAT(NMJD)*DMJX
                END IF
              END IF
              IF (IGPH/4-1.LE.0) THEN
                QMJY=VPBY
              ELSE
                QMJY=CUFY(AMAX1(YMIN,AMIN1(YMAX,YINT)))
              END IF
              DMJY=0.
C
              AMJX=-1.
              AMJY=QMJY
              BMJX=2.
              BMJY=QMJY
              IF ((IGPH/4-1.LT.0.OR.IMJX.GT.0).AND.IYLB.GE.0) THEN
                IF (MOD(IGPH,4)-1.LE.0) THEN
                  AMJX=VPLX
                  BMJX=VPRX
                ELSE
                  AMJX=CUFX(AMAX1(XMIN,AMIN1(XMAX,XINT)))
                END IF
              END IF
C
              TMJX=0.
              TMNX=0.
              IF (IGPH/4-1.LT.0) THEN
                TMJY=VPTY-VPBY
                TMNY=VPTY-VPBY
              ELSE
                TMJY=FLOAT(IMJX)/WPLO
                TMNY=FLOAT(IMNX)/WPLO
              END IF
C
              IF (ITEM.EQ.4) THEN
C
                VLBL=WDLX
                IF (ILGF.EQ.0) THEN
                  DLBL=(WDRX-WDLX)/FLOAT(NMJD)
                  VEPS=EPSI*ABS(WDRX-WDLX)
                ELSE
                  DLBL=10.**IPTX
                  IF (IMIF.NE.0) DLBL=1./DLBL
                  VEPS=0.
                END IF
C
                DLBX=0.
                IF (IDCY.EQ.0) THEN
                  DLBY=-20./HPLO
                ELSE IF (IDCY.EQ.1) THEN
                  DLBY=+20./HPLO
                  IF (IGPH/4-1.LE.0) DLBY=DLBY+VPTY-VPBY
                ELSE
                  DLBY=FLOAT(-IDCY)/HPLO
                END IF
C
                FNLB=FNLX
C
                ICHW=ICWX
                IF (IORX.EQ.0) THEN
                  IORI=0
                  ICEN=0
                  DLBY=DLBY+SIGN(FLOAT(ICHW)/HPLO,DLBY)
                ELSE
                  IORI=90
                  ICEN=IFIX(-SIGN(1.,DLBY))
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
              IF (IYLB.LT.0) GO TO 102
              IF (ITEM.EQ.1.AND.NMNY.LE.1) GO TO 102
              IF (ITEM.EQ.4) GO TO 102
              IF ((ITEM.EQ.1.OR.ITEM.EQ.2).AND.
     +                                     MOD(IGPH,4)-1.NE.0) GO TO 102
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
                QMJX=CUFX(AMAX1(XMIN,AMIN1(XMAX,XINT)))
              END IF
              DMJX=0.
              QMJY=VPBY
              DMJY=VPTY-VPBY
              IF (ITEM.NE.3) THEN
                IF (ILGF.EQ.0) THEN
                  DMJY=DMJY/FLOAT(NMJD)
                ELSE
                  FPTN=FPTY
                  DMJY=DMJY*FPTN/ABS(ALOG10(WDTY/WDBY))
                  IF (IMIF.NE.0) QMJY=VPTY-FLOAT(NMJD)*DMJY
                END IF
              END IF
C
              AMJX=QMJX
              AMJY=-1.
              BMJX=QMJX
              BMJY=2.
              IF (IMJY.GT.0.AND.IXLB.GE.0) THEN
                IF (IGPH/4-1.LE.0) THEN
                  AMJY=VPBY
                  BMJY=VPTY
                ELSE
                  AMJY=CUFY(AMAX1(YMIN,AMIN1(YMAX,YINT)))
                END IF
              END IF
C
              TMJX=-FLOAT(IMJY)/WPLO
              TMNX=-FLOAT(IMNY)/WPLO
              TMJY=0.
              TMNY=0.
C
C On the fourth pass through the loop, set up the required parameters
C to do the top axis.
C
            ELSE IF (IAXS.EQ.4) THEN
C
              IF (IXLB.LT.0) GO TO 102
              IF (ITEM.EQ.1.AND.NMNX.LE.1) GO TO 102
              IF ((ITEM.EQ.1.OR.ITEM.EQ.2).AND.IGPH/4-1.NE.0) GO TO 102
              IF (ITEM.EQ.4) GO TO 102
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
                  DMJX=DMJX/FLOAT(NMJD)
                ELSE
                  FPTN=FPTX
                  DMJX=DMJX*FPTN/ABS(ALOG10(WDRX/WDLX))
                  IF (IMIF.NE.0) QMJX=VPLX-FLOAT(NMJD)*DMJX
                END IF
              END IF
              IF (IGPH/4-1.LE.0) THEN
                QMJY=VPTY
              ELSE
                QMJY=CUFY(AMAX1(YMIN,AMIN1(YMAX,YINT)))
              END IF
              DMJY=0.
C
              AMJX=2.
              AMJY=QMJY
              BMJX=-1.
              BMJY=QMJY
              IF (IMJX.GT.0.AND.IYLB.GE.0) THEN
                IF (MOD(IGPH,4)-1.LE.0) THEN
                  AMJX=VPRX
                  BMJX=VPLX
                ELSE
                  AMJX=CUFX(AMAX1(XMIN,AMIN1(XMAX,XINT)))
                END IF
              END IF
C
              TMJX=0.
              TMNX=0.
              TMJY=-FLOAT(IMJX)/WPLO
              TMNY=-FLOAT(IMNX)/WPLO
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
              DO 101 IMRK=1,NMJD*NMND+1
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
                        CALL PLOTIF (PMJX+TMJX,PMJY+TMJY,1)
                      ELSE
                        CALL PLOTIF (PMJX+TMJX,PMJY+TMJY,0)
                        CALL PLOTIF (PMJX,     PMJY     ,1)
                      END IF
                    END IF
                  END IF
                  IF (ITEM.EQ.4) THEN
                    IF (FNLB(2:2).NE.'I'.AND.FNLB(2:2).NE.'i') THEN
                      VNCD=VLBL*OPEP
                      IF (ABS(VLBL).LT.VEPS) VNCD=0.
                      WRITE (LABL,FNLB) VNCD
                    ELSE
                      ILBL=NINT(VLBL)
                      WRITE (LABL,FNLB) ILBL
                    END IF
                    IF (NCFR.EQ.0) CALL GALBEX (LABL,MLBL,NLBL)
                    CALL WTSTR (CFUX(PMJX+DLBX),CFUY(PMJY+DLBY),
     +                          LABL(MLBL:NLBL),ICHW,IORI,ICEN)
                    IF (ILGF.EQ.0) THEN
                      VLBL=VLBL+DLBL
                    ELSE
                      VLBL=VLBL*DLBL
                    END IF
                  END IF
                ELSE
                  IF (ILGF.EQ.0) THEN
                    PMNX=PMJX+(QMJX-PMJX)*FLOAT(IMND)/FLOAT(NMND)
                    PMNY=PMJY+(QMJY-PMJY)*FLOAT(IMND)/FLOAT(NMND)
                  ELSE
                    IF (IMIF.EQ.0) THEN
                      PMNX=PMJX+(QMJX-PMJX)*ALOG10(FLOAT(IMND+1))/FPTN
                      PMNY=PMJY+(QMJY-PMJY)*ALOG10(FLOAT(IMND+1))/FPTN
                    ELSE
                      PMNX=QMJX+(PMJX-QMJX)*ALOG10(FLOAT(NMND-IMND+1))
     +                                                             /FPTN
                      PMNY=QMJY+(PMJY-QMJY)*ALOG10(FLOAT(NMND-IMND+1))
     +                                                             /FPTN
                    END IF
                  END IF
                  IFLP=1-IFLP
                  IF (IFLP.EQ.0) THEN
                    CALL PLOTIF (PMNX,     PMNY     ,0)
                    CALL PLOTIF (PMNX+TMNX,PMNY+TMNY,1)
                  ELSE
                    CALL PLOTIF (PMNX+TMNX,PMNY+TMNY,0)
                    CALL PLOTIF (PMNX,     PMNY     ,1)
                  END IF
                END IF
                IMND=MOD(IMND+1,NMND)
  101         CONTINUE
            ELSE
C
C Draw the axis.
C
              CALL PLOTIF (QMJX,QMJY,0)
              CALL PLOTIF (QMJX+DMJX,QMJY+DMJY,1)
C
            END IF
C
  102     CONTINUE
C
C Reset the color index.
C
          IF ((ITEM.EQ.1.AND.ICMN.GE.0).OR.
     +        (ITEM.EQ.2.AND.ICMJ.GE.0).OR.
     +        (ITEM.EQ.3.AND.ICAX.GE.0)) THEN
            CALL PLOTIF (0.,0.,2)
            CALL GSPLCI (ICSV)
          ELSE IF (ITEM.EQ.4.AND.ICLB.GE.0) THEN
            CALL GSTXCI (ICSV)
          END IF
C
  103   CONTINUE
C
C Flush the SPPS pen-move buffer.
C
        CALL PLOTIF (0.,0.,2)
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
