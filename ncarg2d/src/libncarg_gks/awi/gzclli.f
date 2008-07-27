C
C $Id: gzclli.f,v 1.5 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZCLLI (XMIN,XMAX,YMIN,YMAX,XCPL,YCPL,NCPL,
     +                                  RWRK,LRWK,IERR)
C
        DIMENSION XCPL(NCPL),YCPL(NCPL),RWRK(LRWK)
C
C  This is a polyline clipping routine.  XMIN, XMAX, YMIN, and YMAX
C  define a clipping rectangle.  The points (XCPL(I),YCPL(I)), for I
C  from 1 to NCPL, define the polyline to be clipped.  The array RWRK,
C  which is of length LRWK, is a real workspace array to be used for
C  the fragments of the polyline that result from the clipping process.
C  The value of LRWK must be at least 4; using a small value will have
C  the effect of chopping up the polyline into pieces of length LRWK/2.
C  IERR is an error flag: its value on return will be non-zero if and
C  only if an error occurred; currently, the only errors detected are
C  when NCPL is less than or equal to zero (IERR = 1) and when LRWK is
C  less than than 4 (IERR = 2).
C
C  This code is adapted from a code written by David Kennison at NCAR
C  in the summer of 1994.
C
        IERR=0
C
C Check for error in the arguments.
C
        IF (NCPL.LE.0) THEN
          IERR=1
          RETURN
        ELSE IF (LRWK.LT.4) THEN
          IERR=2
          RETURN
        END IF
C
C Check to see if the input polygon is entirely within the clipping
C rectangle and treat that case specially.
C
      XCSPMN = XCPL(1)
      XCSPMX = XCPL(1)
      YCSPMN = YCPL(1)
      YCSPMX = YCPL(1)
      DO 100 I=1,NCPL
        XCSPMN = MIN(XCSPMN,XCPL(I))
        XCSPMX = MAX(XCSPMX,XCPL(I))
        YCSPMN = MIN(YCSPMN,YCPL(I))
        YCSPMX = MAX(YCSPMX,YCPL(I))
  100 CONTINUE
      IF (XCSPMN.GE.XMIN .AND. XCSPMX.LE.XMAX .AND.
     +    YCSPMN.GE.YMIN .AND. YCSPMX.LE.YMAX) THEN
        CALL GZPUTR(NCPL,NCPL,XCPL,YCPL,1,IERR)
        RETURN
      ENDIF
C
C Define a pointer to the end of the first half of the workspace.
C
        IPHW=LRWK/2
C
C Zero the count of the number of points in the current fragment.
C
        NPIF=0
C
C If there is only one point in the polyline, that's a special case ...
C
        IF (NCPL.EQ.1) THEN
C
          NPOW=INT(3.*(SIGN(.51,XCPL(1)-XMIN)+SIGN(.51,XCPL(1)-XMAX))+
     +                 (SIGN(.51,YCPL(1)-YMIN)+SIGN(.51,YCPL(1)-YMAX)))
          IF (NPOW.EQ.0) THEN
            NPIF=1
            RWRK(     1)=XCPL(1)
            RWRK(IPHW+1)=YCPL(1)
          END IF
C
C Otherwise ...
C
        ELSE
C
C Loop through the given points.
C
          DO 116 ICPL=1,NCPL
C
C Extract the coordinates of the next point.
C
            XNXT=XCPL(ICPL)
            YNXT=YCPL(ICPL)
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
            NPOW=INT(3.*(SIGN(.51,XNXT-XMIN)+SIGN(.51,XNXT-XMAX))+
     +                   (SIGN(.51,YNXT-YMIN)+SIGN(.51,YNXT-YMAX)))
C
C If the next point is not the first point of a line, there is work to
C be done.
C
            IF (ICPL.NE.1) THEN
C
C There are various possible cases, depending on whether the last point
C was inside or outside the window and whether the next point is inside
C or outside the window.
C
              IF (LPOW.EQ.0) THEN
                IF (NPOW.NE.0) GO TO 101
                IF (NPIF.EQ.0) THEN
                  NPIF=1
                  RWRK(     1)=XLST
                  RWRK(IPHW+1)=YLST
                END IF
                NPIF=NPIF+1
                RWRK(     NPIF)=XNXT
                RWRK(IPHW+NPIF)=YNXT
                IF (NPIF.EQ.IPHW) THEN
                  CALL GZPUTR(NPIF,NPIF,RWRK,RWRK(IPHW+1),1,IERR)
                  NPIF=0
                END IF
                GO TO 115
              ELSE
                IF (NPOW.EQ.0) GO TO 103
                GO TO 105
              END IF
C
C Last point inside, next point outside.
C
  101         XPEW=XLST
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
  102         IF (NPIF.EQ.0) THEN
                NPIF=1
                RWRK(     1)=XLST
                RWRK(IPHW+1)=YLST
              END IF
              NPIF=NPIF+1
              RWRK(NPIF)=XPEW
              RWRK(IPHW+NPIF)=YPEW
              CALL GZPUTR(NPIF,NPIF,RWRK,RWRK(IPHW+1),1,IERR)
              NPIF=0
C
              GO TO 115
C
C Last point outside, next point inside.
C
  103         XPEW=XNXT
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

  104         NPIF=2
              RWRK(     1)=XPEW
              RWRK(IPHW+1)=YPEW
              RWRK(     2)=XNXT
              RWRK(IPHW+2)=YNXT
              IF (NPIF.EQ.IPHW) THEN
                CALL GZPUTR(NPIF,NPIF,RWRK,RWRK(IPHW+1),1,IERR)
                NPIF=0
              END IF
C
              GO TO 115
C
C Last point outside, next point outside.  Check whether or not part of
C the line joining them lies in the window.
C
  105         MPOW=9*LPOW+NPOW+41
C
              GO TO ( 115,115,115,115,115,106,115,106,106,
     +                115,115,115,107,115,106,107,106,106,
     +                115,115,115,107,115,115,107,107,115,
     +                115,109,109,115,115,106,115,106,106,
     +                115,115,115,115,115,115,115,115,115,
     +                108,108,115,108,115,115,107,107,115,
     +                115,109,109,115,115,109,115,115,115,
     +                108,108,109,108,115,109,115,115,115,
     +                108,108,115,108,115,115,115,115,115 ) , MPOW
C
  106         XPE1=XMIN
              YPT1=YMIN
              XPE2=XMAX
              YPT2=YMAX
              GO TO 110
C
  107         XPE1=XMIN
              YPT1=YMAX
              XPE2=XMAX
              YPT2=YMIN
              GO TO 110
C
  108         XPE1=XMAX
              YPT1=YMAX
              XPE2=XMIN
              YPT2=YMIN
              GO TO 110
C
  109         XPE1=XMAX
              YPT1=YMIN
              XPE2=XMIN
              YPT2=YMAX
C
  110         XDIF=XNXT-XLST
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
  111         IF (YPE2.GE.YMIN.AND.YPE2.LE.YMAX) GO TO 114
              GO TO 113
C
  112         YPE1=YPT1
              XPE1=XLST+(YPE1-YLST)*XDIF/YDIF
              IF (XPE1.LT.XMIN.OR.XPE1.GT.XMAX) GO TO 115
C
  113         YPE2=YPT2
              XPE2=XLST+(YPE2-YLST)*XDIF/YDIF
              IF (XPE2.LT.XMIN.OR.XPE2.GT.XMAX) GO TO 115

  114         RWRK(     1)=XPE1
              RWRK(IPHW+1)=YPE1
              RWRK(     2)=XPE2
              RWRK(IPHW+2)=YPE2
              CALL GZPUTR(2,2,RWRK,RWRK(IPHW+1),1,IERR)
              NPIF=0
C
            END IF
C
C The next point now becomes the last point and we continue the loop
C to get a new next point.
C
  115       LPOW=NPOW
            XLST=XNXT
            YLST=YNXT
C
  116     CONTINUE
C
        END IF
C
C Dump the remaining fragment, if any.
C
        IF (NPIF.NE.0) THEN
          CALL GZPUTR(NPIF,NPIF,RWRK,RWRK(IPHW+1),1,IERR)
        ENDIF
C
        RETURN
      END
