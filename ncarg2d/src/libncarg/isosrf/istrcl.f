C
C $Id: istrcl.f,v 1.5 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ISTRCL (ZDAT,IZD1,IZDM,IZDN,CLVL)
C
      DIMENSION ZDAT(IZD1,*)
C
C Given the data slice in ((ZDAT(I,J),I=1,IZDM),J=1,IZDN), the routine
C ISTRCL finds the beginning of each contour line at the level CLVL and
C traces it.  The generated contour lines are mapped to the projection
C plane, smoothed if the tension parameter indicates that they should
C be, and then passed on, one point at a time, to the routine ISPLTF.
C
C This routine is a modified version of CPTRCL, from CONPACK.
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C XSVE and YSVE are arrays in which to save for smoothing the X and Y
C coordinates of points along contour lines.  They must be dimensioned
C the same as the arrays needed by the smoother, in ISSMTH.
C
      DIMENSION XSVE(100),YSVE(100)
C
C INCX and INCY define the x and y components of the eight possible
C directions the contour-line-following vector can assume.
C
      DIMENSION INCX(8),INCY(8)
C
      DATA INCX / -1 , -1 ,  0 ,  1 ,  1 ,  1 ,  0 , -1 /
      DATA INCY /  0 ,  1 ,  1 ,  1 ,  0 , -1 , -1 , -1 /
C
C Define an interpolation function.
C
      FRCT(ZDT1,ZDT2)=(CLVL-ZDT1)/(ZDT2-ZDT1)
C
C Set the flag which indicates fill is unnecessary; it gets turned back
C on by ISPLTF whenever bits get set in the screen model (which makes
C fill necessary).
C
        IFILL=0
C
C Zero the count of horizontal segments seen so far.
C
        NHSS=0
C
C Search the interior of the grid for contour lines to trace.  (Because
C of the way ISOSRF works, there are no contour lines intersecting the
C edge of the grid, so we only look for ones in the interior.)
C
        DO 105 INDY=2,IZDN-1
C
          DO 104 INDX=2,IZDM
C
C Look for horizontal segments with a value less than CLVL at the left
C end and a value greater than or equal to CLVL at the right end.
C
            IF (ZDAT(INDX-1,INDY).LT.CLVL.AND.
     +          ZDAT(INDX  ,INDY).GE.CLVL) THEN
C
C Pack the indices of the segment's right end into a single integer.
C
              IPXY=IZDN*INDX+INDY
C
C Skip if the contour line through this segment has been traced before.
C
              DO 101 I=1,NHSS
                IF (IPXY.EQ.IHSS(I)) GO TO 104
  101         CONTINUE
C
C It hasn't.  Add the packed indices to the list.  (Or, if the list is
C full, quit.)
C
              IF (NHSS.GE.LHSS) RETURN
              NHSS=NHSS+1
              IHSS(NHSS)=IPXY
C
C Initialize the line-following vector to have its basepoint at
C (INDX,INDY) and point to the left.
C
              IVBX=INDX
              IVBY=INDY
              INCI=1
C
C Line-following algorithm.  Loop, moving the line-following vector
C (defined by the base point (IVBX,IVBY) and the components INCX(INCI)
C and INCY(INCI)) along a contour line.  The points defining the contour
C line are thereby determined.  The process stops when the starting
C position is encountered.  Save the parameters defining that position.
C
              MVBX=IVBX
              MVBY=IVBY
              MNCI=INCI
C
C Set parameters defining the position of the end of the vector.
C
              IVEX=IVBX+INCX(INCI)
              IVEY=IVBY+INCY(INCI)
C
C Compute the coordinates, in the data-index coordinate system, of the
C starting position of the contour line.
C
              IF (IVEX.NE.IVBX) THEN
                XCND=REAL(IVBX)+REAL(IVEX-IVBX)*FRCT(ZDAT(IVBX,IVBY),
     +                                               ZDAT(IVEX,IVEY))
                YCND=REAL(IVBY)
              ELSE
                XCND=REAL(IVBX)
                YCND=REAL(IVBY)+REAL(IVEY-IVBY)*FRCT(ZDAT(IVBX,IVBY),
     +                                               ZDAT(IVEX,IVEY))
              END IF
C
C Map the data-index coordinates XCND and YCND into user coordinates and
C send them to the routine ISPLTF.  If the smoother is turned on, save
C the coordinates for the smoother.
C
              CALL ISGFXY (XCND,YCND,XCNU,YCNU)
              CALL ISPLTF (XCNU,YCNU,1)
C
              IF (TENSN.NE.0.) THEN
                NSVE=1
                XSVE(1)=XCNU
                YSVE(1)=YCNU
              END IF
C
C Initialize the flag that indicates we're working on the first segment.
C
              IFSF=1
C
C Loop, moving the line-following vector as dictated by the positions
C of its end points.  We know that the base of the line-following vector
C is on the high side of the contour and that the end of it is on the
C other side.  Move the vector clockwise and see what happens.
C
  102         INCI=INCI+1
              IF (INCI.GT.8) INCI=INCI-8
              IVEX=IVBX+INCX(INCI)
              IVEY=IVBY+INCY(INCI)
C
C Exit the loop if we've hit the edge.  (This should not be possible,
C but what the hey...)
C
              IF (IVEX.LT.1.OR.IVEX.GT.IZDM.OR.
     +            IVEY.LT.1.OR.IVEY.GT.IZDN) GO TO 103
C
C If the end of the line-following vector is now on the same side of
C the contour line as its base ...
C
              IF (ZDAT(IVEX,IVEY).GE.CLVL) THEN
C
C flip it end-for-end and continue the loop.
C
                IVBX=IVEX
                IVBY=IVEY
                INCI=INCI+4
C
C Otherwise, if the line-following vector is currently horizontal or
C vertical, we have another point to add to the contour line ...
C
              ELSE IF ((INCI/2)*2.NE.INCI) THEN
C
C so compute the coordinates of the new point and either pass them on
C to ISPLTF or save them for the smoother.
C
                IF (IVEX.NE.IVBX) THEN
                  XCND=REAL(IVBX)+REAL(IVEX-IVBX)*FRCT(ZDAT(IVBX,IVBY),
     +                                                 ZDAT(IVEX,IVEY))
                  YCND=REAL(IVBY)
                ELSE
                  XCND=REAL(IVBX)
                  YCND=REAL(IVBY)+REAL(IVEY-IVBY)*FRCT(ZDAT(IVBX,IVBY),
     +                                                 ZDAT(IVEX,IVEY))
                END IF
C
                  CALL ISGFXY (XCND,YCND,XCNU,YCNU)
C
                  IF (TENSN.EQ.0.) THEN
                    CALL ISPLTF (XCNU,YCNU,2)
                  ELSE
                    IF (ABS(XCNU-XSVE(NSVE)).GT.SMALL.OR.
     +                  ABS(YCNU-YSVE(NSVE)).GT.SMALL) THEN
                      NSVE=NSVE+1
                      XSVE(NSVE)=XCNU
                      YSVE(NSVE)=YCNU
                      IF (NSVE.EQ.100) THEN
                        CALL ISSMTH (XSVE,YSVE,NSVE,IFSF)
                        XSVE(1)=XSVE(NSVE)
                        YSVE(1)=YSVE(NSVE)
                        NSVE=1
                        IFSF=0
                        CALL ISPLTF (XCNU,YCNU,2)
                      END IF
                    END IF
                  END IF
C
C If we just crossed a horizontal grid line in the upwards direction,
C save that information.
C
                IF (INCI.EQ.1) THEN
                  IF (NHSS.GE.LHSS) RETURN
                  NHSS=NHSS+1
                  IHSS(NHSS)=IZDN*IVBX+IVBY
                END IF
C
C If we just arrived at our starting point, quit the loop.
C
                IF (IVBX.EQ.MVBX.AND.IVBY.EQ.MVBY.AND.INCI.EQ.MNCI)
     +                                                         GO TO 103
C
              END IF
C
C Loop back.
C
              GO TO 102
C
C Exit from loop.  Process any remaining portion of the contour line
C which was saved for smoothing.
C
  103         IF (TENSN.NE.0..AND.NSVE.GT.1) THEN
                CALL ISSMTH (XSVE,YSVE,NSVE,IFSF)
                CALL ISPLTF (XCNU,YCNU,2)
              END IF
C
            END IF
C
C End of loops on IVBX and IVBY.
C
  104     CONTINUE
C
  105   CONTINUE
C
C Done.
C
      RETURN
C
      END
