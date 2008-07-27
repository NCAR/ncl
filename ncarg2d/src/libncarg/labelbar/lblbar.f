C
C $Id: lblbar.f,v 1.12 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LBLBAR (IHOV,XLEB,XREB,YBEB,YTEB,NBOX,WSFB,HSFB,LFIN,
     +                   IFTP,LLBS,NLBS,LBAB)
C
        DIMENSION LFIN(*)
        CHARACTER*(*) LLBS(*)
C
C This routine draws a horizontal or vertical label bar to serve as a
C key for a solid-filled plot.
C
C IHOV is 0 if a horizontal label bar is to be drawn, 1 if a vertical
C label bar is to be drawn.
C
C XLEB is a value between 0 and 1, specifying the position of the left
C edge of the bar.
C
C XREB is a value between 0 and 1, specifying the position of the right
C edge of the bar.
C
C YBEB is a value between 0 and 1, specifying the position of the bottom
C edge of the bar.
C
C YTEB is a value between 0 and 1, specifying the position of the top
C edge of the bar.
C
C ABS(NBOX) is the number of boxes into which the bar is to be divided.
C If NBOX is positive, the boxes will be outlined after being filled;
C if NBOX is negative, this will not be done.
C
C WSFB and HSFB are the width and height, respectively, of each little
C solid-filled box, as fractions of the rectangles resulting from the
C division of the bar into ABS(NBOX) pieces.
C
C LFIN is a list of indices, each of which specifies, in some manner,
C how one of the solid-filled boxes is to be filled.  (For example,
C each may be a color index.)
C
C IFTP specifies the type of solid fill to be used.  If IFTP is zero,
C the routine SFSGFA, in the package SOFTFILL, will be called, with
C an index from LFIN as the value of the argument ICI.  (By default,
C this will result in color fill; the value of the SOFTFILL internal
C parameter 'TY' may be changed to select some other kind of fill by
C SFSGFA.)  If IFTP is non-zero, the user-replaceable routine LBFILL
C will be used to fill the boxes; the default version of this routine
C just does color fill.
C
C LLBS is a list of labels for the solid-filled boxes.
C
C NLBS is the number of labels in the list LLBS.  If NLBS is equal to
C ABS(NBOX)-1, then label I applies to the line separating box I from
C box I+1.  If NLBS is equal to NBOX, then label I applies to box I.  If
C NLBS is equal to ABS(NBOX)+1, then labels 1 and NLBS apply to the left
C and right ends (if IHOV is non-zero, the bottom and top ends) of the
C whole color bar; for values of I not equal to 1 or NLBS, label I
C applies to the line separating box I-1 from box I.
C
C LBAB is a flag having the value 0 if the bar is to be unlabelled, 1
C if the labels are to be below a horizontal bar or to the right of a
C vertical bar, 2 if the labels are to be above a horizontal bar or to
C the left of a vertical bar, 3 if the labels are to be on both sides
C of the bar.
C
C
C Declare the common block where internal parameters are stored.
C
        COMMON /LBCOMN/ ICBL,ICFL,ICLB,WOBL,WOFL,WOLB
        SAVE   /LBCOMN/
C
C Define local arrays to hold X and Y coordinates of boxes.
C
        DIMENSION XCRA(5),YCRA(5)
C
C Define local arrays for use as work arrays by the routine SFSGFA.
C
        DIMENSION RWRK(6),IWRK(8)
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL LBBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('LBLBAR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Save the current SET parameters and arrange for the use of normalized
C device coordinates.
C
        CALL GETSET (XLVP,XRVP,YBVP,YTVP,XLWD,XRWD,YBWD,YTWD,LNLG)
        IF (ICFELL('LBLBAR',2).NE.0) RETURN
        CALL    SET (  0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.,   1)
        IF (ICFELL('LBLBAR',3).NE.0) RETURN
C
C Compute the width and height of each section of the bar and the
C coordinates of the edges of the first solid-filled box.
C
        IF (IHOV.EQ.0) THEN
          WSOB=(XREB-XLEB)/REAL(ABS(NBOX))
          WINC=WSOB
          HSOB=YTEB-YBEB
          HINC=0.
          XLB1=XLEB+.5*(1.-WSFB)*WSOB
          XRB1=XLB1+WSFB*WSOB
          IF (LBAB.EQ.1) THEN
            YBB1=YTEB-HSFB*HSOB
            YTB1=YTEB
          ELSE IF (LBAB.EQ.2) THEN
            YBB1=YBEB
            YTB1=YBEB+HSFB*HSOB
          ELSE
            YBB1=YBEB+.5*(1.-HSFB)*HSOB
            YTB1=YTEB-.5*(1.-HSFB)*HSOB
          END IF
        ELSE
          WSOB=XREB-XLEB
          WINC=0.
          HSOB=(YTEB-YBEB)/REAL(ABS(NBOX))
          HINC=HSOB
          IF (LBAB.EQ.1) THEN
            XLB1=XLEB
            XRB1=XLEB+WSFB*WSOB
          ELSE IF (LBAB.EQ.2) THEN
            XLB1=XREB-WSFB*WSOB
            XRB1=XREB
          ELSE
            XLB1=XLEB+.5*(1.-WSFB)*WSOB
            XRB1=XREB-.5*(1.-WSFB)*WSOB
          END IF
          YBB1=YBEB+.5*(1.-HSFB)*HSOB
          YTB1=YBB1+HSFB*HSOB
        END IF
C
C Draw the bar by filling all of the individual boxes.
C
        CALL GQFACI (IERR,ISFC)
        IF (IERR.NE.0) THEN
          CALL SETER ('LBLBAR - ERROR EXIT FROM GQFACI',4,1)
          RETURN
        END IF
C
        IF (ICFL.GE.0) THEN
          CALL GQPLCI (IERR,ISPC)
          IF (IERR.NE.0) THEN
            CALL SETER ('LBLBAR - ERROR EXIT FROM GQPLCI',5,1)
            RETURN
          END IF
          CALL GSPLCI (ICFL)
        END IF
C
        IF (WOFL.GT.0.) THEN
          CALL GQLWSC (IERR,STLW)
          IF (IERR.NE.0) THEN
            CALL SETER ('LBLBAR - ERROR EXIT FROM GQLWSC',6,1)
            RETURN
          END IF
          CALL GSLWSC (WOFL)
        END IF
C
        DO 101 I=1,ABS(NBOX)
          XCRA(1)=XLB1+REAL(I-1)*WINC
          YCRA(1)=YBB1+REAL(I-1)*HINC
          XCRA(2)=XRB1+REAL(I-1)*WINC
          YCRA(2)=YCRA(1)
          XCRA(3)=XCRA(2)
          YCRA(3)=YTB1+REAL(I-1)*HINC
          XCRA(4)=XCRA(1)
          YCRA(4)=YCRA(3)
          XCRA(5)=XCRA(1)
          YCRA(5)=YCRA(1)
          IF (IFTP.EQ.0) THEN
            CALL SFSGFA (XCRA,YCRA,4,RWRK,6,IWRK,8,LFIN(I))
            IF (ICFELL('LBLBAR',7).NE.0) RETURN
          ELSE
            CALL LBFILL (IFTP,XCRA,YCRA,5,LFIN(I))
            IF (ICFELL('LBLBAR',8).NE.0) RETURN
          END IF
  101   CONTINUE
C
        CALL GSFACI (ISFC)
        IF (ICFL.GE.0) CALL GSPLCI (ISPC)
        IF (WOFL.GT.0.) CALL GSLWSC (STLW)
C
C If it is to be done, outline the boxes now.
C
        IF (NBOX.GT.0) THEN
C
          IF (ICBL.GE.0) THEN
            CALL GQPLCI (IERR,ISPC)
            IF (IERR.NE.0) THEN
              CALL SETER ('LBLBAR - ERROR EXIT FROM GQPLCI',9,1)
              RETURN
            END IF
            CALL GSPLCI (ICBL)
          END IF
C
          IF (WOBL.GT.0.) THEN
            CALL GQLWSC (IERR,STLW)
            IF (IERR.NE.0) THEN
              CALL SETER ('LBLBAR - ERROR EXIT FROM GQLWSC',10,1)
              RETURN
            END IF
            CALL GSLWSC (WOBL)
          END IF
C
          DO 102 I=1,ABS(NBOX)
            XCRA(1)=XLB1+REAL(I-1)*WINC
            YCRA(1)=YBB1+REAL(I-1)*HINC
            XCRA(2)=XRB1+REAL(I-1)*WINC
            YCRA(2)=YCRA(1)
            XCRA(3)=XCRA(2)
            YCRA(3)=YTB1+REAL(I-1)*HINC
            XCRA(4)=XCRA(1)
            YCRA(4)=YCRA(3)
            XCRA(5)=XCRA(1)
            YCRA(5)=YCRA(1)
            IF (IHOV.EQ.0) THEN
              IF (I.EQ.1.OR.WSFB.NE.1.) THEN
                CALL GPL (5,XCRA,YCRA)
              ELSE
                CALL GPL (4,XCRA,YCRA)
              END IF
            ELSE
              IF (I.EQ.1.OR.HSFB.NE.1.) THEN
                CALL GPL (5,XCRA,YCRA)
              ELSE
                CALL GPL (4,XCRA(2),YCRA(2))
              END IF
            END IF
  102     CONTINUE
C
          IF (ICBL.GE.0) CALL GSPLCI (ISPC)
          IF (WOBL.GT.0.) CALL GSLWSC (STLW)

        END IF
C
C If labelling is to be done at all ...
C
        IF (LBAB.NE.0) THEN
C
C ... save the current setting of the PLOTCHAR "text extent" parameter
C and reset it to force computation of "text extent" quantities.
C
          CALL PCGETI ('TE - TEXT EXTENT FLAG',ITEX)
          IF (ICFELL('LBLBAR',11).NE.0) RETURN
          CALL PCSETI ('TE - TEXT EXTENT FLAG',1)
          IF (ICFELL('LBLBAR',12).NE.0) RETURN
C
C Determine the character width to be used in drawing the labels.  We
C start with a character width of .1, which is much too big, and then
C reduce the value as required to avoid various overlap problems.
C
C RMUL is a real multiplier used to compensate for the fact that labels
C are sometimes put on only one side of the bar and sometimes on both
C sides of the bar.
C
C HMAX ends up expressing the largest vertical dimension of any label
C (including some white space).
C
C XPLL and YPLL are used for the values of X and Y at the center of the
C last label along the label bar, while DRLL and DTLL are distances
C from the center to the right edge or the top edge, respectively, of
C the last label.  (For a horizontal label bar, we use only XPLL and
C DRLL; for a vertical one, we use only YPLL and DTLL.)
C
          WCHR=.1
C
          RMUL=1.+REAL(MAX(0,MIN(3,LBAB))/3)
C
          HMAX=0.
C
          XPLL=0.
          YPLL=0.
          DRLL=0.
          DTLL=0.
C
          IF (NLBS.LT.ABS(NBOX)) THEN
            XPB1=XLB1+WSOB
            YPB1=YBB1+HSOB
          ELSE IF (NLBS.EQ.ABS(NBOX)) THEN
            XPB1=XLB1+WSFB*WSOB/2.
            YPB1=YBB1+HSFB*HSOB/2.
          ELSE
            XPB1=XLB1
            YPB1=YBB1
          END IF
C
          DO 104 I=1,NLBS
            NCLB=LEN(LLBS(I))
  103       IF (LLBS(I)(NCLB:NCLB).EQ.' ') THEN
              NCLB=NCLB-1
              IF (NCLB.GT.0) GO TO 103
            END IF
            IF (NCLB.NE.0) THEN
              WCHO=WCHR
              CALL PLCHHQ (.5,.5,LLBS(I)(1:NCLB),WCHO,360.,0.)
              IF (ICFELL('LBLBAR',13).NE.0) RETURN
              CALL PCGETR ('DL - DISTANCE TO LEFT EDGE'  ,DSTL)
              IF (ICFELL('LBLBAR',14).NE.0) RETURN
              CALL PCGETR ('DR - DISTANCE TO RIGHT EDGE' ,DSTR)
              IF (ICFELL('LBLBAR',15).NE.0) RETURN
              CALL PCGETR ('DB - DISTANCE TO BOTTOM EDGE',DSTB)
              IF (ICFELL('LBLBAR',16).NE.0) RETURN
              CALL PCGETR ('DT - DISTANCE TO TOP EDGE'   ,DSTT)
              IF (ICFELL('LBLBAR',17).NE.0) RETURN
              IF (IHOV.EQ.0) THEN
                IF ((1.-HSFB)*HSOB.LT.RMUL*(DSTB+DSTT+WCHO))
     +          WCHR=WCHO*(((1.-HSFB)*HSOB)/(RMUL*(DSTB+DSTT+WCHO)))
                IF (1.-XPB1.LT.DSTR+WCHO)
     +          WCHR=MIN(WCHR,WCHO*((1.-XPB1)/(DSTR+WCHO)))
                IF (XPB1-XPLL.LT.DRLL+DSTL+2.*WCHO)
     +          WCHR=MIN(WCHR,WCHO*((XPB1-XPLL)/(DRLL+DSTL+2.*WCHO)))
                HMAX=MAX(HMAX,DSTB+DSTT+WCHO)*(WCHR/WCHO)
              ELSE
                IF ((1.-WSFB)*WSOB.LT.RMUL*(DSTL+DSTR+2.*WCHO))
     +          WCHR=WCHO*(((1.-WSFB)*WSOB)/(RMUL*(DSTL+DSTR+2.*WCHO)))
                IF (1.-YPB1.LT.DSTT+.5*WCHO)
     +          WCHR=MIN(WCHR,WCHO*((1.-YPB1)/(DSTT+.5*WCHO)))
                IF (YPB1-YPLL.LT.DTLL+DSTB+WCHO)
     +          WCHR=MIN(WCHR,WCHO*((YPB1-YPLL)/(DTLL+DSTB+WCHO)))
              END IF
              XPLL=XPB1
              YPLL=YPB1
              DRLL=DSTR*(WCHR/WCHO)
              DTLL=DSTT*(WCHR/WCHO)
            END IF
            XPB1=XPB1+WSOB
            YPB1=YPB1+HSOB
  104     CONTINUE
C
C Draw the labels.
C
          CALL GQPLCI (IERR,ISCL)
          IF (IERR.NE.0) THEN
            CALL SETER ('LBLBAR - ERROR EXIT FROM GQPLCI',18,1)
            RETURN
          END IF
C
          CALL GQTXCI (IERR,ISCT)
          IF (IERR.NE.0) THEN
            CALL SETER ('LBLBAR - ERROR EXIT FROM GQTXCI',19,1)
            RETURN
          END IF
C
          IF (ICLB.LT.0) THEN
            CALL GSPLCI (ISCT)
          ELSE
            CALL GSPLCI (ICLB)
            CALL GSTXCI (ICLB)
          END IF
C
          IF (WOLB.GT.0.) THEN
            CALL GQLWSC (IERR,STLW)
            IF (IERR.NE.0) THEN
              CALL SETER ('LBLBAR - ERROR EXIT FROM GQLWSC',20,1)
              RETURN
            END IF
            CALL GSLWSC (WOLB)
          END IF
C
          IF (NLBS.LT.ABS(NBOX)) THEN
            XLB1=XLB1+WINC
            YBB1=YBB1+HINC
          ELSE IF (NLBS.EQ.ABS(NBOX)) THEN
            XLB1=XLB1+WSFB*WINC/2.
            YBB1=YBB1+HSFB*HINC/2.
          END IF
C
          DO 106 I=1,NLBS
            NCLB=LEN(LLBS(I))
  105       IF (LLBS(I)(NCLB:NCLB).EQ.' ') THEN
              NCLB=NCLB-1
              IF (NCLB.GT.0) GO TO 105
            END IF
            IF (NCLB.NE.0) THEN
              IF (IHOV.EQ.0) THEN
                IF (LBAB.EQ.1.OR.LBAB.GE.3)
     +            CALL PLCHHQ (XLB1+REAL(I-1)*WSOB,YBB1-HMAX/2.,
     +                               LLBS(I)(1:NCLB),WCHR,0.,0.)
                  IF (ICFELL('LBLBAR',21).NE.0) RETURN
                IF (LBAB.EQ.2.OR.LBAB.GE.3)
     +            CALL PLCHHQ (XLB1+REAL(I-1)*WSOB,YTB1+HMAX/2.,
     +                               LLBS(I)(1:NCLB),WCHR,0.,0.)
                  IF (ICFELL('LBLBAR',22).NE.0) RETURN
              ELSE
                IF (LBAB.EQ.1.OR.LBAB.GE.3)
     +            CALL PLCHHQ (XRB1+WCHR,YBB1+REAL(I-1)*HSOB,
     +                            LLBS(I)(1:NCLB),WCHR,0.,-1.)
                  IF (ICFELL('LBLBAR',23).NE.0) RETURN
                IF (LBAB.EQ.2.OR.LBAB.GE.3)
     +            CALL PLCHHQ (XLB1-WCHR,YBB1+REAL(I-1)*HSOB,
     +                            LLBS(I)(1:NCLB),WCHR,0.,+1.)
                  IF (ICFELL('LBLBAR',24).NE.0) RETURN
              END IF
            END IF
  106     CONTINUE
C
          CALL GSPLCI (ISCL)
          IF (ICLB.GE.0) CALL GSTXCI (ISCT)
          IF (WOLB.GT.0.) CALL GSLWSC (STLW)
C
C Restore the original setting of the PLOTCHAR text extent flag.
C
  107     CALL PCSETI ('TE - TEXT EXTENT FLAG',ITEX)
          IF (ICFELL('LBLBAR',25).NE.0) RETURN
C
        END IF
C
C Restore the original SET parameters.
C
        CALL SET (XLVP,XRVP,YBVP,YTVP,XLWD,XRWD,YBWD,YTWD,LNLG)
        IF (ICFELL('LBLBAR',26).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
