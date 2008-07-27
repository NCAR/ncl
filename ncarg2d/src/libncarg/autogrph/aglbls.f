C
C $Id: aglbls.f,v 1.8 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGLBLS (ITST,WCWP,HCWP,FLLB,LBIM,FLLN,DBOX,SBOX,RBOX)
C
      DIMENSION FLLB(10,8),FLLN(6,16),DBOX(6,4),SBOX(6,4),RBOX(6)
C
C The routine AGLBLS is used (if ITST .LE. 0) to predict the amount of
C space which will be required for graph labels (excluding the numeric
C labels on the axes, which are handled by AGAXIS) or (if ITST .GT. 0)
C to actually draw the graph labels.
C
C The labels in question are defined by the label list (FLLB array) and
C the line list (FLLN array).  Each label is assumed to lie in one of
C five boxes, as follows:
C
C    Box 1 is to the left of the curve window.
C    Box 2 is to the right of the curve window.
C    Box 3 is below the curve window.
C    Box 4 is above the curve window.
C    Box 5 is the curve window itself.
C    Box 6 is the entire plot (graph) window.
C
C A test run of AGLBLS returns two sets of box dimensions to the caller.
C DBOX contains the dimensions required if all labels are to have their
C desired sizes, SBOX the dimensions required if all labels are to have
C their smallest sizes.  The caller is expected to use this information
C to determine a final set of box dimensions (stored in DBOX), and then
C call AGLBLS again to actually draw the labels in those boxes.
C
C The arguments of AGLBLS are as follows:
C
C -- ITST specifies whether the call is a test call (ITST .LE. 0) or a
C    real call (ITST .GT. 0).  If ABSV(ITST) .GT. 1, AGLBLS is allowed
C    to shrink the labels if they would not otherwise fit in their box.
C    If ABSV(ITST) .EQ. 1, shrinkage of labels is prohibited.  If ITST
C    .EQ. 0, labels are suppressed.
C
C -- WCWP and HCWP are the width and height of the curve window, in
C    plotter-coordinate-system units.  AGLBLS assumes that the last call
C    to the plot package routine "SET" had arguments XLCW, XRCW, YBCW,
C    YTCW, 0., 1., 0., 1., and 1 - defining the most convenient system
C    of coordinates for it.
C
C -- FLLB is the array in which the label list is stored.  The array is
C    doubly-dimensioned.  The first subscript specifies one of ten label
C    attributes, the second a particular label.  The attributes are as
C    follows (the name ILLB(M,N) refers to a label attribute which is
C    intrinsically an integer, despite being stored as a real):
C
C    -- ILLB(1,N) specifies the name of label N.  If ILLB(1,N) is zero,
C       no label is defined.  Otherwise, ILLB(1,N) is an identifier
C       returned by AGSTCH when the name of the label (a character
C       string) was stored away.
C
C    -- ILLB(2,N) may be set non-zero to suppress label N.
C
C    -- FLLB(3,N) and FLLB(4,N) are the x and y coordinates of a base-
C       point relative to which label N is positioned, as fractions of
C       the width and height, respectively, of the curve window.  The
C       position of the base-point determines the box in which label N
C       is considered to lie.
C
C    -- FLLB(5,N) and FLLB(6,N) are small offsets (typically about the
C       size of a character width), stated as fractions of the smaller
C       side of the curve window.  They are used to offset the label
C       base-point (after the box number is determined).  Typically,
C       this provides a minimum spacing between the label and one side
C       of the curve window.
C
C    -- ILLB(7,N) is the orientation angle of the label, in degrees
C       counter-clockwise from horizontal.  The base-line for label N
C       is a vector emanating from the base-point at this angle.  The
C       specified angle must be a multiple of 90 degrees.
C
C    -- ILLB(8,N) is the centering option for the label.  It specifies
C       how each line of the label is to be positioned relative to a
C       line perpendicular to the base-line at the base-point.
C
C       -- If ILLB(8,N) .LT. 0, the left edge of each line lies on
C          the perpendicular.
C
C       -- If ILLB(8,N) .EQ. 0, the center of each line lies on the
C          perpendicular.
C
C       -- If ILLB(8,N) .GT. 0, the right edge of each line lies on
C          the perpendicular.
C
C    -- ILLB(9,N) is the number of lines in label N.
C
C    -- ILLB(10,N) is the second subscript (in the line list) of the
C       first line of label N.
C
C -- LBIM is the maximum number of labels the label list will hold.
C
C -- FLLN is the array in which the line list is stored.  The array is
C    doubly-dimensioned.  The first subscript specifies one of six line
C    attributes, the second a particular line.  The attributes are as
C    follows (the name ILLN(M,N) refers to a line attribute which is
C    intrinsically an integer, despite being stored as a real):
C
C    -- ILLN(1,N) is the position number of line N.  The lines of a
C       label are ordered according to their position numbers, the one
C       having the largest position number being top-most.  Moreover,
C       lines having position numbers .GT. 0 are placed above the label
C       base-line, those having position numbers .EQ. 0 (of which there
C       should be but one) are placed on the label base-line, and those
C       having position numbers .LT. 0 are placed below the label base-
C       line.  The magnitudes of the position numbers have nothing to
C       do with inter-line spacing - that is up to AGLBLS to determine.
C
C    -- ILLN(2,N) may be set non-zero to suppress line N.
C
C    -- FLLN(3,N) is the desired width of characters in the line, as a
C       fraction of the smaller side of the curve window.
C
C    -- ILLN(4,N) is the identifier of the character string comprising
C       the text of the line, as returned by AGSTCH at the time the
C       string was stored.
C
C    -- ILLN(5,N) is the number of characters in the line.
C
C    -- ILLN(6,N) is the index of the next line of the label.  The
C       lines of a label must be ordered by position number (largest
C       to smallest).
C
C -- DBOX and SBOX, dimensioned 6 X 4, contain box dimensions, as dis-
C    cussed above.  D/SBOX(M,N) is the Nth edge-coordinate of box M,
C    where N .EQ. 1 for the left edge, 2 for the right edge, 3 for the
C    bottom edge, and 4 for the top edge, of the box.  The first two are
C    stated as fractions of the width, the second two as fractions of
C    the height, of the curve window.
C
C    RBOX, dimensioned 6, holds reduction factors for the sizes of the
C    characters in labels in each of the six boxes.  Each RBOX(M) is
C
C    -- negative to specify smallest-size characters, or
C
C    -- zero to specify that no reduction factor has been chosen, or
C
C    -- positive, between 0. and 1. (an actual reduction factor).
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following common block contains other AUTOGRAPH variables, both
C real and integer, which are not control parameters.  The only one of
C interest here is MWCL, which is the minimum usable character width,
C in plotter units.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
      SAVE /AGORIP/
C
C The following common block contains other AUTOGRAPH variables, of type
C character.
C
      COMMON /AGOCHP/ CHS1,CHS2
      CHARACTER*504 CHS1,CHS2
      SAVE /AGOCHP/
C
C HCFW(WDTH) specifies the height of a character as a function of width.
C
      HCFW(WDTH)=2.*WDTH
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C This is the main section of AGLBLS.
C
C Compute the length of the smallest side of the curve window.
C
      SCWP=MIN(WCWP,HCWP)
C
C Preset certain jumps in the internal procedure which follows.
C
      JMP1=2
      JMP2=2
      JMP3=2
C
C Jump if this is a test run.
C
      IF (ITST.LE.0) GO TO 101
C
C This is not a test run.  If the reduction factors for the six boxes
C are already set, jump directly to the plotting section; otherwise, we
C must first compute the coordinates of the six smallest-size boxes.
C
      IF (RBOX(1).NE.0.) GO TO 115
      GO TO 105
C
C This is a test run.  Compute the coordinates of the edges of the six
C desired-size boxes.
C
  101 RWCL=1.
      NBOX=0
      JMP4=1
      GO TO 200
C
  102 DBOX(NBOX,1)=XLBX
      DBOX(NBOX,2)=XRBX
      DBOX(NBOX,3)=YBBX
      DBOX(NBOX,4)=YTBX
C
      IF (NBOX.LT.6) GO TO 200
C
C This is a test run.  Compute the coordinates of the edges of the six
C smallest-size boxes, in one of two ways.
C
      IF (ABS(ITST).GT.1) GO TO 105
C
C This is a test run.  Determine smallest-size boxes (no shrinking).
C
      DO 104 J=1,4
        DO 103 I=1,6
          SBOX(I,J)=DBOX(I,J)
  103   CONTINUE
  104 CONTINUE
      RETURN
C
C Determine smallest-size boxes (shrinking allowed).
C
  105 RWCL=0.
      NBOX=0
      JMP4=2
      GO TO 200
C
  106 SBOX(NBOX,1)=XLBX
      SBOX(NBOX,2)=XRBX
      SBOX(NBOX,3)=YBBX
      SBOX(NBOX,4)=YTBX
C
      IF (NBOX.LT.6) GO TO 200
C
C If this is not a test run, jump to compute reduction factors for each
C of the six boxes and then plot the labels.  Otherwise, return.
C
      IF (ITST.GT.0) GO TO 107
      RETURN
C
C This is not a test run.  Compute reduction factors for each of the
C six boxes.
C
  107 NBOX=1
      JMP4=3
C
C (DBOX(NBOX,I),I=1,4) specifies the box in which the labels are to be
C drawn, (SBOX(NBOX,I),I=1,4) the minimum box in which they can be drawn
C if shrunk.  Check first whether the latter is contained in the former.
C If so, we have a chance.  If not, the best we can do is shrink the
C labels to minimum size and hope for the best.
C
  108 IF (SBOX(NBOX,1).LT.SBOX(NBOX,2).AND.
     +    DBOX(NBOX,1)-SBOX(NBOX,1).LT..0001.AND.
     +    SBOX(NBOX,2)-DBOX(NBOX,2).LT..0001.AND.
     +    DBOX(NBOX,3)-SBOX(NBOX,3).LT..0001.AND.
     +    SBOX(NBOX,4)-DBOX(NBOX,4).LT..0001      ) GO TO 109
C
      RBOX(NBOX)=-1.
      GO TO 114
C
C Mimimum-size labels will fit.  Find the largest value of RBOX(NBOX)
C for which the labels will fit.
C
  109 RWCL=1.
      DWCL=.5
      SWCL=0.
      GO TO 201
C
C See if the last value of RBOX(NBOX) gave us labels which would fit or
C not and adjust the value accordingly.
C
  110 IF (DBOX(NBOX,1)-XLBX.LT..0001.AND.
     +    XRBX-DBOX(NBOX,2).LT..0001.AND.
     +    DBOX(NBOX,3)-YBBX.LT..0001.AND.
     +    YTBX-DBOX(NBOX,4).LT..0001      ) GO TO 111
C
C Labels did not fit.  Adjust RBOX(NBOX) downward.
C
      RWCL=RWCL-DWCL
      DWCL=.5*DWCL
      IF (DWCL.LT..001) RWCL=SWCL
      GO TO 201
C
C Labels did fit.  Adjust RBOX(NBOX) upward, unless it is equal to 1.
C
  111 IF (RWCL.EQ.1.) GO TO 113
      SWCL=RWCL
      RWCL=RWCL+DWCL
      DWCL=.5*DWCL
      IF (DWCL.GT..001) GO TO 201
C
C The current value of RBOX(NBOX) is acceptable.  Do next box, if any.
C
  113 IF (NBOX.GE.5) GO TO 114
C
C Return updated box-edge coordinates for boxes 1 through 4.
C
      DBOX(NBOX,1)=XLBX
      DBOX(NBOX,2)=XRBX
      DBOX(NBOX,3)=YBBX
      DBOX(NBOX,4)=YTBX
C
  114 NBOX=NBOX+1
      IF (NBOX.LE.6) GO TO 108
C
C We have done all we can to make the labels fit.  Plot them now.
C
  115 NBOX=0
      LBIN=0
      JMP3=1
      JMP4=4
C
C Get a label to chew on.
C
  116 JMP1=2
      JMP2=2
      GO TO 202
C
C We have a label.  Initialize the re-loop through the lines in it.
C
  117 XPLN=XPLB-DTLB*YDLB/WCWP
      YPLN=YPLB+DTLB*XDLB/HCWP
      PHCL=0.
      LNIN=LNII
      JMP1=1
      JMP2=1
      GO TO 210
C
C Get ready to plot the label line.
C
  118 XPLN=XPLN+.5*(PHCL+FHCL)*YDLB/WCWP
      YPLN=YPLN-.5*(PHCL+FHCL)*XDLB/HCWP
      PHCL=FHCL
      CALL AGGTCH (INT(FLLN(4,LNIN)),CHS2,LNC2)
C
C Give the user a chance to change the appearance of the label line.
C
      CALL AGCHIL (0,CHS1(1:LNC1),INT(FLLN(1,LNIN)))
C
C Plot the label line.
C
      CALL AGPWRT (XPLN,YPLN,CHS2,LNC2,IWCL,LBOR,LBCN)
C
C Give the user a chance to undo the changes he made above.
C
      CALL AGCHIL (1,CHS1(1:LNC1),INT(FLLN(1,LNIN)))
C
C Go get the next line, if any.
C
      GO TO 215
C
C All labels are drawn.  Return.
C
  120 RETURN
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C This internal procedure, which may be entered and exited in a number
C of different ways, is used to scan the label list and the line list
C and to return information about the labels and lines defined there.
C
C Entry occurs here to bump the box number, store away a reduction
C factor for the sizes of labels in that box, and then compute the edge
C coordinates of the box required to hold labels of the size implied by
C that reduction factor.
C
  200 NBOX=NBOX+1
C
C Entry occurs here to do all of the above except the bumping of the box
C number.
C
  201 RBOX(NBOX)=RWCL
C
C Initialize the label-list index and the box-edge parameters.
C
      LBIN=0
      XLBX=+1000.
      XRBX=-1000.
      YBBX=+1000.
      YTBX=-1000.
      IF (ITST.EQ.0) GO TO 222
C
C This is the beginning of the loop through the labels.  Entry occurs
C here to find the next label in the list and return positioning info.
C
C Increment the label index and test for end of label list.
C
  202 LBIN=LBIN+1
      IF (LBIN.GT.LBIM) GO TO 222
C
C Skip this label if it is non-existent, suppressed, or empty.
C
      IF (FLLB(1,LBIN).EQ.0..OR.FLLB(2,LBIN).NE.0.
     +                      .OR.FLLB(9,LBIN).EQ.0.) GO TO 202
C
C Unpack the parameters specifying the label-base-point position.
C
      XBLB=FLLB(3,LBIN)
      YBLB=FLLB(4,LBIN)
      XOLB=FLLB(5,LBIN)
      YOLB=FLLB(6,LBIN)
C
C Determine in which of five boxes the label lies:
C
C in the box to the left of the curve window.
C
      LBBX=1
      IF (XBLB.EQ.0..AND.XOLB.LE.0.) GO TO 203
C
C in the box to the right of the curve window,
C
      LBBX=2
      IF (XBLB.EQ.1..AND.XOLB.GE.0.) GO TO 203
C
C in the box below the curve window,
C
      LBBX=3
      IF (YBLB.EQ.0..AND.YOLB.LE.0.) GO TO 203
C
C in the box above the curve window,
C
      LBBX=4
      IF (YBLB.EQ.1..AND.YOLB.GE.0.) GO TO 203
C
C in the curve window,
C
      LBBX=5
      IF ( (XBLB.EQ.0..AND.XOLB.GT.0.).OR.
     +     (XBLB.EQ.1..AND.XOLB.LT.0.).OR.
     +     (YBLB.EQ.0..AND.YOLB.GT.0.).OR.
     +     (YBLB.EQ.1..AND.YOLB.LT.0.)    ) GO TO 203
C
C or elsewhere.
C
      LBBX=6
C
C If we are interested in a particular box and this label is not in that
C box, skip it.
C
  203 IF (NBOX.NE.0.AND.LBBX.NE.NBOX) GO TO 202
C
C On a non-test run, get the label name and length for call to AGCHIL.
C
      IF (ITST.GT.0) CALL AGGTCH (INT(FLLB(1,LBIN)),CHS1,LNC1)
C
C Unpack the label orientation and compute its direction cosines.
C
      LBOR=INT(FLLB(7,LBIN))
C
      XDLB=COS(.017453292519943*FLLB(7,LBIN))
      YDLB=SIN(.017453292519943*FLLB(7,LBIN))
C
C Unpack the label-centering option.
C
      LBCN=INT(FLLB(8,LBIN))
C
C Unpack the index of the initial line of the label and save it.
C
      LNIN=INT(FLLB(10,LBIN))
      LNII=LNIN
C
C If this is not a test run, modify the label-base-point position as
C needed to move the label into the actual box in which it must fit.
C
      IF (ITST.LE.0) GO TO 209
C
      GO TO (204,205,206,207,208,209) , LBBX
C
  204 XBLB=XBLB+DBOX(1,2)
      GO TO 209
C
  205 XBLB=XBLB+DBOX(2,1)-1.
      GO TO 209
C
  206 YBLB=YBLB+DBOX(3,4)
      GO TO 209
C
  207 YBLB=YBLB+DBOX(4,3)-1.
      GO TO 209
C
  208 IF (XBLB.EQ.0.) XBLB=XBLB+DBOX(5,1)
      IF (XBLB.EQ.1.) XBLB=XBLB+DBOX(5,2)-1.
      IF (YBLB.EQ.0.) YBLB=YBLB+DBOX(5,3)
      IF (YBLB.EQ.1.) YBLB=YBLB+DBOX(5,4)-1.
C
C Compute the final label-base-point position.
C
  209 XPLB=XBLB+XOLB*SCWP/WCWP
      YPLB=YBLB+YOLB*SCWP/HCWP
C
C Before entering the loop through the line list, initialize the label-
C dimension parameters.
C
      DLLB=0.
      DRLB=0.
      DBLB=0.
      DTLB=0.
C
C This is the beginning of the loop through the lines in a given label.
C Entry may occur here to find the next line and return info about it.
C
C If the line is suppressed or of zero length, skip it.
C
  210 IF (FLLN(2,LNIN).NE.0..OR.FLLN(5,LNIN).LE.0.) GO TO 215
C
C Unpack the position-number, character-width, and character-count
C parameters for the line.
C
      LNPN=INT(FLLN(1,LNIN))
      WCLN=FLLN(3,LNIN)
      LNCC=INT(FLLN(5,LNIN))
C
C Compute the integer width (IWCL) and the floating-point width and
C height (FWCL and FHCL) of characters in the label.  All are expressed
C in plotter-coordinate-system units.
C
      IWCL=MAX(MWCL,INT(RBOX(LBBX)*WCLN*SCWP+.5))
      FWCL=REAL(IWCL)
      FHCL=HCFW(FWCL)
C
C Jump back with line information or drop through, as directed.
C
      GO TO (118,211) , JMP1
C
C Update the label-dimension parameters.
C
  211 DRLB=MAX(DRLB,REAL(LNCC)*FWCL)
C
      IF (LNPN) 212,213,214
C
  212 DBLB=DBLB+FHCL
      GO TO 215
C
  213 DBLB=DBLB+.5*FHCL
      DTLB=DTLB+.5*FHCL
      GO TO 215
C
  214 DTLB=DTLB+FHCL
C
C Go to the next line in the label, if there is one.
C
  215 LNIN=INT(FLLN(6,LNIN))
      IF (LNIN.NE.0) GO TO 210
C
C Jump back on end of lines or drop through, as directed.
C
      GO TO (116,216) , JMP2
C
C If all the lines in the label were either suppressed or of zero
C length, skip this label.
C
  216 IF (DRLB.EQ.0.) GO TO 202
C
C Complete the computation of the label dimensions.  The four parameters
C DLLB, DRLB, DBLB, and DTLB represent the distances from the base-point
C to the left edge, right edge, bottom edge, and top edge of the label,
C in plotter-coordinate-system units, where left, right, etc., are as
C viewed by a reader of the label.
C
      IF (LBCN) 217,218,219
C
C Left edges of lines are aligned.
C
  217 GO TO 220
C
C Centers of lines are aligned.
C
  218 DLLB=.5*(DLLB+DRLB)
      DRLB=DLLB
      GO TO 220
C
C Right edges of lines are aligned.
C
  219 SWAP=DLLB
      DLLB=DRLB
      DRLB=SWAP
C
C Jump back with label information or drop through, as directed.
C
  220 GO TO (117,221) , JMP3
C
C Update the x and y coordinates of the label box edges.
C
  221 XLBX=MIN(XLBX,XBLB,
     +     XPLB-MAX(+DLLB*XDLB,-DRLB*XDLB,-DBLB*YDLB,+DTLB*YDLB)/WCWP)
      XRBX=MAX(XRBX,XBLB,
     +     XPLB+MAX(-DLLB*XDLB,+DRLB*XDLB,+DBLB*YDLB,-DTLB*YDLB)/WCWP)
      YBBX=MIN(YBBX,YBLB,
     +     YPLB-MAX(+DLLB*YDLB,-DRLB*YDLB,+DBLB*XDLB,-DTLB*XDLB)/HCWP)
      YTBX=MAX(YTBX,YBLB,
     +     YPLB+MAX(-DLLB*YDLB,+DRLB*YDLB,-DBLB*XDLB,+DTLB*XDLB)/HCWP)
C
C Go back for the next label.
C
      GO TO 202
C
C End of label list.  Jump as directed.
C
  222 GO TO (102,106,110,120) , JMP4
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
      END
