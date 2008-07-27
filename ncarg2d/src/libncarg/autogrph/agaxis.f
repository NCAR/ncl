C
C $Id: agaxis.f,v 1.11 2008-07-27 00:14:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGAXIS (IAXS,QTST,QSPA,WCWP,HCWP,XBGA,YBGA,XNDA,YNDA,
     +                   QLUA,UBGA,UNDA,FUNS,QBTP,BASE,QJDP,WMJL,WMJR,
     +                   QMNT,QNDP,WMNL,WMNR,QLTP,QLEX,QLFL,QLOF,QLOS,
     +                   DNLA,WCLM,WCLE,RFNL,QCIM,QCIE,WNLL,WNLR,WNLB,
     +                                                           WNLE)
C
C The routine AGAXIS is used to draw, tick-mark, and label an axis or,
C if ITST is non-zero, to pre-compute the amount of space which will be
C required for numeric labels when the axis is actually drawn.  AGAXIS
C assumes that the last call to the plot-package routine SET was as
C follows (or the equivalent thereof):
C
C    CALL SET (XLCW,XRCW,YBCW,YTCW,0.,1.,0.,1.,1)
C
C where XLCW, XRCW, YBCW, and YTCW are the coordinates of the left,
C right, bottom, and top edges of the curve window, stated as fractions
C of the appropriate edge of the plotter frame.
C
C The arguments of AGAXIS are as follows:
C
C -- IAXS is the number of the axis being drawn - 1, 2, 3, or 4, meaning
C    the left, right, bottom, and top axes, respectively.
C
C -- ITST is an integer specifying what the caller wishes AGAXIS to do,
C    as follows:
C
C    -- If ITST .LT. 0, AGAXIS is to draw only the axis, nothing else.
C
C    -- If ITST .EQ. 0, AGAXIS is to draw, tick, and label the axis.
C
C    -- If ITST .GT. 0, AGAXIS is to pre-compute the amount of space
C       which will be required for numeric labels.  If the labels will
C       not fit in the space provided, AGAXIS is instructed to take
C       action as follows:
C
C       -- ITST .EQ. 1 - no action.
C
C       -- ITST .EQ. 2 - shrink the labels.
C
C       -- ITST .EQ. 3 - re-orient the labels.
C
C       -- ITST .EQ. 4 - shrink and/or re-orient the labels.
C
C -- ISPA is a 0 or a 1, specifying whether or not the axis itself is
C    to be drawn.  If ISPA .NE. 0, the axis is suppressed.  Tick marks
C    and/or labels may still be drawn.
C
C -- WCWP is the width of the curve window, in plotter units.
C
C -- HCWP is the height of the curve window, in plotter units.
C
C -- XBGA, YBGA, XNDA, and YNDA are the x and y coordinates of the ends
C    of the axis.  X coordinates are stated as fractions of the width,
C    y coordinates as fractions of the height, of the curve window.  The
C    axis to be drawn must be either horizontal or vertical (at an angle
C    of 0, 90, 180, or 270 degrees).  The left side, right side, begin-
C    ning, and end of the axis are defined from the viewpoint of a demon
C    standing at (XBGA,YBGA) and staring balefully toward (XNDA,YNDA).
C
C -- LLUA, UBGA, and UNDA define the mapping of the "user" coordinate
C    system (used for data-point coordinates) onto the axis.  If LLUA
C    is zero, the mapping is linear; if LLUA is non-zero, the mapping
C    is logarithmic.  UBGA is the user-system value at the beginning of
C    the axis, UNDA the value at the end of the axis.  The subroutine
C    AGFTOL, which needs these parameters, is actually passed LLUA,
C    UBEG=F(UBGA), and UDIF=F(UNDA)-F(UBGA), where F is the function
C    F(X)=X or the function F(X)=ALOG10(X), depending on LLUA.
C
C -- FUNS is a function-selector, to be used in calls to AGUTOL, which
C    defines the mappings from the user system to the label system and
C    vice-versa for each of the four axes.  The functions defined must
C    be continuous, monotonic, and bounded within the user-system range
C    (UBGA,UNDA) and a little bit outside that range.  The positions
C    of numeric labels and tick marks are chosen in the label system,
C    mapped to the user system, and then onto the axis.
C
C -- NBTP and BASE specify how major ticks are to be positioned in the
C    label coordinate system.  See the routine AGNUMB (arguments NBTP,
C    SBSE, and EXMU) for a description of these arguments.  Note that
C    NBTP .EQ. 0 or BASE .EQ. 0. suppresses both major tick marks and
C    their labels.  Note:  SBSE .EQ. +BASE or -BASE, as needed.
C
C -- QJDP is the major-tick-mark dash pattern (0. .LE. QJDP .LE. 65535.)
C    QJDP .LE. 0 suppresses major ticks.
C
C -- WMJL and WMJR are the distances to the left and right ends of the
C    major tick marks, stated as fractions of the shortest side of the
C    curve window.  Values .EQ. 0 may be used to suppress one or both
C    portions.  Values .GE. 1 may be used to extend a given portion all
C    the way to the edge of the curve window.  (See routine AGCTKO.)
C
C -- NMNT is the number of minor tick marks to be placed between each
C    pair of consecutive major tick marks.  NMNT .EQ. 0 suppresses them.
C
C -- QNDP, WMNL, and WMNR are analogous to QJDP, WMJL, and WMJR, but
C    specify minor-tick-mark characteristics.
C
C -- NLTP, NLEX, and NLFL specify the graphic form of numeric labels, as
C    described in the routine AGNUMB (which see).  Note that NLTP .LE. 0
C    suppresses numeric labels.
C
C -- NLOF and NLOS are first and second choices for the numeric label
C    orientation.  Both must be multiples of 90, specifying an angle
C    measured in degrees counter-clockwise from a vector running from
C    left to right in the curve window.  If ITST .EQ. 0, AGAXIS uses
C    NLOF if it is .GE. 0, NLOS otherwise, for the label orientation.
C    If ITST .NE. 0, AGAXIS initially makes both NLOF and NLOS positive.
C    Then, if ITST .GE. 3, NLOF may or may not be made negative.  (To
C    set the sign of NLOF or NLOS, AGAXIS adds or subtracts 360*K.)
C
C -- DNLA is the desired distance of numeric labels from the axis,
C    positive to the left, negative to the right, of the axis.  The
C    magnitude of DNLA is the size of the gap between the axis and the
C    nearest edge of a label, expressed as a fraction of the smaller
C    dimension of the curve window.  See also RFNL, below.
C
C -- WCLM and WCLE are the desired widths of characters in the mantissa
C    or the exponent, respectively, of numeric labels, expressed as a
C    fraction of the smaller dimension of the curve window.  See also
C    RFNL, below.
C
C -- RFNL is a reduction factor, used as a multiplier for DNLA, WCLM,
C    and WCLE.  If ITST .NE. 0, RFNL is initially set to 1. - then, if
C    ITST .EQ. 2 or 4, it is reset as necessary to shrink the labels.
C
C -- MCIM and MCIE specify the maximum number of characters in the
C    mantissa and exponent, respectively, of a numeric label.  These
C    are input parameters if ITST .EQ. 0, output parameters otherwise.
C
C -- WNLL, WNLR, WNLB, and WNLE are the widths of numeric-label strips
C    on the left side, on the right side, at the beginning, and at the
C    end, of the axis.  These are both input and output parameters of
C    AGAXIS.  On input, they specify the amount of space available for
C    numeric labels - on output, they specify the amount of space used
C    (if ITST .EQ. 0) or required (if ITST .NE. 0).  Each is stated as
C    a fraction of either the width or the height of the curve window,
C    depending on the orientation of the axis in the curve window.
C
C The following common block contains other AUTOGRAPH variables, both
C real and integer, which are not control parameters.  The only ones
C actually used here are ISLD, MWCM, MWCE, and MDLA.  ISLD is a solid-
C line dash pattern (sixteen one bits).  MWCM, MWCE, and MDLA specify
C the minimum allowed values of the width of a character in a label
C mantissa, the width of a character in a label exponent, and the
C distance of a label from the axis.  All are in plotter coordinate
C units.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
      SAVE /AGORIP/
C
C The AUTOGRAPH function AGFPBN is of type integer.
C
      INTEGER AGFPBN
C
C Local data required are as follows:
C
C BFRM is a buffer in which the routine AGNUMB returns the characters of
C a label mantissa.  CTMP holds a sub-string from an AGPWRT call.
C
      CHARACTER*40 BFRM
      CHARACTER*40 CTMP
C
C BFRE is a buffer in which the routine AGNUMB returns the characters of
C a label exponent.
C
      CHARACTER*5 BFRE
C
C XMJT, YMJT, XMNT, and YMNT are used to hold x and y offsets to the
C endpoints of left-of-label and right-of-label portions of major and
C minor tick marks.
C
      DIMENSION XMJT(4),YMJT(4),XMNT(4),YMNT(4)
C
C SMJP is the minimum distance allowed between major tick marks, in
C plotter coordinate units.
C
      DATA SMJP / 1. /
C
C FBGM, FBGP, FNDM, and FNDP are the coordinates of points a little on
C either side of the beginning and end of the axis, as fractions of the
C distance along the axis.
C
      DATA FBGM / -0.000001 /
      DATA FBGP / +0.000001 /
      DATA FNDM / +0.999999 /
      DATA FNDP / +1.000001 /
C
C HCFW is an arithmetic statement function specifying the height of a
C character as a function of its width (not counting "white space").
C The value of the multiplier was determined heuristically, by trying
C various values and seeing which gave the best results.
C
      HCFW(WDTH)=1.25*WDTH
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C This is the initialization section of AGAXIS.
C
C Unpack integer values from floating-point arguments.
C
      ITST=IFIXMM(QTST)
      ISPA=IFIXMM(QSPA)
      LLUA=IFIXMM(QLUA)
      NBTP=IFIXMM(QBTP)
      NMNT=IFIXMM(QMNT)
      NLTP=IFIXMM(QLTP)
      NLEX=IFIXMM(QLEX)
      NLFL=IFIXMM(QLFL)
      NLOF=IFIXMM(QLOF)
      NLOS=IFIXMM(QLOS)
      MCIM=IFIXMM(QCIM)
      MCIE=IFIXMM(QCIE)
C
C Initialize the local flags which specify what entities to draw, using
C values appropriate for the following quick exit.
C
      LDAX=1-ISPA
      LDNL=0
      LDMN=0
C
C If AGAXIS is to draw only the axis, exit immediately.
C
      IF (ITST.LT.0) GO TO 800
C
C If either NBTP or BASE is zeroed, exit immediately.
C
      IF (NBTP.EQ.0.OR.BASE.EQ.0.) GO TO 800
C
C Re-initialize the flag controlling the drawing of numeric labels.
C
      IF (NLTP.NE.0) LDNL=1
C
C If this is not a test run, skip.
C
      IF (ITST.EQ.0) GO TO 101
C
C This is a test run - exit if there are no numeric labels.
C
      IF (LDNL.EQ.0) GO TO 800
C
C This is a test run and the axis is to have numeric labels - initialize
C the numeric-label orientation and sizing parameters.  Clobber drawing.
C
      NLOF=MOD(NLOF+3600,360)
      NLOS=MOD(NLOS+3600,360)
      RFNL=1.
      MCIM=0
      MCIE=0
      LDMJ=0
      LDMN=0
C
C The main body of the initialization follows.
C
C Compute the length of the smaller side of the curve window, in the
C plotter coordinate system.
C
  101 SCWP=MIN(WCWP,HCWP)
C
C Compute a set of direction numbers for the axis, in the curve-window
C coordinate system (the change in x and y from the beginning to the
C end of the axis).
C
      XDNA=XNDA-XBGA
      YDNA=YNDA-YBGA
C
C Compute the length of the axis in the plotter coordinate system and
C its direction cosines.
C
      XDNP=XDNA*WCWP
      YDNP=YDNA*HCWP
      AXLP=MAX(ABS(XDNP),ABS(YDNP))
      AXLP=AXLP*SQRT((XDNP/AXLP)**2+(YDNP/AXLP)**2)
      XDCA=XDNP/AXLP
      YDCA=YDNP/AXLP
C
C Compute the axis orientation angle, in degrees counter-clockwise.
C
      IAOR=MOD(INT(57.2957795130823*ATAN2(YDCA,XDCA)+3600.5),360)
C
C Compute the multiplicative constants required to convert a fraction of
C the axis length to a fraction of the width or height of the curve
C window (a distance in x or y).
C
      CFAX=AXLP/WCWP
      CFAY=AXLP/HCWP
C
C Compute the multiplicative constants required to convert a fraction of
C the axis length to a fraction of the along-axis and perpendicular-to-
C axis sides of the curve window.
C
      CFAA=ABS(XDCA*CFAX+YDCA*CFAY)
      CFAP=ABS(XDCA*CFAY+YDCA*CFAX)
C
C Compute the quantities (UBEG) and (UDIF) for AGFTOL.
C
      IF (LLUA.NE.0) GO TO 102
C
      UBEG=UBGA
      UDIF=UNDA-UBGA
      GO TO 103
C
  102 UBEG=ALOG10(UBGA)
      UDIF=ALOG10(UNDA)-UBEG
C
C SMJT and SMNT are fractions of the axis length and specify the minimum
C space which must be available between two major ticks before the major
C ticks themselves or the minor ticks between them, respectively, may be
C drawn.
C
  103 SMJT=SMJP/AXLP
      SMNT=SMJT*REAL(NMNT+1)
C
C Initialize the fractional numeric-label character heights.
C
      FHCM=0.
      FHCE=0.
C
C If the axis has no numeric labels, skip the following code.
C
      IF (LDNL.EQ.0) GO TO 104
C
C Zero the numeric-label offset.
C
      FNLO=0.
C
C The numeric-label parameters are computed by an internal procedure
C (which see, below).
C
      JMP3=1
      GO TO 500
C
C If this is a test run, skip the following code.
C
  104 IF (ITST.NE.0) GO TO 200
C
C This is not a test run.  First, set up the tick-mark parameters.
C
C Compute the multiplicative constant required to convert a fraction of
C the smaller dimension of the grid to a fraction of the axis length.
C
      CSFA=SCWP/AXLP
C
C Compute the widths of the left and right portions of the numeric-label
C space as fractions of the axis length, affixing an appropriate sign.
C
      FNLL=-WNLL/CFAP
      FNLR=+WNLR/CFAP
C
C Compute a jump parameter to sort out the axis orientations.
C
      JAOR=1+IAOR/90
C
C The routine AGCTKO is used to compute the rest of the tick parameters.
C
      CALL AGCTKO (XBGA,YBGA,XDCA,YDCA,CFAX,CFAY,CSFA,JAOR,   1,QJDP,
     +             WMJL,WMJR,FNLL,FNLR,MJ12,MJ34,XMJT,YMJT)
C
      CALL AGCTKO (XBGA,YBGA,XDCA,YDCA,CFAX,CFAY,CSFA,JAOR,NMNT,QNDP,
     +             WMNL,WMNR,FNLL,FNLR,MN12,MN34,XMNT,YMNT)
C
C Set the flags controlling the drawing of tick marks.
C
      LDMJ=MJ12+MJ34
      LDMN=MN12+MN34
      LDLR=-(LDMJ+LDMN)
C
C If no numeric labels are to be drawn, skip the following code.
C
      IF (LDNL.EQ.0) GO TO 117
C
C Numeric labels are to be drawn.  Precompute parameters which will be
C used to position labels relative to the axis.
C
C Compute the widths and heights of the longest possible label mantissa
C and exponent, as fractions of the length of the axis.
C
      FWLM=REAL(MCIM)*FWCM
      FWLE=REAL(MCIE)*FWCE
      FHLM=FHCM
      FHLE=FHCE
      IF (MCIE.EQ.0) FHLE=0.
C
C Jump on the label-to-axis orientation.
C
      GO TO (105,106,107,108) , JLAO
C
C Label is at a 0-degree angle to the axis.
C
  105 FBLP=-FHLM
      GO TO 109
C
C Label is at a 90-degree angle to the axis.
C
  106 FBLA=0.
      FBLQ=-FWLM-FWLE
      GO TO 110
C
C Label is at a 180-degree angle to the axis.
C
  107 FBLP=FHLM+FHLE
      GO TO 109
C
C Label is at a 270-degree angle to the axis.
C
  108 FBLA=0.
      FBLQ=FWLM+FWLE
      GO TO 110
C
C Label is parallel to the axis.
C
  109 FNLW=FHLM+.5*FHLE
      FBLQ=0.
      GO TO 111
C
C Label is perpendicular to the axis.
C
  110 FNLW=FWLM+FWLE
      FBLP=0.
C
C If the labels will not fit in the space provided, clobber them.
C
  111 IF (.999999*FNLW.LT.FNLR-FNLL) GO TO 112
C
      LDNL=0
      GO TO 117
C
C Jump on the signed value of the numeric-label distance from the axis.
C
  112 IF (DNLA) 113,114,115
C
C Labels are to the right of the axis.
C
  113 FNLC=FDLA+.5*FNLW
      FBLP=FDLA+.5*ABS(FBLP-FHLE)
      FBLQ=FDLA+.5*ABS(FBLQ+FWLM-FWLE)
      GO TO 116
C
C Labels are centered on the axis.
C
  114 FNLC=0.
      FBLP=0.
      FBLQ=0.
      GO TO 116
C
C Labels are to the left of the axis.
C
  115 FNLC=-(FDLA+.5*FNLW)
      FBLP=-(FDLA+.5*ABS(FBLP))
      FBLQ=-(FDLA+.5*ABS(FBLQ-FWLM+FWLE))
C
  116 FNLO=.5*(FNLL+FNLR)-FNLC
C
C If the axis would pass through the offset labels, clobber it.
C
      IF (FNLL*FNLR.LT.0.) LDAX=0
C
C Jump to draw numeric labels and/or tick marks.
C
      GO TO 200
C
C No numeric labels are to be drawn.  If no tick marks are to be drawn
C either, exit.
C
  117 IF (LDLR.EQ.0) GO TO 800
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following code directs the process of tick-marking and labelling
C the axis, using the internal procedures which follow it.  If the
C label-coordinate-system value 0 maps onto the axis, tick-marking and
C labelling are done in two passes, one starting at 0 and proceeding
C in a positive direction and the other starting at 0 and proceeding
C in a negative direction.  If the label-coordinate-system value 0 does
C not map onto the axis, only one pass is required.
C
C First, determine the label-coordinate-system values VBGM and VNDP at
C the points FBGM and FNDP, a little beyond the ends of the axis.
C
  200 CALL AGFTOL (IAXS,1,FBGM,VBGM,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,BASE)
      CALL AGFTOL (IAXS,1,FNDP,VNDP,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,BASE)
C
C If zero falls on the axis, jump to the two-pass section of the code.
C
      IF ((VBGM.LE.0..AND.VNDP.GE.0.).OR.(VBGM.GE.0..AND.VNDP.LE.0.))
     +                                                         GO TO 201
C
C We may tick-mark and label the axis in a single pass.  Compute an
C appropriate starting value for the exponent/multiplier EXMU.
C
      SBSE=SIGN(BASE,VBGM)
      CALL AGFTOL (IAXS,2,FBGM,EBGM,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
      CALL AGFTOL (IAXS,2,FNDP,ENDP,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
      EXMU=MIN(EBGM,ENDP)
      ETMP=MOD(EXMU,1.)
      IF (ETMP.NE.0.) EXMU=EXMU-ETMP+.5+SIGN(.5,EXMU)
C
C Set the numeric-label-space limits for the beginning and end of the
C axis.
C
      FNLB=FBGM-WNLB/CFAA-.5*(FHCM+FHCE)
      FNLE=FNDP+WNLE/CFAA+.5*(FHCM+FHCE)
C
C Jump to an internal procedure to tick-mark and label the axis.  Return
C from there to the termination section of AGAXIS.
C
      JMP1=2
      GO TO 300
C
C Tick marks and labels must be done in two passes.  First, draw the
C tick mark and/or label at the zero position in the label system, using
C an internal procedure below.  A number of parameters must be preset.
C
  201 CALL AGFTOL (IAXS,-1,0.,FRAX,VLCS,LLUA,UBEG,UDIF,FUNS,NBTP,BASE)
C
C Determine whether label is to be drawn or not.
C
      LDLB=0
      IF (LDNL.EQ.0) GO TO 202
      LDLB=1
C
C The mantissa portion of the label consists of the single character 0.
C
      BFRM(1:1)='0'
      NCIM=1
      IPXM=0
C
C The label has no exponent portion.
C
      NCIE=0
C
C Allow the user to change the numeric label.
C
      CALL AGCHNL (IAXS,VLCS,BFRM,40,NCIM,IPXM,BFRE,5,NCIE)
C
C Compute the length of the mantissa, the exponent, and the whole label.
C
      FLLM=REAL(NCIM)*FWCM
      FLLE=REAL(NCIE)*FWCE
      FLLB=FLLM+FLLE
C
C The numeric-label space begins and ends at impossible values.
C
      FNLB=-10.
      FNLE=+10.
C
C Force the labeler to update FNLB, rather than FNLE.
C
      FDIR=1.
C
C Jump to an internal procedure to draw the label and/or the tick mark.
C
  202 JMP2=1
      GO TO 400
C
C Save the position of the zero-point (FRAX, expressed as a fraction of
C the axis length) and preset the parameter DZRT, which is the minimum
C distance from the zero-point at which a major tick mark could occur,
C and the parameter DZRL, which is the minimum distance from the zero-
C point at which a label could occur.  Set the label-space limit FNLE.
C Preset the internal-procedure exit parameter JMP1.
C
  203 JMP1=1
      FZRO=FRAX
      DZRT=MAX(SMJT,1.6*REAL(LDNL)*FHCM)
      IF (LDNL.EQ.0) GO TO 204
      DZRL=FNLB-FZRO
      FNLE=FNDP+WNLE/CFAA+.5*(FHCM+FHCE)
C
C Do the portion of the axis lying in the direction specified by DZRT.
C If it is too short, skip it entirely.
C
  204 FRAX=FZRO+DZRT
      IF (FRAX.LT.FBGM.OR.FRAX.GT.FNDP) GO TO (205,800) , JMP1
C
C Find out whether BASE must be negated for this portion.
C
      CALL AGFTOL (IAXS,1,FRAX,VLCS,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,BASE)
      SBSE=SIGN(BASE,VLCS)
C
C Compute a starting value of the exponent/multiplier EXMU.
C
      CALL AGFTOL (IAXS,2,FRAX,EXMU,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
      EXMU=EXMU-MOD(EXMU,1.)+.5+SIGN(.5,EXMU)
C
C Jump to an internal procedure to draw the tick marks and/or labels.
C
      GO TO 300
C
C Set up to do the second portion of the axis, then go do it.
C
  205 JMP1=2
      DZRT=-DZRT
      IF (LDNL.EQ.0) GO TO 204
      FNLB=FBGM-WNLB/CFAA-.5*(FHCM+FHCE)
      FNLE=FZRO-DZRL
      GO TO 204
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following is an internal procedure, exited via the assigned-go-to
C variable JMP1.  Its purpose is to tick-mark and label a portion of the
C axis (perhaps the entire axis) at positions determined by consecutive
C values of the parameter EXMU.  It prevents tick marks from piling up
C or passing through the label space and prevents overlapping of labels.
C Tick marks are drawn alternately from left to right or vice-versa.
C
C The caller has provided an initial value of EXMU, but we must consider
C possible minor tick marks in the interval (EXMU-1.,EXMU).
C
  300 EXMU=EXMU-1.
C
C Compute FRAX, which is the fractional distance along the axis, and
C VLCS, which is the value in the label coordinate system corresponding
C to the current value of EXMU.
C
      CALL AGFTOL (IAXS,-2,EXMU,FRAX,VLCS,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
C
C Move the current values of EXMU, FRAX, and VLCS to ELST, FLST, and
C VLST, specifying the last values of these parameters.  Then increment
C EXMU by 1. and recompute FRAX and VLCS.  (The loop through consecutive
C values of EXMU begins here.)
C
  301 ELST=EXMU
      FLST=FRAX
      VLST=VLCS
C
      EXMU=EXMU+1.
      CALL AGFTOL (IAXS,-2,EXMU,FRAX,VLCS,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
C
C FDIR indicates the direction, FDST the magnitude, of step along axis.
C
      FDIR=FRAX-FLST
      FDST=ABS(FDIR)
C
C Draw minor tick marks, if any, in the interval (FLST,FRAX).
C
      IF (LDMN.EQ.0.OR.FDST.LT.SMNT) GO TO 304
C
C Use the dashed-line pattern for minor tick marks.
C
      CALL DASHDB (AGFPBN(QNDP))
C
C Minor tick marks are equally spaced in the label-coordinate system.
C
      VINC=(VLCS-VLST)/REAL(NMNT+1)
C
      DO 303 I=1,NMNT
        VMNT=VLST+VINC*REAL(I)
        CALL AGFTOL (IAXS,-1,VMNT,FMNT,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,
     +                                                             SBSE)
        IF (FMNT.LT.FBGP.OR.FMNT.GT.FNDM) GO TO 303
        XPAX=XBGA+FMNT*XDNA
        YPAX=YBGA+FMNT*YDNA
        LDLR=-LDLR
        IF (LDLR.LT.0) GO TO 302
        CALL AGCHAX (0,IAXS,3,VMNT)
        IF (MN12.NE.0) CALL LINED (XPAX+XMNT(1),YPAX+YMNT(1),
     +                             XPAX+XMNT(2),YPAX+YMNT(2))
        IF (MN34.NE.0) CALL LINED (XPAX+XMNT(3),YPAX+YMNT(3),
     +                             XPAX+XMNT(4),YPAX+YMNT(4))
        CALL AGCHAX (1,IAXS,3,VMNT)
        GO TO 303
  302   CALL AGCHAX (0,IAXS,3,VMNT)
        IF (MN34.NE.0) CALL LINED (XPAX+XMNT(4),YPAX+YMNT(4),
     +                             XPAX+XMNT(3),YPAX+YMNT(3))
        IF (MN12.NE.0) CALL LINED (XPAX+XMNT(2),YPAX+YMNT(2),
     +                             XPAX+XMNT(1),YPAX+YMNT(1))
        CALL AGCHAX (1,IAXS,3,VMNT)
  303 CONTINUE
C
C If the end of the axis has been reached, return to caller.
C
  304 IF (FRAX.LT.FBGM.OR.FRAX.GT.FNDP) GO TO (205,800) , JMP1
C
C Draw the major tick mark and/or the numeric label at FRAX.
C
      IF (FDST.LT.SMJT) GO TO 301
      LDLB=0
      IF (LDNL.EQ.0) GO TO 305
      CALL AGNUMB (NBTP,SBSE,EXMU,NLTP,NLEX,NLFL,BFRM,40,NCIM,IPXM,BFRE,
     +                                                           5,NCIE)
      CALL AGCHNL (IAXS,VLCS,BFRM,40,NCIM,IPXM,BFRE,5,NCIE)
C
C If this is not a test run, mantissa and exponent length are checked.
C
      IF (ITST.EQ.0.AND.(NCIM.GT.MCIM.OR.NCIE.GT.MCIE)) GO TO 305
      LDLB=1
      FLLM=REAL(NCIM)*FWCM
      FLLE=REAL(NCIE)*FWCE
      FLLB=FLLM+FLLE
C
C Use the next internal procedure to draw the major tick and/or label.
C
  305 JMP2=2
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following is an internal procedure, exited via the assigned-go-to
C variable JMP2.  Its purpose is to draw the major tick mark and/or the
C numeric label at a specified point on the axis or, if ITST is .NE. 0,
C to predict the amount of space which will be required for such items.
C
C Jump if no label is to be drawn.
C
  400 IF (LDLB.EQ.0.OR.NCIM.LE.0) GO TO 410
C
C See if the label will fit without overlapping another label.  To do
C this, first compute its fractional length along the axis (FLAA).
C
      GO TO (401,402,401,402) , JLAO
C
C Label is parallel to the axis.  Allow for inter-label spacing.
C
  401 FLAA=FLLB+FWCM
      GO TO 403
C
C Label is perpendicular to the axis.  Ignore exponent portion.
C
  402 FLAA=1.6*FHCM
C
C Compute the fractional coordinates of the endpoints of the label
C (along the axis) and see if it will fit in the available label space.
C
  403 FLBB=FRAX-.5*FLAA
      FLBE=FRAX+.5*FLAA
C
      IF (FLBB.GE.FNLB.AND.FLBE.LE.FNLE) GO TO 407
C
C Label will not fit.  Omit it or, if this is a test run, see if any
C remedial action is to be taken.
C
      LDLB=0
      IF (ITST.EQ.0) GO TO 411
C
C This is a test run and we have two consecutive labels which overlap.
C See what can be done about it.
C
      GO TO (424,404,406,404) , ITST
C
C We are allowed to shrink the labels.  See if they are minimum-size
C already.  If so, the only other possibility is to re-orient them.
C
  404 IF (IWCM.LE.MWCM.AND.IWCE.LE.MWCE.AND.IDLA.LE.MDLA) GO TO 405
C
C If not, shrink them by an amount based on the extent of the overlap,
C reset the parameters affected, and start from square one.
C
      RFNL=MIN(.9,FDST/(FDST+MAX(FNLB-FLBB,FLBE-FNLE)))*RFNL
      MCIM=0
      MCIE=0
      JMP3=2
      GO TO 500
C
C If labels have already been shrunk to minimum size, see if we can
C re-orient them.  If not, at least continue with finding the maximum
C mantissa and exponent lengths.
C
  405 IF (ITST.NE.4) GO TO 424
C
C Try re-orienting the labels.  If this has already been tried, or it it
C would be pointless, skip it, but continue with finding the maximum
C mantissa and exponent lengths.
C
  406 IF (NLOF.LT.0.OR.NLOS.EQ.NLOF.OR.JLAO.EQ.2.OR.JLAO.EQ.4) GO TO 424
C
C If re-orienting makes sense, reset the appropriate parameters and
C start from square one.
C
      NLOF=NLOF-360
      RFNL=1.
      MCIM=0
      MCIE=0
      JMP3=2
      GO TO 500
C
C Label will fit.  Update the label space limits for next time.
C
  407 IF (FDIR.GE.0.) GO TO 408
      FNLE=FLBB
      GO TO 409
  408 FNLB=FLBE
C
C If this is not just a test shot, go off and draw the tick mark/label.
C
  409 IF (ITST.EQ.0) GO TO 411
C
C If this is a test shot, update the maximum mantissa and exponent
C lengths being generated and exit from this internal procedure.
C
      MCIM=MAX(MCIM,NCIM)
      MCIE=MAX(MCIE,NCIE)
      GO TO 424
C
C No label is to be drawn.  If this is a test shot, exit from this
C internal procedure without drawing the tick mark.
C
  410 IF (ITST.NE.0) GO TO 424
C
C Compute x and y coordinates of current axis point.
C
  411 XPAX=XBGA+FRAX*XDNA
      YPAX=YBGA+FRAX*YDNA
C
C Jump if no major tick-mark is to be drawn.  Otherwise, set up the
C dash pattern for major tick-marks.
C
      IF (LDMJ.EQ.0) GO TO 414
      CALL DASHDB (AGFPBN(QJDP))
C
C Flip the left-to-right/right-to-left direction flag.
C
      LDLR=-LDLR
C
C Draw the first portion of the tick mark.
C
      IF (LDLR) 413,414,412
C
  412 IF (MJ12.NE.0) THEN
        CALL AGCHAX (0,IAXS,2,VLCS)
        CALL LINED (XPAX+XMJT(1),YPAX+YMJT(1),XPAX+XMJT(2),YPAX+YMJT(2))
        CALL AGCHAX (1,IAXS,2,VLCS)
      END IF
      GO TO 414
C
  413 IF (MJ34.NE.0) THEN
        CALL AGCHAX (0,IAXS,2,VLCS)
        CALL LINED (XPAX+XMJT(4),YPAX+YMJT(4),XPAX+XMJT(3),YPAX+YMJT(3))
        CALL AGCHAX (1,IAXS,2,VLCS)
      END IF
C
C Draw the label, if any.
C
  414 IF (LDLB.EQ.0.OR.NCIM.LE.0) GO TO 421
C
C Compute the distances from (XPAX,YPAX) to the beginning of the label -
C along the axis (FBLA) and perpendicular to the axis (FBLP).  Each is a
C directed distance whose magnitude represents a fraction of the length
C of the axis.  The values depend on the label/axis orientation and the
C distance of the label from the axis.  In some cases, these quantities,
C or portions of them, have already been computed.
C
      GO TO (415,416,417,418) , JLAO
C
C Label is at a 0-degree angle to the axis.
C
  415 FBLA=-.5*FLLB
      GO TO 419
C
C Label is at a 90-degree angle to the axis.
C
  416 FBLP=FBLQ+FLLM
      IF (DNLA.EQ.0.) FBLP=.5*FLLB
      GO TO 419
C
C Label is at a 180-degree angle to the axis.
C
  417 FBLA=.5*FLLB
      GO TO 419
C
C Label is at a 270-degree angle to the axis.
C
  418 FBLP=FBLQ-FLLM
      IF (DNLA.EQ.0.) FBLP=-.5*FLLB
C
C Draw the mantissa portion of the label (excluding the "X", if any).
C
  419 DEEX=FBLA*XDCA+(FBLP+FNLO)*YDCA
      DEEY=FBLA*YDCA-(FBLP+FNLO)*XDCA
      CALL AGCHAX (0,IAXS,4,VLCS)
      IF (IPXM.EQ.0) THEN
        CALL AGPWRT (XPAX+CFAX*DEEX,
     +               YPAX+CFAY*DEEY,BFRM,NCIM,IWCM,NLOR,-1)
      ELSE
        CALL AGPWRT (XPAX+CFAX*(DEEX+(FLLM-3.*FWCM)*XDCL),
     +               YPAX+CFAY*(DEEY+(FLLM-3.*FWCM)*YDCL),
     +               BFRM,IPXM-1,IWCM,NLOR,+1)
        CTMP=BFRM(IPXM+1:NCIM)
        CALL AGPWRT (XPAX+CFAX*(DEEX+(FLLM-2.*FWCM)*XDCL),
     +               YPAX+CFAY*(DEEY+(FLLM-2.*FWCM)*YDCL),
     +               CTMP,NCIM-IPXM,IWCM,NLOR,-1)
      END IF
      DEEX=DEEX+FLLM*XDCL
      DEEY=DEEY+FLLM*YDCL
C
C Draw the "X" portion of the mantissa, if it was left out above.
C
      IF (IPXM.EQ.0) GO TO 420
      DEEX=DEEX-2.5*FWCM*XDCL
      DEEY=DEEY-2.5*FWCM*YDCL
      CALL LINE (XPAX+CFAX*(DEEX-.3*FWCM*(XDCL-YDCL)),
     +           YPAX+CFAY*(DEEY-.3*FWCM*(YDCL+XDCL)),
     +           XPAX+CFAX*(DEEX+.3*FWCM*(XDCL-YDCL)),
     +           YPAX+CFAY*(DEEY+.3*FWCM*(YDCL+XDCL)))
      CALL LINE (XPAX+CFAX*(DEEX-.3*FWCM*(XDCL+YDCL)),
     +           YPAX+CFAY*(DEEY-.3*FWCM*(YDCL-XDCL)),
     +           XPAX+CFAX*(DEEX+.3*FWCM*(XDCL+YDCL)),
     +           YPAX+CFAY*(DEEY+.3*FWCM*(YDCL-XDCL)))
      DEEX=DEEX+2.5*FWCM*XDCL
      DEEY=DEEY+2.5*FWCM*YDCL
  420 CALL AGCHAX (1,IAXS,4,VLCS)
C
C Draw the exponent portion of the label (if it has one).
C
      IF (NCIE.EQ.0) GO TO 421
      DEEX=DEEX-.5*FHCM*YDCL
      DEEY=DEEY+.5*FHCM*XDCL
      CALL AGCHAX (0,IAXS,5,VLCS)
      CALL AGPWRT (XPAX+CFAX*DEEX,YPAX+CFAY*DEEY,BFRE,NCIE,IWCE,NLOR,-1)
      CALL AGCHAX (1,IAXS,5,VLCS)
C
C Draw the second portion of the tick mark, if any.
C
  421 IF (LDLR) 423,424,422
C
  422 IF (MJ34.NE.0) THEN
        CALL AGCHAX (0,IAXS,2,VLCS)
        CALL LINED (XPAX+XMJT(3),YPAX+YMJT(3),XPAX+XMJT(4),YPAX+YMJT(4))
        CALL AGCHAX (1,IAXS,2,VLCS)
      END IF
      GO TO 424
C
  423 IF (MJ12.NE.0) THEN
        CALL AGCHAX (0,IAXS,2,VLCS)
        CALL LINED (XPAX+XMJT(2),YPAX+YMJT(2),XPAX+XMJT(1),YPAX+YMJT(1))
        CALL AGCHAX (1,IAXS,2,VLCS)
      END IF
C
C Exit from internal procedure.
C
  424 GO TO (203,301) , JMP2
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following is an internal procedure, exited via the assigned-go-to
C variable JMP3.  Its purpose is to compute all numeric-label parameters
C required by AGAXIS.
C
C Compute the desired label orientation and its direction cosines.
C
  500 NLOR=NLOF
      IF (NLOR.LT.0) NLOR=NLOS
C
      XDCL=COS(.017453292519943*REAL(NLOR))
      YDCL=SIN(.017453292519943*REAL(NLOR))
C
C Compute JLAO, which is a computed-go-to jump parameter specifying the
C label-to-axis orientation.
C
      JLAO=1+MOD(NLOR-IAOR+3600,360)/90
C
C Compute the width of a character in the label mantissa, the width of a
C character in the label exponent, and the distance of a label from the
C axis, in the plotter coordinate system.
C
      IWCM=MAX(MWCM,INT(RFNL*ABS(WCLM)*SCWP+.5))
      IWCE=MAX(MWCE,INT(RFNL*ABS(WCLE)*SCWP+.5))
      IDLA=MAX(MDLA,INT(RFNL*ABS(DNLA)*SCWP+.5))
C
C Compute the same quantities as fractions of the axis length.
C
      FWCM=REAL(IWCM)/AXLP
      FWCE=REAL(IWCE)/AXLP
      FDLA=REAL(IDLA)/AXLP
C
C Compute character heights as fractions of the axis length.
C
      FHCM=HCFW(FWCM)
      FHCE=HCFW(FWCE)
C
C Return to internal-procedure caller.
C
      GO TO (104,200,801) , JMP3
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C This is the termination section of AGAXIS.
C
C Update the parameters WNLL and WNLR to reflect the amount of space
C used/needed for numeric labels to the left and right of the axis.
C
  800 IF (LDNL.NE.0) GO TO 801
C
C No numeric labels occur on the axis.  Zero WNLL and WNLR and jump.
C
      WNLL=0.
      WNLR=0.
      GO TO 815
C
C Numeric labels do occur on the axis.  Compute the space required.
C
  801 GO TO (802,803,802,803) , JLAO
C
C Labels are parallel to the axis.
C
  802 FNLW=FHCM
      IF (MCIE.NE.0) FNLW=FNLW+.5*FHCE
      GO TO 804
C
C Labels are perpendicular to the axis.
C
  803 FNLW=REAL(MCIM)*FWCM+REAL(MCIE)*FWCE
C
C Jump on the numeric-label-distance-from-axis parameter DNLA.
C
  804 IF (DNLA) 805,806,807
C
C Labels are to the right of the axis.
C
  805 FNLL=-FDLA
      FNLR=+FDLA+FNLW
      GO TO 808
C
C Labels are centered on the axis.
C
  806 FNLL=+.5*FNLW
      FNLR=+.5*FNLW
      GO TO 808
C
C Labels are to the left of the axis.
C
  807 FNLL=+FDLA+FNLW
      FNLR=-FDLA
C
C Adjust FNLL and FNLR as implied by the numeric-label offset.
C
  808 FNLL=FNLL-FNLO
      FNLR=FNLR+FNLO
C
C If this is not a test run, jump to reset WNLL and WNLR.
C
      IF (ITST.EQ.0) GO TO 814
C
C If this is a test run, see if the labels will fit.  Jump if so.
C
      IF (CFAP*FNLL.LE.WNLL.AND.CFAP*FNLR.LE.WNLR) GO TO 814
C
C If the labels will not fit, we have a problem.  We may or may not be
C able to do anything about it, depending on ITST.
C
      GO TO (814,809,813,809) , ITST
C
C We are allowed to shrink the labels.  See if they are minimum-size
C already.  If so, the only other possibility is to re-orient them.
C
  809 IF (IWCM.LE.MWCM.AND.IWCE.LE.MWCE.AND.IDLA.LE.MDLA) GO TO 812
C
C If not, shrink them by an amount based on the extent of the problem,
C reset the parameters affected and see if the problem is solved.
C
      IF (WNLR+WNLL.GT.0.) GO TO 810
C
      RFNL=.000001*RFNL
      GO TO 811
C
  810 RFNL=MIN(.9,(WNLL+WNLR)/(CFAP*(FNLL+FNLR)))*RFNL
C
  811 JMP3=3
      GO TO 500
C
C If labels have already been shrunk to minimum size, see if we can
C re-orient them.  If not, give up.
C
  812 IF (ITST.NE.3) GO TO 814
C
C Try re-orienting the labels.  If this has already been tried, or if it
C would be pointless, give up.
C
  813 IF (NLOF.LT.0.OR.NLOS.EQ.NLOF.OR.JLAO.EQ.1.OR.JLAO.EQ.3) GO TO 814
C
C If re-orienting makes sense, reset the parameters affected and see if
C the problem is solved.
C
      NLOF=NLOF-360
      RFNL=1.
      JMP3=3
      GO TO 500
C
C Reset WNLL and WNLR for caller.
C
  814 WNLL=FNLL*CFAP
      WNLR=FNLR*CFAP
C
C If this is a test run, we are now done.
C
  815 IF (ITST.GT.0) GO TO 816
C
C Draw the axis, if it is to be drawn.
C
      IF (LDAX.EQ.0) GO TO 816
C
      CALL DASHDB (ISLD)
      CALL AGCHAX (0,IAXS,1,0.)
      CALL LINED (XBGA,YBGA,XNDA,YNDA)
      CALL AGCHAX (1,IAXS,1,0.)
C
C Pack up integer values which might have been changed into the
C corresponding floating-point arguments.
C
  816 QLOF=REAL(NLOF)
      QLOS=REAL(NLOS)
      QCIM=REAL(MCIM)
      QCIE=REAL(MCIE)
C
C Done.
C
      RETURN
C
      END
