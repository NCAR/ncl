C
C $Id: agcurv.f,v 1.9 2008-07-27 00:14:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGCURV (XVEC,IIEX,YVEC,IIEY,NEXY,KDSH)
C
      DIMENSION XVEC(1),YVEC(1)
C
C AGCURV plots the curve defined by the points ((X(I),Y(I)),I=1,NEXY),
C where, if the primary parameter 'INVERT.' is zero,
C
C    X(I)=XVEC(1+(I-1)*IIEX) (unless IIEX=0, in which case X(I)=I), and
C    Y(I)=YVEC(1+(I-1)*IIEY) (unless IIEY=0, in which case Y(I)=I).
C
C If 'INVERT.' is non-zero, the definitions are interchanged, so that
C
C    X(I)=YVEC(1+(I-1)*IIEY) (unless IIEY=0, in which case X(I)=I), and
C    Y(I)=XVEC(1+(I-1)*IIEX) (unless IIEX=0, in which case Y(I)=I).
C
C If, for some I, X(I)=SVAL or Y(I)=SVAL, curve line segments having
C (X(I),Y(I)) as an endpoint are omitted.
C
C If the primary parameter 'WINDOW.' is zero, AGKURV is called; it does
C no windowing.  If 'WINDOW.' is non-zero, AGQURV is called; it omits
C portions of the curve which fall outside the current curve window.
C
C The argument KDSH specifies the dash pattern to be used.  If KDSH is
C negative, the function MOD(ABS(KDSH),26) is used to select a solid
C line interrupted by one of the alphabetic characters.  If KDSH is
C zero, the user is assumed to have done his own DASHD call.  If KDSH
C is positive, the function MOD(KDSH,NODP) is used to select one of the
C dash patterns in the parameter group 'DASH/PATTERNS.'.
C
C The following common block contains the AUTOGRAPH control parameters,
C all of which are real.  If it is changed, all of AUTOGRAPH (especially
C the routine AGSCAN) must be examined for possible side effects.
C
      COMMON /AGCONP/ QFRA,QSET,QROW,QIXY,QWND,QBAC , SVAL(2) ,
     +                XLGF,XRGF,YBGF,YTGF , XLGD,XRGD,YBGD,YTGD , SOGD ,
     +                XMIN,XMAX,QLUX,QOVX,QCEX,XLOW,XHGH ,
     +                YMIN,YMAX,QLUY,QOVY,QCEY,YLOW,YHGH ,
     +                QDAX(4),QSPA(4),PING(4),PINU(4),FUNS(4),QBTD(4),
     +                BASD(4),QMJD(4),QJDP(4),WMJL(4),WMJR(4),QMND(4),
     +                QNDP(4),WMNL(4),WMNR(4),QLTD(4),QLED(4),QLFD(4),
     +                QLOF(4),QLOS(4),DNLA(4),WCLM(4),WCLE(4) ,
     +                QODP,QCDP,WOCD,WODQ,QDSH(26) ,
     +                     QDLB,QBIM,FLLB(10,8),QBAN ,
     +                QLLN,TCLN,QNIM,FLLN(6,16),QNAN ,
     +                XLGW,XRGW,YBGW,YTGW , XLUW,XRUW,YBUW,YTUW ,
     +                XLCW,XRCW,YBCW,YTCW , WCWP,HCWP,SCWP ,
     +                XBGA(4),YBGA(4),UBGA(4),XNDA(4),YNDA(4),UNDA(4),
     +                QBTP(4),BASE(4),QMNT(4),QLTP(4),QLEX(4),QLFL(4),
     +                QCIM(4),QCIE(4),RFNL(4),WNLL(4),WNLR(4),WNLB(4),
     +                WNLE(4),QLUA(4) ,
     +                RBOX(6),DBOX(6,4),SBOX(6,4)
      SAVE /AGCONP/
C
C DASH receives alphabetic dash patterns.
C
      CHARACTER*10 DASH
C
C ALPH contains an alphabet.
C
      CHARACTER*26 ALPH
C
      DATA ALPH / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL AGDFLT
C
C Check for an alphabetic dash pattern.
C
      IF (KDSH.LT.0) THEN
        IDSH=MOD(-KDSH-1,26)+1
        IPSN=MOD(3*IDSH-1,10)+1
        DASH='$$$$$$$$$$'
        DASH(IPSN:IPSN)=ALPH(IDSH:IDSH)
        CALL AGSTCH (DASH,10,IDCS)
        CALL AGDASH (REAL(IDCS),WODQ,WOCD,SCWP)
        CALL AGDLCH (IDCS)
C
C Check for a dash pattern from the group "DASH/PATTERNS."
C
      ELSE IF (KDSH.GT.0) THEN
        IDSH=MOD(KDSH-1,INT(QODP))+1
        CALL AGDASH (QDSH(IDSH),WODQ,WOCD,SCWP)
C
      END IF
C
C Now that the dash pattern is determined, do the SET call.
C
      CALL PLOTIT (0,0,2)
      CALL SET (XLCW,XRCW,YBCW,YTCW,XLUW,XRUW,YBUW,YTUW,
     +                            1+ABS(INT(QLUX))*2+ABS(INT(QLUY)))
C
C Give the user a chance to modify the curve (by changing line style,
C color, etc.).
C
      CALL AGCHCU (0,KDSH)
C
C Decide whether AGKURV or AGQURV is to draw the curve.
C
      IF (QWND.EQ.0.) THEN
C
C No windowing requested - AGKURV is used.
C
        IF (QIXY.EQ.0.) THEN
          CALL AGKURV (XVEC,IIEX,YVEC,IIEY,NEXY,SVAL(1))
        ELSE
          CALL AGKURV (YVEC,IIEY,XVEC,IIEX,NEXY,SVAL(1))
        END IF
C
      ELSE
C
C Windowing requested - AGQURV is used.
C
        IF (QIXY.EQ.0.) THEN
          CALL AGQURV (XVEC,IIEX,YVEC,IIEY,NEXY,SVAL(1))
        ELSE
          CALL AGQURV (YVEC,IIEY,XVEC,IIEX,NEXY,SVAL(1))
        END IF
C
      END IF
C
C Give the user a chance to change back what he changed above.
C
      CALL AGCHCU (1,KDSH)
C
C Done.
C
      RETURN
C
      END
