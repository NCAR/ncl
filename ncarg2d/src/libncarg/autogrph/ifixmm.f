C
C $Id: ifixmm.f,v 1.1 1996-08-14 19:42:19 kennison Exp $
C
      FUNCTION IFIXMM(RVAL)
C
C This function may be called in place of the FORTRAN function IFIX.
C If the argument is a special value, a zero is returned, and otherwise
C IFIX(RVAL) is returned.
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
      IF (ABS(RVAL).NE.SVAL(1).AND.ABS(RVAL).NE.SVAL(2)) THEN
        IFIXMM=IFIX(RVAL)
      ELSE
        IFIXMM=0
      END IF
C
      RETURN
C
      END
