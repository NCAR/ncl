C
C $Id: agback.f,v 1.11 2008-07-27 00:14:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGBACK
C
C The subroutine AGBACK is used to draw a graph background, as directed
C by the current contents of the parameter list.
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
C The following common block contains other AUTOGRAPH variables, both
C real and integer, which are not control parameters.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
      SAVE /AGORIP/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL AGDFLT
C
C Do an appropriate SET call for the following routines.  The call is
C equivalent to "CALL SET (XLCW,XRCW,YBCW,YTCW,0.,1.,0.,1.,1)", but
C makes the viewport cover the whole plotter frame, which avoids the
C problems resulting from clipping by GKS.
C
      CALL PLOTIT (0,0,2)
      CALL SET (0.,1.,0.,1.,-XLCW/(XRCW-XLCW),(1.-XLCW)/(XRCW-XLCW),
     +                      -YBCW/(YTCW-YBCW),(1.-YBCW)/(YTCW-YBCW),1)
C
C Draw the labels, if any, first.
C
      IDLB=INT(QDLB)
      IF (IDLB.EQ.0) GO TO 101
C
      LBIM=INT(QBIM)
      CALL AGLBLS (IDLB,WCWP,HCWP,FLLB,LBIM,FLLN,DBOX,SBOX,RBOX)
C
C Now draw each of the four axes.
C
  101 I=0
C
  102 I=I+1
C
      IF (I.EQ.5) GO TO 108
C
      IF (QDAX(I).EQ.0.) GO TO 102
C
      GO TO (103,104,105,106) , I
C
C Y axis - left.
C
  103 WNLB(1)=0.-YBGW
      IF (XBGA(1)-WNLL(1).LT.DBOX(3,2).AND.
     +                XBGA(1)+WNLR(1).GT.DBOX(3,1)) WNLB(1)=0.-DBOX(3,4)
C
      WNLE(1)=YTGW-1.
      IF (XBGA(1)-WNLL(1).LT.DBOX(4,2).AND.
     +                XBGA(1)+WNLR(1).GT.DBOX(4,1)) WNLE(1)=DBOX(4,3)-1.
C
      GO TO 107
C
C Y axis - right.
C
  104 WNLB(2)=YTGW-1.
      IF (XBGA(2)-WNLR(2).LT.DBOX(4,2).AND.
     +                XBGA(2)+WNLL(2).GT.DBOX(4,1)) WNLB(2)=DBOX(4,3)-1.
C
      WNLE(2)=0.-YBGW
      IF (XBGA(2)-WNLR(2).LT.DBOX(3,2).AND.
     +                XBGA(2)+WNLL(2).GT.DBOX(3,1)) WNLE(2)=0.-DBOX(3,4)
C
      GO TO 107
C
C X axis - bottom.
C
  105 WNLB(3)=XRGW-1.
      IF (YBGA(3)-WNLL(3).LT.DBOX(2,4).AND.
     +                YBGA(3)+WNLR(3).GT.DBOX(2,3)) WNLB(3)=DBOX(2,1)-1.
C
      WNLE(3)=0.-XLGW
      IF (YBGA(3)-WNLL(3).LT.DBOX(1,4).AND.
     +                YBGA(3)+WNLR(3).GT.DBOX(1,3)) WNLE(3)=0.-DBOX(1,2)
C
      GO TO 107
C
C X axis - top.
C
  106 WNLB(4)=0.-XLGW
      IF (YBGA(4)-WNLR(4).LT.DBOX(1,4).AND.
     +                YBGA(4)+WNLL(4).GT.DBOX(1,3)) WNLB(4)=0.-DBOX(1,2)
C
      WNLE(4)=XRGW-1.
      IF (YBGA(4)-WNLR(4).LT.DBOX(2,4).AND.
     +                YBGA(4)+WNLL(4).GT.DBOX(2,3)) WNLE(4)=DBOX(2,1)-1.
C
  107 Q=MIN(0.,QDAX(I))
C
      CALL AGAXIS (I,Q,
     +             QSPA(I),WCWP,HCWP,XBGA(I),YBGA(I),XNDA(I),YNDA(I),
     +             QLUA(I),UBGA(I),UNDA(I),FUNS(I),QBTP(I),BASE(I),
     +             QJDP(I),WMJL(I),WMJR(I),QMNT(I),QNDP(I),WMNL(I),
     +             WMNR(I),QLTP(I),QLEX(I),QLFL(I),QLOF(I),QLOS(I),
     +             DNLA(I),WCLM(I),WCLE(I),RFNL(I),QCIM(I),QCIE(I),
     +             WNLL(I),WNLR(I),WNLB(I),WNLE(I))
C
      GO TO 102
C
C Do a "SET" call for the user and return.
C
  108 CALL PLOTIT (0,0,2)
      CALL SET (XLCW,XRCW,YBCW,YTCW,XLUW,XRUW,YBUW,YTUW,
     +                            1+ABS(INT(QLUX))*2+ABS(INT(QLUY)))
C
      RETURN
C
      END
