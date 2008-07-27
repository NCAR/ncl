C
C $Id: agstup.f,v 1.12 2008-07-27 00:14:36 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGSTUP (XDRA,NVIX,IIVX,NEVX,IIEX,
     +                   YDRA,NVIY,IIVY,NEVY,IIEY)
C
      DIMENSION XDRA(1),YDRA(1)
C
C The routine AGSTUP is called to examine the parameter list, to provide
C default values for missing parameters, and to check for and cope with
C label overlap problems.
C
C The arguments describe the x and y data arrays to be used for the next
C graph.  See the routine AGEXUS for a description of them.
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
C If initialization has not yet been done, do it.
C
      IF (INIF.EQ.0) THEN
        CALL AGINIT
      END IF
C
C Compute the width and height of the plotter frame.
C
      CALL GETSI (IWFP,IHFP)
      WOFP=2.**IWFP-1.
      HOFP=2.**IHFP-1.
C
C Examine the get-limits-from-last-set-call parameter.
C
      IF (ABS(QSET).EQ.1.) GO TO 141
C
      CALL GETSET (XLCW,XRCW,YBCW,YTCW,XMNT,XMXT,YMNT,YMXT,LILO)
C
      QLUX=REAL((1-LILO)/2)
      QLUY=REAL(MOD(1-LILO,2))
C
      IF (ABS(QSET).EQ.3.) GO TO 140
C
      XLGD=(XLCW-XLGF)/(XRGF-XLGF)
      XRGD=(XRCW-XLGF)/(XRGF-XLGF)
      YBGD=(YBCW-YBGF)/(YTGF-YBGF)
      YTGD=(YTCW-YBGF)/(YTGF-YBGF)
      SOGD=0.
C
      IF (ABS(QSET).EQ.2.) GO TO 141
C
  140 XMIN=MIN(XMNT,XMXT)
      XMAX=MAX(XMNT,XMXT)
      QOVX=0.
      IF (XMNT.GT.XMXT) QOVX=1.
      QCEX=0.
C
      YMIN=MIN(YMNT,YMXT)
      YMAX=MAX(YMNT,YMXT)
      QOVY=0.
      IF (YMNT.GT.YMXT) QOVY=1.
      QCEY=0.
C
  141 CONTINUE
C
C Examine the graph-window parameters.
C
      XLGF=MAX(0.,MIN(1.,XLGF))
      XRGF=MAX(0.,MIN(1.,XRGF))
      YBGF=MAX(0.,MIN(1.,YBGF))
      YTGF=MAX(0.,MIN(1.,YTGF))
C
      IF (XLGF.GE.XRGF.OR.YBGF.GE.YTGF) GO TO 901
C
C Examine the grid-window parameters.
C
      XLGD=MAX(0.,MIN(1.,XLGD))
      XRGD=MAX(0.,MIN(1.,XRGD))
      YBGD=MAX(0.,MIN(1.,YBGD))
      YTGD=MAX(0.,MIN(1.,YTGD))
C
      IF (XLGD.GE.XRGD.OR.YBGD.GE.YTGD) GO TO 902
C
C Examine the user-window minima and maxima for special values.  Compute
C tentative values of the user-window edge parameters.
C
      QIXY=MAX(0.,MIN(1.,QIXY))
C
      IF (QIXY.NE.0.) GO TO 142
C
      CALL AGEXUS (SVAL,XMIN,XMAX,XLOW,XHGH,
     +                               XDRA,NVIX,IIVX,NEVX,IIEX,XLUW,XRUW)
      CALL AGEXUS (SVAL,YMIN,YMAX,YLOW,YHGH,
     +                               YDRA,NVIY,IIVY,NEVY,IIEY,YBUW,YTUW)
      GO TO 143
C
  142 CALL AGEXUS (SVAL,XMIN,XMAX,XLOW,XHGH,
     +                               YDRA,NVIY,IIVY,NEVY,IIEY,XLUW,XRUW)
      CALL AGEXUS (SVAL,YMIN,YMAX,YLOW,YHGH,
     +                               XDRA,NVIX,IIVX,NEVX,IIEX,YBUW,YTUW)
C
  143 CONTINUE
C
C Examine the user-window nice-value-at-ends parameters.  INAX and INAY
C specify which axis has the nice values (if any).
C
      QCEX=MAX(-1.,MIN(+1.,QCEX))
      INAX=INT(QCEX)
      IF (INAX.NE.0) INAX=(INAX+7)/2
C
      QCEY=MAX(-1.,MIN(+1.,QCEY))
      INAY=INT(QCEY)
      IF (INAY.NE.0) INAY=(INAY+3)/2
C
C Examine the user-window linear-log flags.
C
      QLUX=MAX(-1.,MIN(1.,QLUX))
      QLUY=MAX(-1.,MIN(1.,QLUY))
C
C Examine the axis parameters.
C
      QLUD=ABS(QLUY)
      INAD=INAY
      UMIN=YBUW
      UMAX=YTUW
      QMIN=YBUW
      QMAX=YTUW
C
      I=0
C
  101 I=I+1
      IF (I.EQ.5) GO TO 104
C
      IF (I.EQ.3) THEN
        QLUD=ABS(QLUX)
        INAD=INAX
        UMIN=XLUW
        UMAX=XRUW
        QMIN=XLUW
        QMAX=XRUW
      END IF
C
      QDAX(I)=MAX(-1.,MIN(4.,QDAX(I)))
      IF (QDAX(I).LE.0.) GO TO 102
      QLUA(I)=QLUD
      QBTP(I)=QBTD(I)
      IF (QBTD(I).EQ.SVAL(1).OR.QBTD(I).EQ.SVAL(2)) QBTP(I)=1.+QLUD
      QBTP(I)=MAX(0.,MIN(3.,QBTP(I)))
      IF (QBTD(I).EQ.SVAL(2)) QBTD(I)=QBTP(I)
C
      CALL AGEXAX (I,SVAL,UMIN,UMAX,INAD-I,QLUD,FUNS(I),QBTP(I),BASD(I),
     +             BASE(I),QMJD(I),QMND(I),QMNT(I),QLTD(I),QLTP(I),
     +             QLED(I),QLEX(I),QLFD(I),QLFL(I),QMIN,QMAX)
C
      QSPA(I)=MAX(0.,MIN(1.,QSPA(I)))
      IF (QJDP(I).EQ.SVAL(1).OR.QJDP(I).EQ.SVAL(2)) QJDP(I)=65535.
      IF (QNDP(I).EQ.SVAL(1).OR.QNDP(I).EQ.SVAL(2)) QNDP(I)=65535.
C
  102 IF (I.EQ.2) THEN
        YBUW=QMIN
        YTUW=QMAX
      ELSE IF (I.EQ.4) THEN
        XLUW=QMIN
        XRUW=QMAX
      END IF
C
      GO TO 101
C
C Examine the user-window min-max/max-min ordering parameters.  Compute
C final values of the user-window edge parameters.
C
  104 QOVX=MAX(0.,MIN(1.,QOVX))
      IF (QOVX.EQ.0.) GO TO 105
      TEMP=XLUW
      XLUW=XRUW
      XRUW=TEMP
C
  105 QOVY=MAX(0.,MIN(1.,QOVY))
      IF (QOVY.EQ.0.) GO TO 106
      TEMP=YBUW
      YBUW=YTUW
      YTUW=TEMP
C
C Determine the exact size and shape of the curve window.
C
  106 XLGW=XLGF*WOFP
      XRGW=XRGF*WOFP
      YBGW=YBGF*HOFP
      YTGW=YTGF*HOFP
C
      XLCW=XLGW+XLGD*(XRGW-XLGW)
      XRCW=XLGW+XRGD*(XRGW-XLGW)
      YBCW=YBGW+YBGD*(YTGW-YBGW)
      YTCW=YBGW+YTGD*(YTGW-YBGW)
C
      WCWP=XRCW-XLCW
      HCWP=YTCW-YBCW
C
      ARWH=WCWP/HCWP
C
      IF (SOGD) 107,115,108
C
  107 DRWH=ABS(SOGD)
      GO TO 111
C
  108 DRWH=ABS((XRUW-XLUW)/(YTUW-YBUW))
      IF (SOGD-1.) 109,110,110
C
  109 IF (DRWH.LT.SOGD.OR.(1./DRWH).LT.SOGD) GO TO 115
      GO TO 111
C
  110 IF (DRWH.GT.SOGD.OR.(1./DRWH).GT.SOGD) DRWH=1.
C
  111 IF (DRWH-ARWH) 112,115,113
C
  112 XLCW=XLCW+.5*(WCWP-HCWP*DRWH)
      XRCW=XRCW-.5*(WCWP-HCWP*DRWH)
      GO TO 114
C
  113 YBCW=YBCW+.5*(HCWP-WCWP/DRWH)
      YTCW=YTCW-.5*(HCWP-WCWP/DRWH)
C
  114 WCWP=XRCW-XLCW
      HCWP=YTCW-YBCW
C
  115 SCWP=MIN(WCWP,HCWP)
C
      XLGW=(XLGW-XLCW)/WCWP
      XRGW=(XRGW-XLCW)/WCWP
      YBGW=(YBGW-YBCW)/HCWP
      YTGW=(YTGW-YBCW)/HCWP
C
      XLCW=XLCW/WOFP
      XRCW=XRCW/WOFP
      YBCW=YBCW/HOFP
      YTCW=YTCW/HOFP
C
C Make sure the number of dash patterns is in range.
C
      QODP=MAX(-26.,MIN(+26.,QODP))
      IF (QODP.EQ.0.) QODP=-1.
C
C Examine the windowing parameter.
C
      QWND=MAX(0.,MIN(1.,QWND))
C
C Do a test run of the routine AGLBLS to find out how much space will be
C required for labels in each of the six label boxes.
C
      QDLB=MAX(0.,MIN(2.,QDLB))
      IDLB=INT(QDLB)
      LBIM=INT(QBIM)
C
      CALL AGLBLS (-IDLB,WCWP,HCWP,FLLB,LBIM,FLLN,DBOX,SBOX,RBOX)
C
C Compute the desired and smallest-possible widths of the labels in
C boxes 1 and 2.
C
      DWB1=MAX(0.,DBOX(1,2)-DBOX(1,1))
      SWB1=MAX(0.,SBOX(1,2)-SBOX(1,1))
      DWB2=MAX(0.,DBOX(2,2)-DBOX(2,1))
      SWB2=MAX(0.,SBOX(2,2)-SBOX(2,1))
C
C Compute the desired and smallest-possible heights of the labels in
C boxes 3 and 4.
C
      DHB3=MAX(0.,DBOX(3,4)-DBOX(3,3))
      SHB3=MAX(0.,SBOX(3,4)-SBOX(3,3))
      DHB4=MAX(0.,DBOX(4,4)-DBOX(4,3))
      SHB4=MAX(0.,SBOX(4,4)-SBOX(4,3))
C
C Do test runs of AGAXIS for each of the four axes to see how much space
C will be required for numeric labels.
C
      I=0
C
  118 I=I+1
      IF (I.EQ.5) GO TO 128
C
      XYPI=REAL(1-MOD(I,2))
      IF (QDAX(I).EQ.0.) GO TO 121
      IF (PING(I).NE.SVAL(1)) XYPI=PING(I)
C
      IF (I.GE.3) GO TO 119
C
      XYMN=XLGW
      XYMX=XRGW
      IF (PINU(I).EQ.SVAL(1)) GO TO 120
      XYPI=(PINU(I)-XLUW)/(XRUW-XLUW)
      IF (QLUX.NE.0.) XYPI=(ALOG10(PINU(I))-ALOG10(XLUW))/
     +                                       (ALOG10(XRUW)-ALOG10(XLUW))
      GO TO 120
C
  119 XYMN=YBGW
      XYMX=YTGW
      IF (PINU(I).EQ.SVAL(1)) GO TO 120
      XYPI=(PINU(I)-YBUW)/(YTUW-YBUW)
      IF (QLUY.NE.0.) XYPI=(ALOG10(PINU(I))-ALOG10(YBUW))/
     +                                       (ALOG10(YTUW)-ALOG10(YBUW))
C
  120 XYPI=MAX(XYMN,MIN(XYMX,XYPI))
C
  121 GO TO (122,123,124,125) , I
C
C Left y axis.
C
  122 XBGA(1)=XYPI
      YBGA(1)=0.
      UBGA(1)=YBUW
      XNDA(1)=XYPI
      YNDA(1)=1.
      UNDA(1)=YTUW
      WNLL(1)=XYPI-XLGW-DWB1
      WNLR(1)=XRGW-XYPI-DWB2
      GO TO 126
C
C Right y axis.
C
  123 XBGA(2)=XYPI
      YBGA(2)=1.
      UBGA(2)=YTUW
      XNDA(2)=XYPI
      YNDA(2)=0.
      UNDA(2)=YBUW
      WNLL(2)=XRGW-XYPI-DWB2
      WNLR(2)=XYPI-XLGW-DWB1
      GO TO 126
C
C Bottom x axis.
C
  124 XBGA(3)=1.
      YBGA(3)=XYPI
      UBGA(3)=XRUW
      XNDA(3)=0.
      YNDA(3)=XYPI
      UNDA(3)=XLUW
      WNLL(3)=XYPI-YBGW-DHB3
      WNLR(3)=YTGW-XYPI-DHB4
      GO TO 126
C
C Top x axis.
C
  125 XBGA(4)=0.
      YBGA(4)=XYPI
      UBGA(4)=XLUW
      XNDA(4)=1.
      YNDA(4)=XYPI
      UNDA(4)=XRUW
      WNLL(4)=YTGW-XYPI-DHB4
      WNLR(4)=XYPI-YBGW-DHB3
C
  126 IF (QDAX(I).GT.0.) THEN
        CALL AGAXIS (I,QDAX(I),QSPA(I),WCWP,HCWP,XBGA(I),YBGA(I),
     +               XNDA(I),YNDA(I),QLUA(I),UBGA(I),UNDA(I),FUNS(I),
     +               QBTP(I),BASE(I),QJDP(I),WMJL(I),WMJR(I),QMNT(I),
     +               QNDP(I),WMNL(I),WMNR(I),QLTP(I),QLEX(I),QLFL(I),
     +               QLOF(I),QLOS(I),DNLA(I),WCLM(I),WCLE(I),RFNL(I),
     +               QCIM(I),QCIE(I),WNLL(I),WNLR(I),10.,11.)
      ELSE
        WNLL(I)=0.
        WNLR(I)=0.
      END IF
      GO TO 118
C
C If no labels are to be drawn, AGSTUP is now done.
C
  128 IF (IDLB.EQ.0) GO TO 138
C
C Check the label boxes, moving and/or shrinking them to prevent the
C labels in them from overlapping any portion of any axis.  The labels
C on an axis may have to be moved, as well.
C
C Box 1 - to the left of the curve window.
C
      IF (DBOX(1,2).GT.0.) GO TO 903
      DBOX(1,2)=MIN(0.,XBGA(1)-WNLL(1),XBGA(2)-WNLR(2))
      DBOX(1,1)=DBOX(1,2)-DWB1
      IF (DBOX(1,1).LT.XLGW) DBOX(1,1)=MIN(DBOX(1,2)-SWB1,XLGW)
      IF (DBOX(1,1).GE.XLGW) GO TO 130
      DBOX(1,1)=XLGW
      DBOX(1,2)=XLGW+SWB1
      TEMP=XBGA(1)-WNLL(1)-DBOX(1,2)
      IF (TEMP.GE.0.) GO TO 129
      WNLL(1)=WNLL(1)+TEMP
      WNLR(1)=WNLR(1)-TEMP
  129 TEMP=XBGA(2)-WNLR(2)-DBOX(1,2)
      IF (TEMP.GE.0.) GO TO 130
      WNLL(2)=WNLL(2)-TEMP
      WNLR(2)=WNLR(2)+TEMP
C
C Box 2 - to the right of the curve window.
C
  130 IF (DBOX(2,1).LT.1.) GO TO 904
      DBOX(2,1)=MAX(1.,XBGA(1)+WNLR(1),XBGA(2)+WNLL(2))
      DBOX(2,2)=DBOX(2,1)+DWB2
      IF (DBOX(2,2).GT.XRGW) DBOX(2,2)=MAX(DBOX(2,1)+SWB2,XRGW)
      IF (DBOX(2,2).LE.XRGW) GO TO 132
      DBOX(2,1)=XRGW-SWB2
      DBOX(2,2)=XRGW
      TEMP=XBGA(1)+WNLR(1)-DBOX(2,1)
      IF (TEMP.LE.0.) GO TO 131
      WNLL(1)=WNLL(1)+TEMP
      WNLR(1)=WNLR(1)-TEMP
  131 TEMP=XBGA(2)+WNLL(2)-DBOX(2,1)
      IF (TEMP.LE.0.) GO TO 132
      WNLL(2)=WNLL(2)-TEMP
      WNLR(2)=WNLR(2)+TEMP
C
C Box 3 - below the curve window.
C
  132 IF (DBOX(3,4).GT.0.) GO TO 905
      DBOX(3,4)=MIN(0.,YBGA(3)-WNLL(3),YBGA(4)-WNLR(4))
      DBOX(3,3)=DBOX(3,4)-DHB3
      IF (DBOX(3,3).LT.YBGW) DBOX(3,3)=MIN(DBOX(3,4)-SHB3,YBGW)
      IF (DBOX(3,3).GE.YBGW) GO TO 134
      DBOX(3,3)=YBGW
      DBOX(3,4)=YBGW+SHB3
      TEMP=YBGA(3)-WNLL(3)-DBOX(3,4)
      IF (TEMP.GE.0.) GO TO 133
      WNLL(3)=WNLL(3)+TEMP
      WNLR(3)=WNLR(3)-TEMP
  133 TEMP=YBGA(4)-WNLR(4)-DBOX(3,4)
      IF (TEMP.GE.0.) GO TO 134
      WNLL(4)=WNLL(4)-TEMP
      WNLR(4)=WNLR(4)+TEMP
C
C Box 4 - above the curve window.
C
  134 IF (DBOX(4,3).LT.1.) GO TO 906
      DBOX(4,3)=MAX(1.,YBGA(3)+WNLR(3),YBGA(4)+WNLL(4))
      DBOX(4,4)=DBOX(4,3)+DHB4
      IF (DBOX(4,4).GT.YTGW) DBOX(4,4)=MAX(DBOX(4,3)+SHB4,YTGW)
      IF (DBOX(4,4).LE.YTGW) GO TO 136
      DBOX(4,3)=YTGW-SHB4
      DBOX(4,4)=YTGW
      TEMP=YBGA(3)+WNLR(3)-DBOX(4,3)
      IF (TEMP.LE.0.) GO TO 135
      WNLL(3)=WNLL(3)+TEMP
      WNLR(3)=WNLR(3)-TEMP
  135 TEMP=YBGA(4)+WNLL(4)-DBOX(4,3)
      IF (TEMP.LE.0.) GO TO 136
      WNLL(4)=WNLL(4)-TEMP
      WNLR(4)=WNLR(4)+TEMP
C
C Box 5 - the curve window itself.
C
  136 IF (DBOX(5,1).LT.0..OR.DBOX(5,2).GT.1..OR.
     +                     DBOX(5,3).LT.0..OR.DBOX(5,4).GT.1.) GO TO 907
C
      DBOX(5,1)=MAX(XLGW,XBGA(1)+WNLR(1))
      DBOX(5,2)=MIN(XRGW,XBGA(2)-WNLR(2))
      DBOX(5,3)=MAX(YBGW,YBGA(3)+WNLR(3))
      DBOX(5,4)=MIN(YTGW,YBGA(4)-WNLR(4))
C
C Do a final check on all boxes for labels running outside the graph
C window.
C
           DO 137 NBOX=1,6
           DBOX(NBOX,1)=MAX(XLGW,DBOX(NBOX,1))
           DBOX(NBOX,2)=MIN(XRGW,DBOX(NBOX,2))
           DBOX(NBOX,3)=MAX(YBGW,DBOX(NBOX,3))
           DBOX(NBOX,4)=MIN(YTGW,DBOX(NBOX,4))
  137      CONTINUE
C
C Do a "SET" call for the user and return.
C
  138 CALL PLOTIT (0,0,2)
      CALL SET (XLCW,XRCW,YBCW,YTCW,XLUW,XRUW,YBUW,YTUW,
     +                            1+ABS(INT(QLUX))*2+ABS(INT(QLUY)))
C
      RETURN
C
C Error exits.
C
  901 CALL SETER ('AGSTUP - GRAPH WINDOW IMPROPERLY SPECIFIED',20,2)
C
  902 CALL SETER ('AGSTUP - GRID WINDOW IMPROPERLY SPECIFIED',21,2)
C
  903 CALL SETER ('AGSTUP - LEFT LABELS IMPROPERLY SPECIFIED',22,2)
C
  904 CALL SETER ('AGSTUP - RIGHT LABELS IMPROPERLY SPECIFIED',23,2)
C
  905 CALL SETER ('AGSTUP - BOTTOM LABELS IMPROPERLY SPECIFIED',24,2)
C
  906 CALL SETER ('AGSTUP - TOP LABELS IMPROPERLY SPECIFIED',25,2)
C
  907 CALL SETER ('AGSTUP - INTERIOR LABELS IMPROPERLY SPECIFIED',26,2)
C
      END
