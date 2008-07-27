C
C $Id: agrstr.f,v 1.6 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGRSTR (IFNO)
C
C This subroutine is called to restore the current state of AUTOGRAPH by
C reading all of its important variables from a record on the file which
C is associated with the unit number IFNO.
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
C The following common blocks contain variables which are required for
C the character-storage-and-retrieval scheme of AUTOGRAPH.
C
      COMMON /AGCHR1/ LNIC,INCH(2,50),LNCA,INCA
      SAVE /AGCHR1/
C
      COMMON /AGCHR2/ CHRA(2000)
      CHARACTER*1 CHRA
      SAVE /AGCHR2/
C
C Read the record.
C
      READ (IFNO,ERR=901,END=902)
     1 BASD,BASE,DBOX,DNLA,FLLB,FLLN,FUNS,HCWP,PING,PINU,QBAC,QBAN,QBIM,
     2 QBTD,QBTP,QCDP,QCEX,QCEY,QCIE,QCIM,QDAX,QDLB,QDSH,QFRA,QIXY,QJDP,
     3 QLED,QLEX,QLFD,QLFL,QLLN,QLOF,QLOS,QLTD,QLTP,QLUA,QLUX,QLUY,QMJD,
     4 QMND,QMNT,QNAN,QNDP,QNIM,QODP,QOVX,QOVY,QROW,QSET,QSPA,QWND,RBOX,
     5 RFNL,SBOX,SCWP,SOGD,SVAL,TCLN,UBGA,UNDA,WCLE,WCLM,WCWP,WMJL,WMJR,
     6 WMNL,WMNR,WNLB,WNLE,WNLL,WNLR,WOCD,WODQ,XBGA,XHGH,XLCW,XLGD,XLGF,
     7 XLGW,XLOW,XLUW,XMAX,XMIN,XNDA,XRCW,XRGD,XRGF,XRGW,XRUW,YBCW,YBGA,
     8 YBGD,YBGF,YBGW,YBUW,YHGH,YLOW,YMAX,YMIN,YNDA,YTCW,YTGD,YTGF,YTGW,
     9 YTUW,
     + INIF,ISLD,MDLA,MWCD,MWCE,MWCL,MWCM,MWDQ,SMRL,
     1 INCA,INCH,LNCA,LNIC,
     2 CHRA
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 CALL SETER ('AGRSTR - ERROR ON READ',8,2)
C
  902 CALL SETER ('AGRSTR - END-OF-FILE ON READ',9,2)
C
      END
