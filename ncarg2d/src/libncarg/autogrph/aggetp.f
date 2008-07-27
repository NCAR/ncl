C
C $Id: aggetp.f,v 1.6 2008-07-27 00:14:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGGETP (TPID,FURA,LURA)
C
      CHARACTER*(*) TPID
      DIMENSION FURA(1)
C
C The routine AGGETP returns to the user the AUTOGRAPH parameter(s)
C specified by the parameter identifier TPID.  The arguments are as
C follows:
C
C -- TPID is the parameter identifier, a string of keywords separated
C    from each other by slashes and followed by a period.
C
C -- FURA is the user array which is to receive the desired parameter(s)
C    specified by TPID.
C
C -- LURA is the length of the user array FURA.
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
C Define the array DUMI, which allows access to the parameter list as
C an array.
C
      DIMENSION DUMI(485)
      EQUIVALENCE (QFRA,DUMI)
C
C If initialization has not yet been done, do it.
C
      IF (INIF.EQ.0) THEN
        CALL AGINIT
      END IF
C
C The routine AGSCAN is called to scan the parameter identifier and to
C return three quantities describing the AUTOGRAPH parameters desired.
C
      CALL AGSCAN (TPID,LOPA,NIPA,IIPA)
C
C Determine the number of elements to transfer.
C
      NURA=MAX(1,MIN(LURA,NIPA))
C
C Transfer the desired parameters to the user array.
C
      IDMI=LOPA-IIPA
C
      DO 101 IURA=1,NURA
        IDMI=IDMI+IIPA
        FURA(IURA)=DUMI(IDMI)
  101 CONTINUE
C
C If the current label name is being gotten, return its identifier.
C
      CALL AGSCAN ('LABE/NAME.',LOLN,NILN,IILN)
      IF (LOPA.EQ.LOLN.AND.NIPA.EQ.NILN.AND.QBAN.NE.0.) THEN
        LBAN=INT(QBAN)
        FURA(1)=FLLB(1,LBAN)
      END IF
C
C Done.
C
      RETURN
C
      END
