C
C $Id: agsetp.f,v 1.6 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGSETP (TPID,FURA,LURA)
C
      CHARACTER*(*) TPID
      DIMENSION FURA(LURA)
C
C The routine AGSETP stores user-provided values of the AUTOGRAPH
C parameters specified by the parameter identifier TPID.  The arguments
C are as follows:
C
C -- TPID is the parameter identifier, a string of keywords separated
C    from each other by slashes and followed by a period.
C
C -- FURA is the user array from which parameter values are to be taken.
C
C -- LURA is the length of the user array.
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
C The following common block contains other AUTOGRAPH variables, of type
C character.
C
      COMMON /AGOCHP/ CHS1,CHS2
      CHARACTER*504 CHS1,CHS2
      SAVE /AGOCHP/
C
C Define the array DUMI, which allows access to the control-parameter
C list as an array.
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
C return three quantities describing the AUTOGRAPH parameters affected.
C
      CALL AGSCAN (TPID,LOPA,NIPA,IIPA)
C
C Determine the number of values to transfer.
C
      NURA=MAX(1,MIN(LURA,NIPA))
C
C If character-string dash patterns are being replaced by integer dash
C patterns, reclaim the space used in the character-storage arrays.
C
      CALL AGSCAN ('DASH/PATT.',LODP,NIDP,IIDP)
      IF (LOPA.LE.LODP+NIDP-1.AND.LOPA+NURA-1.GE.LODP) THEN
        MINI=MAX(LOPA,LODP)-LOPA+1
        MAXI=MIN(LOPA+NURA-1,LODP+NIDP-1)-LOPA+1
        DO 100 I=MINI,MAXI
          IF (FURA(I).GT.0.) CALL AGDLCH (INT(DUMI(LOPA+I-1)))
  100   CONTINUE
      END IF
C
C Save the current values of special values 1 and 2.
C
      SVL1=SVAL(1)
      SVL2=SVAL(2)
C
C Transfer the user-provided values to the parameter list.
C
      IDMI=LOPA-IIPA
C
      DO 101 IURA=1,NURA
        IDMI=IDMI+IIPA
        DUMI(IDMI)=FURA(IURA)
  101 CONTINUE
C
C If a specific item was changed, we may have a bit more work to do;
C otherwise, return to the user.
C
      IF (NIPA.NE.1) RETURN
C
C If the specific item was special value 1 or 2, scan the primary list
C of parameters for other occurrences of the special value and change
C them to the new value.
C
      IF (SVAL(1).NE.SVL1) THEN
        SVLO=SVL1
        SVLN=SVAL(1)
        GO TO 102
      END IF
C
      IF (SVAL(2).NE.SVL2) THEN
        SVLO=SVL2
        SVLN=SVAL(2)
        GO TO 102
      END IF
C
      GO TO 104
C
  102 CALL AGSCAN ('PRIM.',LOPR,NIPR,IIPR)
C
      IDMI=LOPR-IIPR
C
      DO 103 I=1,NIPR
        IDMI=IDMI+IIPR
        IF (DUMI(IDMI).EQ.SVLO) DUMI(IDMI)=SVLN
  103 CONTINUE
C
      RETURN
C
C If the specific item was the label control flag and it was set
C negative, delete all labels and lines.
C
  104 CALL AGSCAN ('LABE/CONT.',LOLC,NILC,IILC)
      IF (LOPA.NE.LOLC) GO TO 107
      IF (QDLB.GE.0.) RETURN
C
      QBAN=0.
      QNAN=0.
C
      LBIM=INT(QBIM)
C
      DO 105 I=1,LBIM
        IF (FLLB(1,I).NE.0.) THEN
          CALL AGDLCH (INT(FLLB(1,I)))
          FLLB(1,I)=0.
        END IF
  105 CONTINUE
C
      LNIM=INT(QNIM)
C
      DO 106 I=1,LNIM
        IF (FLLN(1,I).NE.SVAL(1)) THEN
          CALL AGDLCH (INT(FLLN(4,I)))
          FLLN(1,I)=SVAL(1)
        END IF
  106 CONTINUE
C
      RETURN
C
C If the specific item was the label name, reset it to an appropriate
C index in the label list, providing initial values if appropriate.
C
  107 CALL AGSCAN ('LABE/NAME.',LOLN,NILN,IILN)
      IF (LOPA.NE.LOLN) GO TO 109
C
      LBAN=0
      LBIM=INT(QBIM)
      QNAN=0.
C
      CALL AGGTCH (INT(FURA(1)),CHS1,LCS1)
C
      DO 108 I=1,LBIM
        IF (LBAN.EQ.0.AND.FLLB(1,I).EQ.0.) LBAN=I
        CALL AGGTCH (INT(FLLB(1,I)),CHS2,LCS2)
        IF (LCS1.NE.LCS2) GO TO 108
        IF (CHS1(1:LCS1).NE.CHS2(1:LCS2)) GO TO 108
        QBAN=REAL(I)
        CALL AGDLCH (INT(FURA(1)))
        RETURN
  108 CONTINUE
C
      IF (LBAN.EQ.0) GO TO 901
C
      QBAN=REAL(LBAN)
C
      FLLB( 1,LBAN)=FURA(1)
      FLLB( 2,LBAN)=0.
      FLLB( 3,LBAN)=.5
      FLLB( 4,LBAN)=.5
      FLLB( 5,LBAN)=0.
      FLLB( 6,LBAN)=0.
      FLLB( 7,LBAN)=0.
      FLLB( 8,LBAN)=0.
      FLLB( 9,LBAN)=0.
      FLLB(10,LBAN)=0.
C
      RETURN
C
C If the label access name is not set, skip.
C
  109 IF (QBAN.LE.0.) GO TO 122
C
      LBAN=INT(QBAN)
      LBIM=INT(QBIM)
      LNAN=INT(QNAN)
      LNIM=INT(QNIM)
C
C If the specific item was the suppression flag for the current label
C and it was set negative, delete the label and/or its lines.
C
      CALL AGSCAN ('LABE/SUPP.',LOLS,NILS,IILS)
      IF (LOPA.NE.LOLS) GO TO 111
      IF (FLLB(2,LBAN).GE.0.) RETURN
C
      ITMP=INT(FLLB(2,LBAN))
      FLLB(2,LBAN)=0.
      FLLB(9,LBAN)=0.
      LNIN=INT(FLLB(10,LBAN))
      FLLB(10,LBAN)=0.
      QNAN=0.
      IF (ITMP.EQ.(-1)) GO TO 110
      CALL AGDLCH (INT(FLLB(1,LBAN)))
      FLLB(1,LBAN)=0.
      QBAN=0.
C
  110 IF (LNIN.LT.1.OR.LNIN.GT.LNIM) RETURN
      FLLN(1,LNIN)=SVAL(1)
      CALL AGDLCH (INT(FLLN(4,LNIN)))
      LNIN=INT(FLLN(6,LNIN))
      GO TO 110
C
C If the specific item was the line number, reset it to an appropriate
C index in the line list, providing initial values if appropriate.
C
  111 CALL AGSCAN ('LINE/NUMB.',LOLN,NILN,IILN)
      IF (LOPA.NE.LOLN) GO TO 118
C
      LNIL=0
      LNIN=INT(FLLB(10,LBAN))
C
  112 IF (LNIN.LT.1.OR.LNIN.GT.LNIM) GO TO 115
      IF (LNAN-INT(FLLN(1,LNIN))) 113,114,115
C
  113 LNIL=LNIN
      LNIN=INT(FLLN(6,LNIN))
      GO TO 112
C
  114 QNAN=REAL(LNIN)
      RETURN
C
  115      DO 116 I=1,LNIM
           LNIT=I
           IF (FLLN(1,I).EQ.SVAL(1)) GO TO 117
  116      CONTINUE
C
      GO TO 903
C
  117 CALL AGSTCH (' ',1,ITMP)
C
      FLLN(1,LNIT)=REAL(LNAN)
      FLLN(2,LNIT)=0.
      FLLN(3,LNIT)=.015
      FLLN(4,LNIT)=ITMP
      FLLN(5,LNIT)=1.
      FLLN(6,LNIT)=REAL(LNIN)
C
      LNAN=LNIT
      IF (LNIL.EQ.0) FLLB(10,LBAN)=REAL(LNAN)
      IF (LNIL.NE.0) FLLN( 6,LNIL)=REAL(LNAN)
C
      FLLB(9,LBAN)=FLLB(9,LBAN)+1.
C
      QNAN=REAL(LNAN)
      RETURN
C
C If the line access number is not set, skip.
C
  118 IF (LNAN.LE.0) GO TO 122
C
C If the specific item was the suppression flag for the current line and
C it was set negative, delete the line.
C
      CALL AGSCAN ('LINE/SUPP.',LOLS,NILS,IILS)
      IF (LOPA.NE.LOLS) GO TO 121
      IF (FLLN(2,LNAN).GE.0.) RETURN
C
      LNIL=0
      LNIN=INT(FLLB(10,LBAN))
C
  119 IF (LNIN.LT.1.OR.LNIN.GT.LNIM) RETURN
      IF (LNAN.EQ.LNIN) GO TO 120
      LNIL=LNIN
      LNIN=INT(FLLN(6,LNIN))
      GO TO 119
C
  120 IF (LNIL.EQ.0) FLLB(10,LBAN)=FLLN(6,LNAN)
      IF (LNIL.NE.0) FLLN( 6,LNIL)=FLLN(6,LNAN)
      FLLN(1,LNAN)=SVAL(1)
      CALL AGDLCH (INT(FLLN(4,LNAN)))
      QNAN=0.
      RETURN
C
C If the specific item was the text of a line, set the length of the
C line, as well.
C
  121 CALL AGSCAN ('LINE/TEXT.',LOLT,NILT,IILT)
      IF (LOPA.NE.LOLT) GO TO 123
      CALL AGGTCH (INT(FURA(1)),CHS1,LCS1)
      FLLN(5,LNAN)=REAL(LCS1)
      RETURN
C
C See if the user is trying to get at a line of a non-existent label.
C
  122 CALL AGSCAN ('LINE/NUMB.',LOLN,NILN,IILN)
      IF (LOPA.EQ.LOLN) GO TO 902
C
C If the specific item was the background parameter, set up the back-
C ground requested by the user.
C
  123 CALL AGSCAN ('BACK.',LOBG,NIBG,IIBG)
      IF (LOPA.NE.LOBG) GO TO 130
C
      QBAC=MAX(1.,MIN(4.,QBAC))
      IBAC=INT(QBAC)
      GO TO (124,125,126,127) , IBAC
C
C Perimeter background.
C
  124 QLBC=4.
      QRTC=4.
      WMJI=.015
      WMNI=.010
      GO TO 128
C
C Grid background.
C
  125 QLBC=4.
      QRTC=-1.
      WMJI=1.
      WMNI=1.
      GO TO 128
C
C Half-axis background.
C
  126 QLBC=4.
      QRTC=0.
      WMJI=.015
      WMNI=.010
      GO TO 128
C
C No background.
C
  127 QLBC=0.
      QRTC=0.
      WMJI=.015
      WMNI=.010
C
  128 QDAX(1)=QLBC
      QDAX(2)=QRTC
      QDAX(3)=QLBC
      QDAX(4)=QRTC
C
      DO 129 I=1,4
        WMJR(I)=WMJI
        WMNR(I)=WMNI
  129 CONTINUE
C
      QDLB=REAL(2-2*(IBAC/4))
      RETURN
C
C If the specific item was the get-limits-from-last-SET-call parameter,
C do what is necessary.
C
  130 CALL AGSCAN ('SET .',LOSE,NISE,IISE)
      IF (LOPA.NE.LOSE) GO TO 131
C
      QSET=SIGN(MAX(1.,MIN(4.,ABS(QSET))),QSET)
C
      XLGD=.15
      XRGD=.95
      YBGD=.15
      YTGD=.95
      SOGD=0.
C
      XMIN=SVAL(1)
      XMAX=SVAL(1)
      QLUX=MIN(QLUX,0.)
      QOVX=0.
      QCEX=-1.
      XLOW=SVAL(1)
      XHGH=SVAL(1)
C
      YMIN=SVAL(1)
      YMAX=SVAL(1)
      QLUY=MIN(QLUY,0.)
      QOVY=0.
      QCEY=-1.
      YLOW=SVAL(1)
      YHGH=SVAL(1)
C
      RETURN
C
C Return to caller.
C
  131 RETURN
C
C Error exits.
C
  901 CALL AGPPID (TPID)
      CALL SETER ('AGSETP - LABEL LIST OVERFLOW - SEE AUTOGRAPH SPECIALI
     +ST',15,2)
C
  902 CALL AGPPID (TPID)
      CALL SETER ('AGSETP - ATTEMPT TO DEFINE LINE OF NON-EXISTENT LABEL
     +',16,2)
C
  903 CALL AGPPID (TPID)
      CALL SETER ('AGSETP - LINE LIST OVERFLOW - SEE AUTOGRAPH SPECIALIS
     +T',17,2)
C
      END
