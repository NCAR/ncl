C
C $Id: agscan.f,v 1.9 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGSCAN (TPID,LOPA,NIPA,IIPA)
C
      CHARACTER*(*) TPID
C
C The routine AGSCAN is used by AGGETP and AGSETP to scan a parameter
C identifier and return a description of the parameter-list items which
C are specified by that parameter identifier.  It has the following
C arguments:
C
C -- TPID is the parameter identifier.
C
C -- LOPA is the index of the first parameter-list item specified.
C
C -- NIPA is the number of parameter-list items specified.
C
C -- IIPA is the index increment between one of the parameter-list items
C    specified and the next (meaningless if NIPA=1).
C
C
C BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
C
C Originally, this routine used the function "LOC" to return, in LOPA,
C the base address, in core, of the specified parameter group.  To some
C degree, it was thereby insulated from changes in the labelled common
C block AGCONP.  With the demise of "LOC", LOPA has been re-defined and
C that insulation no longer exists.  In the following code, there are
C integers which represent the indices of desired quantities in common.
C
C BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
C
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
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL AGDFLT
C
C Extract the values of a couple of required dimensions.
C
      LBIM=INT(QBIM)
      LNIM=INT(QNIM)
C
C Initialize the parameter-identifier character index.
C
      IPID=0
C
C Initialize the value of the index increment to be returned.
C
      IIPA=1
C
C Find the first keyword in the parameter identifier.
C
      CALL AGSRCH (TPID,IPID,IKWL,'PRIMFRAMSET ROW INVEWINDNULLGRAPGRIDX
     +   Y   AXISLEFTRIGHBOTTTOP DASHLABELINESECOBACK',
     +                            'primframset row invewindnullgrapgridx
     +   y   axisleftrighbotttop dashlabelinesecoback')
C
      GO TO (101,102,103,104,105,106,107,108,109,110,
     +       111,113,114,114,114,114,132,133,147,155,166,901) , IKWL
C
C PRIMARY CONTROL PARAMETERS.
C
  101 LOPA=1
      NIPA=336
      GO TO 203
C
C FRAME PARAMETER.
C
  102 LOPA=1
      GO TO 202
C
C SET PARAMETER.
C
  103 LOPA=2
      GO TO 202
C
C ROW PARAMETER.
C
  104 LOPA=3
      GO TO 202
C
C X/Y INVERSION PARAMETER.
C
  105 LOPA=4
      GO TO 202
C
C WINDOWING PARAMETER.
C
  106 LOPA=5
      GO TO 202
C
C BACKGROUND PARAMETER.
C
  166 LOPA=6
      GO TO 202
C
C NULL PARAMETER(S).
C
  107 LOPA=7
      NIPA=2
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'1   2   ',
     +                            '1   2   ')
C
      IF (IKWL.EQ.3) GO TO 901
      GO TO 201
C
C PLOT (GRAPH) WINDOW PARAMETERS.
C
  108 LOPA=9
      NIPA=4
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'LEFTRIGHBOTTTOP ',
     +                            'leftrighbotttop ')
C
      IF (IKWL.EQ.5) GO TO 901
      GO TO 201
C
C GRID WINDOW PARAMETERS.
C
  109 LOPA=13
      NIPA=5
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'LEFTRIGHBOTTTOP SHAP',
     +                            'leftrighbotttop shap')
C
      IF (IKWL.EQ.6) GO TO 901
      GO TO 201
C
C X DATA PARAMETERS.
C
  110 LOPA=18
      GO TO 112
C
C Y DATA PARAMETERS.
C
  111 LOPA=25
C
C X OR Y DATA PARAMETERS.
C
  112 NIPA=7
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'MINIMAXILOGAORDENICESMALLARG',
     +                            'minimaxilogaordenicesmallarg')
C
      IF (IKWL.EQ.8) GO TO 901
      GO TO 201
C
C AXIS PARAMETERS.
C
  113 LOPA=32
      NIPA=92
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'LEFTRIGHBOTTTOP ',
     +                            'leftrighbotttop ')
C
      IF (IKWL.EQ.5) GO TO 901
      IKWL=IKWL+12
C
C LEFT, RIGHT, BOTTOM, OR TOP AXIS PARAMETERS.
C
  114 LOPA=19+IKWL
      NIPA=23
      IIPA=4
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,
     +       'CONTLINEINTEFUNCTICKMAJOMINONUMETYPEEXPOFRACANGLOFFSWIDT',
     +       'contlineintefunctickmajominonumetypeexpofracangloffswidt')
C
      GO TO (202,201,115,167,116,117,123,126,127,127,127,
     +       127,127,127,901) , IKWL
C
C AXIS INTERSECTION PARAMETERS.
C
  115 LOPA=LOPA+8
      NIPA=2
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'GRIDUSER',
     +                            'griduser')
C
      IF (IKWL.EQ.3) GO TO 901
      GO TO 201
C
C AXIS MAPPING FUNCTION.
C
  167 LOPA=LOPA+16
      GO TO 202
C
C AXIS TICK PARAMETERS.
C
  116 LOPA=LOPA+20
      NIPA=10
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'MAJOMINO',
     +                            'majomino')
C
      LOPA=LOPA-20
      GO TO (117,123,901) , IKWL
C
C AXIS MAJOR-TICK PARAMETERS.
C
  117 LOPA=LOPA+20
      NIPA=6
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'SPACTYPEBASECOUNPATTLENGOUTWINWA',
     +                            'spactypebasecounpattlengoutwinwa')
C
      GO TO (118,119,119,119,120,121,122,122,901) , IKWL
C
C AXIS MAJOR-TICK SPACING PARAMETERS.
C
  118 NIPA=3
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'TYPEBASECOUN',
     +                            'typebasecoun')
C
      IF (IKWL.EQ.4) GO TO 901
C
      GO TO 201
C
  119 IKWL=IKWL-1
      GO TO 201
C
C AXIS MAJOR-TICK DASH PATTERN.
C
  120 LOPA=LOPA+12
      GO TO 202
C
C AXIS MAJOR-TICK LENGTH PARAMETERS.
C
  121 LOPA=LOPA+16
      NIPA=2
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'OUTWINWA',
     +                            'outwinwa')
C
      IF (IKWL.EQ.3) GO TO 901
      GO TO 201
C
  122 LOPA=LOPA+16
      IKWL=IKWL-6
      GO TO 201
C
C AXIS MINOR-TICK PARAMETERS.
C
  123 LOPA=LOPA+44
      NIPA=4
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'SPACPATTLENGOUTWINWA',
     +                            'spacpattlengoutwinwa')
C
      GO TO (202,201,124,125,125,901) , IKWL
C
C AXIS MINOR-TICK LENGTH PARAMETERS.
C
  124 LOPA=LOPA+8
      NIPA=2
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'OUTWINWA',
     +                            'outwinwa')
C
      IF (IKWL.EQ.3) GO TO 901
      GO TO 201
C
  125 LOPA=LOPA+8
      IKWL=IKWL-3
      GO TO 201
C
C AXIS NUMERIC-LABEL PARAMETERS.
C
  126 LOPA=LOPA+60
      NIPA=8
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'TYPEEXPOFRACANGLOFFSWIDT',
     +                            'typeexpofracangloffswidt')
C
      GO TO 128
C
  127 LOPA=LOPA+60
      IKWL=IKWL-8
C
  128 GO TO (202,201,201,129,130,131,901) ,IKWL
C
C AXIS NUMERIC-LABEL ORIENTATION ANGLE.
C
  129 LOPA=LOPA+12
      NIPA=2
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'1ST 2ND ',
     +                            '1st 2nd ')
C
      IF (IKWL.EQ.3) GO TO 901
      GO TO 201
C
C AXIS NUMERIC-LABEL OFFSET.
C
  130 LOPA=LOPA+20
      GO TO 202
C
C AXIS NUMERIC-LABEL WIDTH PARAMETERS.
C
  131 LOPA=LOPA+24
      NIPA=2
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'MANTEXPO',
     +                            'mantexpo')
C
      IF (IKWL.EQ.3) GO TO 901
      GO TO 201
C
C DASH-PATTERN PARAMETERS.
C
  132 LOPA=124
      NIPA=30
      IF (TPID(IPID:IPID).EQ.'.') RETURN
      JPID=IPID
      CALL AGSRCH (TPID,IPID,IKWL,'SELELENGCHARDOLLPATT',
     +                            'selelengchardollpatt')
      IF (IKWL.EQ.6) THEN
        IPID=JPID
        GO TO 168
      END IF
      IF (IKWL.NE.5) GO TO 201
  168 LOPA=LOPA+4
      NIPA=26
      IF (TPID(IPID:IPID).EQ.'.') RETURN
      CALL AGSRCH (TPID,IPID,IKWL,
     +'1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  1
     +7  18  19  20  21  22  23  24  25  26  ',
     +'1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  1
     +7  18  19  20  21  22  23  24  25  26  ')
      IF (IKWL.EQ.27) GO TO 901
      GO TO 201
C
C LABEL PARAMETERS.
C
  133 LBAN=INT(QBAN)
C
      LOPA=154
      NIPA=3+LBIM*10
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,
     +                   'CONTBUFFNAMEDEFISUPPBASEOFFSANGLCENTLINEINDE',
     +                   'contbuffnamedefisuppbaseoffsanglcentlineinde')
C
      GO TO (202,136,139,140,141,141,141,141,141,141,141,901) , IKWL
C
C LABEL BUFFER PARAMETERS.
C
  136 LOPA=155
      NIPA=1+LBIM*10
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'LENGCONTNAME',
     +                            'lengcontname')
C
      GO TO (202,137,138,901) , IKWL
C
C LABEL BUFFER CONTENTS.
C
  137 LOPA=156
      NIPA=LBIM*10
      GO TO 203
C
C LABEL BUFFER NAMES.
C
  138 LOPA=156
      NIPA=LBIM
      IIPA=10
      GO TO 203
C
C LABEL NAME.
C
  139 LOPA=236+10*LBIM-80
      GO TO 202
C
C LABEL DEFINITION.
C
  140 IF (LBAN.LT.1.OR.LBAN.GT.LBIM) GO TO 902
C
      LOPA=157+(LBAN-1)*10
      NIPA=9
      IF (TPID(IPID:IPID).EQ.'.') GO TO 203
C
      CALL AGSRCH (TPID,IPID,IKWL,'SUPPBASEOFFSANGLCENTLINEINDE',
     +                            'suppbaseoffsanglcentlineinde')
C
      GO TO 142
C
  141 IF (LBAN.LT.1.OR.LBAN.GT.LBIM) GO TO 902
C
      LOPA=157+(LBAN-1)*10
      IKWL=IKWL-4
C
  142 GO TO (202,143,144,146,146,146,146,901) , IKWL
C
C LABEL POSITION.
C
  143 LOPA=LOPA+1
      GO TO 145
C
C LABEL OFFSET.
C
  144 LOPA=LOPA+3
C
C LABEL POSITION OR OFFSET.
C
  145 NIPA=2
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'X   Y   ',
     +                            'x   y   ')
C
      IF (IKWL.EQ.3) GO TO 901
      GO TO 201
C
C OTHER LABEL ATTRIBUTES.
C
  146 LOPA=LOPA+5
      IKWL=IKWL-3
      GO TO 201
C
C LINE PARAMETERS.
C
  147 LNAN=INT(QNAN)
C
      LOPA=237+10*LBIM-80
      NIPA=4+LNIM*6
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,
     +                       'MAXIEND BUFFNUMBDEFISUPPCHARTEXTLENGINDE',
     +                       'maxiend buffnumbdefisuppchartextlenginde')
C
      GO TO (202,201,150,152,153,154,154,154,154,154,901) , IKWL
C
C LINE BUFFER PARAMETERS.
C
  150 LOPA=239+10*LBIM-80
      NIPA=1+LNIM*6
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'LENGCONT',
     +                            'lengcont')
C
      GO TO (202,151,901) , IKWL
C
C LINE BUFFER CONTENTS.
C
  151 LOPA=240+10*LBIM-80
      NIPA=LNIM*6
      GO TO 203
C
C LINE NUMBER.
C
  152 LOPA=336+10*LBIM-80+6*LNIM-96
      GO TO 202
C
C LINE DEFINITION.
C
  153 IF (LNAN.LT.1.OR.LNAN.GT.LNIM) GO TO 903
C
      LOPA=241+10*LBIM-80+(LNAN-1)*6
      NIPA=5
      IF (TPID(IPID:IPID).EQ.'.') GO TO 203
C
      CALL AGSRCH (TPID,IPID,IKWL,'SUPPCHARTEXTLENGINDE',
     +                            'suppchartextlenginde')
C
      IF (IKWL.EQ.6) GO TO 901
      GO TO 201
C
  154 IF (LNAN.LT.1.OR.LNAN.GT.LNIM) GO TO 903
      LOPA=241+10*LBIM-80+(LNAN-1)*6
      IKWL=IKWL-5
      GO TO 201
C
C SECONDARY CONTROL PARAMETERS.
C
  155 LOPA=337+10*LBIM-80+6*LNIM-96
      NIPA=149
      IF (TPID(IPID:IPID).EQ.'.') GO TO 203
C
      CALL AGSRCH (TPID,IPID,IKWL,
     +                       'GRAPUSERCURVDIMEAXISLEFTRIGHBOTTTOP LABE',
     +                       'grapusercurvdimeaxisleftrighbotttop labe')
C
      GO TO (156,157,158,159,160,161,161,161,161,165,901) , IKWL
C
C PLOT (GRAPH) WINDOW EDGES.
C
  156 LOPA=337+10*LBIM-80+6*LNIM-96
      NIPA=4
      GO TO 203
C
C USER WINDOW PARAMETERS.
C
  157 LOPA=341+10*LBIM-80+6*LNIM-96
      NIPA=4
      GO TO 203
C
C CURVE WINDOW PARAMETERS.
C
  158 LOPA=345+10*LBIM-80+6*LNIM-96
      NIPA=4
      GO TO 203
C
C CURVE WINDOW DIMENSIONS.
C
  159 LOPA=349+10*LBIM-80+6*LNIM-96
      NIPA=3
      GO TO 203
C
C AXIS PARAMETERS.
C
  160 LOPA=352+10*LBIM-80+6*LNIM-96
      NIPA=80
      IF (TPID(IPID:IPID).EQ.'.') GO TO 203
C
      CALL AGSRCH (TPID,IPID,IKWL,'LEFTRIGHBOTTTOP ',
     +                            'leftrighbotttop ')
C
      IF (IKWL.EQ.5) GO TO 901
C
      IKWL=IKWL+5
C
C LEFT, RIGHT, BOTTOM, OR TOP AXIS PARAMETERS.
C
  161 LOPA=346+10*LBIM-80+6*LNIM-96+IKWL
      NIPA=20
      IIPA=4
      IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGSRCH (TPID,IPID,IKWL,'POSITICKNUME',
     +                            'positicknume')
C
      GO TO (162,163,164,901) , IKWL
C
C AXIS POSITIONING PARAMETERS.
C
  162 NIPA=6
      GO TO 203
C
C AXIS TICK PARAMETERS.
C
  163 LOPA=LOPA+24
      NIPA=3
      GO TO 203
C
C AXIS NUMERIC-LABEL PARAMETERS.
C
  164 LOPA=LOPA+36
      NIPA=11
      GO TO 203
C
C LABEL BOXES.
C
  165 LOPA=432+10*LBIM-80+6*LNIM-96
      NIPA=54
      IF (TPID(IPID:IPID).EQ.'.') GO TO 203
C
      CALL AGSRCH (TPID,IPID,IKWL,'LEFTRIGHBOTTTOP CENTGRAP',
     +                            'leftrighbotttop centgrap')
C
      IF (IKWL.EQ.7) GO TO 901
C
      LOPA=LOPA+IKWL-1
      NIPA=9
      IIPA=6
      GO TO 203
C
C Normal exits.
C
  201 LOPA=LOPA+(IKWL-1)*IIPA
C
  202 NIPA=1
C
  203 IF (TPID(IPID:IPID).EQ.'.') RETURN
C
      CALL AGPPID (TPID)
      WRITE (I1MACH(4),1001)
      RETURN
C
C Error exits.
C
  901 CALL AGPPID (TPID)
      CALL SETER ('AGGETP OR AGSETP - ILLEGAL KEYWORD USED IN PARAMETER
     +IDENTIFIER',11,2)
C
  902 CALL AGPPID (TPID)
      CALL SETER ('AGGETP OR AGSETP - ATTEMPT TO ACCESS LABEL ATTRIBUTES
     + BEFORE SETTING LABEL NAME',12,2)
C
  903 CALL AGPPID (TPID)
      CALL SETER ('AGGETP OR AGSETP - ATTEMPT TO ACCESS LINE ATTRIBUTES
     +BEFORE SETTING LINE NUMBER',13,2)
C
C Formats.
C
 1001 FORMAT (' WARNING - ABOVE PARAMETER IDENTIFIER HAS TOO MANY KEYWOR
     +DS')
C
      END
