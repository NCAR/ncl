
      PROGRAM AGEX10
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Define the data arrays.
C
      REAL XDRA(1201),YDRA(1201)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays.  The independent variable represents
C time during the year (a hypothetical year with equal-length
C months) and is set up so that minor ticks can be lengthened
C to delimit the months; the major ticks, though shortened to
C invisibility, still determine where the labels go.
C
      DO 101 I=1,1201
        XDRA(I)=REAL(I-51)
        YDRA(I)=COSH(REAL(I-601)/202.)
  101 CONTINUE
C
C Change the labels on the bottom and left axes.
C
      CALL ANOTAT ('MONTHS OF THE YEAR$',
     +             'ROMAN NUMERALS$',0,0,0,' ')
C
C Fix the minimum and maximum values on both axes and prevent
C AUTOGRAPH from using rounded values at the ends of the axes.
C
      CALL AGSETF ('X/MIN.',-50.)
      CALL AGSETF ('X/MAX.',1150.)
      CALL AGSETI ('X/NICE.',0)
C
      CALL AGSETF ('Y/MIN.',1.)
      CALL AGSETF ('Y/MAX.',10.)
      CALL AGSETI ('Y/NICE.',0)
C
C Specify the spacing between major tick marks on all axes.
C Note that the AUTOGRAPH dummy routine AGCHNL is supplanted
C (below) by one which supplies dates for the bottom axis and
C Roman numerals for the left axis in place of the numeric
C labels one would otherwise get.
C
      CALL AGSETI ('  LEFT/MAJOR/TYPE.',1)
      CALL AGSETI (' RIGHT/MAJOR/TYPE.',1)
      CALL AGSETI ('BOTTOM/MAJOR/TYPE.',1)
      CALL AGSETI ('   TOP/MAJOR/TYPE.',1)
C
      CALL AGSETF ('  LEFT/MAJOR/BASE.',  1.)
      CALL AGSETF (' RIGHT/MAJOR/BASE.',  1.)
      CALL AGSETF ('BOTTOM/MAJOR/BASE.',100.)
      CALL AGSETF ('   TOP/MAJOR/BASE.',100.)
C
C Suppress minor ticks on the left and right axes.
C
      CALL AGSETI ('  LEFT/MINOR/SPACING.',0)
      CALL AGSETI (' RIGHT/MINOR/SPACING.',0)
C
C On the bottom and top axes, put one minor tick between each
C pair of major ticks, shorten major ticks to invisibility,
C and lengthen minor ticks.  The net effect is to make the
C minor ticks delimit the beginning and end of each month,
C while the major ticks, though invisible, cause the names of
C the months to be where we want them.
C
      CALL AGSETI ('BOTTOM/MINOR/SPACING.',1)
      CALL AGSETI ('   TOP/MINOR/SPACING.',1)
C
      CALL AGSETF ('BOTTOM/MAJOR/INWARD. ',0.)
      CALL AGSETF ('BOTTOM/MINOR/INWARD. ',.015)
      CALL AGSETF ('   TOP/MAJOR/INWARD. ',0.)
      CALL AGSETF ('   TOP/MINOR/INWARD. ',.015)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the graph, using EZXY.
C
      CALL EZXY (XDRA,YDRA,1201,
     +           'EXAMPLE 10 (MODIFIED NUMERIC LABELS)$')
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
C
      END
      SUBROUTINE AGCHNL (IAXS,VILS,CHRM,MCIM,NCIM,IPXM,
     +                                  CHRE,MCIE,NCIE)
C
      CHARACTER*(*) CHRM,CHRE
C
C Define the names of the months for use on the bottom axis.
C
      CHARACTER*3 MONS(12)
      DATA MONS / 'JAN','FEB','MAR','APR','MAY','JUN',
     +            'JUL','AUG','SEP','OCT','NOV','DEC'/
C
C Modify the numeric labels on the left axis.
C
      IF (IAXS.EQ.1) THEN
        CALL AGCORN (INT(VILS),CHRM,NCIM)
        IPXM=0
        NCIE=0
C
C Modify the numeric labels on the bottom axis.
C
      ELSE IF (IAXS.EQ.3) THEN
        IMON=INT(VILS+.5)/100+1
        CHRM(1:3)=MONS(IMON)
        NCIM=3
        IPXM=0
        NCIE=0
      END IF
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE AGCORN (NTGR,BCRN,NCRN)
C
      CHARACTER*(*) BCRN
C
C This routine receives an integer in NTGR and returns its
C Roman-numeral equivalent in the first NCRN characters of
C the character variable BCRN.  It only works for integers
C within a limited range and it does some rather unorthodox
C things (like using zero and minus).
C
C ICH1, ICH5, and IC10 are character variables used for the
C single-unit, five-unit, and ten-unit symbols at a given
C level.
C
      CHARACTER*1 ICH1,ICH5,IC10
C
C Treat numbers outside the range (-4000,+4000) as infinites.
C
      IF (ABS(NTGR).GE.4000) THEN
        IF (NTGR.GT.0) THEN
          NCRN=5
          BCRN(1:5)='(INF)'
        ELSE
          NCRN=6
          BCRN(1:6)='(-INF)'
        END IF
        RETURN
      END IF
C
C Use a '0' for the zero.  The Romans never had it so good.
C
      IF (NTGR.EQ.0) THEN
        NCRN=1
        BCRN(1:1)='0'
        RETURN
      END IF
C
C Zero the character counter.
C
      NCRN=0
C
C Handle negative integers by prefixing a minus sign.
C
      IF (NTGR.LT.0) THEN
        NCRN=NCRN+1
        BCRN(NCRN:NCRN)='-'
      END IF
C
C Initialize constants.  We'll check for thousands first.
C
      IMOD=10000
      IDIV=1000
      ICH1='M'
C
C Find out how many thousands (hundreds, tens, units) there
C are and jump to the proper code block for each case.
C
  101 INTG=MOD(ABS(NTGR),IMOD)/IDIV
C
      GO TO (107,104,104,104,102,103,103,103,103,106),INTG+1
C
C Four - add ICH1 followed by ICH5.
C
  102 NCRN=NCRN+1
      BCRN(NCRN:NCRN)=ICH1
C
C Five through eight - add ICH5, followed by INTG-5 ICH1's.
C
  103 NCRN=NCRN+1
      BCRN(NCRN:NCRN)=ICH5
C
      INTG=INTG-5
      IF (INTG.LE.0) GO TO 107
C
C One through three - add that many ICH1's.
C
  104 DO 105 I=1,INTG
        NCRN=NCRN+1
        BCRN(NCRN:NCRN)=ICH1
  105 CONTINUE
C
      GO TO 107
C
C Nine - add ICH1, followed by IC10.
C
  106 NCRN=NCRN+1
      BCRN(NCRN:NCRN)=ICH1
      NCRN=NCRN+1
      BCRN(NCRN:NCRN)=IC10
C
C If we're done, exit.
C
  107 IF (IDIV.EQ.1) RETURN
C
C Otherwise, tool up for the next digit and loop back.
C
      IMOD=IMOD/10
      IDIV=IDIV/10
      IC10=ICH1
C
      IF (IDIV.EQ.100) THEN
        ICH5='D'
        ICH1='C'
      ELSE IF (IDIV.EQ.10) THEN
        ICH5='L'
        ICH1='X'
      ELSE
        ICH5='V'
        ICH1='I'
      END IF
C
      GO TO 101
C
      END
      SUBROUTINE BNDARY
C
C Routine to draw the plotter-frame edge.
C
      CALL PLOTIT (    0,    0,0)
      CALL PLOTIT (32767,    0,1)
      CALL PLOTIT (32767,32767,1)
      CALL PLOTIT (    0,32767,1)
      CALL PLOTIT (    0,    0,1)
      RETURN
      END
