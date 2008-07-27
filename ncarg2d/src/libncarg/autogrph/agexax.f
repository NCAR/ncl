C
C $Id: agexax.f,v 1.9 2008-07-27 00:14:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGEXAX (IAXS,SVAL,UMIN,UMAX,NICE,QLUA,FUNS,QBTP,BASD,
     +                   BASE,QMJD,QMND,QMNT,QLTD,QLTP,QLED,QLEX,QLFD,
     +                   QLFL,QMIN,QMAX)
C
      DIMENSION SVAL(2)
C
C The routine AGEXAX is used by AGSTUP to examine the parameters which
C determine how a given axis is tick-marked and labelled and to provide
C default values for missing ones.  Its arguments are as follows:
C
C -- IAXS is the number of the axis being drawn - 1, 2, 3, or 4.
C
C -- SVAL is the array of special values.
C
C -- UMIN and UMAX are the minimum and maximum values along the axis, in
C    the user coordinate system.  Rounded values of UMIN and UMAX are
C    returned in QMIN and QMAX if the following argument (NICE) is zero.
C
C -- NICE is a flag indicating whether rounded values of UMIN and UMAX
C    are to be returned (NICE.EQ.0) or not (NICE.NE.0).
C
C -- LLUA and FUNS specify the user-system-to-label-system mapping along
C    the axis.  See the routine AGAXIS for a discussion of them.
C
C -- NBTP, BASD, BASE, and NMJD are used to determine the positioning of
C    major tick marks in the label coordinate system.  NBTP and BASE are
C    described in the routine AGNUMB.  BASD is the desired value of BASE
C    supplied by the user.  If BASD has a null value, BASE is computed
C    by AGEXAX.  NMJD is a user-supplied-or-defaulted parameter giving
C    the approximate number of major ticks (and therefore the number of
C    numeric labels) to be placed on the axis.
C
C -- NMND and NMNT are the desired and actual (to be determined) number
C    of minor ticks per major division.  See discussion in AGAXIS.
C
C -- NLTD, NLTP, NLED, NLEX, NLFD, and NLFL are desired and actual (to
C    be determined) values of the parameters describing the form to be
C    used for numeric labels.  See discussion in AGNUMB.
C
C -- QMIN and QMAX are rounded values of UMIN and UMAX, returned only if
C    NICE.EQ.0.
C
C The following common block contains AUTOGRAPH variables which are
C not control parameters.  The only one used here is SMRL, which is a
C (machine-dependent) small real which, when added to a number in the
C range (1,10), will round it upward without seriously affecting the
C leading significant digits.  The object of this is to get rid of
C strings of nines.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
      SAVE /AGORIP/
C
C The following common block contains the new variable SMLL, which is
C essentially the smallest positive real number X such that "1.+X" is
C different from "1.".  This is used for some purposes for which the
C variable SMRL was previously used.  (10/21/89)
C
      COMMON /AGORIQ/ SMLL
      SAVE /AGORIQ/
C
C The arrays BASP and NMNP specify possible default values for BASE and
C NMNT when NBTP.EQ.1.
C
      DIMENSION BASP(5),NMNP(5)
C
      DATA BASP(1) / 10. / , NMNP(1) / 1 / ,
     *     BASP(2) /  5. / , NMNP(2) / 4 / ,
     *     BASP(3) /  2. / , NMNP(3) / 1 / ,
     *     BASP(4) /  1. / , NMNP(4) / 1 / ,
     *     BASP(5) /  .5 / , NMNP(5) / 4 /
C
C If the parameter NBTP is zero, tick marks and labels are suppressed.
C
      NBTP=IFIXMM(QBTP)
      IF (NBTP.EQ.0) RETURN
C
C Unpack integer values from floating-point arguments.
C
      LLUA=IFIXMM(QLUA)
      NMJD=IFIXMM(QMJD)
      NMND=IFIXMM(QMND)
      NMNT=0
      NLTD=IFIXMM(QLTD)
      NLTP=0
      NLED=IFIXMM(QLED)
      NLEX=0
      NLFD=IFIXMM(QLFD)
      NLFL=0
C
C Compute label-coordinate-system values at the ends of the axis.
C
      CALL AGUTOL (IAXS,FUNS,1,UMIN,VMIN)
      CALL AGUTOL (IAXS,FUNS,1,UMAX,VMAX)
C
C Error if the label-coordinate-system values are equal.
C
      IF (VMIN.EQ.VMAX) GO TO 901
C
C If a special value is specified for the parameter BASD, AGEXAX must
C pick a value for the parameter BASE.
C
      IF (BASD.EQ.SVAL(1).OR.BASD.EQ.SVAL(2)) GO TO 101
C
C The user has specified a value for the parameter BASE.  If that value
C is less than or equal to zero, tick marks and labels are suppressed.
C
      BASE=MAX(0.,BASD)
      IF (BASE.EQ.0.) RETURN
      NMNT=0
      GO TO 108
C
C Pick a value for the parameter BASE, depending on the number type.
C
  101 GO TO (102,105,106) , NBTP
C
C Major ticks and labels are at numbers of the form (-) BASE * EXMU.
C
  102 NMJD=MAX(0,NMJD)
C
C Compute an approximate value for BASE.
C
      FTMP=ABS(VMAX-VMIN)/REAL(NMJD+1)
C
C Reduce the approximate value to the form FTMP * 10 ** ITMP.
C
      JMP1=1
      GO TO 200
C
C Pick a reasonable value for BASE (1., 2., OR 5. * 10**ITMP).
C
  103 DO 104 I=1,5
        IF (FTMP.LT.BASP(I)) GO TO 104
        BASE=REAL(DBLE(BASP(I))*10.D0**ITMP)
        NMNT=NMNP(I)
        GO TO 107
  104 CONTINUE
C
C Major ticks and labels are at numbers of the form (-) BASE * 10**EXMU.
C
  105 BASE=1.
      NMNT=8
      GO TO 107
C
C Major ticks and labels are at numbers of the form (-) BASE**EXMU.
C
  106 BASE=10.
      NMNT=8
C
  107 IF (BASD.EQ.SVAL(2)) BASD=BASE
C
  108 IF (QMND.NE.SVAL(1).AND.QMND.NE.SVAL(2)) NMNT=MAX(0,NMND)
      IF (QMND.EQ.SVAL(2)) QMND=REAL(NMNT)
C
C If the user wants nice values at the axis ends, reset UMIN and UMAX.
C
      IF (NICE.NE.0) GO TO 115
C
      LOOP=0
C
      WMIN=VMIN
      WMAX=VMAX
C
      GO TO (109,110,112) , NBTP
C
  109 EMIN=VMIN/BASE+.5+SIGN(.5,VMIN-VMAX)
      EMAX=VMAX/BASE+.5+SIGN(.5,VMAX-VMIN)
      EMIN=EMIN-.5+SIGN(.5,EMIN)-SIGN(SMRL*EMIN,VMIN-VMAX)
     +                                                +1.E-6*(EMAX-EMIN)
      EMAX=EMAX-.5+SIGN(.5,EMAX)-SIGN(SMRL*EMAX,VMAX-VMIN)
     +                                                -1.E-6*(EMAX-EMIN)
      WMIN=BASE*(EMIN-MOD(EMIN,1.))
      WMAX=BASE*(EMAX-MOD(EMAX,1.))
      GO TO 114
C
  110 IF ((VMIN.LT.0..AND.VMAX.LT.0.).OR.(VMIN.GT.0..AND.VMAX.GT.0.))
     +                                                              THEN
        EMIN=ALOG10(ABS(VMIN)/BASE)+.5+SIGN(.5,VMIN)*SIGN(1.,VMIN-VMAX)
        EMAX=ALOG10(ABS(VMAX)/BASE)+.5+SIGN(.5,VMAX)*SIGN(1.,VMAX-VMIN)
        EMIN=EMIN-.5+SIGN(.5,EMIN)
     +        -SIGN(SMRL*EMIN,VMIN)*SIGN(1.,VMIN-VMAX)+1.E-6*(EMAX-EMIN)
        EMAX=EMAX-.5+SIGN(.5,EMAX)
     +        -SIGN(SMRL*EMAX,VMAX)*SIGN(1.,VMAX-VMIN)-1.E-6*(EMAX-EMIN)
        WMIN=SIGN(BASE,VMIN)*10.**(EMIN-MOD(EMIN,1.))
        WMAX=SIGN(BASE,VMAX)*10.**(EMAX-MOD(EMAX,1.))
      ELSE
        IF (VMIN.NE.0.) THEN
          EMIN=ALOG10(ABS(VMIN)/BASE)+.5+SIGN(.5,VMIN)*
     +                                                SIGN(1.,VMIN-VMAX)
          EMIN=EMIN-.5+SIGN(.5,EMIN)-SIGN(SMRL*EMIN,VMIN)*
     +                                                SIGN(1.,VMIN-VMAX)
          WMIN=SIGN(BASE,VMIN)*10.**(EMIN-MOD(EMIN,1.))
        END IF
        IF (VMAX.NE.0.) THEN
          EMAX=ALOG10(ABS(VMAX)/BASE)+.5+SIGN(.5,VMAX)*
     +                                                SIGN(1.,VMAX-VMIN)
          EMAX=EMAX-.5+SIGN(.5,EMAX)-SIGN(SMRL*EMAX,VMAX)*
     +                                                SIGN(1.,VMAX-VMIN)
          WMAX=SIGN(BASE,VMAX)*10.**(EMAX-MOD(EMAX,1.))
        END IF
      END IF
      GO TO 114
C
  112 IF (BASE.EQ.1.) GO TO 115
      IF (VMIN*VMAX.GT.0.) THEN
        EMIN=ALOG10(ABS(VMIN))/ALOG10(BASE)+.5+SIGN(.5,VMIN)*
     +                                                SIGN(1.,VMIN-VMAX)
        EMAX=ALOG10(ABS(VMAX))/ALOG10(BASE)+.5+SIGN(.5,VMAX)*
     +                                                SIGN(1.,VMAX-VMIN)
        EMIN=EMIN-.5+SIGN(.5,EMIN)-SIGN(SMRL*EMIN,VMIN)*
     +                              SIGN(1.,VMIN-VMAX)+1.E-6*(EMAX-EMIN)
        EMAX=EMAX-.5+SIGN(.5,EMAX)-SIGN(SMRL*EMAX,VMAX)*
     +                              SIGN(1.,VMAX-VMIN)-1.E-6*(EMAX-EMIN)
        WMIN=SIGN(1.,VMIN)*BASE**(EMIN-MOD(EMIN,1.))
        WMAX=SIGN(1.,VMAX)*BASE**(EMAX-MOD(EMAX,1.))
      ELSE
        IF (VMIN.NE.0.) THEN
          EMIN=ALOG10(ABS(VMIN))/ALOG10(BASE)
     +                              +.5+SIGN(.5,VMIN)*SIGN(1.,VMIN-VMAX)
          EMIN=EMIN-.5+SIGN(.5,EMIN)-SIGN(SMRL*EMIN,VMIN)*
     +                                                SIGN(1.,VMIN-VMAX)
          WMIN=SIGN(1.,VMIN)*BASE**(EMIN-MOD(EMIN,1.))
        END IF
        IF (VMAX.NE.0.) THEN
          EMAX=ALOG10(ABS(VMAX))/ALOG10(BASE)
     +                              +.5+SIGN(.5,VMAX)*SIGN(1.,VMAX-VMIN)
          EMAX=EMAX-.5+SIGN(.5,EMAX)-SIGN(SMRL*EMAX,VMAX)*
     +                                                SIGN(1.,VMAX-VMIN)
          WMAX=SIGN(1.,VMAX)*BASE**(EMAX-MOD(EMAX,1.))
        END IF
      END IF
C
C Re-compute the user-coordinate-system minimum and maximum values.
C
  114 CALL AGUTOL (IAXS,FUNS,-1,WMIN,QMIN)
      CALL AGUTOL (IAXS,FUNS,-1,WMAX,QMAX)
C
C Test for problems with nice values chosen.
C
      IF (QMIN.LT.QMAX) GO TO 140
      IF (QMIN.GT.QMAX) GO TO 901
C
C We have a pathological case - user values are clustered very close to
C a label position.  See what can be done about it.
C
      LOOP=LOOP+1
      IF (LOOP.GT.1) GO TO 901
C
      GO TO (137,138,139) , NBTP
C
  137 VMIN=VMIN+SIGN(BASE,VMIN-VMAX)
      VMAX=VMAX+SIGN(BASE,VMAX-VMIN)
      GO TO 109
C
  138 VMIN=VMIN*10.**(SIGN(1.,VMIN)*SIGN(1.,VMIN-VMAX))
      VMAX=VMAX*10.**(SIGN(1.,VMAX)*SIGN(1.,VMAX-VMIN))
      GO TO 110
C
  139 VMIN=VMIN*BASE**(SIGN(1.,VMIN)*SIGN(1.,VMIN-VMAX))
      VMAX=VMAX*BASE**(SIGN(1.,VMAX)*SIGN(1.,VMAX-VMIN))
      GO TO 112
C
  140 VMIN=WMIN
      VMAX=WMAX
C
C Now we examine the parameters defining the appearance of the numeric
C labels.  If the numeric-label type is zero, there is no more to do.
C
  115 IF (QLTD.EQ.SVAL(1).OR.QLTD.EQ.SVAL(2)) GO TO 116
      NLTP=MAX(0,MIN(3,NLTD))
      IF (NLTP.EQ.0) GO TO 136
C
C The numeric-label type (NLTP) is specified.  If both the numeric-label
C exponent and numeric-label fraction-length are also specified, quit.
C
      NLEX=NLED
      NLFL=NLFD
      IF (QLED.NE.SVAL(1).AND.QLED.NE.SVAL(2).AND.
     +    QLFD.NE.SVAL(1).AND.QLFD.NE.SVAL(2)       ) GO TO 136
      GO TO 117
C
C We must pick a value for the numeric-label type.  Start with the dummy
C value 4 so as to jump to the proper piece of code.
C
  116 NLTP=4
C
C Reduce the value of BASE to the form RBSE * 10**KBSE, where RBSE is
C in the range (1,10) and KBSE is an integer.
C
  117 FTMP=BASE
      JMP1=2
      GO TO 200
C
  118 RBSE=FTMP
      KBSE=ITMP
C
C Compute LBSE = the number of significant digits in RBSE.
C
      GO TO 300
C
  119 LBSE=1+ITMP
C
C Jump depending on the value of the numeric-label type.
C
      GO TO (120,128,131,132) , NLTP
C
C Scientific notation is to be used.  Estimate the number of significant
C digits that are likely to be required, depending on the number type.
C
  120 GO TO (121,123,124) , NBTP
C
  121 FTMP=MAX(ABS(VMIN),ABS(VMAX))/BASE
      JMP1=3
      GO TO 200
C
  122 NSIG=MAX(1,ITMP+1+LBSE)
      GO TO 125
C
  123 NSIG=LBSE
      GO TO 125
C
  124 NSIG=10
C
C NLEX + NLFL should be equal to NSIG.  Make that the case.
C
  125 IF (QLED.NE.SVAL(1).AND.QLED.NE.SVAL(2)) GO TO 127
      IF (QLFD.EQ.SVAL(1).OR. QLFD.EQ.SVAL(2)) GO TO 126
      NLEX=NSIG-MAX(0,NLFL)
      GO TO 135
  126 NLEX=1
  127 NLFL=NSIG-NLEX
      IF (NLFL.LE.0) NLFL=-1
      GO TO 135
C
C Exponential notation is to be used.  Compute the exponent NEXP such
C that BASE / 10**NEXP is an integer.
C
  128 NEXP=KBSE-LBSE+1
C
C NLEX - NLFL should be equal to NEXP.  Make that the case.  (Note that,
C if NBTP is 3, NLEX is forced to zero.)
C
      IF (NBTP.EQ.3) NLEX=0
C
      IF (QLFD.NE.SVAL(1).AND.QLFD.NE.SVAL(2)) GO TO 129
      IF (QLED.NE.SVAL(1).AND.QLED.NE.SVAL(2)) GO TO 130
      NLFL=-1
  129 NLEX=MAX(0,NLFL)+NEXP
      GO TO 135
  130 NLFL=NLEX-NEXP
      IF (NLFL.LE.0) NLFL=-1
      GO TO 135
C
C No-exponent notation is to be used.  NLFL is the only parameter we
C need to worry about.  If it is already set, quit.
C
  131 IF (QLFD.NE.SVAL(1).AND.QLFD.NE.SVAL(2)) GO TO 136
C
C Set NLFL to the actual number of digits in the fractional portion of
C BASE.
C
      NLFL=LBSE-KBSE-1
      IF (NLFL.LE.0) NLFL=-1
      GO TO 135
C
C We must pick a value for the numeric-label type, depending on the
C number type.
C
  132 GO TO (133,134,134) , NBTP
C
C Nunbers are of the form (-) BASE * EXMU.  Use labels with no exponent
C unless the use of an exponent would result in shorter labels.
C
  133 IF (MAX(KBSE+1-LBSE,-KBSE-1).GT.4) GO TO 134
      NLTP=3
      NLFL=LBSE-KBSE-1
      IF (NLFL.LE.0) NLFL=-1
      GO TO 135
C
C Exponential notation is used.
C
  134 NLTP=2
      NLEX=KBSE-LBSE+1
      NLFL=-1
C
C Back-store the computed parameters, if requested, and return.
C
  135 IF (QLTD.EQ.SVAL(2)) QLTD=REAL(NLTP)
      IF (QLED.EQ.SVAL(2)) QLED=REAL(NLEX)
      IF (QLFD.EQ.SVAL(2)) QLFD=REAL(NLFL)
C
C Pack up integer values to floating-point arguments and return.
C
  136 QMNT=REAL(NMNT)
      QLTP=REAL(NLTP)
      QLEX=REAL(NLEX)
      QLFL=REAL(NLFL)
      RETURN
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C This internal procedure reduces the number (FTMP) to the range (1,10),
C returning (FTMP) and (ITMP) such that (FTMP) * 10**(ITMP) is equal to
C the original value of (FTMP).  (FTMP) must be positive.
C
  200 FTM1=ALOG10(FTMP+SMRL*FTMP)
      IF (FTM1.LT.0.) FTM1=FTM1-1.
      ITMP=INT(FTM1)
      FTMP=MAX(1.,FTMP*REAL(10.D0**(-ITMP)))
      GO TO (103,118,122) , JMP1
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C This internal procedure counts the number of digits in the fractional
C portion of (FTMP), returning the count as the value of (ITMP).
C
  300 FTM1=MOD(FTMP+SMRL*FTMP,1.)
      FTM2=10.*SMRL*FTMP
      ITMP=0
C
  301 IF (FTM1.LT.FTM2) GO TO 302
      ITMP=ITMP+1
      IF (ITMP.GE.10) GO TO 302
      FTM1=MOD(10.*FTM1,1.)
      FTM2=10.*FTM2
      GO TO 301
C
  302 GO TO 119
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C Error exit.
C
  901 WRITE (I1MACH(4),9001) IAXS
      CALL SETER ('AGEXAX (CALLED BY AGSTUP) - USER-SYSTEM-TO-LABEL-SYST
     +EM MAPPING IS NOT MONOTONIC',1,2)
C
C Formats.
C
 9001 FORMAT ('0PROBLEM WITH AXIS NUMBER',I2,
     +        ' (1, 2, 3, AND 4 IMPLY LEFT, RIGHT, BOTTOM, AND TOP)')
C
      END
