C
C $Id: agnumb.f,v 1.7 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGNUMB (NBTP,SBSE,EXMU , NLTP,NLEX,NLFL ,
     +                             BFRM,MCIM,NCIM,IPXM , BFRE,MCIE,NCIE)
C
      CHARACTER*(*) BFRM,BFRE
C
C The routine AGNUMB converts the number specified by the arguments
C NBTP, SBSE, and EXMU to the label format specified by the arguments
C NLTP, NLEX, and NLFL, returning the characters of the mantissa in the
C buffer BFRM and the characters of the exponent in the buffer BFRE,
C ready for plotting.  The arguments of AGNUMB are as follows:
C
C -- NBTP is an integer specifying the type of number to be converted.
C    There are three possibilities:
C
C        NBTP = 1 - number of the form SBSE * EXMU.
C
C        NBTP = 2 - number of the form SBSE * 10**EXMU.
C
C        NBTP = 3 - number of the form SIGN(SBSE) * ABSV(SBSE)**EXMU.
C
C -- SBSE is a base value for a set of labels.  See NBTP description.
C
C -- EXMU is an exponent or a multiplier for a given label.  Although it
C    is a floating-point number, its value should be integral, unless
C    NBTP equals 1 and/or NLTP equals 1.  Using a non-integral EXMU in
C    other cases will have undesirable effects.  See NBTP description.
C
C -- NLTP is an integer specifying the type of label to be generated.
C    There are three possibilities:
C
C      -- NLTP = 1 - label is to have an exponent portion and is to be
C         expressed in scientific notation.
C
C      -- NLTP = 2 - label is to have an exponent portion and is to be
C         expressed in a form determined by the number type NBTP.
C
C      -- NLTP = 3 - label is to have no exponent portion and is to be
C         expressed in a form determined by the number type NBTP.
C
C    The possible label types will be described in greater detail below.
C
C -- NLEX (when used) is an integer specifying (in a manner depending on
C    the values of other parameters) the value of the exponent portion
C    of the label.  See the detailed discussion of label types, below.
C
C -- NLFL (when used) is an integer specifying (in a manner depending on
C    the values of other parameters) the length of the fractional por-
C    tion of the mantissa of the label.  See the detailed discussion of
C    label types, below.
C
C -- BFRM is a character variable in which the mantissa portion of the
C    label is to be returned.
C
C -- MCIM specifies the maximum number of characters BFRM can hold.
C
C -- NCIM is the number of characters returned in BFRM by AGNUMB.
C
C -- IPXM is the position of the character X in the mantissa.  If IPXM
C    is zero, the character X does not occur in the mantissa.
C
C -- BFRE, MCIE, and NCIE are analogous to BFRM, MCIM, and NCIM, but
C    pertain to the exponent portion of the label.
C
C Label types:  AGNUMB will produce many different types of labels, as
C directed by the various input parameters.  Each of these is described
C below.  The general form of a label is
C
C    (-) (1/) (I) (.) (F) (X 10) (E)
C
C where the parentheses are used to mark portions which may either be
C present or absent.  The minus sign is included only if the label value
C is negative.  I is the integer portion of the mantissa, included only
C if its value is non-zero.  The decimal point is included if the input
C parameter NLFL does not specifically direct that it should be omitted
C or if the fractional portion of the mantissa (F) is present.  F is the
C fractional portion of the mantissa.  The "X 10" is included if it is
C appropriate, and is considered to be a part of the mantissa; if it is
C included, a blank is actually returned for the character X, so the
C routine which plots the label should construct this character by
C drawing two short lines.  E is the exponent, returned in a separate
C buffer so that it may be plotted in a superscript form.  The possible
C label types are, then, as follows:
C
C -- Scientific notation - if the label type NLTP equals 1, the form
C
C        (-) (I) (.) (F) X 10 (E)
C
C    is used.  NLEX specifies the length of I (thus also specifying the
C    value of the exponent E).  If NLEX is .LE. 0, I is omitted.  If
C    NLEX is .LT. 0 and has the absolute value N, the fraction F is
C    forced to have N leading zeroes.  NLFL specifies the length of F.
C    If NLFL is .LE. 0, F is omitted.  If NLFL is .LT. 0, the decimal
C    point is omitted.  If (I.F) has the value 1, (I.F X) is omitted.
C    If the entire label has zero value, the character 0 is used.
C
C -- Exponential, but non-scientific notation - if the label type NLTP
C    equals 2, the form used depends on the argument NBTP, as follows:
C
C     -- If NBTP equals 1 (number of the form SBSE * EXMU), the form
C
C            (-) (I) (.) (F) X 10 (E)
C
C        is used.  NLEX specifies the value of the exponent E.  The
C        length of F is specified by NLFL.  If NLFL is .LE. 0, F is
C        omitted.  If NLFL is .LT. 0, the decimal point is omitted.  If
C        the label value is exactly 0, the character 0 is used.
C
C     -- If NBTP equals 2 (number of the form SBSE*10**EXMU), the form
C
C            (-) (I) (.) (F) X 10 (E)
C
C        is used.  The exponent E has the value NLEX+EXMU.  The length
C        of F is specified by NLFL.  If NLFL is .LE. 0, F is omitted.
C        If NLFL is .LT. 0, the decimal point is omitted.  If the label
C        value is exactly 0, the character 0 is used.  If (I.F) has the
C        value 1., then (I.F X) is omitted.
C
C     -- If NBTP equals 3, specifying that the number is of the form
C        SIGN(SBSE) * ABSV(SBSE)**EXMU, the form
C
C            (-) (I) (.) (F) (E)
C
C        is used.  The exponent E has the value EXMU.  The length of F
C        is specified by NLFL.  If NLFL is .LE. 0, F is omitted.  If
C        NLFL is .LT. 0, the decimal point is omitted.
C
C -- No-exponent notation - if the label type NLTP equals 3, the form
C    used depends on the argument NBTP, as follows:
C
C     -- If NBTP equals 1 (number of the form SBSE * EXMU), the form
C
C            (-) (I) (.) (F)
C
C        is used.  NLFL specifies the length of F.  If NLFL is .LE. 0,
C        F is omitted.  If NLFL is .LT. 0, the decimal point is omitted.
C        If the entire label has zero value, the character 0 is used.
C
C     -- If NBTP equals 2 (number of the form SBSE*10**EXMU), the form
C
C            (-) (I) (.) (F)
C
C        is used.  The length of F is specified by the function
C
C            MAX(NLFL,0)-EXMU  (if EXMU is .LT. MAX(NLFL,0))
C            MIN(NLFL,0)       (if EXMU is .GE. MAX(NLFL,0))
C
C        which may appear somewhat formidable, but produces a simple,
C        desirable result.  Suppose, for example, that SBSE = 3.6,
C        NLFL = 1, and EXMU ranges from -3 to +3 - the labels produced
C        are as follows:
C
C            .0036   .036   .36   3.6   36.   360.   3600.
C
C        NLFL may be viewed as specifying the length of F if EXMU is 0.
C        If the value of the function is .LE. 0, F is omitted - if its
C        value is .LT. 0, the decimal point is omitted.
C
C     -- If NBTP equals 3, specifying that the number is of the form
C        SIGN(SBSE) * ABSV(SBSE)**EXMU, the form
C
C            (-) (I) (.) (F)
C
C        is used if EXMU is positive (or zero), and the form
C
C            (-) 1 / (I) (.) (F)
C
C        is used if EXMU is negative.  The length of F is specified by
C        the function
C
C            NLFL * ABSV(EXMU)  (if EXMU is .NE. 0)
C            MIN(NLFL,0)        (if EXMU is .EQ. 0)
C
C        Again, this function produces a simple result.  Suppose that
C        SBSE = 1.1, NLFL = 1, and EXMU ranges from -3 to +3 - the
C        labels produced are as follows:
C
C            1/1.331   1/1.21   1/1.1   1.   1.1   1.21   1.331
C
C        NLFL may be viewed as specifying the length of F if EXMU is 1.
C        If the value of the function is .LE. 0, F is omitted - if its
C        value is .LT. 0, the decimal point is omitted.  As another
C        example, suppose that SBSE = 2., NLFL = -1, and EXMU ranges
C        from -4 to +4.  The labels produced are as follows:
C
C            1/16   1/8   1/4   1/2   1   2   4   8   16
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
C KHAR holds single characters to be stored away in BFRM or BFRE.
C
      CHARACTER*1 KHAR
C
C Zero character counters and pointers.
C
      NCIM=0
      NCIE=0
      IPXM=0
C
C Compute a jump parameter to allow a quick sorting-out of the possible
C number-type/label-type combinations below.
C
      NTLT=NBTP+3*(NLTP-1)
C
C Compute the value (XMAN) from which the characters of the mantissa
C will be generated.
C
      GO TO (101,102,103,101,102,104,101,102,105) , NTLT
C
  101 XMAN=SBSE*EXMU
      GO TO 106
C
  102 XMAN=SBSE*REAL(10.D0**DBLE(EXMU))
      GO TO 106
C
  103 XMAN=SIGN(1.,SBSE)*REAL(DBLE(ABS(SBSE))**DBLE(EXMU))
      GO TO 106
C
  104 XMAN=SBSE
      GO TO 106
C
  105 XMAN=SIGN(1.,SBSE)*REAL(DBLE(ABS(SBSE))**DBLE(ABS(EXMU)))
C
C If the mantissa-generator is negative, make it positive and put a
C minus sign in the mantissa buffer.
C
  106 IF (XMAN.LT.0.) THEN
        NCIM=NCIM+1
        IF (NCIM.GT.MCIM) GO TO 901
        BFRM(NCIM:NCIM)='-'
        XMAN=-XMAN
      END IF
C
C If the number is zero, put a zero in the mantissa buffer and quit.
C
      IF (XMAN.EQ.0.) THEN
        NCIM=NCIM+1
        IF (NCIM.GT.MCIM) GO TO 901
        BFRM(NCIM:NCIM)='0'
        RETURN
      END IF
C
C Reduce the mantissa-generator to the range (1.,10.), keeping track of
C the power of 10 required to do it.  Round the result, keeping in mind
C that the rounding may kick the value past 10. .
C
      IMAN=INT(ALOG10(XMAN))
      IF (XMAN.LT.1.) IMAN=IMAN-1
      XMAN=REAL(DBLE(XMAN)*10.D0**(-IMAN))+SMRL
      IF (XMAN.GE.10.) THEN
        XMAN=XMAN/10.
        IMAN=IMAN+1
      END IF
C
C Jump (depending on the number-type/label-type combination) to set up
C the label-generation control parameters, as follows:
C
C    NDPD - number of digits to precede decimal point - if NDPD .LT. 0,
C           ABS(NDPD) leading zeroes follow the decimal point, preceding
C           the first digit generated from XMAN.
C    NDFD - number of digits to follow decimal point - if NDFD .LT. 0,
C           the decimal point is suppressed.
C    IF10 - flag, set non-zero to force generation of the (X 10) portion
C           of the label.
C    IFEX - flag, set non-zero to force generation of an exponent.
C    IVEX - value of exponent (if any) - always equals (IMAN+1) - NDPD.
C
      GO TO (107,107,107,108,109,110,111,112,113) , NTLT
C
C Scientific notation.
C
  107 NDPD=NLEX
      NDFD=NLFL
      IF10=1
      IFEX=1
      GO TO 114
C
C Non-scientific exponential notation for SBSE * EXMU.
C
  108 NDPD=IMAN+1-NLEX
      NDFD=NLFL
      IF10=1
      IFEX=1
      GO TO 114
C
C Non-scientific exponential notation for SBSE * 10**EXMU.
C
  109 NDPD=IMAN+1-(NLEX+INT(EXMU+SMRL*EXMU))
      NDFD=NLFL
      IF10=1
      IFEX=1
      GO TO 114
C
C Non-scientific exponential notation for SIGN(SBSE) * ABSV(SBSE)**EXMU.
C
  110 NDPD=IMAN+1
      IMAN=IMAN+INT(EXMU+SMRL*EXMU)
      NDFD=NLFL
      IF10=0
      IFEX=1
      GO TO 115
C
C No-exponent notation for SBSE * EXMU.
C
  111 NDPD=IMAN+1
      NDFD=NLFL
      IF10=0
      IFEX=0
      GO TO 115
C
C No-exponent notation for SBSE * 10**EXMU.
C
  112 NDPD=IMAN+1
      NDFD=MAX(NLFL,0)-INT(EXMU+SMRL*EXMU)
      IF (NDFD.LE.0) NDFD=MIN(NLFL,0)
      IF10=0
      IFEX=0
      GO TO 115
C
C No-exponent notation for SIGN(SBSE) * ABSV(SBSE)**EXMU
C
  113 IF (EXMU.LT.0.) THEN
        NCIM=NCIM+1
        IF (NCIM.GT.MCIM) GO TO 901
        BFRM(NCIM:NCIM)='1'
        NCIM=NCIM+1
        IF (NCIM.GT.MCIM) GO TO 901
        BFRM(NCIM:NCIM)='/'
      END IF
C
      NDPD=IMAN+1
      NDFD=NLFL*INT(ABS(EXMU+SMRL*EXMU))
      IF (NDFD.EQ.0) NDFD=MIN(NLFL,0)
      IF10=0
      IFEX=0
      GO TO 115
C
C If there is an exponent of 10 and the mantissa is precisely 1, omit
C the (I.F X) portion of the mantissa.
C
  114 IF (NDPD.NE.1) GO TO 115
      IF (INT(XMAN).NE.1) GO TO 115
      IF (((XMAN-1.)*10.**MAX(0,NDFD)).GE.1.) GO TO 115
      IVEX=IMAN+1-NDPD
      GO TO 123
C
C Generate the characters of the mantissa (I.F).  Check first for zero-
C or-negative-length error.
C
  115 LMAN=MAX(NDPD,0)+1+MAX(NDFD,-1)
      IF (LMAN.LE.0) GO TO 903
C
C Make sure the mantissa buffer is big enough to hold (I.F).
C
      IF (NCIM+LMAN.GT.MCIM) GO TO 901
C
C Compute the value of the parameter IVEX before changing NDPD.
C
      IVEX=IMAN+1-NDPD
C
C Generate the digits preceding the decimal point, if any.
C
      IF (NDPD.LE.0) GO TO 117
C
      JUMP=1
      GO TO 121
C
  116 NDPD=NDPD-1
      IF (NDPD.NE.0) GO TO 121
C
C Generate the decimal point.
C
  117 KHAR='.'
      JUMP=2
      GO TO 122
C
C Generate leading zeroes, if any, after the decimal point.
C
  118 IF (NDPD.EQ.0) GO TO 120
      KHAR='0'
      JUMP=3
      GO TO 122
C
  119 NDPD=NDPD+1
      IF (NDPD.NE.0) GO TO 122
C
C Generate remaining fractional digits.
C
  120 JUMP=4
C
C Generate a digit from the mantissa-generator.  It is assumed that, for
C n between 1 and 9, ICHAR('n') = ICHAR('n-1') + 1 .
C
  121 IDGT=INT(XMAN)
      KHAR=CHAR(ICHAR('0')+IDGT)
      XMAN=XMAN-REAL(IDGT)
      XMAN=XMAN*10.
C
C Store a digit from KHAR into the mantissa buffer.
C
  122 NCIM=NCIM+1
      BFRM(NCIM:NCIM)=KHAR
C
C Check whether (I.F) is complete.
C
      LMAN=LMAN-1
      IF (LMAN.NE.0) GO TO (116,118,119,121) , JUMP
C
C If appropriate, leave space in the mantissa buffer for the "X" .
C
      IF (IF10.EQ.0) GO TO 124
      NCIM=NCIM+1
      IF (NCIM.GT.MCIM) GO TO 901
      IPXM=NCIM
      BFRM(IPXM:IPXM)=' '
C
C If appropriate, put a "10" in the mantissa buffer.
C
  123 NCIM=NCIM+1
      IF (NCIM.GT.MCIM) GO TO 901
      BFRM(NCIM:NCIM)='1'
      NCIM=NCIM+1
      IF (NCIM.GT.MCIM) GO TO 901
      BFRM(NCIM:NCIM)='0'
C
C If appropriate, generate an exponent in the exponent buffer.
C
  124 IF (IFEX.EQ.0) RETURN
C
      IF (IVEX) 126,125,127
C
  125 NCIE=NCIE+1
      IF (NCIE.GT.MCIE) GO TO 902
      BFRE(NCIE:NCIE)='0'
      RETURN
C
  126 NCIE=NCIE+1
      IF (NCIE.GT.MCIE) GO TO 902
      BFRE(NCIE:NCIE)='-'
      IVEX=-IVEX
C
  127 NCIE=NCIE+1
      IF (IVEX.GE.10) NCIE=NCIE+1
      IF (IVEX.GE.100) NCIE=NCIE+1
      IF (IVEX.GE.1000) NCIE=NCIE+1
      IF (NCIE.GT.MCIE) GO TO 902
C
      DO 128 I=1,4
        J=NCIE+1-I
        BFRE(J:J)=CHAR(ICHAR('0')+MOD(IVEX,10))
        IVEX=IVEX/10
        IF (IVEX.EQ.0) RETURN
  128 CONTINUE
C
      IF (IVEX.NE.0) GO TO 902
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 CALL SETER ('AGNUMB - MANTISSA TOO LONG',4,2)
C
  902 CALL SETER ('AGNUMB - EXPONENT TOO LARGE',5,2)
C
  903 CALL SETER ('AGNUMB - ZERO-LENGTH MANTISSA',6,2)
C
      END
