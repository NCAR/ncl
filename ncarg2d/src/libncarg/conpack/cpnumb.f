C
C $Id: cpnumb.f,v 1.2 1994-03-17 01:51:28 kennison Exp $
C
      SUBROUTINE CPNUMB (VALU,NDGD,LMSD,IEXP,LEXP,CEX1,CEX2,CEX3,LEX1,
     +                   LEX2,LEX3,IOMA,IODP,IOTZ,CBUF,NBUF,NDGS,IEVA)
C
      CHARACTER*(*) CEX1,CEX2,CEX3,CBUF
C
C This subroutine expresses the value of a real number in a character
C form.  Depending on the values of the arguments, an exponential form
C (for example, "1.36E-2") or a no-exponent form (for example, ".0136")
C may be used.  The arguments are as follows:
C
C VALU is the real number whose value is to be expressed.
C
C NDGD is the desired number of significant digits to be used in the
C character expression of the number.
C
C LMSD is a flag indicating how the leftmost significant digit of VALU
C is to be determined.  VALU may be written in the form
C
C   ... D(3) D(2) D(1) D(0) . D(-1) D(-2) D(-3) D(-4) ...
C
C where, for all integer values of I, D(I) is an integer between 0 and
C 9, inclusive.  There exists an integer ILFT such that D(ILFT) is non-
C zero and, for all I greater than ILFT, D(I) is zero.  The leftmost
C significant digit of VALU is considered to occur in the position
C MAX(ILFT,LMSD).
C
C LMSD may be used to achieve consistency in expressing the values of a
C group of numbers.  For example, suppose that, with NDGD = 3 and LMSD
C = -10000, we get the numbers
C
C   5.00, 10.0, 15.0, ..., 95.0, 100., 105.              (no exponents)
C   5.00E0, 1.00E1, 1.50E1, ..., 9.50E1, 1.00E2, 1.05E2  (exponents)
C
C By resetting LMSD to 2 (which is the position of the leftmost non-zero
C digit in the whole group), we can get instead
C
C   5., 10., 15., ..., 95., 100., 105.                   (no exponents)
C   0.05E2, 0.10E2, 0.15E2, ..., 0.95E2, 1.00E2, 1.05E2  (exponents)
C
C Whether one prefers to see numbers like those in the first set or the
C second set is to some extent a matter of preference.  The second set
C includes fewer extraneous zeroes and allows the values with exponents
C to be compared with each other more easily.  Note that, in the case of
C the exponential form, LMSD may be viewed as specifying the minimum
C exponent value to be used.  Use LMSD = -10000 to indicate that no
C attempt should be made to force consistency.
C
C IEXP specifies how it is to be decided whether to use the exponential
C form or not, as follows:  If IEXP is less than or equal to zero, the
C exponential form is used, no matter what.  If IEXP is greater than
C zero, the no-exponent form is used if the length of the resulting
C string is less than or equal to IEXP; otherwise, the form resulting
C in the shorter string is used.
C
C LEXP is set less than or equal to zero if exponents are to be written
C in their shortest possible form (plus signs are omitted and the fewest
C digits required to express the value of the exponent are used).  LEXP
C is set greater than zero if exponents are to be written in a manner
C more nearly consistent with one another (the exponent is written with
C either a plus sign or a minus sign and the value of LEXP is the
C desired minimum number of digits to be used, leading zeroes being
C supplied to pad the exponent to the desired length).
C
C CEX1 and CEX2 are character strings to be used in the exponential form
C between the mantissa and the exponent.  If IOMA is non-zero, and, as
C a result, a mantissa exactly equal to one is omitted, CEX1 is omitted
C as well.  Blanks are treated as null strings.  Some possibilities are
C 1) CEX1='E' and CEX2=' ' (or vice-versa), which gives a sort of E
C format (in which case IOMA should not be set non-zero), 2) CEX1='x'
C and CEX2='10**', which gives numbers like "1.36453x10**13", and 3)
C CEX1=':L1:4' and CEX2='10:S:', which generates the function codes
C necessary to make the utility PLCHHQ write the number in exponential
C form.
C
C CEX3 is a character string to be used in the exponential form after
C the exponent.  This will usually be a blank, which is treated as a
C null string; an exception is when function codes for PLCHHQ are being
C generated, in which case it is desirable to use ':N:', in order to
C return to normal level.
C
C LEX1, LEX2, and LEX3 are the lengths to be assumed for the character
C strings CEX1, CEX2, and CEX3 in making decisions about the length of
C the exponential form and the no-exponent form.  (Note that these are
C not the actual lengths of the strings CEX1, CEX2, and CEX3.  If, for
C example, CEX1, CEX2 and CEX3 contain the function codes for PLCHHQ
C mentioned above, use LEX1=1, LEX2=2, and LEX3=0.)
C
C IOMA specifies whether or not it is permissible to omit, from the
C exponential form, mantissas of the form "1" or "1." which are not
C necessary to express the value (as, for example, in "1.x10**2").  If
C IOMA is non-zero, such mantissas are omitted; the part of the exponent
C given by CEX1 (probably the "x" above) is also omitted (thus changing
C "1.x10**2" into "10**2").  Such omission takes place even if IODP
C (which see, below) is zero.
C
C IODP specifies whether or not it is allowed to omit a decimal point
C which is unnecessary (as for example, in "23487.").  If IODP is
C non-zero, such decimal points are omitted.
C
C IOTZ specifies whether or not it is allowed to omit trailing zeroes.
C If IOTZ is non-zero, trailing zeroes are omitted.
C
C CBUF is a character buffer in which the character string is returned.
C If this buffer is not long enough to hold all the characters, no error
C results; the extra characters are simply lost.  This is potentially
C useful, since the object of the call may be simply to obtain the
C number of significant digits and the exponent value.
C
C NBUF is an output parameter; it says how many characters have been
C put into the character buffer CBUF.
C
C NDGS is an output parameter; it contains the number of significant
C digits which were used to express the value of VALU.
C
C IEVA is another output parameter; it is the power to which 10 must be
C raised to obtain a scale factor which will reduce VALU to the range
C from .1 to 1.  That is, the expression "VALU/10.**IEVA" is guaranteed
C (subject to round-off problems) to be greater than or equal to .1 and
C less than 1.  Another way of interpreting IEVA is that it specifies
C the position preceding the leftmost significant digit of VALU (where
C the one's position is numbered 0, the ten's position 1, the hundred's
C position 2, the tenth's position -1, etc.  Thus, the significant
C digits occur in positions IEVA-1 (the leftmost) through IEVA-NDGS
C (the rightmost).
C
C
C Declare all of the CONPACK common blocks.
C
C
C CPCOM1 contains integer and real variables.
C
      COMMON /CPCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CPCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CPCOM1/ CLDT(256),CLEV(256),CLWA(259),CXCF
      COMMON /CPCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DOPT
      COMMON /CPCOM1/ EPSI,FNCM,GRAV,GRSD,GSDM,HCHL,HCHS,IAIA(259)
      COMMON /CPCOM1/ IAIB(256),IBCF,IBHL,IBIL,IBLL,ICAF,ICCF
      COMMON /CPCOM1/ ICCL(259),ICFF,ICHI,ICHL,ICIL,ICLL(256)
      COMMON /CPCOM1/ ICLO,ICLP(256),ICLS,ICLU(259),ICLV,ICLW
      COMMON /CPCOM1/ IDUF,IGCL,IGLB,IGRM,IGRN,IGVS,IHCF,IHLX,IHLY
      COMMON /CPCOM1/ IIWS(2),IIWU,ILBC,IMPF,INCX(8),INCY(8)
      COMMON /CPCOM1/ INHL,INIL,INIT,INLL,IOCF,IOHL,IOLL,IPAI,IPCF
      COMMON /CPCOM1/ IPIC,IPIE,IPIL,IPLL,IRWS(4),IRWU,ISET,IWSO
      COMMON /CPCOM1/ IZD1,IZDM,IZDN,IZDS,JODP,JOMA,JOTZ,LCTM,LEA1
      COMMON /CPCOM1/ LEA2,LEA3,LEE1,LEE2,LEE3,LINS,LINT(10),LINU
      COMMON /CPCOM1/ LIWK,LIWM,LIWS(2),LNLG,LRWC,LRWG,LRWK
      COMMON /CPCOM1/ LRWM,LRWS(4),LSDD,LSDL,LSDM,LTCF,LTHI
      COMMON /CPCOM1/ LTIL,LTLO,MIRO,NCLB(256),NCLV,NDGL,NEXL
      COMMON /CPCOM1/ NEXT,NEXU,NLBS,NLSD,NLZF,NOMF,NOVS,NR04,NSDL
      COMMON /CPCOM1/ NSDR,OORV,SCFS,SCFU,SEGL,SVAL,T2DS,T3DS,UCMN
      COMMON /CPCOM1/ UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL,UWDR
      COMMON /CPCOM1/ UWDT,UXA1,UXAM,UYA1,UYAN,WCCF,WCHL,WCIL,WCLL
      COMMON /CPCOM1/ WLCF,WLHL,WLIL,WLLL,WOCH,WODA,WTCD,WTGR,WTNC
      COMMON /CPCOM1/ WTOD,WWCF,WWHL,WWIL,WWLL,XAT1,XATM,XLBC,XVPL
      COMMON /CPCOM1/ XVPR,XWDL,XWDR,YAT1,YATN,YLBC,YVPB,YVPT,YWDB
      COMMON /CPCOM1/ YWDT,ZDVL,ZMAX,ZMIN
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CPCOM1/
C
C CPCOM2 holds character parameters.
C
      COMMON /CPCOM2/ CHEX,CLBL(256),CLDP(259),CTMA,CTMB,FRMT
      COMMON /CPCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*40 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*40 TXCF
      CHARACTER*20 TXHI
      CHARACTER*100 TXIL
      CHARACTER*20 TXLO
      SAVE   /CPCOM2/
C
C Declare a variable to hold single characters for testing purposes.
C
      CHARACTER*1 SCHR
C
C Find the real lengths of the three parts of the exponent-creating
C string.
C
      LCX1=LEN(CEX1)
      IF (CEX1.EQ.' ') LCX1=0
      LCX2=LEN(CEX2)
      IF (CEX2.EQ.' ') LCX2=0
      LCX3=LEN(CEX3)
      IF (CEX3.EQ.' ') LCX3=0
C
C Find the length of the character buffer and initialize it to blanks.
C
      LBUF=LEN(CBUF)
      CBUF=' '
C
C Use the local I/O routines to generate an E-format representation of
C the number.
C
      WRITE (CTMB(1:NSDR+8),FRMT) VALU
C
C We're about to scan the E-format representation.  Initialize NBUF,
C which is the number of characters put into CBUF, NDGS, which is the
C number of significant digits found in CTMB, IDPT, which is the number
C of the significant digit after which the decimal point was found,
C IEXF, which is a flag indicating whether or not the exponent has been
C found yet, and IRND, which is a rounding flag.
C
      NBUF=0
      NDGS=0
      IDPT=0
      IEXF=0
      IRND=0
C
C Scan the E-format representation.
C
      DO 10001 I=1,NSDR+8
C
C If a minus sign is found, and it's not part of the exponent, put it
C into the user's character buffer.  If it is a part of the exponent,
C set the exponent sign.  On the Cray, large exponents will cause the
C 'E' to be omitted, in which case the sign introduces the exponent.
C
        IF (CTMB(I:I).EQ.'-') THEN
          IF (IEXF.EQ.0) THEN
            IF (NDGS.EQ.0) THEN
              NBUF=NBUF+1
              IF (NBUF.LE.LBUF) CBUF(NBUF:NBUF)='-'
            ELSE
              IEXF=1
              IESI=-1
              IEVA=0
            END IF
          ELSE
            IESI=-1
          END IF
C
C If a plus sign is found, it can usually just be skipped.  On the Cray,
C large exponents will cause the 'E' to be omitted, in which case the
C sign introduces the exponent.
C
        ELSE IF (CTMB(I:I).EQ.'+') THEN
          IF (IEXF.EQ.0.AND.NDGS.NE.0) THEN
            IEXF=1
            IESI=1
            IEVA=0
          END IF
C
C If a digit is found, and it's not a part of the exponent, copy it to
C the beginning of the temporary buffer; save at most NDGD such digits.
C If a digit is found, and it's part of the exponent, update the value
C of the exponent.
C
        ELSE IF (ICHAR(CTMB(I:I)).GE.ICHAR('0').AND.ICHAR(CTMB(I:I)).LE.
     +ICHAR('9')) THEN
          IF (IEXF.EQ.0) THEN
            IF (NDGS.EQ.0) THEN
              IF (CTMB(I:I).NE.'0') THEN
                NDGS=1
                SCHR=CTMB(I:I)
                CTMB(1:1)=SCHR
                NZRS=0
                IF (SCHR.EQ.'9') THEN
                  NNNS=1
                ELSE
                  NNNS=0
                END IF
              ELSE
                IDPT=IDPT-1
              END IF
            ELSE IF (NDGS.LT.NDGD) THEN
              NDGS=NDGS+1
              SCHR=CTMB(I:I)
              CTMB(NDGS:NDGS)=SCHR
              IF (SCHR.EQ.'0') THEN
                NZRS=NZRS+1
                NNNS=0
              ELSE
                NZRS=0
                IF (SCHR.EQ.'9') THEN
                  NNNS=NNNS+1
                ELSE
                  NNNS=0
                END IF
              END IF
            ELSE IF (IRND.EQ.0) THEN
              IRND=1+(ICHAR(CTMB(I:I))-ICHAR('0'))/5
            END IF
          ELSE
            IEVA=10*IEVA+ICHAR(CTMB(I:I))-ICHAR('0')
          END IF
C
C If a decimal point is found, record the index of the digit which it
C followed.
C
        ELSE IF (CTMB(I:I).EQ.'.') THEN
          IDPT=NDGS
C
C If an "E" or an "e" is found, reset the flags to start processing of
C the exponent.
C
        ELSE IF (CTMB(I:I).EQ.'E'.OR.CTMB(I:I).EQ.'e') THEN
          IEXF=1
          IESI=1
          IEVA=0
        END IF
C
10001 CONTINUE
C
C If no significant digits were found, or if no exponent was found,
C assume that the number was exactly zero and return a character string
C reflecting that (unless the use of consistent exponents is forced,
C which requires special action).
C
      IF (NDGS.EQ.0.OR.IEXF.EQ.0) THEN
        IF (IEXP.GT.0.OR.LMSD.EQ.-10000) THEN
          CBUF='0'
          NBUF=1
          NDGS=1
          IEVA=0
          RETURN
        ELSE
          NBUF=0
          GO TO 10003
        END IF
      END IF
C
C Round the number, take care of trailing zeroes and nines, and compute
C the final number of significant digits.
C
      IF (IRND.LT.2) THEN
        IF (NZRS.NE.0) NDGS=NDGS-NZRS
      ELSE
        IF (NNNS.NE.0) NDGS=NDGS-NNNS
        IF (NDGS.EQ.0) THEN
          IDPT=IDPT+1
          CTMB(1:1)='1'
          NDGS=1
        ELSE
          SCHR=CHAR(ICHAR(CTMB(NDGS:NDGS))+1)
          CTMB(NDGS:NDGS)=SCHR
        END IF
      END IF
C
C Compute the final value of the exponent which would be required if
C the decimal point preceded the first significant digit in CTMB.
C
      IEVA=IESI*IEVA+IDPT
C
C If the leftmost significant digit is to the right of the one the user
C wants, supply some leading zeroes and adjust the parameters giving the
C number of digits in CTMB and the exponent value.  We must provide for
C the possibility that this will reduce the number to zero.
C
      IF (IEVA-1.LT.LMSD) THEN
        NLZS=LMSD-(IEVA-1)
        IF (NLZS.LT.NDGD) THEN
          NDGT=MIN(NDGS+NLZS,NDGD)
          DO 10004 I=NDGT,NLZS+1,-1
            SCHR=CTMB(I-NLZS:I-NLZS)
            CTMB(I:I)=SCHR
10004     CONTINUE
          DO 10005 I=1,NLZS
            CTMB(I:I)='0'
10005     CONTINUE
          NDGS=NDGT
          IEVA=LMSD+1
        ELSE
          GO TO 10003
        END IF
      ELSE
        NLZS=0
      END IF
C
C Control arrives at this block to generate a multi-digit zero.
C
      GO TO 10007
10003 CONTINUE
        CTMB(1:1)='0'
        NDGS=1
        NLZS=0
        IEVA=LMSD+1
10007 CONTINUE
C
C Decide how many digits to output.  This depends on whether the user
C wants to omit trailing zeroes or not.
C
      IF (IOTZ.EQ.0) THEN
        NDTO=NDGD
      ELSE
        NDTO=NDGS
      END IF
C
C Compute the lengths of the character strings required for the form
C without an exponent (LWOE) and for the form with an exponent (LWIE).
C In certain cases, the values given are dummies, intended to force the
C use of one form or the other.  Note that leading zeroes are included
C in computing LWOE, even though they may be omitted from the output,
C in order to achieve consistency of sets of labels.
C
      IF (IEXP.GT.0) THEN
        LWOE=NBUF+MAX(NDTO,IEVA)-MIN(IEVA,0)
        IF (IEVA.LE.NLZS.AND.NLZF.NE.0) LWOE=LWOE+1
        IF (IEVA.GE.NDTO.AND.IODP.EQ.0) LWOE=LWOE+1
        IF (LWOE.LE.IEXP) THEN
          LWOE=0
          LWIE=0
        ELSE
          LWIE=NBUF+NDTO+2+LEX1+LEX2+LEX3
          IF (NDTO.EQ.1) THEN
            IF (IOMA.NE.0.AND.CTMB(1:1).EQ.'1') THEN
              LWIE=LWIE-2-LEX1
            ELSE IF (IODP.NE.0) THEN
              LWIE=LWIE-1
            END IF
          END IF
          IF (IEVA-1.LT.0.OR.LEXP.GT.0) LWIE=LWIE+1
          IF (ABS(IEVA-1).GT.9.OR.LEXP.GE.2) LWIE=LWIE+1
          IF (ABS(IEVA-1).GT.99.OR.LEXP.GE.3) LWIE=LWIE+1
          IF (ABS(IEVA-1).GT.999.OR.LEXP.GE.4) LWIE=LWIE+1
        END IF
      ELSE
        LWOE=1
        LWIE=0
      END IF
C
C Depending on the lengths, generate a string without an exponent ...
C
      IF (LWOE.LE.LWIE) THEN
C
        DO 10008 I=MIN(IEVA+1,NLZS+1),MAX(NDTO,IEVA)
          IF (I.EQ.IEVA+1) THEN
            IF (I.LE.NLZS+1.AND.NLZF.NE.0) THEN
              NBUF=NBUF+1
              IF (NBUF.LE.LBUF) CBUF(NBUF:NBUF)='0'
            END IF
            NBUF=NBUF+1
            IF (NBUF.LE.LBUF) CBUF(NBUF:NBUF)='.'
          END IF
          NBUF=NBUF+1
          IF (NBUF.LE.LBUF) THEN
            IF (I.GE.1.AND.I.LE.NDGS) THEN
              CBUF(NBUF:NBUF)=CTMB(I:I)
            ELSE
              CBUF(NBUF:NBUF)='0'
            END IF
          END IF
10008   CONTINUE
C
        IF (IEVA.GE.NDTO.AND.IODP.EQ.0) THEN
          NBUF=NBUF+1
          IF (NBUF.LE.LBUF) CBUF(NBUF:NBUF)='.'
        END IF
C
C ... or a string with an exponent.
C
      ELSE
C
        IF (NDTO.NE.1.OR.CTMB(1:1).NE.'1'.OR.IOMA.EQ.0) THEN
          NBUF=NBUF+1
          IF (NBUF.LE.LBUF) CBUF(NBUF:NBUF)=CTMB(1:1)
        END IF
C
        IF (NDTO.NE.1.OR.((CTMB(1:1).NE.'1'.OR.IOMA.EQ.0).AND.IODP.EQ.0)
     +) THEN
          NBUF=NBUF+1
          IF (NBUF.LE.LBUF) CBUF(NBUF:NBUF)='.'
        END IF
C
        DO 10009 I=2,NDTO
          NBUF=NBUF+1
          IF (NBUF.LE.LBUF) THEN
            IF (I.LE.NDGS) THEN
              CBUF(NBUF:NBUF)=CTMB(I:I)
            ELSE
              CBUF(NBUF:NBUF)='0'
            END IF
          END IF
10009   CONTINUE
C
        IF (LCX1.NE.0.AND.(NDTO.NE.1.OR.CTMB(1:1).NE.'1'.OR.IOMA.EQ.0))
     +THEN
          IF (NBUF.LT.LBUF) CBUF(NBUF+1:MIN(NBUF+LCX1,LBUF))=CEX1
          NBUF=NBUF+LCX1
        END IF
C
        IF (LCX2.NE.0) THEN
          IF (NBUF.LT.LBUF) CBUF(NBUF+1:MIN(NBUF+LCX2,LBUF))=CEX2
          NBUF=NBUF+LCX2
        END IF
C
        ITMP=IEVA-1
C
        IF (ITMP.LT.0.OR.LEXP.GT.0) THEN
          NBUF=NBUF+1
          IF (NBUF.LE.LBUF) THEN
            IF (ITMP.LT.0) THEN
              CBUF(NBUF:NBUF)='-'
            ELSE
              CBUF(NBUF:NBUF)='+'
            END IF
          END IF
        END IF
C
        ITMP=MIN(ABS(ITMP),9999)
C
        IF (ITMP.GT.999) THEN
          NTTL=4
          IDIV=1000
        ELSE IF (ITMP.GT.99) THEN
          NTTL=3
          IDIV=100
        ELSE IF (ITMP.GT.9) THEN
          NTTL=2
          IDIV=10
        ELSE
          NTTL=1
          IDIV=1
        END IF
C
        IF (LEXP.GT.0) THEN
          DO 10010 I=1,LEXP-NTTL
            NBUF=NBUF+1
            IF (NBUF.LE.LBUF) CBUF(NBUF:NBUF)='0'
10010     CONTINUE
        END IF
C
        DO 10011 I=1,NTTL
          NBUF=NBUF+1
          IF (NBUF.LE.LBUF) CBUF(NBUF:NBUF)=CHAR(ICHAR('0')+ITMP/IDIV)
          ITMP=MOD(ITMP,IDIV)
          IDIV=IDIV/10
10011   CONTINUE
C
        IF (LCX3.NE.0) THEN
          IF (NBUF.LT.LBUF) CBUF(NBUF+1:MIN(NBUF+LCX3,LBUF))=CEX3
          NBUF=NBUF+LCX3
        END IF
C
      END IF
C
C Limit the value of NBUF to the length of the character buffer CBUF.
C
      IF (NBUF.GT.LBUF) NBUF=LBUF
C
C Done.
C
      RETURN
C
      END
