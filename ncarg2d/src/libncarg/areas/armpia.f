C
C $Id: armpia.f,v 1.9 1995-04-28 19:41:05 kennison Exp $
C
      SUBROUTINE ARMPIA (IOP,DPV,IER)
C
      DOUBLE PRECISION DPV
C
      DIMENSION IOP(4,*),SGN(10),VAL(10,20),LEN(10)
      SAVE SGN,VAL,LEN
C
C Frequently AREAS needs to evaluate simple expressions in which the
C use of extended precision becomes important.  Consider, for example,
C the expression
C
C      (X4-X3)(X2*Y1-X1*Y2)-(X2-X1)(X4*Y3-X3*Y4)
C      -----------------------------------------
C            (X4-X3)(Y1-Y2)-(X2-X1)(Y3-Y4)
C
C All of the simple terms in this expression have integer values which
C represent coordinates and are small enough to be exactly represented,
C either as integers or as reals.  Unfortunately, as the numerator and
C the denominator are evaluated, intermediate values may arise which
C are too large to be represented as integers or as reals.  Sometimes,
C it happens that one such quantity is subtracted from another and the
C two are nearly enough equal so that most or all of the precision is
C lost.  Then, the final division will yield an erroneous result.
C
C The routine ARMPIA allows one to evaluate the numerator and the
C denominator of such an expression exactly, using multiple-precision
C adds, subtracts, and multiplies.  (The final division is done using
C double precision arithmetic.)
C
C The operations to be performed are specified by the contents of the
C array IOP, operands are kept in the arrays SGN, VAL, and LEN, and the
C final result is returned as the value of the argument DPV.  For an
C arbitrary value of I (I = 1, 2, 3, ...), let I1 be the value of
C IOP(1,I), I2 the value of IOP(2,I), I3 the value of IOP(3,I), and I4
C the value of IOP(4,I).
C
C When I1 = 1, the multiple-precision integer with index I2 is given
C the value I3.
C
C When I1 = 2, the multiple-precision integer with index I2 is reset
C to the sum of those with indices I3 and I4.
C
C When I1 = 3, the multiple-precision integer with index I2 is reset
C to the difference of those with indices I3 and I4.
C
C When I1 = 4, the multiple-precision integer with index I2 is reset
C to the product of those with indices I3 and I4.
C
C When I1 = 5, the multiple-precision integer with index I2 is returned
C as the real value of DPV.
C
C The argument IER is an error flag; it is normally returned with value
C zero.  When an error occurs, IER is returned with a positive value
C indicating the nature of the error (currently, the only error is
C error number 1).
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI,IRC(16),RLA,RWA,RDI,RSI
      SAVE   /ARCOMN/
C
      NOP=1
C
10001 CONTINUE
C
        II1=IOP(1,NOP)
        II2=IOP(2,NOP)
        II3=IOP(3,NOP)
        II4=IOP(4,NOP)
C
        IF (.NOT.(II1.EQ.1)) GO TO 10002
C
          SGN(II2)=REAL(SIGN(1,II3))
          II3=ABS(II3)
          LEN(II2)=0
10003     CONTINUE
            LEN(II2)=LEN(II2)+1
            IF (.NOT.(LEN(II2).GT.20)) GO TO 10004
              GO TO 10006
10004       CONTINUE
            VAL(II2,LEN(II2))=REAL(MOD(II3,IBS))
            II3=II3/IBS
          IF (.NOT.(II3.EQ.0)) GO TO 10003
C
        GO TO 10007
10002   CONTINUE
        IF (.NOT.(II1.EQ.2.OR.II1.EQ.3)) GO TO 10008
C
          IF (II1.EQ.3) SGN(II4)=-SGN(II4)
          IF (.NOT.(SGN(II3).EQ.SGN(II4))) GO TO 10009
            SGN(II2)=SGN(II3)
            LEN(II2)=MAX(LEN(II3),LEN(II4))
            CRY=0.
            DO 10010 III=1,LEN(II2)
              VAL(II2,III)=CRY
              IF (III.LE.LEN(II3)) VAL(II2,III)=VAL(II2,III)+
     +                                          VAL(II3,III)
              IF (III.LE.LEN(II4)) VAL(II2,III)=VAL(II2,III)+
     +                                          VAL(II4,III)
              IF (.NOT.(VAL(II2,III).LT.RBS)) GO TO 10011
                CRY=0.
              GO TO 10012
10011         CONTINUE
                CRY=1.
                VAL(II2,III)=VAL(II2,III)-RBS
10012         CONTINUE
10010       CONTINUE
            IF (.NOT.(CRY.NE.0.)) GO TO 10013
              IF (.NOT.(LEN(II2).GE.20)) GO TO 10014
                GO TO 10006
10014         CONTINUE
              LEN(II2)=LEN(II2)+1
              VAL(II2,LEN(II2))=1.
10013       CONTINUE
          GO TO 10016
10009     CONTINUE
            IF (.NOT.(LEN(II3).GT.LEN(II4))) GO TO 10017
              JMP=0
            GO TO 10018
10017       CONTINUE
            IF (.NOT.(LEN(II3).LT.LEN(II4))) GO TO 10019
              JMP=1
            GO TO 10018
10019       CONTINUE
              JMP=-1
              IIA=LEN(II3)
10020         CONTINUE
                IF (.NOT.(VAL(II3,IIA).GT.VAL(II4,IIA))) GO TO 10021
                  JMP=0
                  GO TO 10022
10021           CONTINUE
                IF (.NOT.(VAL(II3,IIA).LT.VAL(II4,IIA))) GO TO 10023
                  JMP=1
                  GO TO 10022
10023           CONTINUE
                IIA=IIA-1
                IF (IIA.EQ.0) GO TO 10022
              GO TO 10020
10022         CONTINUE
10018       CONTINUE
            IF (.NOT.(JMP.EQ.0)) GO TO 10024
              SGN(II2)=SGN(II3)
              BRO=0.
              DO 10025 III=1,LEN(II3)
                VAL(II2,III)=VAL(II3,III)-BRO
                IF (III.LE.LEN(II4)) VAL(II2,III)=VAL(II2,III)-
     +                                            VAL(II4,III)
                IF (.NOT.(VAL(II2,III).GE.0.)) GO TO 10026
                  BRO=0.
                GO TO 10027
10026           CONTINUE
                  VAL(II2,III)=VAL(II2,III)+RBS
                  BRO=1.
10027           CONTINUE
                IF (VAL(II2,III).NE.0.) LEN(II2)=III
10025         CONTINUE
            GO TO 10028
10024       CONTINUE
            IF (.NOT.(JMP.GT.0)) GO TO 10029
              SGN(II2)=SGN(II4)
              BRO=0.
              DO 10030 III=1,LEN(II4)
                VAL(II2,III)=VAL(II4,III)-BRO
                IF (III.LE.LEN(II3)) VAL(II2,III)=VAL(II2,III)-
     +                                            VAL(II3,III)
                IF (.NOT.(VAL(II2,III).GE.0.)) GO TO 10031
                  BRO=0.
                GO TO 10032
10031           CONTINUE
                  VAL(II2,III)=VAL(II2,III)+RBS
                  BRO=1.
10032           CONTINUE
                IF (VAL(II2,III).NE.0.) LEN(II2)=III
10030         CONTINUE
            GO TO 10028
10029       CONTINUE
              SGN(II2)=1.
              LEN(II2)=1
              VAL(II2,1)=0.
10028       CONTINUE
10016     CONTINUE
          IF (II1.EQ.3) SGN(II4)=-SGN(II4)
C
        GO TO 10007
10008   CONTINUE
        IF (.NOT.(II1.EQ.4)) GO TO 10033
C
          IF (.NOT.((LEN(II3).EQ.1.AND.VAL(II3,1).EQ.0.).OR.(LEN(II4).EQ
     +.1.AND.VAL(II4,1).EQ.0.))) GO TO 10034
            SGN(II2)=1.
            LEN(II2)=1
            VAL(II2,1)=0.
          GO TO 10035
10034     CONTINUE
            SGN(II2)=SGN(II3)*SGN(II4)
            LEN(II2)=LEN(II3)+LEN(II4)
            IF (.NOT.(LEN(II2).GT.20)) GO TO 10036
              GO TO 10006
10036       CONTINUE
            DO 10038 III=1,LEN(II2)
              VAL(II2,III)=0.
10038       CONTINUE
            DO 10039 IIA=1,LEN(II3)
              DO 10040 IIB=1,LEN(II4)
                IIC=IIA+IIB-1
                VAL(II2,IIC)=VAL(II2,IIC)+VAL(II3,IIA)*VAL(II4,IIB)
                IF (.NOT.(VAL(II2,IIC).GE.RBS)) GO TO 10041
                  VAL(II2,IIC+1)=VAL(II2,IIC+1)+
     +                           REAL(INT(VAL(II2,IIC))/IBS)
                  VAL(II2,IIC)=REAL(MOD(INT(VAL(II2,IIC)),IBS))
10041           CONTINUE
10040         CONTINUE
10039       CONTINUE
            IF (VAL(II2,LEN(II2)).EQ.0.) LEN(II2)=LEN(II2)-1
10035     CONTINUE
C
        GO TO 10007
10033   CONTINUE
C
          DPV=DBLE(VAL(II2,LEN(II2)))
          DO 10042 III=LEN(II2)-1,1,-1
            DPV=DPV*DBS+DBLE(VAL(II2,III))
10042     CONTINUE
          DPV=DPV*DBLE(SGN(II2))
          GO TO 10043
C
10007   CONTINUE
C
        NOP=NOP+1
C
      GO TO 10001
10043 CONTINUE
C
C Done.
C
      IER=0
      RETURN
C
C Error exit.
C
10006 CONTINUE
        IER=1
        RETURN
C
      END
