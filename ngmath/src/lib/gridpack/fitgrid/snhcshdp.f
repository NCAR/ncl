C
C $Id: snhcshdp.f,v 1.3 2008-07-27 03:10:12 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding single precision routine.
C
      SUBROUTINE SNHCSHDP(SINHM,COSHM,X,ISW)
      DOUBLE PRECISION SP14
      DOUBLE PRECISION SP13
      DOUBLE PRECISION SP12
      DOUBLE PRECISION SP11
      DOUBLE PRECISION SQ12
      DOUBLE PRECISION SQ11
      DOUBLE PRECISION SQ10
      DOUBLE PRECISION SP25
      DOUBLE PRECISION SP24
      DOUBLE PRECISION SP23
      DOUBLE PRECISION SP22
      DOUBLE PRECISION SP21
      DOUBLE PRECISION SQ22
      DOUBLE PRECISION SQ21
      DOUBLE PRECISION SQ20
      DOUBLE PRECISION SP35
      DOUBLE PRECISION SP34
      DOUBLE PRECISION SP33
      DOUBLE PRECISION SP32
      DOUBLE PRECISION SP31
      DOUBLE PRECISION SQ32
      DOUBLE PRECISION SQ31
      DOUBLE PRECISION SQ30
      DOUBLE PRECISION SP45
      DOUBLE PRECISION SP44
      DOUBLE PRECISION SP43
      DOUBLE PRECISION SP42
      DOUBLE PRECISION SP41
      DOUBLE PRECISION SQ42
      DOUBLE PRECISION SQ41
      DOUBLE PRECISION SQ40
      DOUBLE PRECISION CP5
      DOUBLE PRECISION CP4
      DOUBLE PRECISION CP3
      DOUBLE PRECISION CP2
      DOUBLE PRECISION CP1
      DOUBLE PRECISION CQ2
      DOUBLE PRECISION CQ1
      DOUBLE PRECISION CQ0
      DOUBLE PRECISION ZP4
      DOUBLE PRECISION ZP3
      DOUBLE PRECISION ZP2
      DOUBLE PRECISION ZP1
      DOUBLE PRECISION ZQ2
      DOUBLE PRECISION ZQ1
      DOUBLE PRECISION ZQ0
      DOUBLE PRECISION AX
      DOUBLE PRECISION XS
      DOUBLE PRECISION EXPX
c
      INTEGER ISW
      DOUBLE PRECISION SINHM,COSHM,X
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine returns approximations to
c       sinhm(x) = sinh(x)/x-1
c       coshm(x) = cosh(x)-1
c and
c       coshmm(x) = (cosh(x)-1-x*x/2)/(x*x)
c with relative error less than 4.0e-14.
c
c on input--
c
c   x contains the value of the independent variable.
c
c   isw indicates the function desired
c           = -1 if only sinhm is desired,
c           =  0 if both sinhm and coshm are desired,
c           =  1 if only coshm is desired,
c           =  2 if only coshmm is desired,
c           =  3 if both sinhm and coshmm are desired.
c
c on output--
c
c   sinhm contains the value of sinhm(x) if isw .le. 0 or
c   isw .eq. 3 (sinhm is unaltered if isw .eq.1 or isw .eq.
c   2).
c
c   coshm contains the value of coshm(x) if isw .eq. 0 or
c   isw .eq. 1 and contains the value of coshmm(x) if isw
c   .ge. 2 (coshm is unaltered if isw .eq. -1).
c
c and
c
c   x and isw are unaltered.
c
c-----------------------------------------------------------
c
      DATA SP14/.227581660976348D-7/,SP13/.612189863171694D-5/,
     +     SP12/.715314759211209D-3/,SP11/.398088289992973D-1/,
     +     SQ12/.206382701413725D-3/,SQ11/-.611470260009508D-1/,
     +     SQ10/.599999999999986D+1/
      DATA SP25/.129094158037272D-9/,SP24/.473731823101666D-7/,
     +     SP23/.849213455598455D-5/,SP22/.833264803327242D-3/,
     +     SP21/.425024142813226D-1/,SQ22/.106008515744821D-3/,
     +     SQ21/-.449855169512505D-1/,SQ20/.600000000268619D+1/
      DATA SP35/.155193945864942D-9/,SP34/.511529451668737D-7/,
     +     SP33/.884775635776784D-5/,SP32/.850447617691392D-3/,
     +     SP31/.428888148791777D-1/,SQ32/.933128831061610D-4/,
     +     SQ31/-.426677570538507D-1/,SQ30/.600000145086489D+1/
      DATA SP45/.188070632058331D-9/,SP44/.545792817714192D-7/,
     +     SP43/.920119535795222D-5/,SP42/.866559391672985D-3/,
     +     SP41/.432535234960858D-1/,SQ42/.824891748820670D-4/,
     +     SQ41/-.404938841672262D-1/,SQ40/.600005006283834D+1/
      DATA CP5/.552200614584744D-9/,CP4/.181666923620944D-6/,
     +     CP3/.270540125846525D-4/,CP2/.206270719503934D-2/,
     +     CP1/.744437205569040D-1/,CQ2/.514609638642689D-4/,
     +     CQ1/-.177792255528382D-1/,CQ0/.200000000000000D+1/
      DATA ZP4/.664418805876835D-8/,ZP3/.218274535686385D-5/,
     +     ZP2/.324851059327161D-3/,ZP1/.244515150174258D-1/,
     +     ZQ2/.616165782306621D-3/,ZQ1/-.213163639579425D0/,
     +     ZQ0/.240000000000000D+2/
c
      AX = ABS(X)
      IF (ISW.GE.0) GO TO 5
c
c sinhm approximation
c
      IF (AX.GT.3.9D0) GO TO 2
      XS = AX*AX
      IF (AX.GT.2.2D0) GO TO 1
c
c sinhm approximation on (0.,2.2)
c
      SINHM = XS* ((((SP14*XS+SP13)*XS+SP12)*XS+SP11)*XS+1.D0)/
     +        ((SQ12*XS+SQ11)*XS+SQ10)
      RETURN
c
c sinhm approximation on (2.2,3.9)
c
    1 SINHM = XS* (((((SP25*XS+SP24)*XS+SP23)*XS+SP22)*XS+SP21)*XS+
     +        1.D0)/ ((SQ22*XS+SQ21)*XS+SQ20)
      RETURN
    2 IF (AX.GT.5.1D0) GO TO 3
c
c sinhm approximation on (3.9,5.1)
c
      XS = AX*AX
      SINHM = XS* (((((SP35*XS+SP34)*XS+SP33)*XS+SP32)*XS+SP31)*XS+
     +        1.D0)/ ((SQ32*XS+SQ31)*XS+SQ30)
      RETURN
    3 IF (AX.GT.6.1D0) GO TO 4
c
c sinhm approximation on (5.1,6.1)
c
      XS = AX*AX
      SINHM = XS* (((((SP45*XS+SP44)*XS+SP43)*XS+SP42)*XS+SP41)*XS+
     +        1.D0)/ ((SQ42*XS+SQ41)*XS+SQ40)
      RETURN
c
c sinhm approximation above 6.1
c
    4 EXPX = EXP(AX)
      SINHM = (EXPX-1.D0/EXPX)/ (AX+AX) - 1.D0
      RETURN
c
c coshm and (possibly) sinhm approximation
c
    5 IF (ISW.GE.2) GO TO 7
      IF (AX.GT.2.2D0) GO TO 6
      XS = AX*AX
      COSHM = XS* (((((CP5*XS+CP4)*XS+CP3)*XS+CP2)*XS+CP1)*XS+1.D0)/
     +        ((CQ2*XS+CQ1)*XS+CQ0)
      IF (ISW.EQ.0) SINHM = XS* ((((SP14*XS+SP13)*XS+SP12)*XS+SP11)*XS+
     +                      1.D0)/ ((SQ12*XS+SQ11)*XS+SQ10)
      RETURN
    6 EXPX = EXP(AX)
      COSHM = (EXPX+1.D0/EXPX)/2.D0 - 1.D0
      IF (ISW.EQ.0) SINHM = (EXPX-1.D0/EXPX)/ (AX+AX) - 1.D0
      RETURN
c
c coshmm and (possibly) sinhm approximation
c
    7 XS = AX*AX
      IF (AX.GT.2.2D0) GO TO 8
      COSHM = XS* ((((ZP4*XS+ZP3)*XS+ZP2)*XS+ZP1)*XS+1.D0)/
     +        ((ZQ2*XS+ZQ1)*XS+ZQ0)
      IF (ISW.EQ.3) SINHM = XS* ((((SP14*XS+SP13)*XS+SP12)*XS+SP11)*XS+
     +                      1.D0)/ ((SQ12*XS+SQ11)*XS+SQ10)
      RETURN
    8 EXPX = EXP(AX)
      COSHM = ((EXPX+1.D0/EXPX-XS)/2.D0-1.D0)/XS
      IF (ISW.EQ.3) SINHM = (EXPX-1.D0/EXPX)/ (AX+AX) - 1.D0
      RETURN
      END
