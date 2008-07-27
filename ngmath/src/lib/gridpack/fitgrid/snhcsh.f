C
C $Id: snhcsh.f,v 1.5 2008-07-27 03:10:12 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding double precision routine.
C
      subroutine snhcsh (sinhm,coshm,x,isw)
c
      integer isw
      real sinhm,coshm,x
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
      data sp14/.227581660976348e-7/,
     *     sp13/.612189863171694e-5/,
     *     sp12/.715314759211209e-3/,
     *     sp11/.398088289992973e-1/,
     *     sq12/.206382701413725e-3/,
     *     sq11/-.611470260009508e-1/,
     *     sq10/.599999999999986e+1/
      data sp25/.129094158037272e-9/,
     *     sp24/.473731823101666e-7/,
     *     sp23/.849213455598455e-5/,
     *     sp22/.833264803327242e-3/,
     *     sp21/.425024142813226e-1/,
     *     sq22/.106008515744821e-3/,
     *     sq21/-.449855169512505e-1/,
     *     sq20/.600000000268619e+1/
      data sp35/.155193945864942e-9/,
     *     sp34/.511529451668737e-7/,
     *     sp33/.884775635776784e-5/,
     *     sp32/.850447617691392e-3/,
     *     sp31/.428888148791777e-1/,
     *     sq32/.933128831061610e-4/,
     *     sq31/-.426677570538507e-1/,
     *     sq30/.600000145086489e+1/
      data sp45/.188070632058331e-9/,
     *     sp44/.545792817714192e-7/,
     *     sp43/.920119535795222e-5/,
     *     sp42/.866559391672985e-3/,
     *     sp41/.432535234960858e-1/,
     *     sq42/.824891748820670e-4/,
     *     sq41/-.404938841672262e-1/,
     *     sq40/.600005006283834e+1/
      data cp5/.552200614584744e-9/,
     *     cp4/.181666923620944e-6/,
     *     cp3/.270540125846525e-4/,
     *     cp2/.206270719503934e-2/,
     *     cp1/.744437205569040e-1/,
     *     cq2/.514609638642689e-4/,
     *     cq1/-.177792255528382e-1/,
     *     cq0/.200000000000000e+1/
      data zp4/.664418805876835e-8/,
     *     zp3/.218274535686385e-5/,
     *     zp2/.324851059327161e-3/,
     *     zp1/.244515150174258e-1/,
     *     zq2/.616165782306621e-3/,
     *     zq1/-.213163639579425e0/,
     *     zq0/.240000000000000e+2/
c
      ax = abs(x)
      if (isw .ge. 0) go to 5
c
c sinhm approximation
c
      if (ax .gt. 3.9) go to 2
      xs = ax*ax
      if (ax .gt. 2.2) go to 1
c
c sinhm approximation on (0.,2.2)
c
      sinhm = xs*((((sp14*xs+sp13)*xs+sp12)*xs+sp11)*xs+1.)/
     .             ((sq12*xs+sq11)*xs+sq10)
      return
c
c sinhm approximation on (2.2,3.9)
c
    1 sinhm = xs*(((((sp25*xs+sp24)*xs+sp23)*xs+sp22)*xs+sp21)
     .        *xs+1.)/((sq22*xs+sq21)*xs+sq20)
      return
    2 if (ax .gt. 5.1) go to 3
c
c sinhm approximation on (3.9,5.1)
c
      xs = ax*ax
      sinhm = xs*(((((sp35*xs+sp34)*xs+sp33)*xs+sp32)*xs+sp31)
     .        *xs+1.)/((sq32*xs+sq31)*xs+sq30)
      return
    3 if (ax .gt. 6.1) go to 4
c
c sinhm approximation on (5.1,6.1)
c
      xs = ax*ax
      sinhm = xs*(((((sp45*xs+sp44)*xs+sp43)*xs+sp42)*xs+sp41)
     .        *xs+1.)/((sq42*xs+sq41)*xs+sq40)
      return
c
c sinhm approximation above 6.1
c
    4 expx = exp(ax)
      sinhm = (expx-1./expx)/(ax+ax)-1.
      return
c
c coshm and (possibly) sinhm approximation
c
    5 if (isw .ge. 2) go to 7
      if (ax .gt. 2.2) go to 6
      xs = ax*ax
      coshm = xs*(((((cp5*xs+cp4)*xs+cp3)*xs+cp2)*xs+cp1)
     .        *xs+1.)/((cq2*xs+cq1)*xs+cq0)
      if (isw .eq. 0) sinhm = xs*((((sp14*xs+sp13)*xs+sp12)
     .          *xs+sp11)*xs+1.)/((sq12*xs+sq11)*xs+sq10)
      return
    6 expx = exp(ax)
      coshm = (expx+1./expx)/2.-1.
      if (isw .eq. 0) sinhm = (expx-1./expx)/(ax+ax)-1.
      return
c
c coshmm and (possibly) sinhm approximation
c
    7 xs = ax*ax
      if (ax .gt. 2.2) go to 8
      coshm = xs*((((zp4*xs+zp3)*xs+zp2)*xs+zp1)*xs+1.)/
     .             ((zq2*xs+zq1)*xs+zq0)
      if (isw .eq. 3) sinhm = xs*((((sp14*xs+sp13)*xs+sp12)
     .          *xs+sp11)*xs+1.)/((sq12*xs+sq11)*xs+sq10)
      return
    8 expx = exp(ax)
      coshm = ((expx+1./expx-xs)/2.-1.)/xs
      if (isw .eq. 3) sinhm = (expx-1./expx)/(ax+ax)-1.
      return
      end
