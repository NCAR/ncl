C
C	$Id: msshch.f,v 1.4 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MSSHCH (SINHM,COSHM,X,ISW)
C
C ---------------------------------------------------------------------
C Note:  This routine comes from a proprietary package called FITPACK.
C It is used in the NCAR graphics package by permission of the author,
C Alan Cline.
C ---------------------------------------------------------------------
C
C                                            CODED BY ALAN KAYLOR CLINE
C                                         FROM FITPACK -- JUNE 22, 1986
C                                   A CURVE AND SURFACE FITTING PACKAGE
C                                 A PRODUCT OF PLEASANT VALLEY SOFTWARE
C                             8603 ALTUS COVE, AUSTIN, TEXAS 78759, USA
C
C ---------------------------------------------------------------------
C
C THIS SUBROUTINE RETURNS APPROXIMATIONS TO
C
C       SINHM(X) = SINH(X)/X-1
C       COSHM(X) = COSH(X)-1
C       COSHMM(X) = (COSH(X)-1-X*X/2)/(X*X)
C
C WITH RELATIVE ERROR LESS THAN 1.0E-6
C
C ON INPUT--
C
C   X CONTAINS THE VALUE OF THE INDEPENDENT VARIABLE.
C
C   ISW INDICATES THE FUNCTION DESIRED
C           = -1 IF ONLY SINHM IS DESIRED,
C           =  0 IF BOTH SINHM AND COSHM ARE DESIRED,
C           =  1 IF ONLY COSHM IS DESIRED,
C           =  2 IF ONLY COSHMM IS DESIRED,
C           =  3 IF BOTH SINHM AND COSHMM ARE DESIRED.
C
C ON OUTPUT--
C
C   SINHM CONTAINS THE VALUE OF SINHM(X) IF ISW .LE. 0 OR
C   ISW .EQ. 3 (SINHM IS UNALTERED IF ISW .EQ.1 OR ISW .EQ.
C   2).
C
C   COSHM CONTAINS THE VALUE OF COSHM(X) IF ISW .EQ. 0 OR
C   ISW .EQ. 1 AND CONTAINS THE VALUE OF COSHMM(X) IF ISW
C   .GE. 2 (COSHM IS UNALTERED IF ISW .EQ. -1).
C
C   X AND ISW ARE UNALTERED.
C
C-----------------------------------------------------------
C
      DATA SP13/.3029390E-5/,
     *     SP12/.1975135E-3/,
     *     SP11/.8334261E-2/,
     *     SP10/.1666665E0/
      DATA SP24/.3693467E-7/,
     *     SP23/.2459974E-5/,
     *     SP22/.2018107E-3/,
     *     SP21/.8315072E-2/,
     *     SP20/.1667035E0/
      DATA SP33/.6666558E-5/,
     *     SP32/.6646307E-3/,
     *     SP31/.4001477E-1/,
     *     SQ32/.2037930E-3/,
     *     SQ31/-.6372739E-1/,
     *     SQ30/.6017497E1/
      DATA SP43/.2311816E-4/,
     *     SP42/.2729702E-3/,
     *     SP41/.9868757E-1/,
     *     SQ42/.1776637E-3/,
     *     SQ41/-.7549779E-1/,
     *     SQ40/.9110034E1/
      DATA CP4/.2982628E-6/,
     *     CP3/.2472673E-4/,
     *     CP2/.1388967E-2/,
     *     CP1/.4166665E-1/,
     *     CP0/.5000000E0/
C
      AX = ABS(X)
      IF (ISW .GE. 0) GO TO 5
C
C SINHM APPROXIMATION
C
      IF (AX .GT. 4.45) GO TO 2
      XS = AX*AX
      IF (AX .GT. 2.3) GO TO 1
C
C SINHM APPROXIMATION ON (0.,2.3)
C
      SINHM = XS*(((SP13*XS+SP12)*XS+SP11)*XS+SP10)
      RETURN
C
C SINHM APPROXIMATION ON (2.3,4.45)
C
    1 SINHM = XS*((((SP24*XS+SP23)*XS+SP22)*XS+SP21)
     .               *XS+SP20)
      RETURN
    2 IF (AX .GT. 7.65) GO TO 3
C
C SINHM APPROXIMATION ON (4.45,7.65)
C
      XS = AX*AX
      SINHM = XS*(((SP33*XS+SP32)*XS+SP31)*XS+1.)/
     .             ((SQ32*XS+SQ31)*XS+SQ30)
      RETURN
    3 IF (AX .GT. 10.1) GO TO 4
C
C SINHM APPROXIMATION ON (7.65,10.1)
C
      XS = AX*AX
      SINHM = XS*(((SP43*XS+SP42)*XS+SP41)*XS+1.)/
     .             ((SQ42*XS+SQ41)*XS+SQ40)
      RETURN
C
C SINHM APPROXIMATION ABOVE 10.1
C
    4 SINHM = EXP(AX)/(AX+AX)-1.
      RETURN
C
C COSHM AND (POSSIBLY) SINHM APPROXIMATION
C
    5 IF (ISW .GE. 2) GO TO 7
      IF (AX .GT. 2.3) GO TO 6
      XS = AX*AX
      COSHM = XS*((((CP4*XS+CP3)*XS+CP2)*XS+CP1)*XS+CP0)
      IF (ISW .EQ. 0) SINHM = XS*(((SP13*XS+SP12)*XS+SP11)
     .                              *XS+SP10)
      RETURN
    6 EXPX = EXP(AX)
      COSHM = (EXPX+1./EXPX)/2.-1.
      IF (ISW .EQ. 0) SINHM = (EXPX-1./EXPX)/(AX+AX)-1.
      RETURN
C
C COSHMM AND (POSSIBLY) SINHM APPROXIMATION
C
    7 XS = AX*AX
      IF (AX .GT. 2.3) GO TO 8
      COSHM = XS*(((CP4*XS+CP3)*XS+CP2)*XS+CP1)
      IF (ISW .EQ. 3) SINHM = XS*(((SP13*XS+SP12)*XS+SP11)
     .                              *XS+SP10)
      RETURN
    8 EXPX = EXP(AX)
      COSHM = ((EXPX+1./EXPX-XS)/2.-1.)/XS
      IF (ISW .EQ. 3) SINHM = (EXPX-1./EXPX)/(AX+AX)-1.
      RETURN
      END
