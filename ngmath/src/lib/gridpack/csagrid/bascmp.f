C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BASCMP (X,NDERIV,XMIN,NODES,ICOL,BASM)
C
C  This routine does basis function computations for natural 
C  splines.  This routine is called by routines SPLCW and SPLDE 
C  to compute ICOL and BASM, which are defined as follows:
C  
C     The MDIM indices in IB (defined through common) determine 
C     a specific node in the node grid (see routine SPLCC for a 
C     description of the node grid).  Every node is associated 
C     with an MDIM-dimensional basis function and a corresponding 
C     column in the least squares matrix (or element of the 
C     coefficient vector).  The column index (which may be thought 
C     of as a linear address for the MDIM-dimensional node grid)
C     corresponding to the specified node is computed as ICOL.  The
C     associated basis function evaluated at X (an MDIM-vector) is
C     computed as BASM (a scalar).
C
C  In case NDERIV is not all zero, BASM will not be the value of 
C  the basis function but rather a partial derivative of that
C  function as follows:
C
C     The order of the partial derivative in the direction of the
C     IDIM coordinate is NDERIV(IDIM) (for IDIM .LE. MDIM).  This 
C     routine will compute incorrect values if NDERIV(IDIM) is not
C     in the range 0 to 2.
C
C
      DIMENSION       X(4)       ,NDERIV(4)  ,XMIN(4)    ,NODES(4)
C
C  The technique of this routine is to transform the independent
C  variable in each dimension such that the nodes fall on
C  suitably chosen integers.  On this transformed space, the
C  1-dimensional basis functions and their derivatives have a
C  particularly simple form.  The desired MDIM-dimensional basis
C  function (or any of its partial derivatives) is computed as
C  a product of such 1-dimensional functions (tensor product
C  method of defining multi-dimensional splines).  The values 
C  which determine the location of the nodes, and hence the
C  above transform, are passed through common and the argument 
C  list.
C
      COMMON /SPLCOM/ MDIM       ,DX(4)      ,DXIN(4)    ,IB(4)
     1               ,IBMN(4)    ,IBMX(4)
      SAVE
C
C  ICOL will be a linear address corresponding to the indices in IB.
C
      ICOL = 0
C
C  BASM will be M-dimensional basis function evaluated at X.
C
      BASM = 1.
      DO 121 IDIM=1,MDIM
C
C  Compute ICOL by Horner's method.
C
         MDMID = MDIM+1-IDIM
         ICOL = NODES(MDMID)*ICOL+IB(MDMID)
C
C  NGO depends upon function type and NDERIV.
C
         NTYP = 1
C
C  Function type 1 (left linear) for IB = 0 or 1.
C
         IF (IB(IDIM) .LE. 1) GO TO 101
         NTYP = 2
C
C  Function type 2 (chapeau function) for 2 LT IB LT NODES-2.
C
         IF (IB(IDIM) .LT. NODES(IDIM)-2) GO TO 101
         NTYP = 3
C
C  Function type 3 (right linear) for IB = NODES-2 or NODES-1.
C
  101    NGO = 3*NTYP+NDERIV(IDIM)-2
C
C  XB is X value of node IB (center of basis function).
C
         XB = XMIN(IDIM)+FLOAT(IB(IDIM))*DX(IDIM)
C
C  BAS1 will be the 1-dimensional basis function evaluated at X.
C
         BAS1 = 0.
         GO TO (102,103,104,105,106,108,110,113,117),NGO
C
C  Function type 1 (left linear) is mirror image of function type 3.
C
C  Transform so that XB is at 2 and the other nodes are at the integers
C  (with ordering reversed to form a mirror image). 
C
  102    Z = DXIN(IDIM)*(XB-X(IDIM))+2.
         GO TO 111
C
C  1st derivative.
C
  103    FACT = -DXIN(IDIM)
         GO TO 114
C
C  2nd derivative.
C
  104    FACT = -DXIN(IDIM)
         GO TO 118
C
C  Function type 2 (chapeau function).
C
C  Transform so that XB is at the origin and the other nodes are at 
C  the integers.
C
  105    Z = ABS(DXIN(IDIM)*(X(IDIM)-XB))-2.
C
C  This chapeau function is then that unique cubic spline which is
C  identically zero for ABS(Z) GE 2 and is 1 at the origin.  This
C  function is the general interior node basis function.
C
         IF (Z .GE. 0.) GO TO 120
         BAS1 = -.25*Z**3
         Z = Z+1.
         IF (Z .GE. 0.) GO TO 120
         BAS1 = BAS1+Z**3
         GO TO 120
C
C  1st derivative.
C
  106    Z = X(IDIM)-XB
         FACT = DXIN(IDIM)
         IF (Z .LT. 0.) FACT = -FACT
         Z = FACT*Z-2.
         IF (Z .GE. 0.) GO TO 120
         BAS1 = -.75*Z**2
         Z = Z+1.
         IF (Z .GE. 0.) GO TO 107
         BAS1 = BAS1+3.*Z**2
  107    BAS1 = FACT*BAS1
         GO TO 120
C
C  2nd derivative.
C
  108    FACT = DXIN(IDIM)
         Z = FACT*ABS(X(IDIM)-XB)-2.
         IF (Z .GE. 0.) GO TO 120
         BAS1 = -1.5*Z
         Z = Z+1.
         IF (Z .GE. 0.) GO TO 109
         BAS1 = BAS1+6.*Z
  109    BAS1 = (FACT**2)*BAS1
         GO TO 120
C
C  Function type 3 (right linear).
C
C  Transform so that XB is at 2 and the other nodes are at the integers.
C
  110    Z = DXIN(IDIM)*(X(IDIM)-XB)+2.
C
C  This right linear function is defined to be that unique cubic spline
C  which is identically zero for Z .LE. 0 and is 3*Z-3 for Z GE 2.
C  This function (obviously having zero 2nd derivative for
C  Z GE 2) is used for the two nodes nearest an edge in order
C  to generate natural splines, which must by definition have
C  zero 2nd derivative at the boundary.
C
C  Note that this method of generating natural splines also provides
C  a linear extrapolation which has 2nd order continuity with
C  the interior splines at the boundary.
C
  111    IF (Z .LE. 0.) GO TO 120
         IF (Z .GE. 2.) GO TO 112
         BAS1 = .5*Z**3
         Z = Z-1.
         IF (Z .LE. 0.) GO TO 120
         BAS1 = BAS1-Z**3
         GO TO 120
  112    BAS1 = 3.*Z-3.
         GO TO 120
C
C  1st derivative.
C
  113    FACT = DXIN(IDIM)
  114    Z = FACT*(X(IDIM)-XB)+2.
         IF (Z .LE. 0.) GO TO 120
         IF (Z .GE. 2.) GO TO 116
         BAS1 = 1.5*Z**2
         Z = Z-1.
         IF (Z .LE. 0.) GO TO 115
         BAS1 = BAS1-3.*Z**2
  115    BAS1 = FACT*BAS1
         GO TO 120
  116    BAS1 = 3.*FACT
         GO TO 120
C
C  2nd derivative.
C
  117    FACT = DXIN(IDIM)
  118    Z = FACT*(X(IDIM)-XB)+2.
         Z1 = Z-1.
         IF (ABS(Z1) .GE. 1.) GO TO 120
         BAS1 = 3.*Z
         IF (Z1 .LE. 0.) GO TO 119
         BAS1 = BAS1-6.*Z1
  119    BAS1 = (FACT**2)*BAS1
  120    BASM = BASM*BAS1
  121 CONTINUE
      ICOL = ICOL+1
      RETURN
      END
