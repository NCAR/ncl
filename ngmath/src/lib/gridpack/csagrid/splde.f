C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION SPLDE (NDIM,X,NDERIV,COEF,XMIN,XMAX,NODES,IERROR)
      DIMENSION       X(NDIM)    ,NDERIV(NDIM)           ,COEF(*)    ,
     1                XMIN(NDIM) ,XMAX(NDIM) ,NODES(NDIM)
      COMMON /SPLCOM/ MDIM       ,DX(4)      ,DXIN(4)    ,IB(4)      ,
     1                IBMN(4)    ,IBMX(4)
      SAVE
C
C The restriction for NDIM to be .LE. 4 can be eliminated by increasing
C the above dimensions.
C
      IERROR = 0
      MDIM = NDIM
      IF (MDIM.LT.1 .OR. MDIM.GT.4) GO TO 105
      IIBMX = 1
      DO 101 IDIM=1,MDIM
         NOD = NODES(IDIM)
         IF (NOD .LT. 4) GO TO 106
         XRNG = XMAX(IDIM)-XMIN(IDIM)
         IF (XRNG .EQ. 0.) GO TO 107
         IF (NDERIV(IDIM).LT.0 .OR. NDERIV(IDIM).GT.2) GO TO 108
C
C  DX(IDIM) is the node spacing along the IDIM coordinate.
C
         DX(IDIM) = XRNG/FLOAT(NOD-1)
         DXIN(IDIM) = 1./DX(IDIM)
C
C  Compute indices of basis functions which are nonzero at X.
C
         IT = DXIN(IDIM)*(X(IDIM)-XMIN(IDIM))
C
C  IBMN must be in the range 0 to NODES-2.
C
         IBMN(IDIM) = MIN0(MAX0(IT-1,0),NOD-2)
C
C  IBMX must be in the range 1 to NODES-1.
C
         IBMX(IDIM) = MAX0(MIN0(IT+2,NOD-1),1)
         IIBMX = IIBMX*(IBMX(IDIM)-IBMN(IDIM)+1)
         IB(IDIM) = IBMN(IDIM)
  101 CONTINUE
C
      SUM = 0.
      IIB = 0
C
C  Begining of basis index loop - traverse all indices corresponding
C  to basis functions which are nonzero at X.
C
  102 IIB = IIB+1
C
C  The indices are in IB and are passed through common to BASCMP.
C
      CALL BASCMP (X,NDERIV,XMIN,NODES,ICOF,BASM)
C
C  BASCMP computes ICOF and BASM where BASM is the value at X of the
C  N-dimensional basis function corresponding to COEF(ICOF).
C
      SUM = SUM+COEF(ICOF)*BASM
      IF (IIB .GE. IIBMX) GO TO 104
C
C  Increment the basis indices.
C
      DO 103 IDIM=1,MDIM
         IB(IDIM) = IB(IDIM)+1
         IF (IB(IDIM) .LE. IBMX(IDIM)) GO TO 102
         IB(IDIM) = IBMN(IDIM)
  103 CONTINUE
C
C  End of basis index loop.
C
  104 SPLDE = SUM
      RETURN
C
C  Errors.
C
  105 CONTINUE
      IERROR = 101
      CALL CFAERR (IERROR,' SPLFE or SPLDE - NDIM is less than 1 or grea
     +ter than 4     ',60)
      GO TO 109
  106 CONTINUE
      IERROR = 102
      CALL CFAERR (IERROR,' SPLFE or SPLDE - NODES(IDIM) is less than  4 
     +for some IDIM  ',60)
      GO TO 109
  107 CONTINUE
      IERROR = 103
      CALL CFAERR (IERROR,' SPLFE or SPLDE - XMIN(IDIM) = XMAX(IDIM) for
     + some IDIM     ',60)
      GO TO 109
  108 CONTINUE
      IERROR = 104
      CALL CFAERR (IERROR,' SPLDE - NDERIV(IDIM) IS less than 0 or great
     +er than 2 for some IDIM  ',70)
C
  109 STOP
      END
