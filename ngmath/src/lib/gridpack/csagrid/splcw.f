C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SPLCW (NDIM,XDATA,L1XDAT,YDATA,WDATA,NDATA,XMIN,XMAX,
     1                  NODES,XTRAP,COEF,NCF,WORK,NWRK,IERROR)
      DIMENSION       XDATA(L1XDAT,NDATA)    ,YDATA(NDATA)           ,
     1                WDATA(NDATA)           ,XMIN(NDIM) ,XMAX(NDIM) ,
     2                NODES(NDIM),COEF(NCF)  ,WORK(NWRK)
      DIMENSION       X(4)       ,NDERIV(4)  ,IN(4)      ,INMX(4)
      COMMON /SPLCOM/ MDIM       ,DX(4)      ,DXIN(4)    ,IB(4)      ,
     1                IBMN(4)    ,IBMX(4)
      SAVE
C
C  The restriction that NDIM be less than are equat to 4 can be 
C  eliminated by increasing the above dimensions, but the required 
C  length of WORK becomes quite large.
C
C  SPCRIT is used to determine data sparseness as follows -
C  the weights assigned to all data points are totaled into the
C  variable TOTLWT. (If no weights are entered, it is set to
C  NDATA.)  Each node of the node network is assigned a
C  rectangle (in which it is contained) and the weights of all
C  data points which fall in that rectangle are totaled.  If that
C  total is less than SPCRIT*EXPECT (EXPECT is defined below),
C  then the node is ascertained to be in a data sparse location.
C  EXPECT is that fraction of TOTLWT that would be expected by
C  comparing the area of the rectangle with the total area under
C  consideration.
C
      DATA SPCRIT/.75/
C
      IERROR = 0
      MDIM = NDIM
      IF (MDIM.LT.1 .OR. MDIM.GT.4) GO TO 127
      NCOL = 1
      DO 101 IDIM=1,MDIM
         NOD = NODES(IDIM)
         IF (NOD .LT. 4) GO TO 128
C
C  Number of columns in least squares matrix = number of coefficients =
C  product of nodes over all dimensions.
C
         NCOL = NCOL*NOD
         XRNG = XMAX(IDIM)-XMIN(IDIM)
         IF (XRNG .EQ. 0.) GO TO 129
C
C  DX(IDIM) is the node spacing along the IDIM coordinate.
C
         DX(IDIM) = XRNG/FLOAT(NOD-1)
         DXIN(IDIM) = 1./DX(IDIM)
         NDERIV(IDIM) = 0
  101 CONTINUE
      IF (NCOL .GT. NCF) GO TO 130
      NWRK1 = 1
      MDATA = NDATA
      IF (MDATA .LT. 1) GO TO 131
C
C  SWGHT is a local variable = XTRAP, and can be considered a smoothing
C  weight for data sparse areas.  If SWGHT .EQ. 0, no smoothing
C  computations are performed.
C
      SWGHT = XTRAP
C
C  Set aside workspace for counting data points.
C
      IF (SWGHT .NE. 0.) NWRK1 = NCOL+1
C
C  NWLFT is the length of the remaining workspace.
C
      NWLFT = NWRK-NWRK1+1
      IF (NWLFT .LT. 1) GO TO 132
      IROW = 0
C
C  ROWWT is used to weight rows of the least squares matrix.
C
      ROWWT = 1.
C
C  Loop through all data points, computing a row for each.
C
      DO 108 IDATA=1,MDATA
C
C  WDATA(1).LT.0 means weights have not been entered.  In that case,
C  ROWWT is left equal to  1. for all points.  Otherwise ROWWT is
C  equal to WDATA(IDATA).
C
C  Every element of the row, as well as the corresponding right hand
C  side, is multiplied by ROWWT.
C
         IF (WDATA(1) .LT. 0.) GO TO 102
         ROWWT = WDATA(IDATA)
C
C  Data points with 0 weight are ignored.
C
         IF (ROWWT .EQ. 0.) GO TO 108
  102    IROW = IROW+1
C
C  One row of the least squares matrix corresponds to each data 
C  point.  The right hand for that row will correspond to the 
C  function value YDATA at that point.
C
         RHS = ROWWT*YDATA(IDATA)
         DO 103 IDIM=1,MDIM
            X(IDIM) = XDATA(IDIM,IDATA)
  103    CONTINUE
C
C  The COEF array serves as a row of least squares matrix.  
C  Its value is zero except for columns corresponding to functions 
C  which are nonzero at X.
C
         DO 104 ICOL=1,NCOL
            COEF(ICOL) = 0.
  104    CONTINUE
C
C  Compute the indices of basis functions which are nonzero at X.
C  IBMN is in the range 0 to nodes-2 and IBMX is in range 1 
C  to NODES-1.
C
         DO 105 IDIM=1,MDIM
            NOD = NODES(IDIM)
            IT = DXIN(IDIM)*(X(IDIM)-XMIN(IDIM))
            IBMN(IDIM) = MIN0(MAX0(IT-1,0),NOD-2)
            IB(IDIM) = IBMN(IDIM)
            IBMX(IDIM) = MAX0(MIN0(IT+2,NOD-1),1)
  105    CONTINUE
C
C  Begining of basis index loop - traverse all indices corresponding
C  to basis functions which are nonzero at X.  The indices are in 
C  IB and are passed through common to BASCMP.
C
  106    CALL BASCMP (X,NDERIV,XMIN,NODES,ICOL,BASM)
C
C  BASCMP computes ICOL and BASM where BASM is the value at X of 
C  the N-dimensional basis function corresponding to column ICOL.
C
         COEF(ICOL) = ROWWT*BASM
C
C  Increment the basis indices.
C
         DO 107 IDIM=1,MDIM
            IB(IDIM) = IB(IDIM)+1
            IF (IB(IDIM) .LE. IBMX(IDIM)) GO TO 106
            IB(IDIM) = IBMN(IDIM)
  107    CONTINUE
C
C  End of basis index loop.
C
C
C  Send a row of the least squares matrix to the reduction routine.
C
         CALL SUPRLS (IROW,COEF,NCOL,RHS,WORK(NWRK1),NWLFT,COEF,RESERR,
     1                LSERR)
         IF (LSERR .NE. 0) GO TO 133
  108 CONTINUE
C
C  Row computations for all data points are now complete.
C
C  If SWGHT.EQ.0, the least squares matrix is complete and no 
C  smoothing rows are computed.
C
      IF (SWGHT .EQ. 0.) GO TO 126
C
C  Initialize smoothing computations for data sparse areas.
C  Derivative constraints will always have zero right hand side.
C
      RHS = 0.
      NRECT = 1
C
C  Initialize the node indices and compute number of rectangles 
C  formed by the node network.
C
      DO 109 IDIM=1,MDIM
         IN(IDIM) = 0
         INMX(IDIM) = NODES(IDIM)-1
         NRECT = NRECT*INMX(IDIM)
  109 CONTINUE
C
C  Every node is assigned an element of the workspace (set aside
C  previously) in which data points are counted.
C
      DO 110 IIN=1,NCOL
         WORK(IIN) = 0.
  110 CONTINUE
C
C  Assign each data point to a node, total the assignments for 
C  each node, and save in the workspace.
C
      TOTLWT = 0.
      DO 112 IDATA=1,MDATA
C
C  BUMP is the weight associated with the data point.
C
         BUMP = 1.
         IF (WDATA(1) .GE. 0.) BUMP = WDATA(IDATA)
         IF (BUMP .EQ. 0.) GO TO 112
C
C  Find the nearest node.
C
         IIN = 0
         DO 111 IDIMC=1,MDIM
            IDIM = MDIM+1-IDIMC
            INIDIM = INT(DXIN(IDIM)*(XDATA(IDIM,IDATA)-XMIN(IDIM))+.5)
C
C  Points not in range (+ or - 1/2 node spacing) are not counted.
C
            IF (INIDIM.LT.0 .OR. INIDIM.GT.INMX(IDIM)) GO TO 112
C
C  Compute linear address of node in workspace by Horner's method.
C
            IIN = (INMX(IDIM)+1)*IIN+INIDIM
  111    CONTINUE
C
C  Bump counter for that node.
C
         WORK(IIN+1) = WORK(IIN+1)+BUMP
         TOTLWT = TOTLWT+BUMP
  112 CONTINUE
C
C  Compute the expected weight per rectangle.
C
      WTPRRC = TOTLWT/FLOAT(NRECT)
C
C  IN contains indices of the node (previously initialized).
C  IIN will be the linear address of the node in the workspace.
C
      IIN = 0
C
C  Loop through all nodes, computing derivative constraint rows 
C  for those in data sparse locations.
C
C  Begining of node index loop - traverse all node indices.
C  The indices are in IN.
C
  113 IIN = IIN+1
      EXPECT = WTPRRC
C
C  Rectangles at edge of network are smaller and hence less weight
C  should be expected.
C
      DO 114 IDIM=1,MDIM
         IF (IN(IDIM).EQ.0 .OR. IN(IDIM).EQ.INMX(IDIM))
     1       EXPECT = .5*EXPECT
  114 CONTINUE
C
C  The expected weight minus the actual weight serves to define 
C  data sparseness and is also used to weight the derivative
C  constraint rows.
C
C  There is no constraint if not data sparse.
C
      IF (WORK(IIN) .GE. SPCRIT*EXPECT) GO TO 124
      DCWGHT = EXPECT-WORK(IIN)
      DO 115 IDIM=1,MDIM
         INIDIM = IN(IDIM)
C
C  Compute the location of the node.
C
         X(IDIM) = XMIN(IDIM)+FLOAT(INIDIM)*DX(IDIM)
C
C  Compute the indices of the basis functions which are non-zero 
C  at the node.
C
         IBMN(IDIM) = INIDIM-1
         IBMX(IDIM) = INIDIM+1
C
C  Distinguish the boundaries.
C
         IF (INIDIM .EQ. 0) IBMN(IDIM) = 0
         IF (INIDIM .EQ. INMX(IDIM)) IBMX(IDIM) = INMX(IDIM)
C
C  Initialize the basis indices.
C
         IB(IDIM) = IBMN(IDIM)
  115 CONTINUE
C
C  Multiply by the extrapolation parameter (this acts as a 
C  smoothing weight).
C
      DCWGHT = SWGHT*DCWGHT
C
C  The COEF array serves as a row of the least squares matrix.  
C  Its value is zero except for columns corresponding to functions 
C  which are non-zero at the node.
C
      DO 116 ICOL=1,NCOL
         COEF(ICOL) = 0.
  116 CONTINUE
C
C  The 2nd derivative of a function of MDIM variables may be thought 
C  of as a symmetric MDIM x MDIM matrix of 2nd order partial 
C  derivatives.  Traverse the upper triangle of this matrix and, 
C  for each element, compute a row of the least squares matrix.
C
      DO 123 IDM=1,MDIM
         DO 122 JDM=IDM,MDIM
            DO 117 IDIM=1,MDIM
               NDERIV(IDIM) = 0
  117       CONTINUE
C
C  Off-diagonal elements appear twice by symmetry, so the corresponding
C  row is weighted by a factor of 2.
C
            ROWWT = 2.*DCWGHT
            IF (JDM .NE. IDM) GO TO 118
C
C  Diagonal.
C
            ROWWT = DCWGHT
            NDERIV(JDM) = 2
            IF (IN(IDM).NE.0 .AND. IN(IDM).NE.INMX(IDM)) GO TO 119
C
C  Node is at boundary.
C
C  Normal 2nd derivative constraint at boundary is not appropriate for
C  natural splines (2nd derivative 0 by definition).  Substitute
C  a 1st derivative constraint.
C
  118       NDERIV(IDM) = 1
            NDERIV(JDM) = 1
  119       IROW = IROW+1
C
C  Begining of basis index loop - traverse all indices corresponding
C  to basis functions which are non-zero at X.
C  The indices are in IB and are passed through common to BASCMP.
C
  120       CALL BASCMP (X,NDERIV,XMIN,NODES,ICOL,BASM)
C
C  BASCMP computes ICOL and BASM where BASM is the value at X of the
C  N-dimensional basis function corresponding to column ICOL.
C
            COEF(ICOL) = ROWWT*BASM
C
C  Increment the basis indices.
C
            DO 121 IDIM=1,MDIM
               IB(IDIM) = IB(IDIM)+1
               IF (IB(IDIM) .LE. IBMX(IDIM)) GO TO 120
               IB(IDIM) = IBMN(IDIM)
  121       CONTINUE
C
C  End of basis index loop.
C
C  Send row of least squares matrix to reduction routine.
C
         CALL SUPRLS (IROW,COEF,NCOL,RHS,WORK(NWRK1),NWLFT,COEF,RESERR,
     1                LSERR)
         IF (LSERR .NE. 0) GO TO 133
  122    CONTINUE
  123 CONTINUE
C
C  Increment node indices.
C
  124 DO 125 IDIM=1,MDIM
         IN(IDIM) = IN(IDIM)+1
         IF (IN(IDIM) .LE. INMX(IDIM)) GO TO 113
         IN(IDIM) = 0
  125 CONTINUE
C
C  End of node index loop.
C
C  Call for least squares solution in COEF array.
C
  126 IROW = 0
         CALL SUPRLS (IROW,COEF,NCOL,RHS,WORK(NWRK1),NWLFT,COEF,RESERR,
     1                LSERR)
         IF (LSERR .NE. 0) GO TO 133
      RETURN
C
C  Error section
C
  127 CONTINUE
      IERROR = 101
      CALL CFAERR (IERROR,' SPLCC or SPLCW - NDIM is less than 1 or is g
     +reater than 4  ',60)
      GO TO 134
  128 CONTINUE
      IERROR = 102
      CALL CFAERR (IERROR,' SPLCC or SPLCW - NODES(IDIM) is less than 4
     1for some IDIM  ',60)
      GO TO 134
  129 CONTINUE
      IERROR = 103
      CALL CFAERR (IERROR,' SPLCC or SPLCW - XMIN(IDIM) equals XMAX(IDIM
     +) for some IDIM',60)
      GO TO 134
  130 CONTINUE
      IERROR = 104
      CALL CFAERR (IERROR,' SPLCC or SPLCW - NCF (size of COEF) is too s
     +mall           ',60)
      GO TO 134
  131 CONTINUE
      IERROR = 105
      CALL CFAERR (IERROR,' SPLCC or SPLCW - NDATA is less than 1
     +               ',60)
      GO TO 134
  132 CONTINUE
      IERROR = 106
      CALL CFAERR (IERROR,' SPLCC or SPLCW - NWRK (size of WORK) is too 
     +small          ',60)
      GO TO 134
  133 CONTINUE
      IERROR = 107
      CALL CFAERR (IERROR,' SPLCC or SPLCW - SUPRLS failure (this usuall
     +y indicates insufficient input data',80)
C
  134 RETURN
      END
