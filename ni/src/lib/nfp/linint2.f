c -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DLININT1(NXI,XI,FI,ICYCX,NXO,XO,FO,XMSG,IER)
      IMPLICIT NONE
      INTEGER NXI,NXO,ICYCX,IER
      DOUBLE PRECISION XI(NXI),FI(NXI)
      DOUBLE PRECISION XO(NXO),FO(NXO),XMSG
C NCLEND

C This is written  with GNU f77 acceptable extensions
C .   to allow for compilation on linux platforms.
c .   This could be improved considerably with f90

c NCL:  fo = linint1 (xi,fi, wrapX, xo, foOpt)
c
c            {nxi,nxo} = dimsizes( {xi,xo} )
c            fo is the same size xo and same type as "fi"
c            xmsg = fi@_FillValue
c            wrapX is an NCL logical; a False ==> 0
c                                     a True  ==> 1
c            foOpt unused option
c
c            The NCL wrapper should allow for multiple datasets
c            so the user need only make one call to the function.

c perform piecewise linear interpolation allowing for missing data
c .  nothing fancy

c nomenclature:
c .   nxi     - lengths of xi and dimensions of fi (must be >= 2)
c .   xi      - coordinates of fi (eg, lon)
c .             must be monotonically increasing
c .   fi      - functional input values [2D]
c .   icycx   - 0 if fi is cyclic in x (cyclic pt should NOT included)
c .             .ne.0 if cyclic
c .   nxo     - lengths of xo and dimensions of fo (must be >= 2)
c .   xo      - coordinates of fo (eg, lon)
c .             must be monotonically increasing
c .   fo      - functional output values [interpolated]
c .   xmsg    - missing code
c .   ier     - error code
c .             =0;   no error
c .             =1;   not enough points in input/output array
c .             =2;   xi are not monotonically increasing
c .             =4;   xo yo are not monotonically increasing
c
c                              automatic temporary/work arrays
      DOUBLE PRECISION XIW(0:NXI+1),FIXW(0:NXI+1)

c                              local
      INTEGER NX,NY
      DOUBLE PRECISION DX
c                              error checking
      IER = 0
      IF (NXI.LE.2 .OR. NXO.LE.2) THEN
          IER = 1
          RETURN
      END IF
c                              error mono increasing ?
      CALL DMONOINC(XI,NXI,2,IER)
      IF (IER.NE.0) RETURN
      CALL DMONOINC(XO,NXO,4,IER)
      IF (IER.NE.0) RETURN
c                               is the input array cyclic in x
      IF (ICYCX.EQ.0) THEN
          CALL DLIN2INT1(NXI,XI,FI,NXO,XO,FO,XMSG)
      ELSE
c                               must be cyclic in x
c                               create cyclic "x" coordinates
          DO NX = 1,NXI
              XIW(NX) = XI(NX)
              FIXW(NX) = FI(NX)
          END DO
          DX = XI(2) - XI(1)
          XIW(0) = XI(1) - DX
          XIW(NXI+1) = XI(NXI) + DX
          FIXW(0) = FI(NXI)
          FIXW(NXI+1) = FI(1)

          CALL DLIN2INT1(NXI+2,XIW,FIXW,NXO,XO,FO,XMSG)
      END IF


      RETURN
      END

c -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DLININT2(NXI,XI,NYI,YI,FI,ICYCX,NXO,XO,NYO,YO,FO,XMSG,
     +                   IER)
      IMPLICIT NONE
      INTEGER NXI,NYI,NXO,NYO,ICYCX,IER
      DOUBLE PRECISION XI(NXI),YI(NYI),FI(NXI,NYI)
      DOUBLE PRECISION XO(NXO),YO(NYO),FO(NXO,NYO),XMSG
C NCLEND

C This is written  with GNU f77 acceptable extensions
C .   to allow for compilation on linux platforms.
c .   This could be improved considerably with f90

c NCL:  fo = linint2 (xi,yi,fi, wrapX, xo,yo, foOpt)
c
c            {nxi,nyi,nxo,nyo} = dimsizes( {xi,yi,xo,yo} )
c            fo is the same size xo, yo and same type as "fi"
c            xmsg = fi@_FillValue
c            wrapX is an NCL logical; a False ==> 0
c                                     a True  ==> 1
c            foOpt unused option
c
c            The NCL wrapper should allow for multiple datasets
c            so the user need only make one call to the function.

c perform 2D piecewise linear interpolation allowing for missing data
c .  nothing fancy

c nomenclature:
c .   nxi,nyi - lengths of xi,yi and dimensions of fi (must be >= 2)
c .   xi      - coordinates of fi (eg, lon)
c .             must be monotonically increasing
c .   yi      - coordinates of fi (eg, lat)
c .             must be monotonically increasing
c .   fi      - functional input values [2D]
c .   icycx   - 0 if fi is cyclic in x (cyclic pt should NOT included)
c .             .ne.0 if cyclic
c .   nxo,nyo - lengths of xo,yo and dimensions of fo (must be >= 2)
c .   xo      - coordinates of fo (eg, lon)
c .             must be monotonically increasing
c .   yo      - coordinates of fo (eg, lat)
c .             must be monotonically increasing
c .   fo      - functional output values [interpolated]
c .   xmsg    - missing code
c .   ier     - error code
c .             =0;   no error
c .             =1;   not enough points in input/output array
c .             =2/3; xi or yi are not monotonically increasing
c .             =4/5; xo or yo are not monotonically increasing
c
c                              automatic temporary/work arrays
      DOUBLE PRECISION XIW(0:NXI+1),FIXW(0:NXI+1),FIYW(NYI),FOYW(NYO)
      DOUBLE PRECISION FTMP(NXO,NYI),DX

c                              local
      INTEGER NX,NY
c      PRINT *," LININT2: NXI,NYI,NXO,NYO=",NXI,NYI,NXO,NYO
c                              error checking
      IER = 0
c      IF (NXI.LE.2 .OR. NYI.LE.2 .OR. NXO.LE.2 .OR. NYO.LE.2) THEN
      IF (NXI.LT.2 .OR. NYI.LT.2) THEN
          IER = 1
          RETURN
      END IF
c                              error mono increasing ?
      CALL DMONOINC(XI,NXI,2,IER)
      IF (IER.NE.0) RETURN
      CALL DMONOINC(YI,NYI,3,IER)
      IF (IER.NE.0) RETURN
      CALL DMONOINC(XO,NXO,4,IER)
      IF (IER.NE.0) RETURN
      CALL DMONOINC(YO,NYO,5,IER)
      IF (IER.NE.0) RETURN
c                               is the input array cyclic in x
      IF (ICYCX.EQ.0) THEN
c      PRINT *," LININT2: ICYCX=",ICYCX
c                               interpolate in the x direction
          DO NY = 1,NYI
              CALL DLIN2INT1(NXI,XI,FI(1,NY),NXO,XO,FTMP(1,NY),XMSG)
          END DO
c                               interpolate in the y direction
          DO NX = 1,NXO
              DO NY = 1,NYI
                  FIYW(NY) = FTMP(NX,NY)
              END DO

              CALL DLIN2INT1(NYI,YI,FIYW,NYO,YO,FOYW,XMSG)
              DO NY = 1,NYO
                  FO(NX,NY) = FOYW(NY)
              END DO
          END DO
      ELSE
c      PRINT *," LININT2: ICYCX=",ICYCX
c                               must be cyclic in x
c                               create cyclic "x" coordinates
          DO NX = 1,NXI
              XIW(NX) = XI(NX)
          END DO
          DX = XI(2) - XI(1)
          XIW(0) = XI(1) - DX
          XIW(NXI+1) = XI(NXI) + DX

          DO NY = 1,NYI
              DO NX = 1,NXI
                  FIXW(NX) = FI(NX,NY)
              END DO
              FIXW(0) = FI(NXI,NY)
              FIXW(NXI+1) = FI(1,NY)
              CALL DLIN2INT1(NXI+2,XIW,FIXW,NXO,XO,FTMP(1,NY),XMSG)
          END DO
c                               interpolate in the y direction
          DO NX = 1,NXO
              DO NY = 1,NYI
                  FIYW(NY) = FTMP(NX,NY)
              END DO

              CALL DLIN2INT1(NYI,YI,FIYW,NYO,YO,FOYW,XMSG)

              DO NY = 1,NYO
                  FO(NX,NY) = FOYW(NY)
              END DO
          END DO
      END IF


      RETURN
      END
c -----------------------------------------------------------
      SUBROUTINE DLIN2INT1(NIN,XI,FI,NOUT,XO,FO,XMSG)
      IMPLICIT NONE
      INTEGER NIN,NOUT
      DOUBLE PRECISION XI(NIN),FI(NIN),XO(NOUT),FO(NOUT),XMSG

c perform 1D piecewise linear interpolation allowing for missing data
c .  this works with dlin2int [does no error chk]
c .  nothing fancy
c                              local
      INTEGER NI,NO,NISTRT
      DOUBLE PRECISION SLOPE
c                              initialize
      DO NO=1,NOUT
         FO(NO) = XMSG
      END DO
c                              main loop [exact matches]
c                              nistrt minimizes extra checks
      NISTRT = 1
      DO NO = 1,NOUT
          DO NI = NISTRT,NIN
              IF (XO(NO).EQ.XI(NI)) THEN
                  FO(NO) = FI(NI)
                  NISTRT = NI + 1
                  GO TO 10
              END IF
          END DO
   10     CONTINUE
      END DO
c                              main loop [interpolation]
      DO NO = 1,NOUT
          DO NI = 1,NIN - 1
              IF (XO(NO).GT.XI(NI) .AND. XO(NO).LT.XI(NI+1)) THEN
                  IF (FI(NI).NE.XMSG .AND. FI(NI+1).NE.XMSG) THEN
                      SLOPE = (FI(NI+1)-FI(NI))/ (XI(NI+1)-XI(NI))
                      FO(NO) = FI(NI) + SLOPE* (XO(NO)-XI(NI))
                  END IF
              END IF
          END DO
      END DO

      RETURN
      END
c ---------------------------------------------
      SUBROUTINE DMONOINC(X,NX,NER,IER)
      IMPLICIT NONE

c chk to make sure that x is monotonically increasing

      INTEGER NX,NER,IER
      DOUBLE PRECISION X(NX)
c                          local
      INTEGER N

      IER = 0
      DO N = 1,NX - 1
          IF (X(N+1).LE.X(N)) THEN
              IER = NER
              RETURN
          END IF
      END DO

      RETURN
      END

c -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DLININT2PTS(NXI,XI,NYI,YI,FI,ICYCX,NXYO,XO,YO,FO,
     +                       XMSG,IER)
      IMPLICIT NONE
      INTEGER NXI,NYI,NXYO,ICYCX,IER
      DOUBLE PRECISION XI(NXI),YI(NYI),FI(NXI,NYI)
      DOUBLE PRECISION XO(NXYO),YO(NXYO),FO(NXYO),XMSG
C NCLEND

C This is written  with GNU f77 acceptable extensions
C .   to allow for compilation on linux platforms.
c .   This could be improved considerably with f90

c NCL:  fo = linint2_points (xi,yi,fi, wrapX, xo,yo, foOpt)
c
c            {nxi,nyi} = dimsizes( {xi,yi} )
c            {nxyo} = dimsizes( {xo} )  = dimsizes( {yo} )
c            fo is the same size xo, yo and same type as "fi"
c            xmsg = fi@_FillValue
c            wrapX is an NCL logical; a False ==> 0
c                                     a True  ==> 1
c            foOpt unused option
c
c            The NCL wrapper should allow for multiple datasets
c            so the user need only make one call to the function.

c perform 2D piecewise linear interpolation allowing for missing data
c .  nothing fancy

c nomenclature:
c .   nxi,nyi - lengths of xi,yi and dimensions of fi (must be >= 2)
c .   xi      - coordinates of fi (eg, lon)
c .             must be monotonically increasing
c .   yi      - coordinates of fi (eg, lat)
c .             must be monotonically increasing
c .   fi      - functional input values [2D]
c .   icycx   - 0 if fi is cyclic in x (cyclic pt should NOT included)
c .             .ne.0 if cyclic
c .   nxyo    - number of xo,yo coordinate pairs
c .   xo,yo   - coordinate pairs
c .   fo      - functional output values [interpolated]
c .   xmsg    - missing code
c .   ier     - error code
c .             =0;   no error
c .             =1;   not enough points in input/output array
c .             =2/3; xi or yi are not monotonically increasing
c
c                              automatic temporary/work arrays
      DOUBLE PRECISION XIW(0:NXI+1),FIXW(0:NXI+1,NYI)
      DOUBLE PRECISION DX

c                              local
      INTEGER NX,NY
c                              error checking
      IER = 0
      IF (NXI.LT.2 .OR. NYI.LT.2) THEN
          IER = 1
          RETURN
      END IF
c                              error mono increasing ?
      CALL DMONOINC(XI,NXI,2,IER)
      IF (IER.NE.0) RETURN
      CALL DMONOINC(YI,NYI,3,IER)
      IF (IER.NE.0) RETURN
c                               is the input array cyclic in x
      IF (ICYCX.EQ.0) THEN
          CALL DLINT2XY(NXI,XI,NYI,YI,FI,NXYO,XO,YO,FO,XMSG,IER)
      ELSE
c                               must be cyclic in x
c                               create cyclic "x" coordinates
          DO NX = 1,NXI
              XIW(NX) = XI(NX)
          END DO
          DX = XI(2) - XI(1)
          XIW(0) = XI(1) - DX
          XIW(NXI+1) = XI(NXI) + DX

          DO NY = 1,NYI
              DO NX = 1,NXI
                  FIXW(NX,NY) = FI(NX,NY)
              END DO
              FIXW(0,NY) = FI(NXI,NY)
              FIXW(NXI+1,NY) = FI(1,NY)
          END DO
          CALL DLINT2XY(NXI+2,XIW,NYI,YI,FIXW,NXYO,XO,YO,FO,XMSG,IER)
      END IF


      RETURN
      END
c -----------------------------------------------------------
c                           could be improved with f90 cycle/continue
      SUBROUTINE DLINT2XY(NXI,XI,NYI,YI,FI,NXYO,XO,YO,FO,XMSG,IER)
      IMPLICIT NONE
      INTEGER NXI,NYI,NXYO,IER
      DOUBLE PRECISION XI(NXI),YI(NYI),FI(NXI,NYI),XMSG
      DOUBLE PRECISION XO(NXYO),YO(NXYO),FO(NXYO)
c                               local
      INTEGER N,M,NXY,NN,MM
      DOUBLE PRECISION TMP1,TMP2,SLPX,SLPY

      DO NXY = 1,NXYO
          FO(NXY) = XMSG

          NN = 0
          DO N = 1,NXI - 1
              IF (XO(NXY).GE.XI(N) .AND. XO(NXY).LT.XI(N+1)) THEN
                  NN = N
                  GO TO 20
              END IF
          END DO

   20     MM = 0
          DO M = 1,NYI - 1
              IF (YO(NXY).GE.YI(M) .AND. YO(NXY).LT.YI(M+1)) THEN
                  MM = M
                  GO TO 40
              END IF
          END DO

   40     IF (NN.NE.0 .AND. MM.NE.0) THEN
              IF (XO(NXY).EQ.XI(NN) .AND. YO(NXY).EQ.YI(MM)) THEN
c                               exact location [no interpolation]
                  FO(NXY) = FI(NN,MM)
              ELSE
c                               must interpolate
                  IF (FI(NN,MM).NE.XMSG .AND. FI(NN+1,MM).NE.XMSG .AND.
     +                FI(NN,MM+1).NE.XMSG .AND.
     +                FI(NN+1,MM+1).NE.XMSG) THEN
c                               slopes
                      SLPX = (XO(NXY)-XI(NN))/ (XI(NN+1)-XI(NN))
                      SLPY = (YO(NXY)-YI(MM))/ (YI(MM+1)-YI(MM))
c                               interpolate in "x" first
                      TMP1 = FI(NN,MM) + SLPX* (FI(NN+1,MM)-FI(NN,MM))
                      TMP2 = FI(NN,MM+1) + SLPX*
     +                       (FI(NN+1,MM+1)-FI(NN,MM+1))
c                               interpolate in "y"
                      FO(NXY) = TMP1 + SLPY* (TMP2-TMP1)
                  END IF
              END IF
          END IF

      END DO

      RETURN
      END
