C -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DLININT1(NXI,XI,FI,ICYCX,NXO,XO,FO,XIW,FXIW,NXI2,XMSG,
     +                    IOPT,IER)
      IMPLICIT NONE
      INTEGER NXI,NXO,NXI2,IOPT,ICYCX,IER
      DOUBLE PRECISION XI(NXI),FI(NXI)
      DOUBLE PRECISION XO(NXO),FO(NXO),XMSG
      DOUBLE PRECISION XIW(NXI2),FXIW(NXI2)
C NCLEND

C This is written  with GNU f77 acceptable extensions
C .   to allow for compilation on linux platforms.
c .   This could be improved considerably with f90

c NCL:  fo = linint1 (xi,fi, wrapX, xo, iopt)
c
c            {nxi,nxo} = dimsizes( {xi,xo} )
c            fo is the same size xo and same type as "fi"
c            xmsg = fi@_FillValue
c            wrapX is an NCL logical; a False ==> 0
c                                     a True  ==> 1
c            iopt = 0 try to preserve msg areas
c                 = 1 try to fill in entire grid
c
c            The NCL wrapper should allow for multiple datasets
c            so the user need only make one call to the function.

c perform piecewise linear interpolation allowing for missing data
c .  nothing fancy

c nomenclature:
c .   nxi     - lengths of xi and dimensions of fi (must be >= 2)
c .   xi      - coordinates of fi (eg, lon)
c .             must be monotonically (in/de)creasing [same as xo]
c .   fi      - functional input values [2D]
c .   icycx   - 0 if fi is cyclic in x (cyclic pt should NOT included)
c .             .ne.0 if cyclic
c .   nxo     - lengths of xo and dimensions of fo (must be >= 1)
c .   xo      - coordinates of fo (eg, lon)
c .             must be monotonically (in/de)creasing [same as xi]
c .   fo      - functional output values [interpolated]
c .   xmsg    - missing code
c .   iopt    - options
c .           - iopt=0  means to try to preserve missing areas
c .           - iopt=1  means to try to try to fill-in missing areas
c .   ier     - error code
c .             =0;   no error
c .             =1;   not enough points in input/output array
c .             =2;   xi are not monotonically increasing
c .             =4;   xo yo are not monotonically increasing
c
c                              local
      INTEGER NX,NPTS,IFLAG,NXSTRT,NXLAST
c                              gross error checking
      IER = 0
      IF (NXO.LT.1) THEN
          IER = 1
          RETURN
      END IF
c                              initialize to msg
      DO NX = 1,NXO
          FO(NX) = XMSG
      END DO

      IF (NXI.LE.1) THEN
          IER = 1
          RETURN
      END IF
c                              mono (in/de)creasing ?
      CALL DMONOID2(NXI,XI,NXO,XO,IFLAG,IER)
      IF (IFLAG.EQ.0 .OR. IER.NE.0) RETURN
c                              are data to be treated cyclic?
      IF (ICYCX.EQ.0) THEN
c                              data are not cyclic
          IF (IOPT.EQ.0) THEN

              CALL DLIN2INT1(NXI,XI,FI,NXO,XO,FO,XMSG,IFLAG)
          ELSE IF (IOPT.EQ.1) THEN
c                              collapse data array (eliminate msg)
              NPTS = 0
              DO NX = 1,NXI
                  IF (FI(NX).NE.XMSG) THEN
                      NPTS = NPTS + 1
                      XIW(NPTS+1)  = XI(NX)
                      FXIW(NPTS+1) = FI(NX)
                  END IF
              END DO

              CALL DLIN2INT1(NPTS,XIW(2),FXIW(2),NXO,XO,FO,XMSG,IFLAG)
          END IF
      ELSE
c                              data are cyclic
          IF (IOPT.EQ.0) THEN
c                              preserve msg region
              DO NX = 1,NXI
                  XIW(NX+1)  = XI(NX)
                  FXIW(NX+1) = FI(NX)
              END DO

              CALL DLINCYC(NXI,XI,FI,1,NXI,IFLAG,XIW,FXIW,NXI)
              CALL DLIN2INT1(NXI+2,XIW,FXIW,NXO,XO,FO,XMSG,IFLAG)

          ELSE IF (IOPT.EQ.1) THEN
c                              collapse data array
              NPTS = 0
              NXSTRT = 0
              NXLAST = 0
              DO NX = 1,NXI
                  IF (FI(NX).NE.XMSG) THEN
                      NPTS = NPTS + 1
                      XIW(NPTS+1)  = XI(NX)
                      FXIW(NPTS+1) = FI(NX)
                      IF (NPTS.EQ.1) NXSTRT = NX
                      NXLAST = NX
                  END IF
              END DO

              IF (NPTS.EQ.0) THEN
                  IER = 1
                  RETURN
              END IF

              CALL DLINCYC(NXI,XI,FI,NXSTRT,NXLAST,IFLAG,XIW,FXIW,NPTS)
              CALL DLIN2INT1(NPTS+2,XIW,FXIW,NXO,XO,FO,XMSG,IFLAG)
c c c         do nx=0,npts+1
c c c            write (*,"(i5,2(1x,f10.5))") nx, xiw(nx+1), fxiw(nx+1)
c c c         end do
          END IF

      END IF

      RETURN
      END
c -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DLININT2(NXI,XI,NYI,YI,FI,ICYCX,NXO,XO,NYO,YO,FO,XIW,
     +                    FXIW,NXI2,XMSG,IOPT,IER)
      IMPLICIT NONE
      INTEGER NXI,NYI,NXO,NYO,NXI2,ICYCX,IOPT,IER
      DOUBLE PRECISION XI(NXI),YI(NYI),FI(NXI,NYI)
      DOUBLE PRECISION XO(NXO),YO(NYO),FO(NXO,NYO),XMSG
      DOUBLE PRECISION XIW(NXI2),FXIW(NXI2)
C NCLEND

C This is written  with GNU f77 acceptable extensions
C .   to allow for compilation on linux platforms.
c .   This could be improved considerably with f90

c NCL:  fo = linint2 (xi,yi,fi, wrapX, xo,yo, iopt)
c
c            {nxi,nyi,nxo,nyo} = dimsizes( {xi,yi,xo,yo} )
c            fo is the same size xo, yo and same type as "fi"
c            xmsg = fi@_FillValue
c            wrapX is an NCL logical; a False ==> 0
c                                     a True  ==> 1
c
c            The NCL wrapper should allow for multiple datasets
c            so the user need only make one call to the function.

c perform bilinear interpolation allowing for missing data
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
c .   iopt    - =0 try to preserve msg areas
c .             =1 try to fill in entire grid
c .   ier     - error code
c .             =0;   no error
c .             =1;   not enough points in input/output array
c .             =2;   xi or xo are not monotonically (in/de)creasing
c .             =3;   yi or yo are not monotonically (in/de)creasing
c
c                              local and temporary/work arrays
      INTEGER IFLAG,NPTS,NXSTRT,NXLAST
      DOUBLE PRECISION YIW(NYI),FYIW(NYI),FOYW(NYO)
      DOUBLE PRECISION FTMP(NXO,NYI)

c                              local
      INTEGER NX,NY
c                              error checking
      IER = 0
      IF (NXO.LT.1 .OR. NYO.LT.1) THEN
          IER = 1
          RETURN
      END IF
c                              set to msg
      DO NY = 1,NYO
          DO NX = 1,NXO
              FO(NX,NY) = XMSG
          END DO
      END DO

      IF (NXI.LT.2 .OR. NYI.LT.2) THEN
          IER = 1
          RETURN
      END IF
c                              error mono increasing ?
      CALL DMONOID2(NXI,XI,NXO,XO,IFLAG,IER)
      IF (IFLAG.EQ.0 .OR. IER.NE.0) THEN
          IER = 2
          RETURN
      END IF
      CALL DMONOID2(NYI,YI,NYO,YO,IFLAG,IER)
      IF (IFLAG.EQ.0 .OR. IER.NE.0) THEN
          IER = 3
          RETURN
      END IF
c                               is the input array cyclic in x
      IF (ICYCX.EQ.0) THEN
c                               preserve missing areas
          IF (IOPT.EQ.0) THEN
c                               interpolate in the x direction
              DO NY = 1,NYI
                  CALL DLIN2INT1(NXI,XI,FI(1,NY),NXO,XO,FTMP(1,NY),
     +                          XMSG,IFLAG)
              END DO
c                               interpolate in the y direction
              DO NX = 1,NXO
                  DO NY = 1,NYI
                      FYIW(NY) = FTMP(NX,NY)
                  END DO

                  CALL DLIN2INT1(NYI,YI,FYIW,NYO,YO,FOYW,XMSG,IFLAG)

                  DO NY = 1,NYO
                      FO(NX,NY) = FOYW(NY)
                  END DO
              END DO
          ELSE IF (IOPT.EQ.1) THEN
c                               interpolate in the x direction
c                               collapse data array
              DO NY = 1,NYI
                  NPTS = 0
                  DO NX = 1,NXI
                      IF (FI(NX,NY).NE.XMSG) THEN
                          NPTS = NPTS + 1
                          XIW(NPTS+1)  = XI(NX)
                          FXIW(NPTS+1) = FI(NX,NY)
                      END IF
                  END DO

                  CALL DLIN2INT1(NPTS,XIW(2),FXIW(2),NXO,XO,FTMP(1,NY),
     +                          XMSG,IFLAG)
              END DO
c                               interpolate in the y direction
              DO NX = 1,NXO
                  NPTS = 0
                  DO NY = 1,NYI
                      IF (FTMP(NX,NY).NE.XMSG) THEN
                          NPTS = NPTS + 1
                          YIW(NPTS) = YI(NY)
                          FYIW(NPTS) = FTMP(NX,NY)
                      END IF
                  END DO

                  CALL DLIN2INT1(NPTS,YIW,FYIW,NYO,YO,FOYW,XMSG,IFLAG)

                  DO NY = 1,NYO
                      FO(NX,NY) = FOYW(NY)
                  END DO
              END DO

          END IF
      ELSE
c                               must be cyclic in x
c                               create cyclic "x" coordinates
          IF (IOPT.EQ.0) THEN
              DO NY = 1,NYI
                  DO NX = 1,NXI
                      XIW(NX+1)  = XI(NX)
                      FXIW(NX+1) = FI(NX,NY)
                  END DO

                  NPTS = NXI
                  CALL DLINCYC(NXI,XI,FI(1,NY),1,NXI,IFLAG,XIW,FXIW,
     +                         NPTS)
                  CALL DLIN2INT1(NXI+2,XIW,FXIW,NXO,XO,FTMP(1,NY),XMSG,
     +                           IFLAG)
              END DO
c                               interpolate in the y direction
              DO NX = 1,NXO
                  DO NY = 1,NYI
                      FYIW(NY) = FTMP(NX,NY)
                  END DO

                  CALL DLIN2INT1(NYI,YI,FYIW,NYO,YO,FOYW,XMSG,IFLAG)

                  DO NY = 1,NYO
                      FO(NX,NY) = FOYW(NY)
                  END DO
              END DO

          ELSE IF (IOPT.EQ.1) THEN
              DO NY = 1,NYI
c                              collapse data array
                  NPTS = 0
                  NXSTRT = 0
                  NXLAST = 0
                  DO NX = 1,NXI
                      IF (FI(NX,NY).NE.XMSG) THEN
                          NPTS = NPTS + 1
                          XIW(NPTS+1)  = XI(NX)
                          FXIW(NPTS+1) = FI(NX,NY)
                          IF (NPTS.EQ.1) NXSTRT = NX
                          NXLAST = NX
                      END IF
                  END DO

                  CALL DLINCYC(NXI,XI,FI(1,NY),NXSTRT,NXLAST,IFLAG,XIW,
     +                         FXIW,NPTS)
                  CALL DLIN2INT1(NPTS+2,XIW,FXIW,NXO,XO,FTMP(1,NY),
     +                           XMSG,IFLAG)
              END DO
c                               interpolate in the y direction
              DO NX = 1,NXO
                  NPTS = 0
                  DO NY = 1,NYI
                      IF (FTMP(NX,NY).NE.XMSG) THEN
                          NPTS = NPTS + 1
                          YIW(NPTS) = YI(NY)
                          FYIW(NPTS) = FTMP(NX,NY)
                      END IF
                  END DO

                  CALL DLIN2INT1(NPTS,YIW,FYIW,NYO,YO,FOYW,XMSG,IFLAG)

                  DO NY = 1,NYO
                      FO(NX,NY) = FOYW(NY)
                  END DO
              END DO
          END IF
      END IF

      RETURN
      END
c -----------------------------------------------------------
      SUBROUTINE DLIN2INT1(NIN,XI,FI,NOUT,XO,FO,XMSG,IFLAG)
      IMPLICIT NONE
      INTEGER NIN,NOUT,IFLAG
      DOUBLE PRECISION XI(NIN),FI(NIN),XO(NOUT),FO(NOUT),XMSG

c perform 1D piecewise linear interpolation allowing for missing data
c .  this works with lin2int [does no error chk]
c .  nothing fancy
c                              local
      INTEGER NI,NO,NISTRT,NIS,NEXACT
      DOUBLE PRECISION SLOPE

      DO NO = 1,NOUT
          FO(NO) = XMSG
      END DO
c                              main loop [exact matches]
c                              nistrt minimizes extra checks
      NEXACT = 0
      NISTRT = 1
      NIS = NISTRT
      DO NO = 1,NOUT
          DO NI = NISTRT,NIN
              IF (XO(NO).EQ.XI(NI)) THEN
                  FO(NO) = FI(NI)
                  NIS = NI + 1
                  NEXACT = NEXACT + 1
                  GO TO 10
              END IF
          END DO
   10     NISTRT = NIS
      END DO

c                              main loop [interpolation]
      IF (IFLAG.EQ.1) THEN
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
      ELSE IF (IFLAG.EQ.-1) THEN
          DO NO = 1,NOUT
              DO NI = 1,NIN - 1
                  IF (XO(NO).LT.XI(NI) .AND. XO(NO).GT.XI(NI+1)) THEN
                      IF (FI(NI).NE.XMSG .AND. FI(NI+1).NE.XMSG) THEN
                          SLOPE = (FI(NI+1)-FI(NI))/ (XI(NI+1)-XI(NI))
                          FO(NO) = FI(NI) + SLOPE* (XO(NO)-XI(NI))
                      END IF
                  END IF
              END DO
          END DO
      END IF

      RETURN
      END
c ---------------------------------------------
      SUBROUTINE DLINCYC(NXI,XI,FI,NXSTRT,NXLAST,IFLAG,XIW,FXIW,NPTS)
c
c handle the "x" cyclic point
c
      IMPLICIT NONE
      INTEGER NXI,NXSTRT,NXLAST,IFLAG,NPTS
      DOUBLE PRECISION XI(NXI),FI(NXI),XIW(0:NXI+1),FXIW(0:NXI+1)
      DOUBLE PRECISION DX


      IF (NXSTRT.EQ.1 .AND. NXLAST.EQ.NXI) THEN
          DX = ABS(XI(2)-XI(1))
          XIW(0) = XI(1) - IFLAG*DX
          FXIW(0) = FI(NXI)
          DX = ABS(XI(NXI)-XI(NXI-1))
          XIW(NPTS+1) = XI(NXI) + IFLAG*DX
          FXIW(NPTS+1) = FI(1)
      ELSE
c                              arbitrary
          DX = (NXSTRT+ (NXI-NXLAST))*ABS(XI(2)-XI(1))
          XIW(0) = XIW(1) - IFLAG*DX
          FXIW(0) = FXIW(NPTS)
          DX = ((NXI-NXLAST)+NXSTRT)*ABS(XI(NXI)-XI(NXI-1))
          XIW(NPTS+1) = XIW(NPTS) + IFLAG*DX
          FXIW(NPTS+1) = FXIW(1)
      END IF

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
      IF (NX.LE.1) RETURN

      DO N = 1,NX - 1
          IF (X(N+1).LE.X(N)) THEN
              IER = NER
              RETURN
          END IF
      END DO

      RETURN
      END
c     ---------------------------------------------------
      SUBROUTINE DMONOID1(NIN,XI,IFLAG,IER)
c
c chk to see if a series is mono (in/de)creasing
c
      IMPLICIT NONE
      INTEGER NIN,IFLAG,IER
      DOUBLE PRECISION XI(NIN)
c                              local
      INTEGER NI

      IER = 0
      IFLAG = 0
c
c if only one element, then treat it as monotonically increasing
c
      IF (NIN.LT.2) THEN
         IFLAG = +1
         RETURN
      END IF

      IF (XI(2).GT.XI(1)) THEN
c                              ? mono INcreasing
          DO NI = 1,NIN - 1
              IF (XI(NI+1).LE.XI(NI)) THEN
                  IER = 2
                  RETURN
              END IF
          END DO

          IFLAG = +1
      ELSE
c                              ? mono DEcreasing
          DO NI = 1,NIN - 1
              IF (XI(NI+1).GE.XI(NI)) THEN
                  IER = 2
                  RETURN
              END IF
          END DO

          IFLAG = -1
      END IF

      RETURN
      END
c     ---------------------------------------------------
      SUBROUTINE DMONOID2(NIN,XI,NOUT,XO,IFLAG,IER)
c
c make sure the two series are mono (in/de)creasing
c
      IMPLICIT NONE
      INTEGER NIN,NOUT,IFLAG,IER
      DOUBLE PRECISION XI(NIN),XO(NOUT)
c                              local
      INTEGER NFLAG,NER

      IER = 0
      IFLAG = 0
      NFLAG = 0

      CALL DMONOID1(NIN,XI,IFLAG,IER)
      IF (IFLAG.EQ.0) THEN
         IER = 2
      ELSE
         CALL DMONOID1(NOUT,XO,NFLAG,NER)
         IF (NFLAG.EQ.0) THEN
            IER = 3
         ELSE IF (IFLAG.NE.NFLAG) THEN
            IER = 4
         END IF
      END IF

      RETURN
      END

c -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DLININT2PTS(NXI,XI,NYI,YI,FI,ICYCX,NXYO,XO,YO,FO,
     +                       XIW,FIXW,NXI2,XMSG,IER)
      IMPLICIT NONE
      INTEGER NXI,NYI,NXYO,ICYCX,NXI2,IER
      DOUBLE PRECISION XI(NXI),YI(NYI),FI(NXI,NYI)
      DOUBLE PRECISION XO(NXYO),YO(NXYO),FO(NXYO)
      DOUBLE PRECISION XIW(NXI2),FIXW(NXI2,NYI),XMSG
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
c .   nopt    - option for how to handle case when not all values
c .              present
c .              >= 0 means set the interpolated point to xmsg
c .              =-1 try some sort of weighted average
c .   ier     - error code
c .             =0;   no error
c .             =1;   not enough points in input/output array
c .             =2/3; xi or yi are not monotonically increasing
c
c                              automatic temporary/work arrays
      DOUBLE PRECISION DX

c                              local
      INTEGER NX,NY,NOPT
c                              this could be made an argument
      NOPT = -1
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
          CALL DLINT2XY(NXI,XI,NYI,YI,FI,NXYO,XO,YO,FO,XMSG,NOPT,IER)
      ELSE
c                               must be cyclic in x
c                               create cyclic "x" coordinates
          DO NX = 1,NXI
              XIW(NX+1) = XI(NX)
          END DO
          DX = XI(2) - XI(1)
          XIW(1)    = XI(1) - DX
          XIW(NXI2) = XI(NXI) + DX

          DO NY = 1,NYI
              DO NX = 1,NXI
                  FIXW(NX+1,NY) = FI(NX,NY)
              END DO
              FIXW(1,NY)    = FI(NXI,NY)
              FIXW(NXI2,NY) = FI(1,NY)
          END DO
          CALL DLINT2XY(NXI2,XIW,NYI,YI,FIXW,NXYO,XO,YO,FO,XMSG,NOPT,
     +                  IER)
      END IF


      RETURN
      END

c -----------------------------------------------------------
c                           could be improved with f90 cycle/continue
      SUBROUTINE DLINT2XY(NXI,XI,NYI,YI,FI,NXYO,XO,YO,FO,XMSG,NOPT,IER)
      IMPLICIT NONE
      INTEGER NXI,NYI,NXYO,NOPT,IER
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
              ELSE IF (FI(NN,MM).NE.XMSG .AND. FI(NN+1,MM).NE.XMSG .AND.
     +                 FI(NN,MM+1).NE.XMSG .AND.
     +                 FI(NN+1,MM+1).NE.XMSG) THEN
c                               must interpolate: calculate slopes
                  SLPX = (XO(NXY)-XI(NN))/ (XI(NN+1)-XI(NN))
                  SLPY = (YO(NXY)-YI(MM))/ (YI(MM+1)-YI(MM))
c                               interpolate in "x" first
                  TMP1 = FI(NN,MM) + SLPX* (FI(NN+1,MM)-FI(NN,MM))
                  TMP2 = FI(NN,MM+1) + SLPX* (FI(NN+1,MM+1)-FI(NN,MM+1))
c                               interpolate in "y"
                  FO(NXY) = TMP1 + SLPY* (TMP2-TMP1)
              ELSE IF (NOPT.EQ.-1) THEN
                  CALL ESTFOW(FI(NN,MM),FI(NN+1,MM),FI(NN,MM+1),
     +                        FI(NN+1,MM+1),XI(NN),XI(NN+1),YI(MM),
     +                        YI(MM+1),FO(NXY),XO(NXY),YO(NXY),XMSG)
              END IF
          END IF

      END DO

      RETURN
      END
c ---------------------------------------------
      SUBROUTINE ESTFOW(F1,F2,F3,F4,X1,X2,Y1,Y2,F0,X0,Y0,XMSG)
      IMPLICIT NONE
      DOUBLE PRECISION F1,F2,F3,F4,X1,X2,Y1,Y2,F0,X0,Y0,XMSG
c local
      DOUBLE PRECISION FI(2,2),W(2,2),SUM,SWT
      INTEGER N,M

c     f3(x1,y2)+++++++++++f4(x2,y2)
c        f0(x0,y0)         someplace within the surrounding 4 pts
c     f3(x1,y1)+++++++++++f4(x2,y1)

c if all msg return

      F0 = XMSG
      IF (F1.EQ.XMSG .AND. F2.EQ.XMSG .AND. F3.EQ.XMSG .AND.
     +    F4.EQ.XMSG) RETURN

c compute 'distance wgted average
c .   make assumption that dx/dy are small and
c .   pythag theorem is good enough

c inverse distance wgts
      W(1,1) = 1.D0/SQRT((X1-X0)**2+ (Y1-Y0)**2)
      W(2,1) = 1.D0/SQRT((X2-X0)**2+ (Y1-Y0)**2)
      W(1,2) = 1.D0/SQRT((X1-X0)**2+ (Y2-Y0)**2)
      W(2,2) = 1.D0/SQRT((X2-X0)**2+ (Y2-Y0)**2)

      FI(1,1) = F1
      FI(2,1) = F2
      FI(1,2) = F3
      FI(2,2) = F4

      SUM = 0.D0
      SWT = 0.D0
      DO M = 1,2
          DO N = 1,2
              IF (FI(N,M).NE.XMSG) THEN
                  SUM = SUM + FI(N,M)*W(N,M)
                  SWT = SWT + W(N,M)
              END IF
          END DO
      END DO
c                               wgted average
      IF (SWT.GT.0.D0) THEN
          F0 = SUM/SWT
      END IF

      RETURN
      END
