C NCLFORTSTART
      DOUBLE PRECISION FUNCTION DFLXEDY(X,Y,NPTS,XMSG,IER)
      INTEGER NPTS,IER
      DOUBLE PRECISION X(NPTS),Y(NPTS),XMSG
C*PT*WARNING* Already double-precision
C NCLEND

c NCL: z = fluxEddy (x,y) ; "z" will be allocated by the NCL driver

c NCL Note to Dennis: we could expand the calling sequence as follows
c  double precision function flxedy (x,y,npts,xmsg,xave,yave,xpxp,ypyp,
c                                    cc,ntot,ier)
c   and then return "xave,yave,xpxp,ypyp,cc,ntot" as attributes of "z"
c   Too much memory if doing this over a grid ... attributes
c   can not be multiply dimensioned.

c Calculate time mean eddy flux quantities (eg, ave{v'T'})
c .   Method: xy = XY + x'y' where x and y are instantaneous values
c .                                X     Y are time means of x and y
c .           x'y' = xy-XY
C*PL*ERROR* Comment line too long
c Calculate the correlation coef between x'y' (add to arguments if desired)
c .            cc  = x'y'/sqrt(x'x' + y'y')

C Example:   double u(ntims,mlon,nlat,klev), v(ntims,mlon,nlat,klev)
c .          double upvp(mlon,nlat,klev)
c .          do kl=1,klev
c .           do nl=1,nlat
c .            do ml=1,mlon
c .               upvp(ml,nl,kl) =
c .               dflxedy (u(1,ml,nl,kl),v(1,ml,nl,kl),ntims,xmsg,ier)
c .            end do
c .           end do
c .          end do

c local stuff

      DOUBLE PRECISION XAVE,YAVE,XXAVE,YYAVE,XYAVE,XPYP,XPXP,YPYP,XN,CC
      INTEGER N,NTOT

      IER = 0
      IF (NPTS.LE.0) THEN
          IER = 1
          DFLXEDY = XMSG
          RETURN
      END IF

      NTOT = 0
C*PT*WARNING* Constant already double-precision
      XAVE = 0.d0
C*PT*WARNING* Constant already double-precision
      YAVE = 0.d0
C*PT*WARNING* Constant already double-precision
      XYAVE = 0.d0
C*PT*WARNING* Constant already double-precision
      XXAVE = 0.d0
C*PT*WARNING* Constant already double-precision
      YYAVE = 0.d0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG .AND. Y(N).NE.XMSG) THEN
              NTOT = NTOT + 1
C*PT*WARNING* Already double-precision (DBLE)
              XAVE = XAVE + DBLE(X(N))
C*PT*WARNING* Already double-precision (DBLE)
              YAVE = YAVE + DBLE(Y(N))
C*PT*WARNING* DPROD found - result may be incorrect
              XYAVE = XYAVE + X(N)*Y(N)
C*PT*WARNING* DPROD found - result may be incorrect
              XXAVE = XXAVE + X(N)*X(N)
C*PT*WARNING* DPROD found - result may be incorrect
              YYAVE = YYAVE + Y(N)*Y(N)
          END IF
      END DO

      IF (NTOT.GT.0) THEN
C*PT*WARNING* Already double-precision (DBLE)
          XN = DBLE(DBLE(NTOT))
          XAVE = XAVE/XN
          YAVE = YAVE/XN
          XYAVE = XYAVE/XN
          XXAVE = XXAVE/XN
          YYAVE = YYAVE/XN
          XPYP = XYAVE - XAVE*YAVE
          XPXP = XXAVE - XAVE*XAVE
          YPYP = YYAVE - YAVE*YAVE
          CC = XPYP/DSQRT(XPXP*YPYP)
      ELSE
          XAVE = XMSG
          YAVE = XMSG
          XYAVE = XMSG
          XPYP = XMSG
          XPXP = XMSG
          YPYP = XMSG
          CC = XMSG
      END IF
C*PT*WARNING* Non-reversible tranformation (SNGL)

      DFLXEDY = XPYP

      RETURN
      END
