C NCLFORTSTART
      real function flxedy (x,y,npts,xmsg,ier)
      integer npts, ier
      real    x(npts), y(npts), xmsg
C NCLEND

c NCL: z = fluxEddy (x,y) ; "z" will be allocated by the NCL driver

c NCL Note to Dennis: we could expand the calling sequence as follows
c     real function flxedy (x,y,npts,xmsg,xave,yave,xpxp,ypyp,cc,ntot,ier)
c     and then return "xave,yave,xpxp,ypyp,cc,ntot" as attributes of "z"
c     Too much memory if doing this over a grid ... attributes
c     can not be multiply dimensioned.

c Calculate time mean eddy flux quantities (eg, ave{v'T'})
c .   Method: xy = XY + x'y' where x and y are instantaneous values
c .                                X     Y are time means of x and y
c .           x'y' = xy-XY
c Calculate the correlation coef between x'y' (add to arguments if desired)
c .            cc  = x'y'/sqrt(x'x' + y'y') 

C Example:   real u(ntims,mlon,nlat,klev), v(ntims,mlon,nlat,klev)
c .          real upvp(mlon,nlat,klev)
c .          do kl=1,klev
c .           do nl=1,nlat
c .            do ml=1,mlon
c .               upvp(ml,nl,kl) = 
c .                  flxedy (u(1,ml,nl,kl),v(1,ml,nl,kl),ntims,xmsg,ier)
c .            end do
c .           end do
c .          end do

c local stuff

      double precision xave, yave, xxave, yyave, xyave
     *               , xpyp, xpxp, ypyp, xn, cc 
      integer n, ntot

      ier   = 0
      if (npts.le.0) then
          ier    = 1
          flxEdy = xmsg
          return
      end if

      ntot  = 0
      xave  = 0.d0
      yave  = 0.d0
      xyave = 0.d0
      xxave = 0.d0
      yyave = 0.d0
      do n=1,npts
         if (x(n).ne.xmsg .and. y(n).ne.xmsg) then
             ntot  = ntot + 1
             xave  = xave + dble(x(n))
             yave  = yave + dble(y(n))
             xyave = xyave+ dprod(x(n),y(n))
             xxave = xxave+ dprod(x(n),x(n))
             yyave = yyave+ dprod(y(n),y(n))
         end if
      end do

      if (ntot.gt.0) then
          xn    = dble(real(ntot))
          xave  =  xave/xn
          yave  =  yave/xn
          xyave = xyave/xn
          xxave = xxave/xn
          yyave = yyave/xn
          xpyp  = xyave-xave*yave
          xpxp  = xxave-xave*xave
          ypyp  = yyave-yave*yave
          cc    = xpyp/dsqrt(xpxp*ypyp)
      else
          xave  = xmsg
          yave  = xmsg
          xyave = xmsg
          xpyp  = xmsg
          xpxp  = xmsg
          ypyp  = xmsg
          cc    = xmsg
      end if

      flxEdy = sngl(xpyp)     ! time ave eddy flux ( ave{x'y'} )

      return
      end
