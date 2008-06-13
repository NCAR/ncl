C NCLFORTSTART
      subroutine dcovcormssm (nrow,ncol,x,xmsg,iopt,vcm,lvcm,ier)

c This is a driver routine to a suite of subroutines that
c     will calculate covariance or correlation matrices
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     xmsg     - missing data code (if none set to some no. not encountered)
c     vcm      - var-cov matrix
c                                              ! input
      integer nrow, ncol, iopt, lvcm, ier
      double precision x(nrow,ncol), xmsg 
c                                              ! output
      double precision vcm(lvcm)    
C NCLEND

      ier = 0

      if (iopt.eq.0) then
          call dvcmssm (x,nrow,ncol,nrow,ncol,xmsg,vcm,lvcm,ier)
      else
          call dcrmssm (x,nrow,ncol,nrow,ncol,xmsg,vcm,lvcm,ier)
      end if

      return
      end

C NCLFORTSTART
      subroutine dcovcorm (nrow,ncol,x,xmsg, iopt, vcm, lvcm,ier)
c
c This is a driver routine to a suite of subroutines that
c     will calculate covariance or correlation matrices

c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     xmsg     - missing data code (if none set to some no. not encountered)
c     vcm      - var-cov matrix
c     lvcm     - not used
c                                              ! input
      integer nrow, ncol, iopt,lvcm, ier
      double precision x(nrow,ncol), xmsg 
c                                              ! output
      double precision vcm(ncol,ncol)   
C NCLEND

      ier = 0

      if (iopt.eq.0) then
          call dvcvmns (x,nrow,ncol,nrow,ncol,xmsg,vcm,lvcm,ier)
      else
          call dcormns (x,nrow,ncol,nrow,ncol,xmsg,vcm,lvcm,ier)
      end if

      return
      end
