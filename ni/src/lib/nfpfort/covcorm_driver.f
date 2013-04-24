C BOGUS DRIVER
C     VCM = covcorm(x[*][*], iopt[2]:integer)
C           NCL:   x(nvar,ntim)

C NCLFORTSTART
c     subroutine BOGUS (ntim,nvar,x,xmsg,iopt,ier)
c     implicit none
c                                              ! input
c     integer ntim, nvar, iopt(2), ier
c     double precision x(ntim,nvar), xmsg, trace 
c                                              ! output
c     integer lvcm
c
c     lvcm = (nvar*(nvar+1))/2
c
c     if (iopt(1).eq.0) then
c                                              ; return sym storage mode
c         real, allocatable, dimension(:)   :: vcm
c         allocate (vcm1d(lvcm), stat=ier)
c         call dcovcormssm (ntim,nvar,x,xmsg,iopt(2)
c    +                     ,vcm1d,lvcm,trace,ier) 
c         return(vcm1d)
c     else
c                                              ; return as 2D
c         real, allocatable, dimension(:,:) :: vcm
c         allocate (vcm2d(nvar,nvar), stat=ier)
c         call dcovcorm (ntim,nvar,x,xmsg, iopt(2)
c    +                  ,vcm2d, lvcm,trace,ier)
c         return(vcm2d)
c     end if

c     return 
c     end

C NCLFORTSTART
      subroutine dcovcormssm (ntim,nvar,x,xmsg,iopt
     +                       ,vcm,lvcm,trace,ier)

c This is a driver routine to a suite of subroutines that
c     will calculate covariance or correlation matrices
c     x        - input data array  ( unchanged on output)
c     ntim,nvar- exact dimensions of x in calling routine
c     xmsg     - missing data code (if none set to some no. not encountered)
c     iopt     - 0 ==> covariance, 1 ==> correlation
c     vcm      - var-cov matrix
c     trace    - sum of diagonal elements
c                                              ! input
      integer ntim, nvar, iopt, lvcm, ier
      double precision x(ntim,nvar), xmsg, trace
c                                              ! output
      double precision vcm(lvcm)
C NCLEND

      ier   = 0
c
c iopt == 0 --> calculate variance-covariance matrix
c iopt == 1 --> calculate correlation matrix
c
      if (iopt.eq.0) then
          call dvcmssm (x,ntim,nvar,ntim,nvar,xmsg,vcm,lvcm,ier)
          nn    = 0
          trace = 0.0d0
          do nv=1,nvar
             nn    = nn + nv
             trace = trace + vcm(nn)
          end do
      else
          call dcrmssm (x,ntim,nvar,ntim,nvar,xmsg,vcm,lvcm,ier)
          trace = nvar
      end if

      return
      end


C NCLFORTSTART
      subroutine dcovcorm (ntim,nvar,x,xmsg, iopt, vcm, lvcm, trace,ier)
c
c This is a driver routine to a suite of subroutines that
c     will calculate covariance or correlation matrices

c     x        - input data array  ( unchanged on output)
c     ntim,nvar- exact dimensions of x in calling routine
c     xmsg     - missing data code (if none set to some no. not encountered)
c     iopt     - 0 ==> covariance, 1 ==> correlation
c     vcm      - var-cov matrix
c     lvcm     - not used
c                                              ! input
      integer ntim, nvar, iopt,lvcm, ier
      double precision x(ntim,nvar), xmsg, trace
c                                              ! output
      double precision vcm(nvar,nvar)
C NCLEND
      ier = 0
c
c iopt == 1 --> calculate variance-covariance matrix
c iopt == 0 --> calculate correlation matrix
c
      if (iopt.eq.0) then
          call dvcvmns (x,ntim,nvar,ntim,nvar,xmsg,vcm,lvcm,ier)
          trace = 0.0d0
          do nv=1,nvar
             trace = trace + vcm(nv,nv)
          end do
      else
          call dcormns (x,ntim,nvar,ntim,nvar,xmsg,vcm,lvcm,ier)
          trace = nvar
      end if

      return
      end

