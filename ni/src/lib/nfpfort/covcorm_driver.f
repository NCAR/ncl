c BOGUS driver
c     subroutine dcovcordriver (nrow,ncol,x,xmsg,iopt,ier)
c     implicit none
c                                              ! input
c     integer nrow, ncol, iopt(2), ier
c     double precision x(nrow,ncol), xmsg 
c                                              ! output
c     real, allocatable, dimension(:)   :: vcm1d
c     real, allocatable, dimension(:,:) :: vcm2d
c     
c     integer lvcm
c
c fortran is column major
c     lvcm = (ncol*(ncol+1))/2
c
c     if (iopt(1).eq.0) then
c                                              ; return sym storage mode
c         allocate (vcm1d(lvcm), stat=ier)
c         call dcovcormssm (nrow,ncol,x,xmsg,iopt(2),vcm1d,lvcm,ier) 
c         return(vcm1d)
c     else
c                                              ; return as 2D
c         allocate (vcm2d(ncol,ncol), stat=ier)
c         call dcovcorm (nrow,ncol,x,xmsg, iopt, vcm2d, lvcm,ier)
c         return(vcm2d)
c     end if

c     return 
c     end
          
      
c    
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
