C NCLFORTSTART
      subroutine evssm (nvar, lssm, cssm, neval, weval, wevec,info,iopt)
      implicit none
                                                  ! input
      integer          nvar, lssm, info, neval, iopt
      double precision cssm(lssm)
                                                  ! output
      double precision weval(neval), wevec(nvar,neval)
C NCLEND
      integer          il  , iu  , mout, ldz, n, nn
     +                ,iwork(5*nvar), ifail(nvar)
      double precision vl  , vu  , abstol, work(8*nvar) 
      character*1      jobz, rang, uplo

c .   jobz    = "V"       ; compute eigen{values/vectors}
c .   rang    = "I"       ; compute user specified iL to iU eigenvalues
c .   uplo    = "U"       ; input is lower(L)/upper(U) triangle
c .   vl      = 1d20      ; not used if:  range= "A" or "I"
c .   vu      = 1d20      ; not used
c .   neval   = iu-il+1   ; number of eigenvalues and vectors
c .   abstol  = 0.0       ; this mean use the default tolerance
c .   mout    = 0         ; ouput number of eigenvalues

      jobz    = "V"
      rang    = "I"
      uplo    = "U"

      il      = max(nvar-neval+1,1)
      iu      = nvar

      mout    = -99       
      ldz     = nvar

      vl      = 1d20    
      vu      = 1d20   
      abstol  = 0.0d0     

      call dspevx(jobz, rang, uplo, nvar, cssm, vl, vu, il, iu
     +           ,abstol ,mout, weval, wevec, ldz, work, iwork
     +           ,ifail, info)

      if (iopt.eq.0) then

c reverse the order of arrays returned from "dspevx" so
c .   largest eigenvalues/vectors are first.

          do n=1,neval
             work(n) = weval(n)
          end do
          do n=1,neval
             weval(n) = work(neval+1-n)
          end do
    
          do n=1,nvar
              do nn=1,neval
                 work(nn) = wevec(n,nn)
              end do
              do nn=1,neval
                 wevec(n,nn) = work(neval+1-nn)
              end do
          end do
      end if

      return
      end
