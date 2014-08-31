C NCLFORTSTART
      SUBROUTINE dgeevxint(BALANC,JOBVL,JOBVR,SENSE,N,A,EVLR,WR,WI,
     +                     OPT,WORK,IWORK,SCALEM,RCONDE,RCONDV,VL,VR,
     +                     LWORK,LIWORK)
      IMPLICIT NONE
c                                                      input
      INTEGER           N, OPT
      CHARACTER*(*)     BALANC, JOBVL, JOBVR, SENSE
ccccc CHARACTER*1       BALANC, JOBVL, JOBVR, SENSE
c                                                      input and output
      DOUBLE PRECISION  A(N,N)
c                                                      output
c                                                      (:,:,real/imag,
c                                                           left/right)     
      DOUBLE PRECISION  WR(N), WI(N), EVLR(N,N,2,2)
c
c     These are work arrays originally created by Fortran routine.
c     LWORK should be N*(N+6)
c     LIWORK should be 2*N-1
c
      INTEGER          LWORK,LIWORK
      DOUBLE PRECISION SCALEM(N), RCONDE(N), RCONDV(N)
      DOUBLE PRECISION VL(N,N), VR(N,N)
      DOUBLE PRECISION WORK(LWORK)
      INTEGER          IWORK(LIWORK)
C NCLEND
C --------------------------------------------------------------------------------
C                                                      local dynamically allocated
      INTEGER LDA, ILO, IHI, INFO, I, J
     +      , LDVR, LDVL, K , KP1
C    +      , LDVR, LDVL, LWORK, LIWORK, K , KP1
      DOUBLE PRECISION ABNRM

C just for consistency with LAPACK description   (make large)
      LDA     = N
      LDVR    = N
      LDVL    = N
      INFO    = 0   
C These are passed in above.
C     LWORK   = N*(N+6)
C     LIWORK  = 2*N-1

      CALL DGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, WR, WI,  
     &             VL, LDVL, VR, LDVR, ILO, IHI, SCALEM, ABNRM,
     &             RCONDE, RCONDV, WORK, LWORK, IWORK, INFO )

c
c This is the array returned to NCL | the WR & WI as attributes.
c
      k = 0
      do j=1,N 
         k = k+1
         if (wi(j).eq.0.0d0) then     !   not a complex conjugate
             do i=1,N
                EVLR(i,k,1,1) = VL(i,j) 
                EVLR(i,k,2,1) = 0.0d0   
                EVLR(i,k,1,2) = VR(i,j)
                EVLR(i,k,2,2) = 0.0d0  
             end do
         else if (WR(J).eq.WR(J+1) .and. WI(J).eq.-(WI(J+1))) then  ! conjugate
              kp1 = k+1
              do i=1,N
                 EVLR(i,k  ,1,1) =   VL(i,j)
                 EVLR(i,k  ,2,1) =   VL(i,j+1) 
                 EVLR(i,k  ,1,2) =   VR(i,j)
                 EVLR(i,k  ,2,2) =   VR(i,j+1)

                 EVLR(i,kp1,1,1) =   VL(i,j)
                 EVLR(i,kp1,2,1) = -(VL(i,j+1))
                 EVLR(i,kp1,1,2) =   VR(i,j)
                 EVLR(i,kp1,2,2) = -(VR(i,j+1))
              end do
              k = kp1-1
         end if
      end do

      return
      end


