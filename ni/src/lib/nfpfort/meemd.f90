      subroutine meemd(LXY,indata,An,Ns,Nesb,Nm,Nt,idum,rslt)
!-----------------------------------------------------------------------------
! This is a subroutine to decompose data(LXY) in terms of its EEMD component.
! When the added noise amplitude is zero (An=0) and the ensemble number
! is one (Nesb=0), the code degenerates to standard EMD
!
! In this code, the number of the oscillatory components is specified as an
! input, Nm. For most cases, automatic calculation, Nm can be set to log2(LXY)-2
!
! INPUT DATA:
!		indata(LXY)		input array with a length of LXY
!  		An 			amplitude of added noise
!               Ns                      fixed number of siftings
!  		Nesb 			number of ensemble members
!  		Nm 			number of mode in each decomposition
!               Nt                      Nm + 2 to deal with WRAPIT
!  		idum 			seed for the random number
! OUTPUT DATA:
!  		rslt(LXY, Nt) 	output rseults, which contains Nm+2 columns
!					The first column is the original indata;
!					The second column is the first component;
!					The third column is the second component;
!					and so on
!					The last column is the residue
!-------------------------------------------------------------------------------
      implicit none

      real :: An                                ! noise amplitude
      integer, intent(in) :: LXY, Nesb, Nm, Nt, Ns
      integer, intent(inout):: idum
      real, dimension(LXY), intent(in) :: indata
      real, dimension(LXY) :: ximf              ! data for sifting
      real, dimension(LXY) :: spmax             ! upper envelope, cubic spline
      real, dimension(LXY) :: spmin             ! lower envelope, cubic spline
      real, dimension(LXY) :: ave               ! the average of spmax and spmin
      real, dimension(LXY) :: remp              ! the input data for sifting
      real, dimension(LXY) :: rem               ! the remainder (remp-ximf)

      real, dimension(LXY,Nt) :: rslt           ! the final output data
      real, dimension(LXY,Nt) :: allmode        ! the results of a single EMD decomposition

      integer :: nmax, nmin                     ! numbers of maximum and minimum
      real, dimension(LXY) :: trend             ! linear trend of indata
      real :: std               ! standard deviation of the linearly detrended indata
      integer:: i,j,IE,im,ii
      real :: fNesb, gasdev


!  initialize the output
      rslt=0.0

!
!  ensemble EMD
!
!  *******************************************************
      do IE=1,Nesb
!  *******************************************************
!
        call standev(LXY,indata,trend, std)

!  inputted data + noise
        if(Nesb.eq.1) then
          ximf=indata
        else
          do i=1,LXY
            ximf(i)=indata(i)+An*std*gasdev(idum)
          enddo
        endif

!  save modified data
        do i=1,LXY
          allmode(i,1)=ximf(i)
        enddo

!  calculate modes
!       =======================================================
        do im=1,Nm
!       =======================================================
!
!  leave a copy of the input data before IMF is calculated
          remp=ximf
!
!  Sifting
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          do ii=1,Ns
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            call min_max(LXY,ximf,spmax,spmin,nmax,nmin)
            call natural_spline(spmax,LXY,nmax)
            call natural_spline(spmin,LXY,nmin)

            ave=(spmax+spmin)/2.0
            ximf=ximf-ave

!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          enddo                     ! siftings loop
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     

          rem=remp-ximf

          do i=1,LXY
            allmode(i,im+1)=ximf(i)
          enddo

          ximf=rem

!       =======================================================
        enddo                        ! Modes loop
!       =======================================================

        do i=1,LXY
          allmode(i,Nt)=ximf(i)      ! store final residual as trend
        enddo

        do j=1,Nt
          do i=1,LXY
            rslt(i,j)=rslt(i,j)+allmode(i,j)   ! store and cumulate all IMFs
          enddo
        enddo

!     ---------------------------------------------------------
      enddo                         ! ensemble members loop
!     ---------------------------------------------------------

      fNesb=real(Nesb)
      rslt=rslt/fNesb               ! Take the ensemble mean

      end subroutine meemd

      function ran2(idum)
!---------------------------------------------------------------------
!  See NUMERICAL RECIPES for detail of the function under the same
!  name
!  PARAMETERS:
!       idum       : random seed
!---------------------------------------------------------------------
      implicit none

      real :: ran2
      integer, intent(inout) :: idum
      integer, parameter :: im1=2147483563, im2=2147483399, imm1=im1-1
      integer, parameter :: ia1=40014, ia2=40692, iq1=53668, iq2=52774
      integer, parameter :: ir1=12211, ir2=3791, ntab=32, ndiv=1+imm1/ntab
      real, parameter :: am=1./im1, eps=1.2e-7, rnmx=1.-eps
      integer :: idum2=123456789, iy=0, j, k
      integer, dimension(ntab) :: iv
      
      save iv,iy,idum2


      do j=1,ntab
        iv(j)=0
      enddo

      if(idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do j=ntab+8,1,-1
          k=idum/iq1
          idum=ia1*(idum-k*iq1)-k*ir1
          if(idum.lt.0) idum=idum+im1
          if(j.le.ntab) iv(j)=idum
        enddo
        iy=iv(1)
      endif
      k=idum/iq1
      idum=ia1*(idum-k*iq1)-k*ir1
      if(idum.lt.0) idum=idum+im1
      k=idum2/iq2
      idum2=ia2*(idum2-k*iq2)-k*ir2
      if(idum2.lt.0) idum2=idum2+im2
      j=1+iy/ndiv
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1) iy=iy+imm1
      ran2=min(am*iy,rnmx)
      
      end function ran2

!**********************************************************************


      FUNCTION GASDEV(IDUM)
!----------------------------------------------------------------------
!  Gaussian white noise generator using uniformly distributed white
!  noise generator
!---------------------------------------------------------------------- 

      implicit none
      real:: gasdev, ran2
      integer, intent(inout):: idum
      real :: v1, v2, gv1, gv2
      real, parameter :: pi=3.1415927

      V1=RAN2(IDUM)
      V2=RAN2(IDUM)

      gv1=sqrt(-2.0*log(v1))*cos(2.0*pi*v2)
      gv2=sqrt(-2.0*log(v2))*cos(2.0*pi*v1)

      gasdev=gv1

      END FUNCTION GASDEV



      subroutine min_max(LEX,ximf,spmax,spmin,nmax,nmin)
!------------------------------------------------------------------
!  This is a routine to define maxima and minima from series ximf.
!  All the extrema are defined as the corresponding values of
!  ximf in spmax and spmin. All non-extrema values in spmax and
!  spmin are defined as 1.0e31.
!------------------------------------------------------------------
      implicit none

      integer, intent(in):: LEX
      real, dimension(LEX), intent(in):: ximf
      real, dimension(LEX), intent(out):: spmax, spmin
      integer, intent(out):: nmax, nmin
      integer:: i
      
      nmax=0
      nmin=0

      spmax(1)=ximf(1)
      spmax(LEX)=ximf(LEX)
      spmin(1)=spmax(1)
      spmin(LEX)=spmax(LEX)

      nmax=2
      nmin=2

      do i=2,LEX-1
        if(ximf(i) > ximf(i-1) .and. ximf(i) >= ximf(i+1)) then
          spmax(i) = ximf(i)
          nmax = nmax+1
        else
          spmax(i)=1.0e31
        endif
        if(ximf(i) < ximf(i-1) .and. ximf(i) <= ximf(i+1)) then
          spmin(i)=ximf(i)
          nmin=nmin+1
        else
          spmin(i)=1.0e31
        endif
      enddo

      call endmax(LEX, spmax, nmax)
      call endmin(LEX, spmin, nmin)

      end subroutine min_max

!****************************************************************

      subroutine endmax(LEX, temp, nmax)
!--------------------------------------------------------------------
! This is a subroutine to determine end values of the upper envolope
! using the method described in Appendix B of Wu and Huang (2009, 
! AADA, Vol. 1, pp1).
!--------------------------------------------------------------------
      implicit none

      integer, intent(in) :: nmax, LEX
      real, dimension(LEX), intent(inout):: temp 
      real, dimension(nmax) :: exmax, X
      integer :: I, J, lend
      real :: slope1, slope2, tmp1, tmp2
     
      lend=nmax

      J=1
      DO I=1, LEX
        IF( temp(I).LT.1.0E30 ) THEN
          X(J)=float(I)
          exmax(J)=temp(I)
          J=J+1
        ENDIF
      ENDDO

      if (nmax >= 4) then
        slope1=(exmax(2)-exmax(3))/(X(2)-X(3))
        tmp1=slope1*(X(1)-X(2))+exmax(2)
        if(tmp1 > exmax(1)) then
          temp(1)=tmp1
        endif
        
        slope2=(exmax(lend-1)-exmax(lend-2))/(X(lend-1)-X(lend-2))
        tmp2=slope2*(X(lend)-X(lend-1))+exmax(lend-1)
        if(tmp2 > exmax(lend)) then
          temp(LEX)=tmp2
        endif
      endif
      
      end subroutine endmax


!****************************************************************

      subroutine endmin(LEX, temp, nmax)
!--------------------------------------------------------------------
! This is a subroutine to determine end values of the lower envolope
! using the method described in Appendix B of Wu and Huang (2009, 
! AADA, Vol. 1, pp1).
!--------------------------------------------------------------------
      implicit none

      integer, intent(in) :: nmax, LEX
      real, dimension(LEX), intent(inout):: temp 
      real, dimension(nmax) :: exmax, X
      integer :: I, J, lend
      real :: slope1, slope2, tmp1, tmp2
     
      lend=nmax

      J=1
      DO I=1, LEX
        IF( temp(I).LT.1.0E30 ) THEN
          X(J)=float(I)
          exmax(J)=temp(I)
          J=J+1
        ENDIF
      ENDDO

      if (nmax >= 4) then
        slope1=(exmax(2)-exmax(3))/(X(2)-X(3))
        tmp1=slope1*(X(1)-X(2))+exmax(2)
        if(tmp1 < exmax(1)) then
          temp(1)=tmp1
        endif
        
        slope2=(exmax(lend-1)-exmax(lend-2))/(X(lend-1)-X(lend-2))
        tmp2=slope2*(X(lend)-X(lend-1))+exmax(lend-1)
        if(tmp2 < exmax(lend)) then
          temp(LEX)=tmp2
        endif
      endif
      
      end subroutine endmin



      SUBROUTINE NATURAL_SPLINE(YA,LEX,N)
!----------------------------------------------------------------------
!  This is a program of cubic spline interpolation. The imported
!  series, YA have a length of LEX, with N numbers of value
!  not equal to 1.0E31. The program is to use the cubic line to
!  interpolate the values for the points other thatn these N
!  numbers.        
!-----------------------------------------------------------------------

      implicit none

      integer, intent(in):: N, LEX      
      real, dimension(LEX), intent(inout):: YA
      real, dimension(N):: Y, Y2
      integer, dimension(N):: LX
      integer:: J, I, KLO, KHI
      real:: YP1, YPN, H, A, B 


!  The following code is to realocate the series of X(N), Y(N)

      J=1
      DO I=1, LEX
        IF( YA(I).LT.1.0E30 ) THEN
          LX(J)=I
          Y(J)=YA(I)
          J=J+1
        ENDIF
      ENDDO

!  The following code is used to calculate the second order derivative, 
!  set the derivatives at both ends for a natural cubic spline.
      YP1=1.0E31
      YPN=1.0E31

      CALL SPLINE(LX,Y,N,YP1,YPN,Y2)

!  calculate the cubic spline
      DO I=2,N
        KLO=LX(I-1)
        KHI=LX(I)
        H=real(KHI-KLO)
        DO J=KLO+1,KHI-1
          A=REAL(KHI-J)/H
          B=REAL(J-KLO)/H
          YA(J)=A*Y(I-1)+B*Y(I)   &
                +((A*A*A-A)*Y2(I-1)+(B*B*B-B)*Y2(I))*(H*H)/6.0
        ENDDO
      ENDDO

      END SUBROUTINE NATURAL_SPLINE


!**************************************************************************


      SUBROUTINE SPLINE(LX,Y,N,YP1,YPN,Y2)
!-----------------------------------------------------------------
!  see NUMEIRCAL RECIPES to find out meaning of each variables
!-----------------------------------------------------------------
      implicit none
    
      integer, intent(in) :: N, YP1, YPN
      integer, dimension(N), intent(in):: LX
      real, dimension(N) :: X, U
      real, dimension(N), intent(inout):: Y, Y2 
      integer:: I, K
      real:: SIG, P, QN, UN

      DO I=1,N
        X(I)=real(LX(I))
      ENDDO

      Y2(1)=0.
      U(1)=0.
      
      DO I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1)) &
             /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
      ENDDO

      QN=0.
      UN=0.

      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)

      DO K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
      ENDDO

      END SUBROUTINE SPLINE


      subroutine standev(nsize, indata, trend, std)
!-------------------------------------------------------------------
!  This is a program to obtain standard deviation of the linearly
!  detrended data
!  
!  PARAMETERS:
!  nsize    :    indata size
!  indata   :    input data
!  trend    :    the linear trend of "indata"
!  std      :    standard deviation
!--------------------------------------------------------------------

      implicit none

      integer, intent(in) :: nsize
      real, dimension(nsize) :: indata
      real, dimension(nsize), intent(out):: trend
      real, intent(out) :: std
      real sigmaX, sigmaY, sigmaX2, sigmaXY, real_nsize, Xbar, Ybar, real_i
      integer :: i
      real :: trend_const, trend_slope, temp

      sigmaX = 0.0
      sigmaY = 0.0
      do i = 1, nsize
        sigmaX = sigmaX + real(i)
        sigmaY = sigmaY + indata(i)
      enddo

      real_nsize=real(nsize)

      Xbar=sigmaX/real_nsize
      Ybar=sigmaY/real_nsize

      sigmaX2 = 0.0
      sigmaXY = 0.0
      do i = 1, nsize
        real_i=real(i)
        sigmaX2 = sigmaX2+(real_i-xbar)*(real_i-xbar)
        sigmaXY = sigmaXY+(real_i-xbar)*(indata(i)-ybar)
      enddo

      trend_slope=sigmaXY/sigmaX2
      trend_const=Ybar-trend_slope*Xbar

      do i=1, nsize
        trend(i)= trend_const + trend_slope*real(i)
      enddo

      std=0.0

      do i=1,nsize
        temp= indata(i)-trend(i)
        std=std+temp*temp
      enddo
      std=std/real(nsize)
      std=sqrt(std)

      end subroutine standev
