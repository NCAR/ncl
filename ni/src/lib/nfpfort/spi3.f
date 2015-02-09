C NCLFORTSTART
        subroutine spi3ncdc(ntim,pp,pmsg,nrun,spi3,probne,pcpacc,index,
     +                      y, x, temparr,maxyrs,maxyrsp1,ier)
        implicit none
c                                 INPUT
        integer ntim, nrun, ier, maxyrs, maxyrsp1
        double precision pp(ntim), pmsg
        double precision probne(ntim),pcpacc(ntim)
     +                 , y(ntim),x(maxyrs),temparr(maxyrsp1)
     +                 , index(ntim)
c                                 OUTPUT
        double precision spi3(ntim)
C NCLEND
c
c NCL:   spi3 = dim_spi3_n(p,nrun,opt,dims)   ; same as 'dim_spi_n' 
c
c                                 LOCAL
        integer  num(12),numpos(12), nt
        double precision pzero(12)
        double precision par1(12),par2(12),par3(12),
     +                   xmom1(12),xmom2(12),xmom3(12)

        do nt=1,ntim
           spi3(nt) = pmsg
        end do

        if (mod(ntim,12).ne.0) then
            ier = 1
            return
        end if

C This is being passed in by C calling routine
C       maxyrs = ntim/12     
       
        call  spipe3(nrun,pp,par1,par2,par3,pzero,spi3,probne
     1              ,pcpacc,xmom1,xmom2,xmom3,pmsg,num,numpos
     2              ,maxyrs,index,y,x,temparr)

        return
        end

C --------------  NCDC code --------------------------------------
c To avoid subroutine name conflicts:
C .        change 'subroutine sort' to 'subroutine sortncdc'

        SUBROUTINE SPIPE3(nrun,pp,par1,par2,par3,pzero,spi,probne,
     1  pcpacc,xmom1,xmom2,xmom3,amssng,num,numpos,maxyrs,index,y,
     2  x,temparr)
c    
c       INPUT:  pp                one dimensional array of input data
c                                 where input is monthly precip data 
c                                 beginning with a January value
c               nrun              length of moving totals
c               amssng            value for missing data
c               maxyrs            maximum possible number of years of
c                                 data
c
c       OUTPUT: par1,par2,par3    parameters (mean, s.d., skewness) of
c                                 Pearson Type III distribution used to
c                                 fit the input data > 0
c               xmom1,xmom2,xmom3 l-moments of the input data > 0
c               pzero             relative  frequency of input data = 0
c               spi               standardized precip index
c               probne            probability of observed data <= x
c               pcpacc            data accumulated over length nrun
c               num               number of non-missing observations
c               numpos            number of non-zero observations
c
c       This program used the program of John Kleist, Colorado State
c       Univ., that computed spi values from a 2 parameter ML gamma
c       probability distribution, as a basis for the moving totals, etc.
c       code.  I added the PE3 l-moment computations.  The subroutines
c       and functions in upper case came from Jon Hosking, IBM.
c               Ned Guttman
c               Nov. 1997
c
        double precision  pp(maxyrs*12),probne(maxyrs*12),amssng,
     1                    pcpacc(maxyrs*12),pzero(12),spi(maxyrs*12)
        integer           num(12),numpos(12)

        double precision par1(12),par2(12),par3(12),index(maxyrs*12),
     1    xmom1(12),xmom2(12),xmom3(12),xmom(3),para(3),
     2    y(maxyrs*12),x(maxyrs),temparr(maxyrs+1),
     3    cdfpe3,quastn
c
c
c       save par1,par2,par3 to 8 decimal places if you want to use
c       them later for any additional calculations...not keeping
c       enough decimal places leads to errors in probabilities!     
c
c
c       the first nrun-1 index values will be missing
c
        do 10 j=1,nrun-1   
          index(j)=amssng
          probne(j)=amssng
          pcpacc(j)=amssng
 10     continue 
c
c       sum nrun precip values; store them in appropriate index location
C
c       if any value is missing, set the sum to missing
c
        do 30 j=nrun,maxyrs*12
          index(j)=0.0
          do 20 i=0,nrun-1
            if(pp(j-i).ne.amssng)then
              index(j)=index(j)+pp(j-i)
              pcpacc(j)=index(j)
            else
              index(j)=amssng
              probne(j)=amssng
              pcpacc(j)=amssng
              goto 30
            endif
 20       continue
 30     continue
       
c
c       to maintain seasonality, do everything by month
c
        do 50 i=0,11
           n=0
           nz=0
           np=0
           if (nrun.le.12) then
             do 40 j=nrun+i,maxyrs*12,12
               if(index(j).ne.amssng) then
c
c          this routine calculates lmoments and parameters of the PE3
c          distribution for all input data.  if you want to compute the
c          parameters for a specific period of record, then the limits
c          of j must be set appropriately for temparr. it is recommended
c          that for both monitoring and historical perspective that
c          all data be used for computing the probabilities, and that
c          the historical time series be the "latest" and most complete
c          output.
c
c          n-count for all non-missing data, including zeroes
c
                 n=n+1
                 if(index(j).gt.0) then
c
c          n-count for all non-missing, non-zero data; temparr is for
c          non-zero, non-missing precipitation
c
                   np=np+1
                   temparr(np)=index(j)
                 else 
c
c          n-count for all non-missing, zero data
c
                   nz=nz+1
                 endif
               endif
 40          continue
        elseif (nrun.gt.12) then
          j=nrun+i 
c
c         step the sequence of data by the length of nrun to get
c         independent samples 
c
          if (nrun.gt.12.and.nrun.le.24) nstep=24
          if (nrun.gt.24.and.nrun.le.36) nstep=36
          if (nrun.gt.36.and.nrun.le.48) nstep=48
          if (nrun.gt.48.and.nrun.le.60) nstep=60
          if (nrun.gt.60.and.nrun.le.72) nstep=72
 41       if (j.gt.maxyrs*12) GOTO 42
c
c          this routine calculates lmoments and parameters of the PE3
c          distribution for all input data.  if you want to compute the
c          parameters for a specific period of record, then the limits
c          of j must be set appropriately for temparr
c
             if(index(j).ne.amssng) then
               n=n+1
               if(index(j).gt.0) then
                 np=np+1
                 temparr(np)=index(j)
               else 
                 nz=nz+1
               endif
               j=j+nstep
               goto 41
             else
c
c          look for next non-missing nrun>12 value
c
               j=j+12
               goto 41
             endif
           endif
 42     im=mod(nrun+i-1,12)+1
        pzero(im)=float(nz)/float(n)
        num(im)=n
        numpos(im)=np
c
c       order the data
c
        call sortncdc(temparr,x,np,maxyrs)
c   
c       fit l-moments for non-zero precip; if 2nd moment=0, routine
c       fails and all 3 moments set to zero (condition indicates all
c       data values are equal).  also, if not enough non-zero data
c       (less than 3), routine fails and all moments set to zero.
c
cR c c  call samlmr(x,np,xmom,3,-0.00D0,ODO,ifail) 
        call samlmr(x,np,xmom,3, 0.00D0,0d0,ifail) 
        xmom1(im)=xmom(1)
        xmom2(im)=xmom(2)
        xmom3(im)=xmom(3)
c       
c       compute parameters of Pearson type III (3-parameter gamma); the
c       three parameters are the mean, s.d., skewness of the NON-ZERO data.
c       The mean for all the data is (1-pzero)*par1 or (1-pzero)*xmom1
c       since par1=xmom1
c
        call pelpe3(xmom,para,ifail)
c
c       if ifail not = 0, then all 3 parameters are set to zero
c       if routine fails, output is sent to unit 6 by pelpe3 routine
c
        par1(im)=para(1)
        par2(im)=para(2)
        par3(im)=para(3)
 50     continue
c
c       compute the probability (cdfpe3), take into account the 
c       mixed distribution if pzero not equal to zero, truncate
c       the probability from .001 to .999, transform the probability to
c       spi (quastn). ifail=1 indicates invalid parameter 2 (negative).
c       if routines fail, output is sent to unit 6 by called routines.
c
        do 60 j=nrun,maxyrs*12
          im=mod(j-1,12)+1
          para(1)=par1(im)
          para(2)=par2(im)
          para(3)=par3(im)
c
c         set missing values to amssng
c
 61       if(index(j).eq.amssng) then
             probne(j)=amssng
             spi(j)=amssng
             goto 60
          endif
          if(index(j).ne.amssng) then
             index(j)=cdfpe3(index(j),para,ifail,amssng)
c
c         if cdf routine fails, set probne and spi = amssng
c
             if(ifail.ne.0) then
               index(j)=amssng
               goto 61
             endif
             y(j)=pzero(im)+(1-pzero(im))*index(j)
c
c            force the probabilities and therefore the spi to be bounded
c            by +/- 3.09
c
             if(y(j).gt..999)y(j)=.999
             if(y(j).lt..001)y(J)=.001
             probne(j)=y(j)
             index(j)=quastn(y(j))
             spi(j)=index(j)
          endif
 60    continue
       return
       end
c
c
      DOUBLE PRECISION FUNCTION CDFPE3(X,PARA,ifail,amssng)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  DISTRIBUTION FUNCTION OF THE PEARSON TYPE 3 DISTRIBUTION
C
C  OTHER ROUTINES USED: DERFLM,DLGAMALM,GAMINDLM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(3)
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/,FOUR/4D0/
      DATA RTHALF/0.70710 67811 86547 524D0/
C
C         SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
C
      DATA SMALL/1D-6/
C
      ifail=0
      CDFPE3=ZERO
      IF(PARA(2).LE.ZERO)GOTO 1000
      GAMMA=PARA(3)
      IF(DABS(GAMMA).LE.SMALL)GOTO 10
      ALPHA=FOUR/(GAMMA*GAMMA)
      Z=TWO*(X-PARA(1))/(PARA(2)*GAMMA)+ALPHA
      IF(Z.GT.ZERO)CDFPE3=GAMINDLM(Z,ALPHA,DLGAMALM(ALPHA))
      IF(GAMMA.LT.ZERO)CDFPE3=ONE-CDFPE3
      RETURN
C
C         ZERO SKEWNESS
C
   10 Z=(X-PARA(1))/PARA(2)
      CDFPE3=HALF+HALF*DERFLM(Z*RTHALF)
      RETURN
C
1000  WRITE(6,7000)
      ifail=1
      cdfpe3=amssng
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE CDFPE3 : PARAMETERS INVALID')
      END
c
c
      SUBROUTINE PELPE3(XMOM,PARA,ifail)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  PARAMETER ESTIMATION VIA L-MOMENTS FOR THE PEARSON TYPE 3 DISTRIBUTION
C
C  PARAMETERS OF ROUTINE:
C  XMOM   * INPUT* ARRAY OF LENGTH 3. CONTAINS THE L-MOMENTS LAMBDA-1,
C                  LAMBDA-2 AND TAU-3.
C  PARA   *OUTPUT* ARRAY OF LENGTH 3. ON EXIT, CONTAINS THE PARAMETERS
C                  IN THE ORDER MU, SIGMA, GAMMA (MEAN, S.D., SKEWNESS).
C
C  OTHER ROUTINES USED: DLGAMALM
C
C  METHOD: RATIONAL APPROXIMATION IS USED TO EXPRESS ALPHA, THE SHAPE
C  PARAMETER OF THE GAMMA DISTRIBUTION, AS A FUNCTION OF TAU-3.
C  RELATIVE ACCURACY OF THE APPROXIMATION IS BETTER THAN 3E-5.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION XMOM(3),PARA(3)
      DATA ZERO/0D0/,THIRD/0.33333333D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/
C
C         SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
C
      DATA SMALL/1D-6/
C
C         CONSTANTS USED IN MINIMAX APPROXIMATIONS
C
      DATA C1,C2,C3/ 0.2906D0,  0.1882D0,  0.0442D0/
      DATA D1,D2,D3/ 0.36067D0,-0.59567D0, 0.25361D0/
      DATA D4,D5,D6/-2.78861D0, 2.56096D0,-0.77045D0/
      DATA PI3,ROOTPI/9.4247780D0,1.7724539D0/
C
      ifail=0
      T3=DABS(XMOM(3))
      IF(XMOM(2).LE.ZERO.OR.T3.GE.ONE)GOTO 1000
      IF(T3.LE.SMALL)GOTO 100
      IF(T3.GE.THIRD)GOTO 10
      T=PI3*T3*T3
      ALPHA=(ONE+C1*T)/(T*(ONE+T*(C2+T*C3)))
      GOTO 20
   10 CONTINUE
      T=ONE-T3
      ALPHA=T*(D1+T*(D2+T*D3))/(ONE+T*(D4+T*(D5+T*D6)))
   20 CONTINUE
      RTALPH=DSQRT(ALPHA)
      BETA=ROOTPI*XMOM(2)*DEXP(DLGAMALM(ALPHA)-DLGAMALM(ALPHA+HALF))
      PARA(1)=XMOM(1)
      PARA(2)=BETA*RTALPH
      PARA(3)=TWO/RTALPH
      IF(XMOM(3).LT.ZERO)PARA(3)=-PARA(3)
      RETURN
C
C         ZERO SKEWNESS
C
  100 CONTINUE
      PARA(1)=XMOM(1)
      PARA(2)=XMOM(2)*ROOTPI
      PARA(3)=ZERO
      RETURN
C
c1000 WRITE(6,7000)
 1000 DO 1010 I=1,3
 1010 PARA(I)=ZERO
      ifail=1
      RETURN
C
C 7000 FORMAT(' *** ERROR *** ROUTINE PELPE3 : L-MOMENTS INVALID')
      END
c
c
c
      SUBROUTINE SAMLMR(X,N,XMOM,NMOM,A,B,ifail)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  SAMPLE L-MOMENTS OF A DATA ARRAY
C
C  PARAMETERS OF ROUTINE:
C  X      * INPUT* ARRAY OF LENGTH N. CONTAINS THE DATA, IN ASCENDING
C                  ORDER.
C  N      * INPUT* NUMBER OF DATA VALUES
C  XMOM   *OUTPUT* ARRAY OF LENGTH NMOM. ON EXIT, CONTAINS THE SAMPLE
C                  L-MOMENTS L-1, L-2, T-3, T-4, ... .
C  NMOM   * INPUT* NUMBER OF L-MOMENTS TO BE FOUND. AT MOST MAX(N,20).
C  A      * INPUT* ) PARAMETERS OF PLOTTING
C  B      * INPUT* ) POSITION (SEE BELOW)
C
C  FOR UNBIASED ESTIMATES (OF THE LAMBDA'S) SET A=B=ZERO. OTHERWISE,
C  PLOTTING-POSITION ESTIMATORS ARE USED, BASED ON THE PLOTTING POSITION
C  (J+A)/(N+B)  FOR THE J'TH SMALLEST OF N OBSERVATIONS. FOR EXAMPLE,
C  A=-0.35D0 AND B=0.0D0 YIELDS THE ESTIMATORS RECOMMENDED BY
C  HOSKING ET AL. (1985, TECHNOMETRICS) FOR THE GEV DISTRIBUTION.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION X(N),XMOM(NMOM),SUM(20)
      DATA ZERO/0D0/,ONE/1D0/
      ifail=0
      IF(NMOM.GT.20.OR.NMOM.GT.N)GOTO 1000
      DO 10 J=1,NMOM
   10 SUM(J)=ZERO
      IF(A.EQ.ZERO.AND.B.EQ.ZERO)GOTO 50
      IF(A.LE.-ONE.OR.A.GE.B)GOTO 1010
C
C         PLOTTING-POSITION ESTIMATES OF PWM'S
C
      DO 30 I=1,N
      PPOS=(I+A)/(N+B)
      TERM=X(I)
      SUM(1)=SUM(1)+TERM
      DO 20 J=2,NMOM
      TERM=TERM*PPOS
   20 SUM(J)=SUM(J)+TERM
   30 CONTINUE
      DO 40 J=1,NMOM
   40 SUM(J)=SUM(J)/N
      GOTO 100
C
C         UNBIASED ESTIMATES OF PWM'S
C
   50 DO 70 I=1,N
      Z=I
      TERM=X(I)
      SUM(1)=SUM(1)+TERM
      DO 60 J=2,NMOM
      Z=Z-ONE
      TERM=TERM*Z
   60 SUM(J)=SUM(J)+TERM
   70 CONTINUE
      Y=N
      Z=N
      SUM(1)=SUM(1)/Z
      DO 80 J=2,NMOM
      Y=Y-ONE
      Z=Z*Y
   80 SUM(J)=SUM(J)/Z
C
C         L-MOMENTS
C
  100 K=NMOM
      P0=ONE
      IF(NMOM-NMOM/2*2.EQ.1)P0=-ONE
      DO 120 KK=2,NMOM
      AK=K
      P0=-P0
      P=P0
      TEMP=P*SUM(1)
      DO 110 I=1,K-1
      AI=I
      P=-P*(AK+AI-ONE)*(AK-AI)/(AI*AI)
  110 TEMP=TEMP+P*SUM(I+1)
      SUM(K)=TEMP
  120 K=K-1
      XMOM(1)=SUM(1)
      IF(NMOM.EQ.1)RETURN
      XMOM(2)=SUM(2)
      IF(SUM(2).EQ.ZERO)GOTO 1020
      IF(NMOM.EQ.2)RETURN
      DO 130 K=3,NMOM
  130 XMOM(K)=SUM(K)/SUM(2)
      RETURN
C
 1000 ifail=1
      do i=1,3
        xmom(i)=0
      enddo
      WRITE(6,7000)
      RETURN
 1010 WRITE(6,7010)
      RETURN
 1020 ifail=1
      do i=1,3
        xmom(i)=0
      enddo
      WRITE(6,7020)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE SAMLMR : PARAMETER NMOM INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE SAMLMR :',
     *  ' PLOTTING-POSITION PARAMETERS INVALID')
 7020 FORMAT(' *** ERROR *** ROUTINE SAMLMR : ALL DATA VALUES EQUAL')
      END
c
c
c
c RENAMED DERF to DERFLM to avoid name conflict.
      DOUBLE PRECISION FUNCTION DERFLM(X)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  ERROR FUNCTION
C
C  BASED ON ALGORITHM 5666, J.F.HART ET AL. (1968) 'COMPUTER
C  APPROXIMATIONS'
C
C  ACCURATE TO 15 DECIMAL PLACES
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DATA ZERO/0D0/,ONE/1D0/,TWO/2D0/,THREE/3D0/,FOUR/4D0/,P65/0.65D0/
C
C         COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATION
C
      DATA P0,P1,P2,P3,P4,P5,P6/
     *  0.22020 68679 12376 1D3,    0.22121 35961 69931 1D3,
     *  0.11207 92914 97870 9D3,    0.33912 86607 83830 0D2,
     *  0.63739 62203 53165 0D1,    0.70038 30644 43688 1D0,
     *  0.35262 49659 98910 9D-1/
      DATA Q0,Q1,Q2,Q3,Q4,Q5,Q6,Q7/
     *  0.44041 37358 24752 2D3,   0.79382 65125 19948 4D3,
     *  0.63733 36333 78831 1D3,   0.29656 42487 79673 7D3,
     *  0.86780 73220 29460 8D2,   0.16064 17757 92069 5D2,
     *  0.17556 67163 18264 2D1,   0.88388 34764 83184 4D-1/
C
C         C1 IS SQRT(2), C2 IS SQRT(2/PI)
C         BIG IS THE POINT AT WHICH DERFLM=1 TO MACHINE PRECISION
C
      DATA C1/1.4142 13562 37309 5D0/
      DATA C2/7.9788 45608 02865 4D-1/
      DATA BIG/6.25D0/,CRIT/5D0/
C
      DERFLM=ZERO
      IF(X.EQ.ZERO)RETURN
      XX=DABS(X)
      IF(XX.GT.BIG)GOTO 20
      EXPNTL=DEXP(-X*X)
      ZZ=DABS(X*C1)
      IF(XX.GT.CRIT)GOTO 10
      DERFLM=EXPNTL*((((((P6*ZZ+P5)*ZZ+P4)*ZZ+P3)*ZZ+P2)*ZZ+P1)
     * *ZZ+P0)/(((((((Q7*ZZ+Q6)*ZZ+Q5)*ZZ+Q4)*ZZ+Q3)*ZZ+Q2)*ZZ+Q1)
     * *ZZ+Q0)
      IF(X.GT.ZERO)DERFLM=ONE-TWO*DERFLM
      IF(X.LT.ZERO)DERFLM=TWO*DERFLM-ONE
      RETURN
C
   10 DERFLM=EXPNTL*C2/(ZZ+ONE/(ZZ+TWO/(ZZ+THREE/(ZZ+FOUR/(ZZ+P65)))))
      IF(X.GT.ZERO)DERFLM=ONE-DERFLM
      IF(X.LT.ZERO)DERFLM=DERFLM-ONE
      RETURN
C
   20 DERFLM=ONE
      IF(X.LT.ZERO)DERFLM=-ONE
      RETURN
      END
c
c
c
c RENAMED DLGAMA to DLGAMALM to avoid name conflict.
      DOUBLE PRECISION FUNCTION DLGAMALM(X)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  LOGARITHM OF GAMMA FUNCTION
C
C  BASED ON ALGORITHM ACM291, COMMUN. ASSOC. COMPUT. MACH. (1966)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA SMALL,CRIT,BIG,TOOBIG/1D-7,13D0,1D9,2D36/
C
C         C0 IS 0.5*LOG(2*PI)
C         C1...C7 ARE THE COEFFTS OF THE ASYMPTOTIC EXPANSION OF DLGAMALM
C
      DATA C0,C1,C2,C3,C4,C5,C6,C7/
     *   0.91893 85332 04672 742D 0,  0.83333 33333 33333 333D-1,
     *  -0.27777 77777 77777 778D-2,  0.79365 07936 50793 651D-3,
     *  -0.59523 80952 38095 238D-3,  0.84175 08417 50841 751D-3,
     *  -0.19175 26917 52691 753D-2,  0.64102 56410 25641 026D-2/
C
C         S1 IS -(EULER'S CONSTANT), S2 IS PI**2/12
C
      DATA S1/-0.57721 56649 01532 861D 0/
      DATA S2/ 0.82246 70334 24113 218D 0/
C
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/
      DLGAMALM=ZERO
      IF(X.LE.ZERO)GOTO 1000
      IF(X.GT.TOOBIG)GOTO 1000
C
C         USE SMALL-X APPROXIMATION IF X IS NEAR 0, 1 OR 2
C
      IF(DABS(X-TWO).GT.SMALL)GOTO 10
      DLGAMALM=DLOG(X-ONE)
      XX=X-TWO
      GOTO 20
   10 IF(DABS(X-ONE).GT.SMALL)GOTO 30
      XX=X-ONE
   20 DLGAMALM=DLGAMALM+XX*(S1+XX*S2)
      RETURN
   30 IF(X.GT.SMALL)GOTO 40
      DLGAMALM=-DLOG(X)+S1*X
      RETURN
C
C         REDUCE TO DLGAMALM(X+N) WHERE X+N.GE.CRIT
C
   40 SUM1=ZERO
      Y=X
      IF(Y.GE.CRIT)GOTO 60
      Z=ONE
   50 Z=Z*Y
      Y=Y+ONE
      IF(Y.LT.CRIT)GOTO 50
      SUM1=SUM1-DLOG(Z)
C
C         USE ASYMPTOTIC EXPANSION IF Y.GE.CRIT
C
   60 SUM1=SUM1+(Y-HALF)*DLOG(Y)-Y+C0
      SUM2=ZERO
      IF(Y.GE.BIG)GOTO 70
      Z=ONE/(Y*Y)
      SUM2=((((((C7*Z+C6)*Z+C5)*Z+C4)*Z+C3)*Z+C2)*Z+C1)/Y
   70 DLGAMALM=SUM1+SUM2
      RETURN
C
 1000 WRITE(6,7000)X
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE DLGAMALM :',
     *  ' ARGUMENT OUT OF RANGE :',D24.16)
      END
c
c
c
c RENAMED GAMIND to GAMINDLM to avoid name conflict.
      DOUBLE PRECISION FUNCTION GAMINDLM(X,ALPHA,G)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  THE INCOMPLETE GAMMA INTEGRAL
C
C  BASED ON ALGORITHM AS239, APPL. STATIST. (1988) VOL.37 NO.3
C
C  PARAMETERS OF ROUTINE:
C  X      * INPUT* ARGUMENT OF FUNCTION (UPPER LIMIT OF INTEGRATION)
C  ALPHA  * INPUT* SHAPE PARAMETER
C  G      * INPUT* LOG(GAMMA(ALPHA)). MUST BE SUPPLIED BY THE PROGRAM,
C                  E.G. AS DLGAMALM(ALPHA).
C
C  OTHER ROUTINES USED: DERFLM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/,THREE/3D0/,X13/13D0/,
     *  X36/36D0/,X42/42D0/,X119/119D0/,X1620/1620D0/,X38880/38880D0/,
     *  RTHALF/0.70710 67811 86547 524D0/
C
C         EPS,MAXIT CONTROL THE TEST FOR CONVERGENCE OF THE SERIES AND
C           CONTINUED-FRACTION EXPANSIONS.
C         OFL IS A LARGE NUMBER, USED TO RESCALE THE CONTINUED FRACTION.
C         UFL IS SUCH THAT EXP(UFL) IS JUST .GT. ZERO.
C         AHILL CONTROLS THE SWITCH TO HILL'S APPROXIMATION.
C
      DATA EPS/1D-12/,MAXIT/100000/,OFL/1D30/,UFL/-180D0/,AHILL/1D4/
      GAMINDLM=ZERO
      IF(ALPHA.LE.ZERO)GOTO 1000
      IF(X.LT.ZERO)GOTO 1010
      IF(X.EQ.ZERO)RETURN
C
      IF(ALPHA.GT.AHILL)GOTO 100
      IF(X.GT.ONE.AND.X.GE.ALPHA)GOTO 50
C
C         SERIES EXPANSION
C
      SUM=ONE
      TERM=ONE
      A=ALPHA
      DO 10 IT=1,MAXIT
      A=A+ONE
      TERM=TERM*X/A
      SUM=SUM+TERM
      IF(TERM.LE.EPS)GOTO 20
   10 CONTINUE
      WRITE(6,7020)
   20 ARG=ALPHA*DLOG(X)-X-G+DLOG(SUM/ALPHA)
      GAMINDLM=ZERO
      IF(ARG.GE.UFL)GAMINDLM=DEXP(ARG)
      RETURN
C
C         CONTINUED-FRACTION EXPANSION
C
   50 CONTINUE
      A=ONE-ALPHA
      B=A+X+ONE
      TERM=ZERO
      PN1=ONE
      PN2=X
      PN3=X+ONE
      PN4=X*B
      RATIO=PN3/PN4
      DO 70 IT=1,MAXIT
      A=A+ONE
      B=B+TWO
      TERM=TERM+ONE
      AN=A*TERM
      PN5=B*PN3-AN*PN1
      PN6=B*PN4-AN*PN2
      IF(PN6.EQ.ZERO)GOTO 60
      RN=PN5/PN6
      DIFF=DABS(RATIO-RN)
      IF(DIFF.LE.EPS.AND.DIFF.LE.EPS*RN)GOTO 80
      RATIO=RN
   60 PN1=PN3
      PN2=PN4
      PN3=PN5
      PN4=PN6
      IF(DABS(PN5).LT.OFL)GOTO 70
      PN1=PN1/OFL
      PN2=PN2/OFL
      PN3=PN3/OFL
      PN4=PN4/OFL
   70 CONTINUE
      WRITE(6,7020)
   80 ARG=ALPHA*DLOG(X)-X-G+DLOG(RATIO)
      GAMINDLM=ONE
      IF(ARG.GE.UFL)GAMINDLM=ONE-DEXP(ARG)
      RETURN
C
C         ALPHA IS LARGE: USE HILL'S APPROXIMATION (N.L. JOHNSON AND
C         S. KOTZ, 1970, 'CONTINUOUS UNIVARIATE DISTRIBUTIONS 1', P.180)
C
C         THE 'DO 110' LOOP CALCULATES 2*(X-ALPHA-ALPHA*DLOG(X/ALPHA)),
C         USING POWER-SERIES EXPANSION TO AVOID ROUNDING ERROR
C
  100 CONTINUE
      R=ONE/DSQRT(ALPHA)
      Z=(X-ALPHA)*R
      TERM=Z*Z
      SUM=HALF*TERM
      DO 110 I=1,12
      TERM=-TERM*Z*R
      SUM=SUM+TERM/(I+TWO)
      IF(DABS(TERM).LT.EPS)GOTO 120
  110 CONTINUE
  120 WW=TWO*SUM
      W=DSQRT(WW)
      IF(X.LT.ALPHA)W=-W
      H1=ONE/THREE
      H2=-W/X36
      H3=(-WW+X13)/X1620
      H4=(X42*WW+X119)*W/X38880
      Z=(((H4*R+H3)*R+H2)*R+H1)*R+W
      GAMINDLM=HALF+HALF*DERFLM(Z*RTHALF)
      RETURN
C
 1000 WRITE(6,7000)ALPHA
      RETURN
 1010 WRITE(6,7010)X
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE GAMINDLM :',
     *  ' SHAPE PARAMETER OUT OF RANGE :',D16.8)
 7010 FORMAT(' *** ERROR *** ROUTINE GAMINDLM :',
     *  ' ARGUMENT OF FUNCTION OUT OF RANGE :',D16.8)
 7020 FORMAT(' ** WARNING ** ROUTINE GAMINDLM :',
     *  ' ITERATION HAS NOT CONVERGED. RESULT MAY BE UNRELIABLE.')
      END
c
c
c
      DOUBLE PRECISION FUNCTION QUASTN(F)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  QUANTILE FUNCTION OF THE STANDARD NORMAL DISTRIBUTION
C
C  BASED ON ALGORITHM AS241, APPL. STATIST. (1988) VOL.37 NO.3
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/
      DATA SPLIT1/0.425D0/,SPLIT2/5D0/,CONST1/0.180625D0/,CONST2/1.6D0/
C
C         COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATIONS
C
      DATA A0,A1,A2,A3,A4,A5,A6,A7,B1,B2,B3,B4,B5,B6,B7/
     *                                0.33871 32872 79636 661D  1,
     *  0.13314 16678 91784 377D  3,  0.19715 90950 30655 144D  4,
     *  0.13731 69376 55094 611D  5,  0.45921 95393 15498 715D  5,
     *  0.67265 77092 70087 009D  5,  0.33430 57558 35881 281D  5,
     *  0.25090 80928 73012 267D  4,  0.42313 33070 16009 113D  2,
     *  0.68718 70074 92057 908D  3,  0.53941 96021 42475 111D  4,
     *  0.21213 79430 15865 959D  5,  0.39307 89580 00927 106D  5,
     *  0.28729 08573 57219 427D  5,  0.52264 95278 85285 456D  4/
      DATA C0,C1,C2,C3,C4,C5,C6,C7,D1,D2,D3,D4,D5,D6,D7/
     *                                0.14234 37110 74968 358D  1,
     *  0.46303 37846 15654 530D  1,  0.57694 97221 46069 141D  1,
     *  0.36478 48324 76320 461D  1,  0.12704 58252 45236 838D  1,
     *  0.24178 07251 77450 612D  0,  0.22723 84498 92691 846D -1,
     *  0.77454 50142 78341 408D -3,  0.20531 91626 63775 882D  1,
     *  0.16763 84830 18380 385D  1,  0.68976 73349 85100 005D  0,
     *  0.14810 39764 27480 075D  0,  0.15198 66656 36164 572D -1,
     *  0.54759 38084 99534 495D -3,  0.10507 50071 64441 684D -8/
      DATA E0,E1,E2,E3,E4,E5,E6,E7,F1,F2,F3,F4,F5,F6,F7/
     *                                0.66579 04643 50110 378D  1,
     *  0.54637 84911 16411 437D  1,  0.17848 26539 91729 133D  1,
     *  0.29656 05718 28504 891D  0,  0.26532 18952 65761 230D -1,
     *  0.12426 60947 38807 844D -2,  0.27115 55568 74348 758D -4,
     *  0.20103 34399 29228 813D -6,  0.59983 22065 55887 938D  0,
     *  0.13692 98809 22735 805D  0,  0.14875 36129 08506 149D -1,
     *  0.78686 91311 45613 259D -3,  0.18463 18317 51005 468D -4,
     *  0.14215 11758 31644 589D -6,  0.20442 63103 38993 979D-14/
C
      Q=F-HALF
      IF(DABS(Q).GT.SPLIT1)GOTO 10
      R=CONST1-Q*Q
      QUASTN=Q*(((((((A7*R+A6)*R+A5)*R+A4)*R+A3)*R+A2)*R+A1)*R+A0)
     *        /(((((((B7*R+B6)*R+B5)*R+B4)*R+B3)*R+B2)*R+B1)*R+ONE)
      RETURN
   10 R=F
      IF(Q.GE.ZERO)R=ONE-F
      IF(R.LE.ZERO)GOTO 1000
      R=DSQRT(-DLOG(R))
      IF(R.GT.SPLIT2)GOTO 20
      R=R-CONST2
      QUASTN=(((((((C7*R+C6)*R+C5)*R+C4)*R+C3)*R+C2)*R+C1)*R+C0)
     *      /(((((((D7*R+D6)*R+D5)*R+D4)*R+D3)*R+D2)*R+D1)*R+ONE)
      GOTO 30
   20 R=R-SPLIT2
      QUASTN=(((((((E7*R+E6)*R+E5)*R+E4)*R+E3)*R+E2)*R+E1)*R+E0)
     *      /(((((((F7*R+F6)*R+F5)*R+F4)*R+F3)*R+F2)*R+F1)*R+ONE)
   30 IF(Q.LT.ZERO)QUASTN=-QUASTN
      RETURN
C
 1000 WRITE(6,7000)F
      QUASTN=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUASTN :',
     *  ' ARGUMENT OF FUNCTION INVALID')
      END
c
c
c
      SUBROUTINE SORTNCDC(temparr,X,N,maxyrs)
CCCC  SUBROUTINE SORT(temparr,X,N,maxyrs)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  SORTS THE ARRAY X INTO ASCENDING ORDER
C
C  PARAMETERS OF ROUTINE:
C  X      *IN/OUT* ARRAY OF LENGTH N. CONTAINS THE NUMBERS TO BE SORTED.
C                  ON EXIT, CONTAINS THE SORTED NUMBERS.
C  N      * INPUT* NUMBER OF ELEMENTS TO BE SORTED
C
C  METHOD USED IS SHELL SORT WITH SEQUENCE OF INCREMENTS AS IN
C  D.F.KNUTH (1969) 'THE ART OF COMPUTER PROGRAMMING', VOL.3, P.95
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION X(N),temparr(maxyrs+1)
      IF(N.LE.1)RETURN
      do i=1,n
        x(i)=temparr(i)
      enddo
      J=4
      DO 10 I=1,100
      J=3*J+1
      IF(J.GE.N)GOTO 20
   10 CONTINUE
   20 CONTINUE
      M=(J/3)
      DO 60 MM=1,100
      M=M/3
      IF(M.EQ.0)RETURN
      DO 50 I=M+1,N
      TEST=X(I)
      J=I
      DO 30 JJ=1,100
      J=J-M
      IF(J.LE.0)GOTO 40
      IF(TEST.GE.X(J))GOTO 40
   30 X(J+M)=X(J)
   40 CONTINUE
   50 X(J+M)=TEST
   60 CONTINUE
      END
