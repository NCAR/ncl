C
C	$Id: stex02.f,v 1.2 1993-01-21 01:23:17 dbrown Exp $
C
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL tvelvc(ierr)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
      SUBROUTINE TVELVC (IERROR)
C
C PURPOSE                To provide a simple demonstration of VELVCT.
C
C USAGE                  CALL TVELVC (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, the test was not successful.
C
C I/O                    If the test is successful, the message
C
C               VELVCT TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 1
C                        frame is produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plot.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      VELVCT
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              This test program calls entries EZVEC and
C                        VELVCT.  Each call produces a plot of a
C                        vector field obtained from the function
C
C                          Z(X,Y) = X + Y + 1./((X-.1)**2+Y**2+.09)
C                                   -1./((X+.1)**2+Y**2+.09),
C
C                        by using the direction of the Z gradient
C                        vectors and the logarithm of the absolute
C                        value of the components.
C
C HISTORY                Originally written in November 1976.
C                        Converted to FORTRAN 77 and GKS in July 1984.
C
C
      PARAMETER (M=25,N=25)
      DIMENSION       U(M,N)   ,V(M,N)
      DIMENSION WRK(M*N*2)
      COMMON /TRANS/ MA,NA,ITRANS,A1,A2,D1,D2,ALNMN,ALNMX,ALTMN,ALTMX
C
C Specify coordinates for a plot title.
C
        DATA IX/94/,IY/1000/
C
C Specify VELVCT arguments.
C
      DATA FLO/0./,HI/0./,NSET/0/,LENGTH/0/,ISPV/0/,SPV/0./
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Specify velocity field functions U and V.
C
      t = 1.0E-12
      DO  20 I=1,M
         X = FLOAT(I-11)
         DO  10 J=1,N
            Y = FLOAT(J-13)
c$$$            DZDX = 1.-2.*(X-.10)/((X-.10)**2+Y**2+.09)**2+
c$$$     1             2.*(X+.10)/((X+.10)**2+Y**2+.09)**2
c$$$            DZDY = 1.-2.*Y/((X-.10)**2+Y**2+.09)**2+
c$$$     1             2.*Y/((X+.10)**2+Y**2+.09)**2
c$$$            UVMAG = ALOG(SQRT(DZDX*DZDX+DZDY*DZDY))
c$$$            UVDIR = ATAN2(DZDY,DZDX)
c$$$c            U(I,J) = UVMAG*COS(UVDIR)
c$$$c            V(I,J) = UVMAG*SIN(UVDIR)
c$$$            uvmag = sqrt(x*x + y*y)
c$$$c            uvmag = 1.0
c$$$            if (uvmag .eq. 0.0) then
c$$$               u(i,j) = 0
c$$$               v(i,j) = 0
c$$$            else
c$$$               u(i,j) = - (abs(y) **.33)
c$$$               v(i,j) = x
c$$$            endif
            u(i,j)= 1.0
            v(i,j)= 1.0
c	    t=t*1.01
   10    CONTINUE
   20 CONTINUE
c$$$      do 30 i = 1,n
c$$$         do 25 j=1,m
c$$$            write(*,*) x, y, uvmag, u(i,j), v(i,j)
c$$$ 25      continue
c$$$ 30   continue
C
C
        CALL GQCNTN(IERR,ICN)
C
C Select normalization transformation 0.
C
c        CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
c$$$        X = CPUX(IX)
c$$$        Y = CPUY(IY)
c$$$      CALL PLCHLQ (X,Y,'DEMONSTRATION PLOT FOR ENTRY EZVEC OF VELVCT',
c$$$     1           16.,0.,-1.)
        CALL GSELNT(ICN)
c$$$C
c$$$C Call EZVEC for a default velocity field plot.
c$$$C
c$$$      CALL EZVEC (U,V,M,N)
C
C Call VELVCT to generate the user tuned velocity field plot.
C
c	call stseti('plr -- polar mode', 1)
        CALL STSETR('vnl - normalized vector magnitude', 0.33)
c        CALL STSETI('ckp - check progress', 50)
        ma=m
        na=n
        itrans=0
        ALNMN = 1.0
        ALNMX = 100.0
        ALTMN = 1.0
        ALTMX = 100.0
c        
        call vvsetr('vfr -- vector frac minimum', 0.25)
c	call vvseti('plr -- polar mode', 1)
c        do 200 j = 0,1
           call vvseti('TRT - transformation type', 1)
        do 100 i = 1,5
           
           icpm=-2
c           if (i .eq. 1) icpm = 2
           if (i .eq. 5) then
              xmx=50.0
              ll=1
           else
              xmx=100.0
              ll=i
           endif
C
           call set(0.05,0.95,0.05,0.95,1.0,xmx,1.0,100.0,ll)
	   call stseti('cpm -- compatibility mode', icpm) 
	   call vvseti('cpm -- compatibility mode', -4) 
           call vvsetr('xc1 -- lower X bound', 1.0)
           call vvsetr('xcm -- upper X bound', xmx)
           call vvsetr('yc1 -- lower X bound', 1.0)
           call vvsetr('ycn -- upper Y bound', 100.0)
           call stsetr('xc1 -- lower X bound', 1.0)
           call stsetr('xcm -- upper X bound', xmx)
           call stsetr('yc1 -- lower X bound', 1.0)
           call stsetr('ycn -- upper Y bound', 100.0)
           call stsetr('ssp -- stream spacing', 0.015)
c           
           nset=1
c           
           CALL GSPLCI(3)
           CALL VELVCT (U,M,V,M,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
           CALL GSPLCI(7)
           CALL STRMLN (U,V,WRK,M,M,N,1,IER)
           CALL GQCNTN(IERR,ICN)
C
C Select normalization transformation 0.
C
           CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
           X = CPUX(IX)
           Y = CPUY(IY)
           CALL PLCHLQ (X,Y,
     1           'DEMONSTRATION PLOT FOR ENTRY VELVCT OF VELVCT',16.,
     2           0.,-1.)
           CALL GSELNT(ICN)
           CALL FRAME
 100    CONTINUE
c 200    CONTINUE
C
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' VELVCT TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END



