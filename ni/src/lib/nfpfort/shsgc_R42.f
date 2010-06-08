C NCLFORTSTART
      SUBROUTINE DSHSGCR42(A,B,GVAR,WORK,LWORK)
C NCL  gvar = shsgsc_R42(a,b)
      IMPLICIT NONE
      INTEGER LWORK
      DOUBLE PRECISION A(43,43),B(43,43),GVAR(128,108)
      DOUBLE PRECISION WORK(LWORK)
C NCLEND

c driver to subroutines provided by: "Bao Qing" <baoqing@mail.iap.ac.cn>
c array sizes are *hard wired* for gaussian rhomboidal truncation R42
c
c arguments:
c .     a, b    - real, imaginary spherical harmonic coef (input)
c .     gvar    - grid variable (out)
c local
c .     glat    - gaussian latitude
c .     gw      - gaussian weights

c local
      INTEGER NLAT, IER
C     PARAMETER (NLAT=108, LWORK=4*NLAT*(NLAT+1)+2)
      PARAMETER (NLAT=108)
      INTEGER I
      DOUBLE PRECISION GLAT(108),GWGT(108)
      DOUBLE PRECISION RADTODEG
      PARAMETER (RADTODEG=57.2957795)
C
C Generate gaussian lats and weights.
C 
      CALL GAQDNCL(NLAT,GLAT,GWGT,WORK,LWORK,IER)
C
C Convert to degrees and subtract 90.
C
      DO 10 I=1,NLAT
         GLAT(I) = (RADTODEG * GLAT(I)) - 90.0
 10   END DO

      CALL DTRME(A,B,GVAR,GLAT,GWGT)

      END

      SUBROUTINE DTRME(A,B,GVAR,XLAT,GW)
      DOUBLE PRECISION PS1
c rhombical truncation parameter
      INTEGER IRES
c number of longitudes
      INTEGER IX
c ix>=3*ires for rhombical truncation

c number of latitudes
      INTEGER IY
c iy>=(5*ires+1)/2 for rhombical truncation

      PARAMETER (IRES=42)
      PARAMETER (IX=128)
      PARAMETER (IY=108)

      INTEGER JXX,JX,MX
      INTEGER JXXMX,JXMX,MX2
      INTEGER IYH
      PARAMETER (JXX=IRES+2,JX=IRES+1,MX=IRES+1)
      PARAMETER (JXXMX=JXX*MX,JXMX=JX*MX,MX2=MX*2)
      PARAMETER (IYH=IY/2)
C
      INTEGER IHEM,IH,ILEV
      COMMON /LEVPOS/IHEM,IH,ILEV
      DOUBLE PRECISION EPSINV
      COMMON /KERNEL/EPSINV(JXX,MX)
      COMMON /LEGPOL/POLY(JXX,MX),W(IY),WOCS(IY),COA(IY),SIA(IY),
     +       DELTA(IY)
      DOUBLE PRECISION POLY,W,WOCS,COA,SIA,DELTA
c
c
c
      INTEGER IR,IRP1,IRP2,IRMAX,IRMAX1,IRMAX2,I2IR,I4IR,ND2P1
      COMMON /SPECT/IR,IRP1,IRP2,IRMAX,IRMAX1,IRMAX2,I2IR,I4IR,ND2P1
      COMMON /LOCLOC/EPSI(JXX,MX),ILAT,ILATH,ILONG,ILONGL
      INTEGER ILAT,ILATH,ILONG,ILONGL
      DOUBLE PRECISION EPSI
c
      DOUBLE PRECISION ALLALP(JXX,MX,IYH)
c *** dimension information
c
      DOUBLE COMPLEX SVAR(JX,MX),PSS(JXX,MX),FTPS(MX)
c
      DOUBLE PRECISION A(JX,MX),B(JX,MX),GVAR(IX,IY)

c
c local variable
c
      INTEGER I,J,K
      INTEGER JJ,INHSH,LATREL
      INTEGER KI,KS,KK
      INTEGER M
      DOUBLE PRECISION XLAT(IY),GW(IY)
c----------------------------------------------------------------------
c
c Set some constants.
c
      CALL DINITAL
c
c set lat information
c
      DO J = 1,IY
          W(J) = GW(J)
      END DO
c
      DO J = 1,IY
          COA(J) = SIN(XLAT(J)*ASIN(1.0D0)/90.D0)
      END DO
c
c     svar = cmplx(a,b)       original code had f90  array syntax
      DO I = 1,MX
          DO J = 1,JX
              SVAR(J,I) = DCMPLX(A(J,I),B(J,I))
          END DO
      END DO
c
      CALL DGAUSSV(ILAT,ILONG)
c
      CALL DEPSCAL(EPSI,IR)
c
      DO M = 1,IRP1
          EPSINV(1,M) = 0.0D0
      END DO
      DO M = 1,IRP1
          DO J = 2,IRP2
              EPSINV(J,M) = 1.0D0/EPSI(J,M)
          END DO
      END DO
c
c Precompute legendre functions and put on disk
c
      DO IH = 1,ILATH
          CALL DALPMN1(POLY,COA(IH),MX,JXX,EPSI)
          DO J = 1,MX
              DO I = 1,JXX
                  ALLALP(I,J,IH) = POLY(I,J)
              END DO
          END DO
      END DO
c
c     calculate PS fields and map if requested
c
      PS1 = 0
      DO 335 M = 1,MX
          PSS(JXX,M) = (0.D0,0.D0)
          DO 335 J = 1,JX
              PSS(J,M) = SVAR(J,M)
  335 CONTINUE
      DO 130 IH = 1,IYH
          DO J = 1,MX
              DO I = 1,JXX
                  POLY(I,J) = ALLALP(I,J,IH)
              END DO
          END DO
          DO 130 INHSH = 1,2
              IF (INHSH.EQ.1) LATREL = IH
              IF (INHSH.EQ.2) LATREL = IY + 1 - IH
              IF (INHSH.EQ.2) CALL DASS
              CALL DLGNTJM(FTPS,PSS,POLY,JXX,MX,1)
              CALL DMFFTG(GVAR(1,LATREL),FTPS,1,0)
c
  130 CONTINUE
      END
c
      SUBROUTINE DALPMN1(ALP,SINLAT,IRP1,IRP2,EPSI)
      DOUBLE PRECISION ALP
      DOUBLE PRECISION SINLAT
      DOUBLE PRECISION EPSI
      DOUBLE PRECISION PROD
      DOUBLE PRECISION EPSINV
      DOUBLE PRECISION COS2
      DOUBLE PRECISION COS2A
      DOUBLE PRECISION B
      DOUBLE PRECISION FMPTH
c
c     *fills alp(irp2,irp1) for a given sin(lat)
c
c djs include 'res.h'
c rhombical truncation parameter
      INTEGER IRES
c number of longitudes
      INTEGER IX
c ix>=3*ires for rhombical truncation

c number of latitudes
      INTEGER IY
c iy>=(5*ires+1)/2 for rhombical truncation

      PARAMETER (IRES=42)
      PARAMETER (IX=128)
      PARAMETER (IY=108)

      PARAMETER (JXX=IRES+2,JX=IRES+1,MX=IRES+1)
      PARAMETER (JXXMX=JXX*MX,JXMX=JX*MX,MX2=MX*2)
c
      DIMENSION ALP(IRP2,IRP1),EPSI(IRP2,IRP1),PROD(MX)
      COMMON /KERNEL/EPSINV(JXX,MX)
c     --------------------------------------------------------------
c
      COS2 = 1.0D0 - SINLAT*SINLAT
      COS2A = COS2
      PROD(1) = 1.0D0
      B = 0.0D0
c
      DO M = 2,IRP1
          COS2A = COS2A + COS2 + COS2
          B = B + 2.0D0
          PROD(M) = PROD(M-1)*COS2A/B
      END DO
c
      ALP(1,1) = SQRT(0.5D0)
      ALP(2,1) = SQRT(1.5D0)*SINLAT
      DO M = 2,IRP1
          FMPTH = M + 0.5D0
          ALP(1,M) = SQRT(0.5D0*PROD(M))
          ALP(2,M) = SQRT(FMPTH+FMPTH)*SINLAT*ALP(1,M)
      END DO
c
      DO 20 N = 3,IRP2
          DO 20 M = 1,IRP1
              ALP(N,M) = (SINLAT*ALP(N-1,M)-EPSI(N-1,M)*ALP(N-2,M))*
     +                   EPSINV(N,M)
   20 CONTINUE
c
      RETURN
      END
C
      SUBROUTINE DCOOL(DATA,NPREV,N,NREM,ISIGN,IFACT)
      DOUBLE PRECISION DATA
      DOUBLE PRECISION ROOTRA
      DOUBLE PRECISION ROOTIA
      DOUBLE PRECISION WSTPRA
      DOUBLE PRECISION WSTPIA
      DOUBLE PRECISION ZSTPRA
      DOUBLE PRECISION ZSTPIA
      DOUBLE PRECISION TWOPI
      DOUBLE PRECISION ROOTR
      DOUBLE PRECISION ROOTI
      DOUBLE PRECISION WSTPR
      DOUBLE PRECISION WSTPI
      DOUBLE PRECISION WR
      DOUBLE PRECISION WI
      DOUBLE PRECISION W2R
      DOUBLE PRECISION W2I
      DOUBLE PRECISION W3R
      DOUBLE PRECISION W3I
      DOUBLE PRECISION TEMPR
      DOUBLE PRECISION TEMPI
      DOUBLE PRECISION SUMR
      DOUBLE PRECISION SUMI
      DOUBLE PRECISION DIFR
      DOUBLE PRECISION DIFI
      DOUBLE PRECISION T0R
      DOUBLE PRECISION T0I
      DOUBLE PRECISION T1R
      DOUBLE PRECISION T1I
      DOUBLE PRECISION T2R
      DOUBLE PRECISION T2I
      DOUBLE PRECISION T3R
      DOUBLE PRECISION T3I
c     fourier transform of length n.  in place cooley-tukey method,
c     digit-reversed to normal order, sande-tukey factoring (2).
c     dimension data(nprev,n,nrem)
c     complex data
c     data(i1,j2,i3) = sum(data(i1,i2,i3)*exp(isign*2*pi*i*((i2-1)*
c     (j2-1)/n))), summed over i2 = 1 to n for all i1 from 1 to nprev,
c     j2 from 1 to n and i3 from 1 to nrem.  the factors of n are given
c     in any order in array ifact.  factors of two are done in pairs
c     as much as possible (fourier transform of length four), factors of
c     three are done separately, and all factors five or higher
c     are done by goertzels algorithm (4).
      DIMENSION DATA(1),IFACT(1)
      PARAMETER (IX=128)
      COMMON /BREN2/ROOTRA(6,2),ROOTIA(6,2),WSTPRA(6,2),WSTPIA(6,2),
     +       ZSTPRA(2),ZSTPIA(2),IORD(IX+2)
      COMMON /FFTLOC/IHM1(30),LEV
c
      TWOPI = 6.2831853071795865D0*DBLE(ISIGN)
      IP0 = 2
      IP1 = IP0*NPREV
      IP4 = IP1*N
      IP5 = IP4*NREM
      IF = 0
      IP2 = IP1
   10 IF (IP2-IP4) 20,240,240
   20 IF = IF + 1
      IFCUR = IFACT(IF)
      IF (IFCUR-2) 60,30,60
   30 IF (4*IP2-IP4) 40,40,60
   40 IF (IFACT(IF+1)-2) 60,50,60
   50 IF = IF + 1
      IFCUR = 4
   60 IP3 = IP2*IFCUR
      IS = 1
      IF (ISIGN.GT.0) IS = 2
      ROOTR = ROOTRA(IF,IS)
      ROOTI = ROOTIA(IF,IS)
      WSTPR = WSTPRA(IF,IS)
      WSTPI = WSTPIA(IF,IS)
      WR = 1.D0
      WI = 0.D0
      DO 230 I2 = 1,IP2,IP1
          IF (IFCUR-4) 70,70,210
   70     IF ((I2-1)* (IFCUR-2)) 240,90,80
   80     W2R = WR*WR - WI*WI
          W2I = 2.D0*WR*WI
          W3R = W2R*WR - W2I*WI
          W3I = W2R*WI + W2I*WR
   90     I1MAX = I2 + IP1 - IP0
          DO 200 I1 = I2,I1MAX,IP0
              DO 200 I5 = I1,IP5,IP3
                  DO 200 IH = 1,LEV
                      J0 = I5 + IHM1(IH)
                      J1 = J0 + IP2
                      J2 = J1 + IP2
                      J3 = J2 + IP2
                      IF (I2-1) 140,140,100
  100                 IF (IFCUR-3) 130,120,110
c     apply the phase shift factors
  110                 TEMPR = DATA(J3)
                      DATA(J3) = W3R*TEMPR - W3I*DATA(J3+1)
                      DATA(J3+1) = W3R*DATA(J3+1) + W3I*TEMPR
                      TEMPR = DATA(J2)
                      DATA(J2) = WR*TEMPR - WI*DATA(J2+1)
                      DATA(J2+1) = WR*DATA(J2+1) + WI*TEMPR
                      TEMPR = DATA(J1)
                      DATA(J1) = W2R*TEMPR - W2I*DATA(J1+1)
                      DATA(J1+1) = W2R*DATA(J1+1) + W2I*TEMPR
                      GO TO 140
  120                 TEMPR = DATA(J2)
                      DATA(J2) = W2R*TEMPR - W2I*DATA(J2+1)
                      DATA(J2+1) = W2R*DATA(J2+1) + W2I*TEMPR
  130                 TEMPR = DATA(J1)
                      DATA(J1) = WR*TEMPR - WI*DATA(J1+1)
                      DATA(J1+1) = WR*DATA(J1+1) + WI*TEMPR
  140                 IF (IFCUR-3) 150,160,170
c     do a fourier transform of length two
  150                 TEMPR = DATA(J1)
                      TEMPI = DATA(J1+1)
                      DATA(J1) = DATA(J0) - TEMPR
                      DATA(J1+1) = DATA(J0+1) - TEMPI
                      DATA(J0) = DATA(J0) + TEMPR
                      DATA(J0+1) = DATA(J0+1) + TEMPI
                      GO TO 200
c     do a fourier transform of length three
  160                 SUMR = DATA(J1) + DATA(J2)
                      SUMI = DATA(J1+1) + DATA(J2+1)
                      TEMPR = DATA(J0) - .5D0*SUMR
                      TEMPI = DATA(J0+1) - .5D0*SUMI
                      DATA(J0) = DATA(J0) + SUMR
                      DATA(J0+1) = DATA(J0+1) + SUMI
                      DIFR = ROOTI* (DATA(J2+1)-DATA(J1+1))
                      DIFI = ROOTI* (DATA(J1)-DATA(J2))
                      DATA(J1) = TEMPR + DIFR
                      DATA(J1+1) = TEMPI + DIFI
                      DATA(J2) = TEMPR - DIFR
                      DATA(J2+1) = TEMPI - DIFI
                      GO TO 200
c     do a fourier transform of length four (from bit reversed order)
  170                 T0R = DATA(J0) + DATA(J1)
                      T0I = DATA(J0+1) + DATA(J1+1)
                      T1R = DATA(J0) - DATA(J1)
                      T1I = DATA(J0+1) - DATA(J1+1)
                      T2R = DATA(J2) + DATA(J3)
                      T2I = DATA(J2+1) + DATA(J3+1)
                      T3R = DATA(J2) - DATA(J3)
                      T3I = DATA(J2+1) - DATA(J3+1)
                      DATA(J0) = T0R + T2R
                      DATA(J0+1) = T0I + T2I
                      DATA(J2) = T0R - T2R
                      DATA(J2+1) = T0I - T2I
                      IF (ISIGN) 180,180,190
  180                 T3R = -T3R
                      T3I = -T3I
  190                 DATA(J1) = T1R - T3I
                      DATA(J1+1) = T1I + T3R
                      DATA(J3) = T1R + T3I
                      DATA(J3+1) = T1I - T3R
  200     CONTINUE
          GO TO 220
c     do a fourier transform of length five or more
c210  call goert (data(i2),nprev,ip2/ip1,ifcur,ip5/ip3,work,wr,wi,rootr,
c    $rooti)
  210     CONTINUE
  220     TEMPR = WR
          WR = WSTPR*TEMPR - WSTPI*WI + TEMPR
  230 WI = WSTPR*WI + WSTPI*TEMPR + WI
      IP2 = IP3
      GO TO 10
  240 RETURN
      END
c
      SUBROUTINE DFIXRL(DATA,N,NREM,ISIGN,IFORM)
c     for iform = 0, convert the transform of a doubled-up real array,
c     considered complex, into its true transform.  supply only the
c     first half of the complex transform, as the second half has
c     conjugate symmetry.  for iform = -1, convert the first half
c     of the true transform into the transform of a doubled-up real
c     array.  n must be even.
c     using complex notation and subscripts starting at zero, the
c     transformation is--
c     dimension data(n,nrem)
c     zstp = exp(isign*2*pi*i/n)
c     do 10 i2=0,nrem-1
c     data(0,i2) = conj(data(0,i2))*(1+i)
c     do 10 i1=1,n/4
c     z = (1+(2*iform+1)*i*zstp**i1)/2
c     i1cnj = n/2-i1
c     dif = data(i1,i2)-conj(data(i1cnj,i2))
c     temp = z*dif
c     data(i1,i2) = (data(i1,i2)-temp)*(1-iform)
c 10  data(i1cnj,i2) = (data(i1cnj,i2)+conj(temp))*(1-iform)
c     if i1=i1cnj, the calculation for that value collapses into
c     a simple conjugation of data(i1,i2).
      PARAMETER (IX=128)
      DOUBLE PRECISION DATA
      DOUBLE PRECISION ROOTRA
      DOUBLE PRECISION ROOTIA
      DOUBLE PRECISION WSTPRA
      DOUBLE PRECISION WSTPIA
      DOUBLE PRECISION ZSTPRA
      DOUBLE PRECISION ZSTPIA
      DOUBLE PRECISION TWOPI
      DOUBLE PRECISION TEMPR
      DOUBLE PRECISION ZSTPR
      DOUBLE PRECISION ZSTPI
      DOUBLE PRECISION ZR
      DOUBLE PRECISION ZI
      DOUBLE PRECISION DIFR
      DOUBLE PRECISION DIFI
      DOUBLE PRECISION TEMPI
      COMMON /BREN2/ROOTRA(6,2),ROOTIA(6,2),WSTPRA(6,2),WSTPIA(6,2),
     +       ZSTPRA(2),ZSTPIA(2),IORD(IX+2)
c     common /fftloc/ihm1(9),lev
      COMMON /FFTLOC/IHM1(30),LEV
      DIMENSION DATA(1)
c
      TWOPI = 6.2831853071795865D0*DBLE(ISIGN)
      IP0 = 2
      IP1 = IP0* (N/2)
      IP2 = IP1*NREM
      IF (IFORM) 10,70,70
c     pack the real input values (two per column)
   10 CONTINUE
      DO 119 IH = 1,LEV
          J1 = IP1 + 1 + IHM1(IH)
          DATA(2+IHM1(IH)) = DATA(J1)
  119 CONTINUE
      IF (NREM-1) 70,70,20
   20 DO 60 IH = 1,LEV
          J1 = IP1 + 1 + IHM1(IH) + IP0
          I2MIN = IP1 + 1
          DO 60 I22 = I2MIN,IP2,IP1
              I2 = I22 + IHM1(IH)
              DATA(I2) = DATA(J1)
              J1 = J1 + IP0
              IF (N-2) 50,50,30
   30         I1MIN = I2 + IP0
              I1MAX = I2 + IP1 - IP0
              DO 40 I1 = I1MIN,I1MAX,IP0
                  DATA(I1) = DATA(J1)
                  DATA(I1+1) = DATA(J1+1)
   40         J1 = J1 + IP0
   50         DATA(I2+1) = DATA(J1)
   60 J1 = J1 + IP0
   70 DO 80 IH = 1,LEV
          DO 80 I22 = 1,IP2,IP1
              I2 = I22 + IHM1(IH)
              TEMPR = DATA(I2)
              DATA(I2) = DATA(I2) + DATA(I2+1)
   80 DATA(I2+1) = TEMPR - DATA(I2+1)
      IF (N-2) 200,200,90
   90 IS = 1
      IF (ISIGN.GT.0) IS = 2
      ZSTPR = ZSTPRA(IS)
      ZSTPI = ZSTPIA(IS)
      ZR = (1.D0-ZSTPI)/2.D0
      ZI = (1.D0+ZSTPR)/2.D0
      IF (IFORM) 100,110,110
  100 ZR = 1.D0 - ZR
      ZI = -ZI
  110 I1MIN = IP0 + 1
      I1MAX = IP0* (N/4) + 1
      DO 190 I11 = I1MIN,I1MAX,IP0
          DO 180 I22 = I11,IP2,IP1
              DO 180 IH = 1,LEV
                  I1 = I11 + IHM1(IH)
                  I2 = I22 + IHM1(IH)
                  I2CNJ = IP0* (N/2+1) - 2*I11 + I22
                  I2CNJ = I2CNJ + IHM1(IH)
                  IF (I2-I2CNJ) 150,120,120
  120             IF (ISIGN* (2*IFORM+1)) 130,140,140
  130             DATA(I2+1) = -DATA(I2+1)
  140             IF (IFORM) 170,180,180
  150             DIFR = DATA(I2) - DATA(I2CNJ)
                  DIFI = DATA(I2+1) + DATA(I2CNJ+1)
                  TEMPR = DIFR*ZR - DIFI*ZI
                  TEMPI = DIFR*ZI + DIFI*ZR
                  DATA(I2) = DATA(I2) - TEMPR
                  DATA(I2+1) = DATA(I2+1) - TEMPI
                  DATA(I2CNJ) = DATA(I2CNJ) + TEMPR
                  DATA(I2CNJ+1) = DATA(I2CNJ+1) - TEMPI
                  IF (IFORM) 160,180,180
  160             DATA(I2CNJ) = DATA(I2CNJ) + DATA(I2CNJ)
                  DATA(I2CNJ+1) = DATA(I2CNJ+1) + DATA(I2CNJ+1)
  170             DATA(I2) = DATA(I2) + DATA(I2)
                  DATA(I2+1) = DATA(I2+1) + DATA(I2+1)
  180     CONTINUE
          TEMPR = ZR - .5D0
          ZR = ZSTPR*TEMPR - ZSTPI*ZI + ZR
  190 ZI = ZSTPR*ZI + ZSTPI*TEMPR + ZI
c     recursion saves time, at a slight loss in accuracy.  if available,
c     use double precision to compute zr and zi.
  200 IF (IFORM) 270,210,210
c     unpack the real transform values (two per column)
  210 DO 260 IH = 1,LEV
          I2 = IP2 + 1 + IHM1(IH)
          I1 = I2
          J1 = IP0* (N/2+1)*NREM + 1 + IHM1(IH)
          GO TO 250
  220     DATA(J1) = DATA(I1)
          DATA(J1+1) = DATA(I1+1)
          I1 = I1 - IP0
          J1 = J1 - IP0
  230     IF (I2-I1) 220,240,240
  240     DATA(J1) = DATA(I1)
          DATA(J1+1) = 0.D0
  250     I2 = I2 - IP1
          J1 = J1 - IP0
          DATA(J1) = DATA(I2+1)
          DATA(J1+1) = 0.D0
          I1 = I1 - IP0
          J1 = J1 - IP0
          IF (I2-1-IHM1(IH)) 260,260,230
  260 DATA(2+IHM1(IH)) = 0.0D0
  270 RETURN
      END
c
      SUBROUTINE DGAUSSV(ILAT,ILONG)
      DOUBLE PRECISION ROOTRA
      DOUBLE PRECISION ROOTIA
      DOUBLE PRECISION WSTPRA
      DOUBLE PRECISION WSTPIA
      DOUBLE PRECISION ZSTPRA
      DOUBLE PRECISION ZSTPIA
      DOUBLE PRECISION DATA
      DOUBLE PRECISION WORK
      DOUBLE PRECISION PI
      DOUBLE PRECISION TWOPIC
      DOUBLE PRECISION TWOPI
      DOUBLE PRECISION THETA
      DOUBLE PRECISION SINTH
      DOUBLE PRECISION ROOTR
      DOUBLE PRECISION ROOTI
      DOUBLE PRECISION WSTPR
      DOUBLE PRECISION WSTPI
      DOUBLE PRECISION ZSTPR
      DOUBLE PRECISION ZSTPI
c djs include 'res.h'
c rhombical truncation parameter
      INTEGER IRES
c number of longitudes
      INTEGER IX
c ix>=3*ires for rhombical truncation

c number of latitudes
      INTEGER IY
c iy>=(5*ires+1)/2 for rhombical truncation

      PARAMETER (IRES=42)
      PARAMETER (IX=128)
      PARAMETER (IY=108)

      PARAMETER (JXX=IRES+2,JX=IRES+1,MX=IRES+1)
      PARAMETER (JXXMX=JXX*MX,JXMX=JX*MX,MX2=MX*2)
c
      DOUBLE PRECISION ALP,W,WOCS,COA,SIA,DELTA
      COMMON /LEGPOL/ALP(JXXMX),W(IY),WOCS(IY),COA(IY),SIA(IY),DELTA(IY)
      COMMON /BRENN/NCURR,IFACT(32),NFACT,ISYM,IFSYM(32),NFSYM,ICENT,
     +       IFCNT(10),NFCNT
      COMMON /BREN2/ROOTRA(6,2),ROOTIA(6,2),WSTPRA(6,2),WSTPIA(6,2),
     +       ZSTPRA(2),ZSTPIA(2),IORD(IX+2)

      DIMENSION DATA(IX+2),WORK(12)
      COMMON /FFTLOC/IHM1(30),LEV
      COMMON /LEVPOS/IHEM,IH,ILEV
c
      PI = 4.0D0*ATAN(1.0D0)
c
      DO IH = 1,ILEV
          IHM1(IH) = (IH-1)* (ILONG+2)
      END DO
c
      NCURR = ILONG/2
      DO I = 1,ILAT
          W(I) = W(I)/ILONG
          DELTA(I) = ACOS(COA(I))
          SIA(I) = SIN(DELTA(I))
          WOCS(I) = W(I)/ (SIA(I)*SIA(I))
      END DO
c
      CALL DFACTR(NCURR,IFACT,NFACT)
      CALL DSMFAC(IFACT,NFACT,ISYM,IFSYM,NFSYM,ICENT,IFCNT,NFCNT)
      TWOPIC = 2.0D0*PI
      IPO = 2
      N = ILONG
      IP1 = IPO
      IP4 = IP1*NCURR
      IP5 = IP1
      DO 480 IS = 1,2
          IF = 0
          IP2 = IP1
          TWOPI = -TWOPIC
          IF (IS.EQ.2) TWOPI = TWOPIC
  200     IF (IP2.GE.IP4) GO TO 480
          IF = IF + 1
          IFCUR = IFACT(IF)
          IF (IFCUR.NE.2) GO TO 120
          IF (4*IP2.GT.IP4) GO TO 120
          IF (IFACT(IF+1).NE.2) GO TO 120
          IF = IF + 1
          IFCUR = 4
  120     IP3 = IP2*IFCUR
          THETA = TWOPI/DBLE(IFCUR)
          SINTH = SIN(THETA/2.0D0)
          ROOTR = -2.0D0*SINTH*SINTH
          ROOTI = SIN(THETA)
          THETA = TWOPI/DBLE(IP3/IP1)
          SINTH = SIN(THETA/2.0D0)
          WSTPR = -2.0D0*SINTH*SINTH
          WSTPI = SIN(THETA)
          ROOTRA(IF,IS) = ROOTR
          ROOTIA(IF,IS) = ROOTI
          WSTPRA(IF,IS) = WSTPR
          WSTPIA(IF,IS) = WSTPI
          IP2 = IP3
          GO TO 200
  480 CONTINUE
  240 CONTINUE
c
      IF (N.LE.2) GO TO 900
      DO IS = 1,2
          TWOPI = -TWOPIC
          IF (IS.EQ.2) TWOPI = TWOPIC
          THETA = TWOPI/DBLE(N)
          SINTH = SIN(THETA/2.0D0)
          ZSTPR = -2.0D0*SINTH*SINTH
          ZSTPI = SIN(THETA)
          ZSTPRA(IS) = ZSTPR
          ZSTPIA(IS) = ZSTPI
      END DO
  900 CONTINUE
c
      NP2 = ILONG + 2
      DO I = 1,NP2
          DATA(I) = DBLE(I)
      END DO
c
      CALL DSYMRV(DATA,1,NCURR,1,IFSYM,NFSYM)
      CALL DASMRV(DATA,ISYM,ICENT,ISYM,IFCNT,NFCNT,WORK)
c
      DO I = 1,NP2
          IORD(I) = DATA(I)
      END DO
c
      RETURN
      END
c
      SUBROUTINE DLGNTJM(FTVAR,VAR,P,JX,MX,KX)
      DOUBLE PRECISION P
      DOUBLE COMPLEX VAR(JX,MX,KX),FTVAR(MX,KX)
      DIMENSION P(JX,MX)
c
      DO 10 K = 1,KX
          DO 10 M = 1,MX
              FTVAR(M,K) = (0.0D0,0.0D0)
              DO 10 J = 1,JX
                  FTVAR(M,K) = FTVAR(M,K) + VAR(J,M,K)*P(J,M)
   10 CONTINUE
c
      RETURN
      END
c
      SUBROUTINE DMFFTG(FG,FM,NLEV,NADJ)
      DOUBLE PRECISION FG
      DOUBLE PRECISION EPSI
      DOUBLE PRECISION ROOTRA
      DOUBLE PRECISION ROOTIA
      DOUBLE PRECISION WSTPRA
      DOUBLE PRECISION WSTPIA
      DOUBLE PRECISION ZSTPRA
      DOUBLE PRECISION ZSTPIA
      DOUBLE PRECISION BDATA
c djs include 'res.h'
c rhombical truncation parameter
      INTEGER IRES
c number of longitudes
      INTEGER IX
c ix>=3*ires for rhombical truncation

c number of latitudes
      INTEGER IY
c iy>=(5*ires+1)/2 for rhombical truncation

      PARAMETER (IRES=42)
      PARAMETER (IX=128)
      PARAMETER (IY=108)

      PARAMETER (JXX=IRES+2,JX=IRES+1,MX=IRES+1)
      PARAMETER (JXXMX=JXX*MX,JXMX=JX*MX,MX2=MX*2)
      PARAMETER (IX2=IX+2,IX2H=IX2/2)
c
      DIMENSION FM(MX,1),FG(IX,7),TRAN(IX2H,7),DATA(IX2,7)
      COMMON /FFTLOC/IHM1(30),LEV
      EQUIVALENCE (TRAN(1,1),DATA(1,1))
      DOUBLE COMPLEX WORK(6),TRAN
      DOUBLE PRECISION DATA
      DOUBLE COMPLEX FM
      COMMON /SPECT/IR,IRP1,IRP2,IRMAX,IRMAX1,IRMAX2,I2IR,I4IR,ND2P1
      COMMON /LOCLOC/EPSI(JXXMX),ILAT,ILATH,ILONG,ILONGL
      COMMON /BRENN/NCURR,IFACT(32),NFACT,ISYM,IFSYM(32),NFSYM,ICENT,
     +       IFCNT(10),NFCNT
      COMMON /BREN2/ROOTRA(6,2),ROOTIA(6,2),WSTPRA(6,2),WSTPIA(6,2),
     +       ZSTPRA(2),ZSTPIA(2),IORD(IX2)
      DIMENSION BDATA(IX2)
c
      NWORK = 6
      N = ILONG
      LEV = NLEV
      NP2 = N + 2
      IF (NADJ.EQ.1) N = ILONGL
      ISIGN = 1
      IFORM = -1
      DO 40 IH = 1,LEV
          DO MP = 1,IRP1
              TRAN(MP,IH) = FM(MP,IH)
          END DO
          DO MP = IRP2,ND2P1
              TRAN(MP,IH) = (0.0D0,0.0D0)
          END DO
   40 CONTINUE
c
      NREM = 1
      CALL DFIXRL(DATA,N,NREM,ISIGN,IFORM)
c
      NPREV = 1
      DO 60 IH = 1,LEV
          DO J = 1,NP2
              BDATA(J) = DATA(IORD(J),IH)
          END DO
          DO J = 1,NP2
              DATA(J,IH) = BDATA(J)
          END DO
   60 CONTINUE
c
      CALL DCOOL(DATA,NPREV,NCURR,NREM,ISIGN,IFACT)
c
      DO 100 IH = 1,NLEV
          DO J = 1,N
              FG(J,IH) = DATA(J,IH)
          END DO
  100 CONTINUE
c
      RETURN
      END
c
      SUBROUTINE DINITAL
      DOUBLE PRECISION EPSI
C djs include 'res.h'
c rhombical truncation parameter
      INTEGER IRES
c number of longitudes
      INTEGER IX
c ix>=3*ires for rhombical truncation

c number of latitudes
      INTEGER IY
c iy>=(5*ires+1)/2 for rhombical truncation

      PARAMETER (IRES=42)
      PARAMETER (IX=128)
      PARAMETER (IY=108)

      PARAMETER (JXX=IRES+2,JX=IRES+1,MX=IRES+1)
      PARAMETER (JXXMX=JXX*MX,JXMX=JX*MX,MX2=MX*2)
c
      COMMON /SPECT/IR,IRP1,IRP2,IRMAX,IRMAX1,IRMAX2,I2IR,I4IR,ND2P1
      COMMON /LOCLOC/EPSI(JXX,MX),ILAT,ILATH,ILONG,ILONGL
      COMMON /LEVPOS/IHEM,IH,ILEV
      COMMON /INDEX/KMJX(MX),KMJXX(MX)
c
      ILAT = IY
      ILATH = ILAT/2
      ILONG = IX
      ILONGL = ILONG
c
      IR = IRES
      IRP2 = IR + 2
      IRP1 = IR + 1
      IRMAX = IR
      ND2P1 = ILONG/2 + 1
      IRMAX1 = IRMAX + 1
      IRMAX2 = IRMAX + 2
      I2IR = 2*IR
      I4IR = 2*I2IR
c
      IHEM = 1
      IH = 1
      ILEV = 1
c
c     now set up indexing arrays to improve efficiency
c
      DO 7 M = 1,MX
          KMJX(M) = (M-1)*JX
          KMJXX(M) = (M-1)*JXX
    7 CONTINUE
c
      RETURN
      END
c
      SUBROUTINE DASS
      DOUBLE PRECISION ALP
      DOUBLE PRECISION W
      DOUBLE PRECISION WOCS
      DOUBLE PRECISION COA
      DOUBLE PRECISION SIA
      DOUBLE PRECISION DELTA
c djs include 'res.h'
c rhombical truncation parameter
      INTEGER IRES
c number of longitudes
      INTEGER IX
c ix>=3*ires for rhombical truncation

c number of latitudes
      INTEGER IY
c iy>=(5*ires+1)/2 for rhombical truncation

      PARAMETER (IRES=42)
      PARAMETER (IX=128)
      PARAMETER (IY=108)

      PARAMETER (JXX=IRES+2,JX=IRES+1,MX=IRES+1)
      PARAMETER (JXXMX=JXX*MX,JXMX=JX*MX,MX2=MX*2)
c
      COMMON /SPECT/IR,IRP1,IRP2,IRMAX,IRMAX1,IRMAX2,I2IR,I4IR,ND2P1
      COMMON /INDEX/KMJX(MX),KMJXX(MX)
      COMMON /LEGPOL/ALP(JXXMX),W(IY),WOCS(IY),COA(IY),SIA(IY),DELTA(IY)
c
      LPFIN = IRP1
      IF (MOD(IR,2).EQ.0) LPFIN = JXX

      DO 50 MP = 1,IRP1
          DO 50 LP = 2,LPFIN,2
              ILM = KMJXX(MP) + LP
              ALP(ILM) = -ALP(ILM)
   50 CONTINUE

      RETURN
      END
c
      SUBROUTINE DEPSCAL(EPSI,IR)
      DOUBLE PRECISION EPSI
      DOUBLE PRECISION T
      DOUBLE PRECISION B
c
c     *calculates epsi(lm)=sqrt(l**2-m**2)/(4*l**2-1)
c                  l=0,irp1,   m=0,ir
c               epsi must have size  (irp2,irp1)
c
      DIMENSION EPSI(1)

      IRP1 = IR + 1
      IPM = 1 - IRP1
c
      DO 30 MP = 1,IRP1
          M = MP - 1
          IPM = IPM + IRP1
          INIT = M
          IF (M.EQ.0) INIT = 1
          MPIRP1 = M + IRP1
          DO 30 L = INIT,MPIRP1
              IL = IPM + L
              T = (L+M)* (L-M)
              B = 4*L*L - 1
              EPSI(IL) = SQRT(T/B)
   30 CONTINUE
      EPSI(1) = 0.0D0
c
      RETURN
      END
c
      SUBROUTINE DSYMRV(DATA,NPREV,N,NREM,IFACT,NFACT)
      DOUBLE PRECISION DATA
      DOUBLE PRECISION TEMPR
      DOUBLE PRECISION TEMPI
c     shuffle the data array by reversing the digits of one index.
c     dimension data(nprev,n,nrem)
c     replace data(i1,i2,i3) by data(i1,i2rev,i3) for all i1 from 1 to
c     nprev, i2 from 1 to n and i3 from 1 to nrem.  i2rev-1 is the
c     integer whose digit representation in the multi-radix notation
c     of factors ifact(if) is the reverse of the representation of i2-1.
c     for example, if all ifact(if) = 2, i2-1 = 11001, i2rev-1 = 10011.
c     the factors must be symmetrically arranged, i.e., ifact(if) =
c     ifact(nfact+1-if).
      DIMENSION DATA(1),IFACT(1)
c
      IF (NFACT.LE.1) RETURN
c
      IP0 = 2
      IP1 = IP0*NPREV
      IP4 = IP1*N
      IP5 = IP4*NREM
      I4REV = 1
      DO 70 I4 = 1,IP4,IP1
          IF (I4-I4REV) 20,40,40
   20     I1MAX = I4 + IP1 - IP0
          DO 30 I1 = I4,I1MAX,IP0
              DO 30 I5 = I1,IP5,IP4
                  I5REV = I4REV + I5 - I4
                  TEMPR = DATA(I5)
                  TEMPI = DATA(I5+1)
                  DATA(I5) = DATA(I5REV)
                  DATA(I5+1) = DATA(I5REV+1)
                  DATA(I5REV) = TEMPR
   30     DATA(I5REV+1) = TEMPI
   40     IP3 = IP4
          DO 60 IF = 1,NFACT
              IP2 = IP3/IFACT(IF)
              I4REV = I4REV + IP2
              IF (I4REV-IP3) 70,70,50
   50         I4REV = I4REV - IP3
   60     IP3 = IP2
   70 CONTINUE
c
      RETURN
      END
c
      SUBROUTINE DASMRV(DATA,NPREV,N,NREM,IFACT,NFACT,WORK)
      DOUBLE PRECISION DATA
      DOUBLE PRECISION WORK
c     shuffle the data array by reversing the digits of one index.
c     the operation is the same as in symrv, except that the factors
c     need not be symmetrically arranged, i.e., generally ifact(if) not=
c     ifact(nfact+1-if).  consequently, a work array of length n is
c     needed.
      DIMENSION DATA(1),WORK(1),IFACT(1)
c
      IF (NFACT-1) 60,60,10
   10 IP0 = 2
      IP1 = IP0*NPREV
      IP4 = IP1*N
      IP5 = IP4*NREM
      DO 50 I1 = 1,IP1,IP0
          DO 50 I5 = I1,IP5,IP4
              IWORK = 1
              I4REV = I5
              I4MAX = I5 + IP4 - IP1
              DO 40 I4 = I5,I4MAX,IP1
                  WORK(IWORK) = DATA(I4REV)
                  WORK(IWORK+1) = DATA(I4REV+1)
                  IP3 = IP4
                  DO 30 IF = 1,NFACT
                      IP2 = IP3/IFACT(IF)
                      I4REV = I4REV + IP2
                      IF (I4REV-IP3-I5) 40,20,20
   20                 I4REV = I4REV - IP3
   30             IP3 = IP2
   40         IWORK = IWORK + IP0
              IWORK = 1
              DO 50 I4 = I5,I4MAX,IP1
                  DATA(I4) = WORK(IWORK)
                  DATA(I4+1) = WORK(IWORK+1)
                  IWORK = IWORK + IP0
   50 CONTINUE
c
   60 RETURN
      END
      SUBROUTINE DFACTR(N,IFACT,NFACT)
c     factor n into its prime factors, nfact in number.  for example,
c     for n = 1960, nfact = 6 and ifact(if) = 2, 2, 2, 5, 7 and 7.
      DIMENSION IFACT(1)
c
      IF = 0
      NPART = N
      DO 50 ID = 1,N,2
          IDIV = ID
          IF (ID-1) 10,10,20
   10     IDIV = 2
   20     IQUOT = NPART/IDIV
          IF (NPART-IDIV*IQUOT) 40,30,40
   30     IF = IF + 1
          IFACT(IF) = IDIV
          NPART = IQUOT
          GO TO 20
   40     IF (IQUOT-IDIV) 60,60,50
   50 CONTINUE
c
   60 IF (NPART-1) 80,80,70
   70 IF = IF + 1
      IFACT(IF) = NPART
   80 NFACT = IF
c
      RETURN
      END
c
      SUBROUTINE DSMFAC(IFACT,NFACT,ISYM,IFSYM,NFSYM,ICENT,IFCNT,NFCNT)
c     rearrange the prime factors of n into a square and a non-
c     square.  n = isym*icent*isym, where icent is square-free.
c     isym = ifsym(1)*...*ifsym(nfsym), each a prime factor.
c     icent = ifcnt(1)*...*ifcnt(nfcnt), each a prime factor.
c     for example, n = 1960 = 14*10*14.  then isym = 14, icent = 10,
c     nfsym = 2, nfcnt = 2, nfact = 6, ifsym(ifs) = 2, 7, ifcnt(ifc) =
c     2, 5 and ifact(if) = 2, 7, 2, 5, 7, 2.
      DIMENSION IFSYM(1),IFCNT(1),IFACT(1)
c
      ISYM = 1
      ICENT = 1
      IFS = 0
      IFC = 0
      IF = 1
   10 IF (IF-NFACT) 20,40,50
   20 IF (IFACT(IF)-IFACT(IF+1)) 40,30,40
   30 IFS = IFS + 1
      IFSYM(IFS) = IFACT(IF)
      ISYM = IFACT(IF)*ISYM
      IF = IF + 2
      GO TO 10
   40 IFC = IFC + 1
      IFCNT(IFC) = IFACT(IF)
      ICENT = IFACT(IF)*ICENT
      IF = IF + 1
      GO TO 10
   50 NFSYM = IFS
      NFCNT = IFC
      NFSM2 = 2*NFSYM
      NFACT = 2*NFSYM + NFCNT
      IF (NFCNT) 80,80,60
   60 NFSM2 = NFSM2 + 1
      IFSYM(NFSYM+1) = ICENT
      DO 70 IFC = 1,NFCNT
          IF = NFSYM + IFC
   70 IFACT(IF) = IFCNT(IFC)
   80 IF (NFSYM) 110,110,90
   90 DO 100 IFS = 1,NFSYM
          IFSCJ = NFSM2 + 1 - IFS
          IFSYM(IFSCJ) = IFSYM(IFS)
          IFACT(IFS) = IFSYM(IFS)
          IFCNJ = NFACT + 1 - IFS
  100 IFACT(IFCNJ) = IFSYM(IFS)
  110 NFSYM = NFSM2
c
      RETURN
      END
