      SUBROUTINE DSVDLAP(X,Y,LDXY,MRT,NCX,NCY,NSV,IFLAG,XYMSG,IPRNT,W,
     +                   LWK,PCVAR,HOMLFT,HOMRGT,HETLFT,HETRGT,
     +                   AK,BK,LAB,IER)

c perform a singular value decomposition on the data
c .   as described by Bretherton, Smith and Wallace.
c .   ------------ fortran 77 version --------------
c .   This uses the LAPACK routine "SGESVD"

c Nomenclature:

c .   x,y      - input data arrays (see "iflag" argument)
c .   ldxy     - leading dimensions of x/y in the calling program
C*PL*ERROR* Comment line too long
c .   mrt      - actual number of rows used. This refers to the time dimension
c .              mrt <= ldxy
c .   ncx,ncy  - number of columns for each array. This refers to the
c .              number of grid points or stations being used.
c .   nsv      - desired number of singular values or modes
c .              generally: nsv <= 3; must be nsv <= min(ncx,ncy)
c .   iflag    - flag to indicate if the data should be standardized
c .              if iflag=1 the input data arrays will be REPLACED with
c .              the standardized data.
c .   xymsg    - missing code
c .   iprnt    - print out many of the steps (for debugging)
c .   w        - work array. This array will be partitioned via
c .              pointers.
c .              upon return the first 4 elements of this array
c .              will contain information
c .              (fnorm, condition number, calculated rank, LAPACK error
c .               code)
c .   lwk      - length of work (let nsvmx = min(ncx,ncy) then   lwk >=
c .              ncx*ncy + nsvmx*ncx + nsvmx*ncy + mrt*ncx + mrt*ncy + nsvmx
c .            + max(3*nsvmx+max(ncx,ncy) , 5*nsvmx)
c .   pcvar    - percent variance explained by the modes (dimensioned
c .              nsv)
c .   ak(lab)  - output: expansion coeff
c .   bk(lab)  - output: expansion coeff
c .   homlft   - left  homogeneous array
c .   homrgt   - right homogeneous array
c .   hetlft   - left  heterogeneous array
c .   hetrgt   - right heterogeneous array
c .   ier      - error code

c Note: in many cases ldxy=mrt (Max Rows allocated for Time dimension)

      INTEGER LDXY,MRT,NCX,NCY,NSVMX,IFLAG,IPRNT,LWK,IER

      DOUBLE PRECISION X(LDXY,NCX),Y(LDXY,NCY),W(LWK,*),XYMSG

      DOUBLE PRECISION HOMLFT(NCX,NSV),HETLFT(NCX,NSV),HOMRGT(NCY,NSV),
     +                 HETRGT(NCY,NSV),PCVAR(NSV),AK(LAB),BK(LAB)

      IER = 0
      IF (MRT.GT.LDXY) IER = IER + 1
      IF (NCX.LT.1 .OR. NCY.LT.1 .OR. MRT.LT.1) IER = IER + 10

      NSVMX = MIN(NCX,NCY)

      LW = NCX*NCY + NSVMX*NCX + NSVMX*NCY + MRT*NCX + MRT*NCY + NSVMX +
     +     MAX(3*NSVMX+MAX(NCX,NCY),5*NSVMX)
      IF (LW.GT.LWK) IER = IER + 100
      IF (IER.NE.0) RETURN

c partition the work array. pointers to initial array location

      IPT1 = 1
      IPT2 = IPT1 + NCX*NCY
      IPT3 = IPT2 + NCX*NSVMX
      IPT4 = IPT3 + NSVMX*NCY
      IPT5 = IPT4 + NSVMX
      IPT6 = IPT5 + MRT*NCX
      IPT7 = IPT6 + MRT*NCY

      CALL DSVDLAPACK1(X,Y,LDXY,MRT,NCX,NCY,IFLAG,XYMSG,IPRNT,PCVAR,
     +                HOMLFT,HOMRGT,HETLFT,HETRGT,NSVMX,NSV,W(IPT1,1),
     +                W(IPT2,1),W(IPT3,1),W(IPT4,1),W(IPT5,1),W(IPT6,1),
     +                W(IPT7,1))
      W(1,1) = W(IPT7,1)
      W(2,1) = W(IPT7+1,1)
      W(3,1) = W(IPT7+2,1)
      W(4,1) = W(IPT7+3,1)
      IER = W(4,1)

C This returns the expansion coefficients.

      K  = 0
      K5 = IPT5-1 
      K6 = IPT6-1 
      DO N=1,NSV
         DO M=1,MRT
            K     = K + 1
            AK(K) = W(K5+K,1)
            BK(K) = W(K6+K,1)
         END DO
      END DO
      RETURN
      END
c -------------------------------------------------------------
      SUBROUTINE DSVDLAPACK1(X,Y,LDXY,MRT,NCX,NCY,IFLAG,XYMSG,IPRNT,
     +                      PCVAR,HOMLFT,HOMRGT,HETLFT,HETRGT,NSVMX,NSV,
     +                      CRV,U,VT,SV,AK,BK,WORK)
      DOUBLE PRECISION XBAR
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION XDEV
      DOUBLE PRECISION YBAR
      DOUBLE PRECISION YVAR
      DOUBLE PRECISION YDEV
      DOUBLE PRECISION T
      DOUBLE PRECISION TOL
      DOUBLE PRECISION DEPSMACH
      DOUBLE PRECISION FNORM
      DOUBLE PRECISION CONDN
      DOUBLE PRECISION AMEAN
      DOUBLE PRECISION ASD
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XSD
      DOUBLE PRECISION CCV
      DOUBLE PRECISION BMEAN
      DOUBLE PRECISION BSD
      DOUBLE PRECISION YMEAN
      DOUBLE PRECISION YSD

c see description of SVDLAPACK for a description

c nomenclature for new variables:
c .   crv      - cross covariance matrix between the standardized
c .              x and y data arrays.
c .   u        - left singular vector
c .   vt       - transpose right singular vector
c .   sv       - singular values
c .   ak, bk   - expansion coef
c .   work     - required by the lapack routine

      INTEGER LDXY,MRT,NCX,NCY,IFLAG,IPRNT,NSVMX,NSV

      DOUBLE PRECISION X(LDXY,NCX),Y(LDXY,NCY),XYMSG

      DOUBLE PRECISION HOMLFT(NCX,NSV),HETLFT(NCX,NSV),HOMRGT(NCY,NSV),
     +                 HETRGT(NCY,NSV),PCVAR(NSV)
      DOUBLE PRECISION U(NCX,NSVMX),VT(NSVMX,NCY),CRV(NCX,NCY),
     +                 SV(NSVMX),AK(MRT,NSVMX),BK(MRT,NSVMX),WORK(*)

      CHARACTER LABEL*80

c check for any columns (stations/grid points) that
c .   contain all missing data.

c if (iflag.eq.1) standardize the data
c .   1) compute time mean and st dev at each location (grid pt)
c .   2) subtract mean from each observation
c .   3) divide by the st dev

      DO NC = 1,NCX
          CALL DSTAT2(X(1,NC),MRT,XYMSG,XBAR,XVAR,XDEV,NXUSE,IER)
          IF (XDEV.EQ.XYMSG .OR. XDEV.EQ.0.D0) THEN
              WRITE (*,FMT=
     +'(''SVD: Warning all X values ''
     +  ''in column are missing or are constant'')')
          ELSE
              IF (IFLAG.EQ.1) THEN
                  DO MR = 1,MRT
                      IF (X(MR,NC).NE.XYMSG) THEN
                          X(MR,NC) = (X(MR,NC)-XBAR)/XDEV
                      END IF
                  END DO
              END IF
          END IF
      END DO

      DO NC = 1,NCY
          CALL DSTAT2(Y(1,NC),MRT,XYMSG,YBAR,YVAR,YDEV,NYUSE,IER)
          IF (YDEV.EQ.XYMSG .OR. YDEV.EQ.0.D0) THEN
              WRITE (*,FMT=
     +'(''SVD: Warning all Y values ''
     +  ''in column are missing or are constant'')')
          ELSE
              IF (IFLAG.EQ.1) THEN
                  DO MR = 1,MRT
                      IF (Y(MR,NC).NE.XYMSG) THEN
                          Y(MR,NC) = (Y(MR,NC)-YBAR)/YDEV
                      END IF
                  END DO
              END IF
          END IF
      END DO

      IF (IPRNT.EQ.1) THEN
          LABEL = '$'
          IF (IFLAG.EQ.1) LABEL = 'after standardization$'

          LLAB = INDEX(LABEL,'$')
          CALL DSVDPAR(X,LDXY,MRT,NCX,'SVDLAPACK: X '//LABEL(1:LLAB))
          CALL DSVDPAR(Y,LDXY,MRT,NCY,'SVD_PACK: Y '//LABEL(1:LLAB))
      END IF

c cross-covariance [xt]*[y]/T    [ dimension (ncx,ncy)]
c .   note the subscripts of "x" have been reversed
c .   to "simulate" the transpose

      T = DBLE(MRT)
      DO MR = 1,NCX
          DO NC = 1,NCY
              KNT = 0
              CRV(MR,NC) = 0.0D0
              DO MN = 1,MRT
                  IF (X(MN,MR).NE.XYMSG .AND. Y(MN,NC).NE.XYMSG) THEN
                      CRV(MR,NC) = CRV(MR,NC) + X(MN,MR)*Y(MN,NC)
                      KNT = KNT + 1
                  END IF
              END DO
              IF (KNT.GT.0) THEN
                  CRV(MR,NC) = CRV(MR,NC)/DBLE(KNT)
              END IF
          END DO
      END DO

      IF (IPRNT.EQ.1) CALL DSVDPAR(CRV,NCX,NCX,NCY,'SVDLAPACK: CRV$')

c lapack svd

      LWORK = MAX(3*NSVMX+MAX(NCX,NCY),5*MIN(NCX,NCY))

      CALL DGESVD('S','S',NCX,NCY,CRV,NCX,SV,U,NCX,VT,NSVMX,WORK,LWORK,
     +            MERR)

      TOL = 10.D0*DEPSMACH(0)
      CALL DSVDINFO(SV,NSVMX,NSV,TOL,FNORM,IRANK,CONDN,PCVAR)
      WORK(1) = FNORM
      WORK(2) = CONDN
      WORK(3) = IRANK
      WORK(4) = MERR

      IF (IPRNT.EQ.1) THEN
          NSVX = MIN(IRANK,NCY,NCX)
          WRITE (*,FMT='(/,''SVDLAPACK:  NSVMX,NSV,NSVX='',3i7)') NSVMX,
     +      NSV,NSVX
          WRITE (*,FMT=
     +'(/,''SVDLAPACK:  FNORM,CONDN,IRANK,NSV,MERR=''
     +     , 2f15.3,4i8)') FNORM,CONDN,IRANK,NSV,MERR
          CALL DSVDPVC(SV,NSV,'SVDLAPACK: Singular values$')
          CALL DSVDPVC(PCVAR,NSV,'SVDLAPACK: Percent Var Explained$')
          CALL DSVDPAR(U,NCX,NCX,NSVX,'SVDLAPACK: Left  SV [U]$')
          CALL DSVDPAR(VT,NSVX,NSVX,NCY,'SVDLAPACK: Right SV [VT]$')
      END IF

c calculate the expansion coef
c .   switch the subscripts to simulate the tranpose

      DO MR = 1,MRT
          DO K = 1,NSVMX
              AK(MR,K) = 0.0D0
              DO NC = 1,NCX
                  IF (X(MR,NC).NE.XYMSG) THEN
                      AK(MR,K) = AK(MR,K) + X(MR,NC)*U(NC,K)
                  END IF
              END DO
          END DO
      END DO

      DO MR = 1,MRT
          DO K = 1,NSVMX
              BK(MR,K) = 0.0D0
              DO NC = 1,NCY
                  IF (Y(MR,NC).NE.XYMSG) THEN
                      BK(MR,K) = BK(MR,K) + Y(MR,NC)*VT(K,NC)
                  END IF
              END DO
          END DO
      END DO
      IF (IPRNT.EQ.1) THEN
          CALL DSVDPAR(AK,MRT,MRT,NSV,'SVDLAPACK: Exp Coef: [AK]$')
          CALL DSVDPAR(BK,MRT,MRT,NSV,'SVDLAPACK: Exp Coef: [BK]$')
      END IF

c left and right homogeneous and heterogeneous correlations
c .   this could be done sleeker/faster but this is clear
c .   e.g., calculate the mean of ak, bk, x, y, then enter loops

      DO K = 1,NSV
          DO NC = 1,NCX
              AMEAN = XYMSG
              ASD = XYMSG
              XMEAN = XYMSG
              XSD = XYMSG
              CALL DESCROS(AK(1,K),X(1,NC),MRT,XYMSG,XYMSG,AMEAN,XMEAN,
     +                    ASD,XSD,0,CCV,HOMLFT(NC,K),IER)
              BMEAN = XYMSG
              BSD = XYMSG
              CALL DESCROS(BK(1,K),X(1,NC),MRT,XYMSG,XYMSG,BMEAN,XMEAN,
     +                    BSD,XSD,0,CCV,HETLFT(NC,K),IER)
          END DO

          DO NC = 1,NCY
              AMEAN = XYMSG
              ASD = XYMSG
              YMEAN = XYMSG
              YSD = XYMSG
              CALL DESCROS(AK(1,K),Y(1,NC),MRT,XYMSG,XYMSG,AMEAN,YMEAN,
     +                    ASD,YSD,0,CCV,HETRGT(NC,K),IER)
              BMEAN = XYMSG
              BSD = XYMSG
              CALL DESCROS(BK(1,K),Y(1,NC),MRT,XYMSG,XYMSG,BMEAN,YMEAN,
     +                    BSD,YSD,0,CCV,HOMRGT(NC,K),IER)
          END DO
      END DO

      IF (IPRNT.EQ.1) THEN
          CALL DSVDPAR(HOMLFT,NCX,NCX,NSV,'SVDLAPACK: [HOMLFT]$')
          CALL DSVDPAR(HETLFT,NCX,NCX,NSV,'SVDLAPACK: [HETLFT]$')
          CALL DSVDPAR(HOMRGT,NCY,NCY,NSV,'SVDLAPACK: [HOMRGT]$')
          CALL DSVDPAR(HETRGT,NCY,NCY,NSV,'SVDLAPACK: [HETRGT]$')
      END IF

      RETURN
      END

      SUBROUTINE DSVDSV(X,Y,LDXY,MRT,NCX,NCY,NSV,IFLAG,XYMSG,IPRNT,SVL,
     +                  SVR,PCVAR,CRV,U,VT,SV,NSVMX,WORK,LWORK,IER)
      INTEGER LDXY,MRT,NCX,NCY,NSVMX,IFLAG,IPRNT,LWORK,IER
      DOUBLE PRECISION CRV(NCX,NCY)
      DOUBLE PRECISION U(NCX,NSVMX)
      DOUBLE PRECISION VT(NSVMX,NCY)
      DOUBLE PRECISION WORK(LWORK)
      DOUBLE PRECISION SV(NSVMX)


C NCL: svl = new ( (/nsvd,ncy/), float)   ; left  sing vactor
C NCL: svr = new ( (/nsvd,ncx/), float)   ; right sing vector
C NCL: pcv = svdcov (x,y,nsvd,svl,svr)

c perform a singular value decomposition on the data
c .   return the left and right singular vectors

c Nomenclature:

c .   x,y      - input data arrays (see "iflag" argument)
c .   ldxy     - leading dimensions of x/y in the calling program
C*PL*ERROR* Comment line too long
c .   mrt      - actual number of rows used. This refers to the time dimension
c .              mrt <= ldxy
c .   ncx,ncy  - number of columns for each array. This refers to the
c .              number of grid points or stations being used.
c .   nsv      - desired number of singular values or modes
c .              generally: nsv <= 3; must be nsv <= min(ncx,ncy)
c .   iflag    - flag to indicate if the data should be standardized
c .              if iflag=1 the input data arrays will be REPLACED with
c .              the standardized data.
c .   xymsg    - missing code
c .   iprnt    - print out many of the steps (for debugging)
c .   svl/r    - left and right singular vectors
C*PL*ERROR* Comment line too long
c .   pcvar    - percent variance explained by the modes (dimensioned nsv)
c .   ier      - error code

c Note: in many cases ldxy=mrt (Max Rows allocated for Time dimension)


      DOUBLE PRECISION X(LDXY,NCX),Y(LDXY,NCY),XYMSG

      DOUBLE PRECISION SVL(NCX,NSV),SVR(NCY,NSV),PCVAR(NSV)
C NCLEND

      IER = 0
      IF (MRT.GT.LDXY) IER = IER + 1
      IF (NCX.LT.1 .OR. NCY.LT.1 .OR. MRT.LT.1) IER = IER + 10

      CALL DSVDLAPSV(X,Y,LDXY,MRT,NCX,NCY,IFLAG,XYMSG,IPRNT,PCVAR,NSVMX,
     +               NSV,CRV,U,VT,SV,WORK)

      DO NS = 1,NSV
c          PRINT *,' LOOP: ns=',NS
          DO NC = 1,NCX
c              PRINT *,' SVL LOOP: nc=',NC
              SVL(NC,NS) = U(NC,NS)
          END DO
c          PRINT *,'AFTER  dsvdlapsv: SVL'

          DO NC = 1,NCY
c              PRINT *,' SVR LOOP: nc=',NC
              SVR(NC,NS) = VT(NS,NC)
          END DO
c          PRINT *,'AFTER  dsvdlapsv: SVR'

      END DO

      RETURN
      END
c -------------------------------------------------------------
      SUBROUTINE DSVDLAPSV(X,Y,LDXY,MRT,NCX,NCY,IFLAG,XYMSG,IPRNT,PCVAR,
     +                     NSVMX,NSV,CRV,U,VT,SV,WORK)
      DOUBLE PRECISION XBAR
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION XDEV
      DOUBLE PRECISION YBAR
      DOUBLE PRECISION YVAR
      DOUBLE PRECISION YDEV
      DOUBLE PRECISION T
      DOUBLE PRECISION TOL
      DOUBLE PRECISION DEPSMACH
      DOUBLE PRECISION FNORM
      DOUBLE PRECISION CONDN

c see description of SVD_LAPACK for a description

c nomenclature for new variables:
c .   crv      - cross covariance matrix between the standardized
c .              x and y data arrays.
c .   u        - left singular vector
c .   vt       - transpose right singular vector
c .   sv       - singular values
c .   work     - required by the lapack routine

      INTEGER LDXY,MRT,NCX,NCY,IFLAG,IPRNT,NSVMX,NSV

      DOUBLE PRECISION X(LDXY,NCX),Y(LDXY,NCY),XYMSG

      DOUBLE PRECISION PCVAR(NSV)

      DOUBLE PRECISION U(NCX,NSVMX),VT(NSVMX,NCY),CRV(NCX,NCY),
     +                 SV(NSVMX),WORK(*)

      CHARACTER LABEL*80

c check for any columns (stations/grid points) that
c .   contain all missing data.

c if (iflag.eq.1) standardize the data
c .   1) compute time mean and st dev at each location (grid pt)
c .   2) subtract mean from each observation
c .   3) divide by the st dev

      DO NC = 1,NCX
          CALL DSTAT2(X(1,NC),MRT,XYMSG,XBAR,XVAR,XDEV,NXUSE,IER)
          IF (XDEV.EQ.XYMSG .OR. XDEV.EQ.0.D0) THEN
              WRITE (*,FMT=
     +'(''SVD: Warning all X values ''
     +  ''in column are missing or are constant'')')
          ELSE
              IF (IFLAG.EQ.1) THEN
                  DO MR = 1,MRT
                      IF (X(MR,NC).NE.XYMSG) THEN
                          X(MR,NC) = (X(MR,NC)-XBAR)/XDEV
                      END IF
                  END DO
              END IF
          END IF
      END DO

      DO NC = 1,NCY
          CALL DSTAT2(Y(1,NC),MRT,XYMSG,YBAR,YVAR,YDEV,NYUSE,IER)
          IF (YDEV.EQ.XYMSG .OR. YDEV.EQ.0.D0) THEN
              WRITE (*,FMT=
     +'(''SVD: Warning all Y values ''
     +  ''in column are missing or are constant'')')
          ELSE
              IF (IFLAG.EQ.1) THEN
                  DO MR = 1,MRT
                      IF (Y(MR,NC).NE.XYMSG) THEN
                          Y(MR,NC) = (Y(MR,NC)-YBAR)/YDEV
                      END IF
                  END DO
              END IF
          END IF
      END DO

      IF (IPRNT.GT.1) THEN
          LABEL = '$'
          IF (IFLAG.EQ.1) LABEL = 'after standardization$'

          LLAB = INDEX(LABEL,'$')
          CALL DSVDPAR(X,LDXY,MRT,NCX,'SVD_LAPACK: X '//LABEL(1:LLAB))
          CALL DSVDPAR(Y,LDXY,MRT,NCY,'SVD_PACK: Y '//LABEL(1:LLAB))
      END IF

c cross-covariance [xt]*[y]/T    [ dimension (ncx,ncy)]
c .   note the subscripts of "x" have been reversed
c .   to "simulate" the transpose

      T = DBLE(MRT)
      DO MR = 1,NCX
          DO NC = 1,NCY
              KNT = 0
              CRV(MR,NC) = 0.0D0
              DO MN = 1,MRT
                  IF (X(MN,MR).NE.XYMSG .AND. Y(MN,NC).NE.XYMSG) THEN
                      CRV(MR,NC) = CRV(MR,NC) + X(MN,MR)*Y(MN,NC)
                      KNT = KNT + 1
                  END IF
              END DO
              IF (KNT.GT.0) THEN
                  CRV(MR,NC) = CRV(MR,NC)/DBLE(KNT)
              END IF
          END DO
      END DO

      IF (IPRNT.GT.1) CALL DSVDPAR(CRV,NCX,NCX,NCY,'SVD_LAPACK: CRV$')

c lapack svd

      LWORK = MAX(3*NSVMX+MAX(NCX,NCY),5*MIN(NCX,NCY))

      CALL DGESVD('S','S',NCX,NCY,CRV,NCX,SV,U,NCX,VT,NSVMX,WORK,LWORK,
     +            MERR)

      TOL = 10.D0*DEPSMACH(0)
      CALL DSVDINFO(SV,NSVMX,NSV,TOL,FNORM,IRANK,CONDN,PCVAR)
      WORK(1) = FNORM
      WORK(2) = CONDN
      WORK(3) = IRANK
      WORK(4) = MERR

      IF (IPRNT.EQ.1) THEN
          NSVX = MIN(IRANK,NCY,NCX)
          WRITE (*,FMT='(/,''SVD_LAPACK_SV:  NSVMX,NSV,NSVX='',3i7)')
     +      NSVMX,NSV,NSVX
          WRITE (*,FMT=
     +'(/,''SVD_LAPACK_SV:  FNORM,CONDN,IRANK,NSV,MERR=''
     +     , 2(1x,e15.7),3i8)') FNORM,CONDN,IRANK,NSV,MERR
          CALL DSVDPVC(SV,NSV,'SVD_LAPACK_SV: Singular values$')
          CALL DSVDPVC(PCVAR,NSV,'SVD_LAPACK_SV: PercentVar Explained$ '
     +                 )
c         call dsvdpar (u ,ncx,ncx,nsvx,'SVD_LAPACK_SV: Left  SV [U]$')
C*PL*ERROR* Comment line too long
c         call dsvdpar (vt,nsvx,nsvx,ncy,'SVD_LAPACK_SV: Right SV [VT]$')
      END IF

      RETURN
      END
