c ----------------------------------------------------
      SUBROUTINE DSVDMTR(X,XT,MROW,MRX,NCX,IER)

      INTEGER MROW,MRX,NCX,IER
c
c x (mrx,ncx)
c xt(ncx,mrx)
c
      DOUBLE PRECISION X(MROW,NCX),XT(NCX,MRX)

      DO NC = 1,NCX
          DO MR = 1,MRX
              XT(NC,MR) = X(MR,NC)
          END DO
      END DO

      RETURN
      END
c ----------------------------------------------------
      SUBROUTINE DSVDINFO(SV,NSVMX,NSV,TOL,FNORM,IRANK,CONDN,PCVAR)

c
c input:  number of elements in sv
c
      INTEGER NSV,NSVMX
c
c input:  numerical tolerance
c
      DOUBLE PRECISION TOL
c
c input:  singular values
c
      DOUBLE PRECISION SV(NSVMX)

c
c         ! output: rank
c
      INTEGER IRANK
c
c output: Frobenius norm
c
      DOUBLE PRECISION FNORM
c
c condition number (stability)
c
      DOUBLE PRECISION CONDN
c
c output: pcvar
c
      DOUBLE PRECISION PCVAR(NSV)

      IRANK = 0
c
c Frobenius norm ?
c
      FNORM = 0.D0
      DO N = 1,NSVMX
          IF (ABS(SV(N)).GT.TOL) THEN
              IRANK = IRANK + 1
              FNORM = FNORM + SV(N)**2
          END IF
      END DO
c
c condition number
c assumes sv were in
c strict descending order
c
      CONDN = SV(1)/SV(IRANK)
      IF (FNORM.GT.0.D0) THEN
          DO N = 1,NSV
              PCVAR(N) = (SV(N)**2/FNORM)*100.D0
          END DO
      END IF

      RETURN
      END
c ----------------------------------------------------
      SUBROUTINE DSVDPAR(X,MROW,MRX,NCX,LABEL)

c print data from an array

      INTEGER MROW,MRX,NCX
      DOUBLE PRECISION X(MROW,NCX)
      CHARACTER*(*) LABEL

      LEN = INDEX(LABEL,'$') - 1

      IF(LEN.LE.0) THEN
         WRITE (*,*) 'svdpar: String must end in $'
      ELSE
         WRITE (*,FMT='(/,a)') LABEL(1:LEN)
         DO MR = 1,MRX
            WRITE (*,FMT='(i5,(12f8.4))') MR, (X(MR,NC),NC=1,NCX)
         END DO
      END IF

      RETURN
      END
c ----------------------------------------------------
      SUBROUTINE DSVDPVC(X,NPTS,LABEL)

c print data from a vector

      INTEGER NPTS
      DOUBLE PRECISION X(NPTS)
      CHARACTER*(*) LABEL

      LEN = INDEX(LABEL,'$') - 1
      WRITE (*,FMT='(/,a)') LABEL(1:LEN)

      DO N = 1,NPTS
          WRITE (*,FMT='(i5,f8.4)') N,X(N)
      END DO

      RETURN
      END
