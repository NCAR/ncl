c ----------------------------------------------------------------------
      SUBROUTINE DSTAT2(X,NPTS,XMSG,XMEAN,XVAR,XSD,NPTUSED,IER)
      DOUBLE PRECISION X1
      DOUBLE PRECISION X2
      DOUBLE PRECISION XN
      DOUBLE PRECISION X3

c this routine will calculate estimates of the first two  moments
c .   of the vector x containing missing data.

c input arguments:
c .   x        - input vector
c .   npts     - length of x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.

c output arguments:
c .   xmean    - mean of x (first moment)
c .   xvar     - sample variance
c .   xsd      - sqrt( xvar )
c .   nptused  - no. of points used to calculate the estimates
c .   ier      - if (ier.ne.0) an error has occurred

c note :
c .   uncalculated quantities are set to xmsg (nptused set to zero)

      INTEGER NPTS,NPTUSED,IER
      DOUBLE PRECISION X(1:NPTS),XMSG,XMEAN,XVAR,XSD

      XMEAN = XMSG
      XVAR = XMSG
      XSD = XMSG
      NPTUSED = 0

      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF

      IER = 0
      X1 = 0.D0
      X2 = 0.D0
      XN = 0.D0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG) THEN
              XN = XN + 1.0D0
              X1 = X1 + X(N)
              X2 = X2 + X(N)*X(N)
          END IF
      END DO

      NPTUSED = XN

      IF ((XN-1.D0).GT.0.D0) THEN
          X3 = X1*X1/XN
c
c prevent possible roundoff
c
          XVAR = DMAX1((X2-X3)/ (XN-1.D0),0.D0)
          XSD = SQRT(XVAR)
          XMEAN = X1/XN
      ELSE IF ((XN-1.D0).EQ.0.D0) THEN
          XMEAN = X1
          XVAR = 0.0D0
          XSD = 0.0D0
      ELSE
c
c error code for all msg values
c
          IER = 2
      END IF

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DSTAT2T(X,NPTS,XMSG,XMEANT,XVART,XSDT,NPTUSED,WORK,
     +                  PTRIM,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XMEANT
      DOUBLE PRECISION XVART
      DOUBLE PRECISION XSDT
      DOUBLE PRECISION PTRIM

c this routine will calculate trimmed estimates of the first
c .   two moments of the vector x containing missing data.

c input arguments:
c .   x        - input vector
c .   npts     - length of x  (npts.ge.5 if pctrim.gt.0.)
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   work     - work vector of length npts
c .              upon return work contains x in ascending order
c .   ptrim    - portion of the series to be trimmed (0.<= pctrim <1.)
c .              if (pctrim.eq.0.) then
c .                  calculate whole series mean and st dev
c .              else
c .                  a minimum of two pts will be trimmed
c .                  a maximum of npts-2 will be trimmed
c .              endif
c .
c .              note: if u want to be very conservative and only
c .                    trim the min and max values then set
c .                    ptrim to some very small value (e.g. 1.e-36)
c .
c .              ex: npts=45, ptrim=0.50 then
c .                  0.5*45=22 pts will be trimmed (11 on each end)

c output arguments:
c .   xmeant   - mean of trimmed series x
c .   xvart    - sample trimmed variance
c .   xsdt     - sqrt( xvart ): st deviation of trimmed series
c .   nptused  - no. of points used to calculate the trimmed estimates
c .   ier      - if (ier.ne.0) an error has occurred

c note : uncalculated quantities are set to xmsg (nptused set to zero)

      DOUBLE PRECISION X(1:NPTS),WORK(1:NPTS)

      IER = 0
      XMEANT = XMSG
      XVART = XMSG
      XSDT = XMSG
      NPTUSED = 0

      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF

      IF (PTRIM.EQ.0.D0) THEN
          CALL DSTAT2(X,NPTS,XMSG,XMEANT,XVART,XSDT,NPTUSED,IER)
          RETURN
      ELSE IF (PTRIM.EQ.1.D0) THEN
c
c cannot trim whole series
c
          IER = 2
          RETURN
      END IF

c sort x into numerically ascending order
c .   first transfer the input array to the work array
c .   excluding missing pts

      MPTS = 0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG) THEN
              MPTS = MPTS + 1
              WORK(MPTS) = X(N)
          END IF
      END DO

      IF (MPTS.LT.1) THEN
c
c series contains all msg values
c
          IER = 3
          RETURN
      ELSE IF (MPTS.LT.NPTS) THEN
c
c fill the rest of work with msg code
c
          DO M = MPTS + 1,NPTS
              WORK(M) = XMSG
          END DO
      END IF
c
c sort series in ascending order
c
      CALL DSORTU(WORK,MPTS)
c
c no. of values trimmed from one side
c
      MTRIM = MAX0(INT(DBLE(MPTS)*0.5D0*PTRIM),1)


      IF ((MPTS-2*MTRIM).GT.0) THEN
          CALL DSTAT2(WORK(MTRIM+1),MPTS-2*MTRIM,XMSG,XMEANT,XVART,XSDT,
     +               NPTUSED,IER)
      ELSE
c
c not enough trimmed values
c
          IER = 4
      END IF

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DSTAT4(X,NPTS,XMSG,XMEAN,XVAR,XSD,XSKEW,XKURT,NPTUSED,
     +                 IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION XSD
      DOUBLE PRECISION XSKEW
      DOUBLE PRECISION XKURT
      DOUBLE PRECISION X1
      DOUBLE PRECISION X2
      DOUBLE PRECISION XN
      DOUBLE PRECISION X3
      DOUBLE PRECISION X4

c this routine will calculate estimates of the first four moments
c .   of the vector x containing missing data.

c input arguments:
c .   x        - input vector
c .   npts     - length of x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.

c output arguments:
c .   xmean    - mean of x (first moment)
c .   xvar     - sample variance of x (second moment)
c .   xsd      - sqrt( xvar )
c .   xskew    - coef. of skewness (third moment)
c .              departure from symmetry. if skew>0 [skew<0] the
c .              distribution trails off to the right [left] .
c .   xkurt    - coef. of kurtosis (fourth moment)
c .              the normal distribution has a kurtosis of 3 . this
c .              value is subtracted from the calculated kurtosis.
c .              thus neg values are possible and the returned value
c .              is kurtosis relative to the normal distribution.
c .              if (kurt-3) > 0 [<0] it is usually more sharply peaked
c .              [flatter] than the normal distribution. (leptokurtic
c .              and platykurtic , respectively.) e.g. , a rectangular
c.               function has a kurtosis of 1.8 or 1.8-3 = -1.2 relative
c.               to the normal distribution.
c .   nptused  - no. of points used to calculate the estimates
c .   ier      - if (ier.ne.0) an error has occurred

c note :
c .   uncalculated quantities are set to xmsg (nptused set to zero)

      DOUBLE PRECISION X(1:NPTS)

      XMEAN = XMSG
      XVAR = XMSG
      XSD = XMSG
      XSKEW = XMSG
      XKURT = XMSG
      NPTUSED = 0


      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF

      IER = 0
      X1 = 0.D0
      X2 = 0.D0
      XN = 0.D0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG) THEN
              XN = XN + 1.0D0
              X1 = X1 + X(N)
              X2 = X2 + X(N)*X(N)
          END IF
      END DO

      NPTUSED = XN

      IF ((XN-1.D0).GT.0.D0) THEN
          X3 = X1*X1/XN
c
c prevent possible roundoff
c
          XVAR = DMAX1((X2-X3)/ (XN-1.D0),0.D0)
          XSD = SQRT(XVAR)
          XMEAN = X1/XN

          IF (XVAR.GT.0.D0) THEN
              X1 = 0.D0
              X2 = 0.D0
              X3 = 0.D0
              X4 = 0.D0
              DO N = 1,NPTS
                  IF (X(N).NE.XMSG) THEN
                      X4 = X(N) - XMEAN
                      X1 = X4**3
                      X2 = X2 + X1
                      X3 = X3 + X1*X4
                  END IF
              END DO
              XSKEW = (X2/SQRT(XVAR)**3)/XN
              XKURT = (X3/ (XVAR*XVAR))/XN - 3.D0
          END IF
      ELSE IF ((XN-1.D0).EQ.0.D0) THEN
          XMEAN = X1
          XVAR = 0.0D0
          XSD = 0.0D0
      ELSE
c
c error code for all msg values
c
          IER = 2
          RETURN
      END IF

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DMEDMRNG(X,WORK,NPTS,XMSG,XMEDIAN,XMRANGE,XRANGE,
     +                   NPTUSED,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XMEDIAN
      DOUBLE PRECISION XMRANGE
      DOUBLE PRECISION XRANGE

c this routine will find the median, range and mid-range
c .   of the input  vector x containing missing data.

c arguments :
c .   x        - input vector (series)
c .   work     - work vector of length npts. upon output
c .              work(1) thru work(nptused) are in numerically ascending
c .              order. work(nptused+1) thru work(npts) will be filled
c .              the missing value code.
c .   npts     - length of x
c .   xmsg     - missing code: if no msg values set to some number
c .                            which will not be encountered.
c .   xmedian  - median value of vector x
c .   xmrange  - midrange
c .   xrange   - range of x
c .   nptused  - no. of points used in computing various quantities
c .   ier      - if (ier.ne.0) an error has occurred

c note :
c .   uncalculated quantities are set to xmsg (nptused set to zero)

      DOUBLE PRECISION X(1:NPTS),WORK(1:NPTS)

      XMEDIAN = XMSG
      XMRANGE = XMSG
      XRANGE = XMSG
      NPTUSED = 0

      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF
      IER = 0

c sort x into numerically ascending order
c .   first transfer the input array to the work array
c .   excluding missing pts

      MPTS = 0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG) THEN
              MPTS = MPTS + 1
              WORK(MPTS) = X(N)
          END IF
      END DO

      IF (MPTS.LT.1) THEN
c
c series contains all msg values
c
          IER = 2
          RETURN
      END IF

      CALL DSORTU(WORK,MPTS)

      IF (MPTS.LT.NPTS) THEN
c
c fill the rest of work with msg code
c
          DO N = MPTS + 1,NPTS
              WORK(N) = XMSG
          END DO
      END IF

      XMRANGE = 0.5D0* (WORK(MPTS)+WORK(1))
      XRANGE = WORK(MPTS) - WORK(1)

      IF (DMOD(DBLE(MPTS),2.D0).EQ.0.D0) THEN
          XMEDIAN = 0.5D0* (WORK(MPTS/2)+WORK(MPTS/2+1))
      ELSE
          XMEDIAN = WORK((MPTS+1)/2)
      END IF

c
c for nomenclature compatibility
c
      NPTUSED = MPTS

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DESAUTO(X,NPTS,XMSG,XMEAN,XVAR,MXLAG,ACV,ACR,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION XSD
      DOUBLE PRECISION X1
      DOUBLE PRECISION XN

c this routine will estimate the autocovariances and autocorrelations
c .   for the input vector x with missing data. the normalizing factor
c .   in the denominator (xvar) is kept constant.

c arguments :
c .   x        - input vector
c .   npts     - length of vector x
c .   xmsg     - missing code: if no msg values set to some number
c .                            which will not be encountered.
c .   xmean    - mean of the input vector x
c .   xvar     - sample variance  of the input vector x
c .              note: if xmean and/or xvar = xmsg this routine will
c .                    calculate the quantities and return them
c .   mxlag    - max lag to be estimated  [0 <= mxlag <= npts ]
c .   acv      - sample autocovariances  [vector of length .ge.
c .                                      (mxlag+1) ]
c .   acr      - sample autocorrelations [vector of length .ge. 
c .                                       (mxlag+1) ]

c .              acv(0) = xvar (lag 0  )  : acr(0)   = 1.00 ( lag 0  )
c .              acv(1) =      (lag 1  )  : acr(1)   =      ( lag 1  )
c .                         .                              .
c .                         .                              .
c .              acv(mxlag) = (lag mxlag): acr(mxlag) = ( lag mxlag)

c .   ier      - if (ier.ne.0) an error has occurred

c note :  uncalculated quantities are set to xmsg

      DOUBLE PRECISION X(1:NPTS),ACV(0:MXLAG),ACR(0:MXLAG)

      IER = 0
      IF (NPTS.LT.2) IER = 1
      IF (MXLAG.LT.0 .OR. MXLAG.GT.NPTS) IER = 2
      IF (XVAR.LE.0.D0 .AND. XVAR.NE.XMSG) IER = 3

      DO LAG = 0,MAX0(0,MIN0(MXLAG,NPTS))
         ACV(LAG) = XMSG
         ACR(LAG) = XMSG
      END DO
      IF (IER.NE.0) RETURN


      IF (XMEAN.EQ.XMSG .OR. XVAR.EQ.XMSG) THEN
          CALL DSTAT2(X,NPTS,XMSG,XMEAN,XVAR,XSD,NPTUSED,JER)
          IF (JER.NE.0) THEN
              IER = -JER
          ELSE IF (XVAR.EQ.0.D0) THEN
              IER = -5
          END IF
      END IF
      IF (IER.NE.0) RETURN

      ACV(0) = XVAR
      ACR(0) = 1.00D0

c
c i hope somebody wants more than this
c
      IF (MXLAG.EQ.0) RETURN

      DO LAG = 1,MXLAG
          X1 = 0.D0
          XN = 0.D0
          DO N = 1,NPTS - LAG
              IF (X(N).NE.XMSG .AND. X(N+LAG).NE.XMSG) THEN
                  X1 = X1 + ((X(N+LAG)-XMEAN)* (X(N)-XMEAN))
                  XN = XN + 1.D0
              END IF
          END DO
          IF (XN.GE.2.D0) THEN
              ACV(LAG) = X1/ (XN-1.D0)
              ACR(LAG) = ACV(LAG)/XVAR
          ELSE
              ACV(LAG) = XMSG
              ACR(LAG) = XMSG
          END IF
      END DO

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DESCROS(X,Y,NPTS,XMSG,YMSG,XMEAN,YMEAN,XSD,YSD,MXLAG,
     +                  CCV,CCR,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION YMSG
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION YMEAN
      DOUBLE PRECISION XSD
      DOUBLE PRECISION YSD
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION YVAR
      DOUBLE PRECISION XSDYSD
      DOUBLE PRECISION XYN
      DOUBLE PRECISION XY1


c this routine will estimate the crosscovariances and crosscorrelations
c .   for the input series x and y. missing data is allowed.

c .   to get the (+) and (-) ccv and ccr two calls must be made. the
c .   first with the calling sequence above and the second with the
c .   x and y series reversed (also the other arguments) . note
c .   that  the second ccv(0) and ccr(0) are redundant.

c arguments :
c .   x,y      - input vectors with which the calculations are to take
c .              place.
c .   npts     - length of vectors x and y
c .   xmsg,ymsg- missing code: if no msg values set to some number
c .                            which will not be encountered.
c .              xmsg will be used to fill missing values
c .   xmean    - mean of the x vector (if xmean=xmsg: program will
c .              calculate)
c .   ymean    - mean of the y vector (if ymean=ymsg: program will
c .              calculate)
c .   xsd      - st. dev. of x vector (if xsd  =xmsg: program will
c .              calculate)
c .   ysd      - st. dev. of y vector (if ysd  =ymsg: program will
c .              calculate)
c .   mxlag    - max lag crosscovariance/correlation to be estimated
c .   ccv      - vector of length .ge. (mxlag+1) containing sample
c .              cross cov
c .   ccr      - vector of length .ge. (mxlag+1) containing sample
c .              cross cor

c .                ccv(0) = xyvar (lag 0  )  : ccr(0) =  ( lag 0   )
c .                ccv(1) =       (lag 1  )  : ccr(1) =  ( lag 1   )
c .                           .                               .
c .                           .                               .
c .                ccv(mxlag) =  (lag mxlag): ccr(mxlag) = ( lag mxlag)

c .   ier      - if (ier.ne.0) an error has occurred

c note : uncalculated quantities are set to xmsg

      DOUBLE PRECISION X(1:NPTS),Y(1:NPTS),CCV(0:MXLAG),CCR(0:MXLAG)

      IER = 0
      IF (NPTS.LT.2) IER = 1
      IF (MXLAG.LT.0 .OR. MXLAG.GT.NPTS) IER = 2
      IF ((XSD.NE.XMSG.AND.XSD.LE.0.D0) .OR.
     +    (YSD.NE.YMSG.AND.YSD.LE.0.D0)) IER = 3
      DO LAG = 0,MAX0(0,MIN0(MXLAG,NPTS))
          CCV(LAG) = XMSG
          CCR(LAG) = XMSG
      END DO
      IF (IER.NE.0) RETURN

      IF (XMEAN.EQ.XMSG .OR. XSD.EQ.XMSG) THEN
          CALL DSTAT2(X,NPTS,XMSG,XMEAN,XVAR,XSD,NPTUSED,JER)
          IF (JER.NE.0) THEN
              IER = -JER
          ELSE IF (XSD.EQ.0.D0) THEN
c
c x must be a series of constant values
c
              IER = -5
          ELSE IF (NPTUSED.EQ.0) THEN
c
c x must be a series of missing values
c
              IER = -6
          END IF
          IF (IER.NE.0) RETURN
      END IF

      IF (YMEAN.EQ.YMSG .OR. YSD.EQ.YMSG) THEN
          CALL DSTAT2(Y,NPTS,YMSG,YMEAN,YVAR,YSD,NPTUSED,JER)
          IF (JER.NE.0) THEN
              IER = - (JER+100)
          ELSE IF (YSD.EQ.0.D0) THEN
c
c y must be a series of constant values
c
              IER = -105
          ELSE IF (NPTUSED.EQ.0) THEN
c
c y must be a series of missing values
c
              IER = -106
          END IF
          IF (IER.NE.0) RETURN
      END IF

      XSDYSD = 1.D0/ (XSD*YSD)

      DO LAG = 0,MXLAG
          XYN = 0.D0
          XY1 = 0.D0
          DO N = 1,NPTS - LAG
              IF (X(N).NE.XMSG .AND. Y(N+LAG).NE.YMSG) THEN
                  XY1 = XY1 + ((Y(N+LAG)-YMEAN)* (X(N)-XMEAN))
                  XYN = XYN + 1.D0
              END IF
          END DO
          IF (XYN.GE.2.D0) THEN
              CCV(LAG) = XY1/ (XYN-1.D0)
              CCR(LAG) = CCV(LAG)*XSDYSD
          ELSE
              CCV(LAG) = XMSG
              CCR(LAG) = XMSG
          END IF
      END DO

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DRMVAVE(X,NPTS,XMSG,XMEAN,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XMEAN

c this routine will remove the mean of vector x from each pt.
c .   missing pts are excepted.

c arguments :
c .   x        - input vector
c .   npts     - length of x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   xmean    - mean of the input vector (x)
c .   ier      - if (ier.ne.0) an error has occurred

      DOUBLE PRECISION X(1:NPTS)

      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      ELSE
          IER = 0
      END IF

      DO N = 1,NPTS
          IF (X(N).NE.XMSG) X(N) = X(N) - XMEAN
      END DO

      RETURN
      END
c ----------------------------------------------------------
      SUBROUTINE DSTNDX(X,NPTS,XMSG,XMEAN,XSD,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XSD

c standardize the vector x

c arguments :
c .   x        - vector to be standardized
c .   npts     - length of vector x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   xmean    - mean of the input vector
c .   xsd      - standard deviation of the series
c .   ier      - if (ier.ne.0) an error has ocurred

      DOUBLE PRECISION X(1:NPTS)

      IER = 0
      IF (NPTS.LT.1) THEN
          IER = 1
      ELSE IF (XSD.LE.0.D0) THEN
          IER = 2
      END IF
      IF (IER.NE.0) RETURN

      DO N = 1,NPTS
          IF (X(N).NE.XMSG) X(N) = (X(N)-XMEAN)/XSD
      END DO

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DNMDATA(X,NPTS,XMSG,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XMAX

c normalize the vector x by the absolute value of its largest element

c arguments :
c .   x        - series (vector) to be normalized
c .   npts     - length of x
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   ier      - if (ier.ne.0) an error has occurred

      DOUBLE PRECISION X(1:NPTS)

      IER = 0
      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF

c find the first non-missing pt

      DO N = 1,NPTS
          IF (X(N).NE.XMSG) THEN
              NSTRT = N
              GO TO 20
          END IF
      END DO

c
c must be all msg values
c
      IER = 1
      RETURN

   20 XMAX = ABS(X(NSTRT))
      DO N = NSTRT,NPTS
          IF (X(N).NE.XMSG) XMAX = DMAX1(ABS(X(N)),XMAX)
      END DO

      IF (XMAX.NE.0.D0) THEN
          DO N = NSTRT,NPTS
              IF (X(N).NE.XMSG) X(N) = X(N)/XMAX
          END DO
      ELSE
c
c must be all zeros
c
          IER = 2
      END IF

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DERRCHK(X,NPTS,XMEAN,XSD,XMSG,XBAND,LOCERR,NPTER,IER)
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XSD
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XBAND
      DOUBLE PRECISION XWIDTH

c this routine will check a series of data for potential gross errors
c .   (i.e., wild points). the series will be scanned and the locations
c .   of possibly bad points are stored. this routine
c .   assumes that the series x is normally distributed.

c input arguments:
c .   x        - series (vector) to be checked
c .   npts     - length of x
c .   xmean    - series mean (... or median is the user chooses)
c .   xsd      - series standard deviation
c .   xmsg     - missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.
c .   xband    - standard deviation width to be allowed
c .              (e.g., xband=3.0; check values beyond 99%)

c output arguments:
c .   locerr   - integer vector which will contain the subscript
c .              location of potential gross errors. for example: if
c .              series x has two pts more than xband from xmean and
c .              these pts are located at x(17) and x(204) then
c .              locerr(1) = 17 and locerr(2) = 204.
c .   npter    - no. of pts in locerr. in the above example it would be
c .              two. if a value of zero is returned no gross errors
c .              have been detected.
c .   ier      - if (ier.ne.0) an error has occurred

      DOUBLE PRECISION X(1:NPTS)
      INTEGER LOCERR(1:NPTS)

      IER = 0
      IF (NPTS.LT.1) THEN
          IER = 1
      ELSE IF (XSD.EQ.0.D0) THEN
          IER = 2
      END IF
      IF (IER.NE.0) RETURN

      NN = 0
      NPTER = 0
      XWIDTH = XBAND*XSD
      DO N = 1,NPTS
          IF (X(N).NE.XMSG .AND. ABS(X(N)-XMEAN).GT.XWIDTH) THEN
c
c must be a potential gross error
c
              NPTER = NPTER + 1
              LOCERR(NPTER) = N
          END IF
      END DO

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DDOTPR(X,Y,NPTS,XMSG,YMSG,ANS,NPTUSED,IER)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION YMSG
      DOUBLE PRECISION ANS

c compute the dot product for series containing msg values

c arguments :
c .   x,y       - input series
c .   npts      - length of x and y
c .   xmsg,ymsg - missing code: if there are no msg values
c .                             set xmsg,ymsg to some value which will
c .                             not be encountered.
c .   ans       - answer (dot product)
c .   nptused   - number of points used to compute the dot product
c .   ier       - if (ier.ne.0) an error has occurred

      DIMENSION X(1:NPTS),Y(1:NPTS)

      IER = 0
      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF

      ANS = 0.D0
      NPTUSED = 0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG .AND. Y(N).NE.YMSG) THEN
              ANS = ANS + X(N)*Y(N)
              NPTUSED = NPTUSED + 1
          END IF
      END DO

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DVCVMNS(X,NROW,NCOL,NRT,NCS,XMSG,VCM,LVCM,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XN
      DOUBLE PRECISION XCA
      DOUBLE PRECISION XCB
      DOUBLE PRECISION XCACB

c this routine will calculate the variance-covariance matrix (vcm)
c .   of the array x containing missing data. obviously if x does
c .   contain missing data then vcm is only an approximation.

c note : conventional storage is utilized for vcm

c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt@nrow : ncs@ncol)
c     xmsg     - missing data code (if none set to some no. not
c                encountered)
c     vcm      - var-cov matrix
c     lvcm     - not used in this routine
c     ier      - error code (if ier=-1 then vcm contains missing entry)

      DOUBLE PRECISION X(1:NROW,1:NCOL),VCM(1:NCOL,1:NCOL)

      IER = 0

c calculate the var-cov between columns (stations)

      DO NCA = 1,NCS
          DO NCB = NCA,NCS
              XN = 0.D0
              XCA = 0.D0
              XCB = 0.D0
              XCACB = 0.D0
              DO I = 1,NRT
                  IF (X(I,NCA).NE.XMSG .AND. X(I,NCB).NE.XMSG) THEN
                      XN = XN + 1.D0
                      XCA = XCA + X(I,NCA)
                      XCB = XCB + X(I,NCB)
                      XCACB = XCACB + X(I,NCA)*X(I,NCB)
                  END IF
c
c end "nrt"
c
              END DO
              IF (XN.GE.2.D0) THEN
                  VCM(NCA,NCB) = (XCACB- (XCA*XCB)/ (XN))/ (XN-1.D0)
                  VCM(NCB,NCA) = VCM(NCA,NCB)
              ELSE
                  IER = -1
                  VCM(NCA,NCB) = XMSG
                  VCM(NCB,NCA) = XMSG
              END IF
c
c end "nrt"
c
          END DO
c
c end "ncs"
c
      END DO

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DVCMSSM(X,NROW,NCOL,NRT,NCS,XMSG,VCM,LVCM,IER)
      DOUBLE PRECISION X
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION VCM
      DOUBLE PRECISION XN
      DOUBLE PRECISION XCA
      DOUBLE PRECISION XCB
      DOUBLE PRECISION XCACB

c this routine will calculate the variance-couariance matrix (vcm)
c .   of the array x containing missing data. obviously if x does
c .   contain missing data then vcm is only an approximation.

c note : symmetric storage mode is utilized for vcm to save space.

c input:
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt <= nrow : ncs <= ncol)
c     xmsg     - missing data code (if none set to some no. not
c                encountered)
c output:
c     vcm      - var-cov matrix
c     lvcm     - length of vcm
c     ier      - error code (if ier=-1 then vcm contains missing entry)

      DIMENSION X(1:NROW,1:NCOL),VCM(*)

      IER = 0
      IF (NROW.LT.1 .OR. NCOL.LT.1) IER = IER + 1
      IF (NRT.LT.1 .OR. NCS.LT.1) IER = IER + 10
      IF (IER.NE.0) RETURN

      LVCM = NCS* (NCS+1)/2

c calculate the var-cov between columns (stations)

      NN = 0
      DO NCA = 1,NCS
          DO NCB = 1,NCA
              XN = 0.D0
              XCA = 0.D0
              XCB = 0.D0
              XCACB = 0.D0
              NN = NN + 1
              DO I = 1,NRT
                  IF (X(I,NCA).NE.XMSG .AND. X(I,NCB).NE.XMSG) THEN
                      XN = XN + 1.D0
                      XCA = XCA + X(I,NCA)
                      XCB = XCB + X(I,NCB)
                      XCACB = XCACB + (X(I,NCA))* (X(I,NCB))
                  END IF
              END DO
              IF (XN.GE.2.D0) THEN
                 VCM(NN) = (XCACB-(XCA*XCB)/XN)/ (XN-1.D0)
              ELSEIF (XN.EQ.1.) THEN
                 VCM(NN) = (XCACB-(XCA*XCB)/XN)
              ELSE
                 IER = -1
                 VCM(NN) = XMSG
              END IF
          END DO
      END DO

      RETURN
      END
c
c ----------------------------------------------------------------------
      SUBROUTINE DCORMNS(X,NROW,NCOL,NRT,NCS,XMSG,CRM,LCRM,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XN
      DOUBLE PRECISION XCA
      DOUBLE PRECISION XCB
      DOUBLE PRECISION XCA2
      DOUBLE PRECISION XCB2
      DOUBLE PRECISION XCACB
      DOUBLE PRECISION XVARA
      DOUBLE PRECISION XVARB

c this routine will calculate the correlation matrix   (crm)
c .   of the array x containing missing data. obviously if x does
c .   contain missing data then crm is only an approximation.

c note : conventional storage

c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt@nrow : ncs@ncol)
c     xmsg     - missing data code (if none set to some no. not
c                encountered)
c     crm      - correlation matrix  [full]
c     lcrm     - dummy [not used in this routine]
c     ier      - error code (if ier=-1 then crm contains missing entry)

      DOUBLE PRECISION X(1:NROW,1:NCOL),CRM(1:NCOL,1:NCOL)

      IER = 0
      LCRM = NCS* (NCS+1)/2

c calculate the var-cov between columns (stations)
c .   then standardize

      DO NCA = 1,NCS
          DO NCB = 1,NCA
              XN = 0.D0
              XCA = 0.D0
              XCB = 0.D0
              XCA2 = 0.D0
              XCB2 = 0.D0
              XCACB = 0.D0
              DO I = 1,NRT
                  IF (X(I,NCA).NE.XMSG .AND. X(I,NCB).NE.XMSG) THEN
                      XN = XN + 1.D0
                      XCA = XCA + X(I,NCA)
                      XCB = XCB + X(I,NCB)
                      XCA2 = XCA2 + X(I,NCA)*X(I,NCA)
                      XCB2 = XCB2 + X(I,NCB)*X(I,NCB)
                      XCACB = XCACB + X(I,NCA)*X(I,NCB)
                  END IF
              END DO
              IF (XN.GE.2.D0) THEN
                  XVARA = (XCA2- ((XCA*XCA)/ (XN)))/ (XN-1.D0)
                  XVARB = (XCB2- ((XCB*XCB)/ (XN)))/ (XN-1.D0)
                  IF (XVARA.GT.0.D0 .AND. XVARB.GT.0.D0) THEN
                      CRM(NCB,NCA) = (XCACB- ((XCA*XCB)/ (XN)))/
     +                               (XN-1.D0)
                      CRM(NCB,NCA) = CRM(NCB,NCA)/
     +                               (SQRT(XVARA)*SQRT(XVARB))
                      CRM(NCA,NCB) = CRM(NCB,NCA)
                  ELSE
                      IER = -1
                      CRM(NCB,NCA) = XMSG
                      CRM(NCA,NCB) = XMSG
                  END IF
              ELSE
                  IER = -1
                  CRM(NCB,NCA) = XMSG
                  CRM(NCA,NCB) = XMSG
              END IF
          END DO
      END DO

      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DCRMSSM(X,NROW,NCOL,NRT,NCS,XMSG,CRM,LCRM,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION XN
      DOUBLE PRECISION XCA
      DOUBLE PRECISION XCB
      DOUBLE PRECISION XCA2
      DOUBLE PRECISION XCB2
      DOUBLE PRECISION XCACB
      DOUBLE PRECISION XVARA
      DOUBLE PRECISION XVARB

c this routine will calculate the correlation matrix   (crm)
c .   of the array x containing missing data. obviously if x does
c .   contain missing data then crm is only an approximation.

c note : symmetric storage mode is utilized for crm to save space.

c input:
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt <= nrow : ncs <= ncol)
c     xmsg     - missing data code (if none set to some no. not
c                encountered)
c output:
c     crm      - correlation matrix
c     lcrm     - length of crm
c     ier      - error code (if ier=-1 then crm contains missing entry)

      DOUBLE PRECISION X(1:NROW,1:NCOL),CRM(*)

      IER = 0
      IF (NROW.LT.1 .OR. NCOL.LT.1) IER = IER + 1
      IF (NRT.LT.1 .OR. NCS.LT.1) IER = IER + 10
      IF (IER.NE.0) RETURN

      LCRM = NCS* (NCS+1)/2

c calculate the var-cov between columns (stations)

      NN = 0
      DO NCA = 1,NCS
          DO NCB = 1,NCA
              XN = 0.D0
              XCA = 0.D0
              XCB = 0.D0
              XCA2 = 0.D0
              XCB2 = 0.D0
              XCACB = 0.D0
              NN = NN + 1
              DO I = 1,NRT
                  IF (X(I,NCA).NE.XMSG .AND. X(I,NCB).NE.XMSG) THEN
                      XN = XN + 1.D0
                      XCA = XCA + X(I,NCA)
                      XCB = XCB + X(I,NCB)
                      XCA2 = XCA2 + X(I,NCA)*X(I,NCA)
                      XCB2 = XCB2 + X(I,NCB)*X(I,NCB)
                      XCACB = XCACB + X(I,NCA)*X(I,NCB)
                  END IF
              END DO
              IF (XN.GE.2.D0) THEN
                 XVARA = (XCA2- ((XCA*XCA)/ (XN)))/ (XN-1.D0)
                 XVARB = (XCB2- ((XCB*XCB)/ (XN)))/ (XN-1.D0)
                 IF (XVARA.GT.0.D0 .AND. XVARB.GT.0.D0) THEN
                    CRM(NN) = (XCACB- ((XCA*XCB)/ (XN)))/ (XN-1.D0)
                    CRM(NN) = CRM(NN)/ (SQRT(XVARA)*SQRT(XVARB))
                 ELSE
                    IER = -1
                    CRM(NN) = XMSG
                 END IF
              ELSEIF (XN.EQ.1.D0) THEN
                 XVARA   =  ( XCA2-((XCA*XCA)/(XN)) )
                 XVARB   =  ( XCB2-((XCB*XCB)/(XN)) )
                 IF (XVARA.GT.0.D0 .AND. XVARB.GT.0.D0) THEN
                    CRM(NN) = ( XCACB-((XCA*XCB)/(XN)) )
                    CRM(NN) = CRM(NN)/(SQRT(XVARA)*SQRT(XVARB))
                 ELSE
                    IER     = -1
                    CRM(NN) = XMSG
                 ENDIF
              ELSE
                 IER = -1
                 CRM(NN) = XMSG
              END IF
          END DO
      END DO

      RETURN
      END

c ----------------------------------------------------------------------
      SUBROUTINE DCCORC(Y,X,NROW,NCOL,NRT,NCS,XMSG,CORC,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION YMEAN
      DOUBLE PRECISION YVAR
      DOUBLE PRECISION YSD
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION XSD
      DOUBLE PRECISION XN
      DOUBLE PRECISION XY
      DOUBLE PRECISION COVAR

c this routine will calculate the correlation coef between a
c .   vector (station) y and a matrix x containing observations from
c .   many different stations

c     y        - vector (station data) with which data matrix x is to
c                be correlated
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt@nrow : ncs@ncol)
c     xmsg     - missing data code (if none set to some no. not
c                encountered)
c     corc     - correlation coef. vector
c                corc(1) contain correlation coef between y and x(nrt,1)
c                corc(2) contain correlation coef between y and 
c                    x(nrt,2) etc.
c     ier      - error code (if ier=-1 then corc contains missing entry)

      DOUBLE PRECISION X(1:NROW,1:NCOL),Y(*),CORC(*)

c calculate the mean and var of y

      CALL DSTAT2(Y,NRT,XMSG,YMEAN,YVAR,YSD,NPTUSY,IER)
      IF (IER.NE.0) RETURN
      IF (YSD.EQ.0.D0) THEN
          DO NC = 1,NCS
              CORC(NC) = XMSG
          END DO
          IER = 201
          RETURN
      END IF

c calculate the cross correlation coef
c .   calculate the mean and variance of column nc in the data array

      DO NC = 1,NCS
          CORC(NC) = XMSG
          CALL DSTAT2(X(1,NC),NRT,XMSG,XMEAN,XVAR,XSD,NPTUSX,JER)
          IF (JER.NE.0) THEN
              IER = 205
              GO TO 40
          END IF
          IF (XSD.NE.0.D0) THEN
              XN = 0.D0
              XY = 0.D0
              DO N = 1,NRT
                  IF (Y(N).NE.XMSG .AND. X(N,NC).NE.XMSG) THEN
                      XN = XN + 1.D0
                      XY = XY + (Y(N)-YMEAN)* (X(N,NC)-XMEAN)
                  END IF
              END DO
              IF (XN.GT.2.D0) THEN
                  COVAR = XY/ (XN-1.D0)
                  CORC(NC) = COVAR/ (XSD*YSD)
              END IF
          END IF
   40     CONTINUE
      END DO

      RETURN
      END
c------------------------------------------------------------------
      SUBROUTINE DSSMIOX(X,NCS)

c output a symmetric storage mode matrix [x]

c note: format statement have to be changed

c .   x(1)
c .   x(2)  x(3)
c .   x(4)  x(5)  x(6)
c .   etc.

      DOUBLE PRECISION X(1:*)

      DATA IPR/6/

      NCEND = 0
      DO NC = 1,NCS
          NCSTRT = NCEND + 1
          NCEND = NCSTRT + NC - 1
          WRITE (IPR,FMT='(2x,i5,10(1x,f12.3),/, (7x,(10(1x,f12.3))))')
     +      NC, (X(N),N=NCSTRT,NCEND)
      END DO

      RETURN
      END
c ------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DEPSMACH(IPR)

c
c =1 print else no print
c
      INTEGER IPR
      DOUBLE PRECISION EPS

      EPS = 1.0D0
    1 EPS = 0.5D0*EPS
      IF ((1.0D0+EPS).GT.1.0D0) GO TO 1
      DEPSMACH = 2.0D0*EPS
      IF (IPR.EQ.1) WRITE (*,FMT='(''DEPSMACH: eps='',e15.7)') DEPSMACH

      RETURN
      END

c ----------------------------------------------------------------------
      SUBROUTINE DSORTU(A,LA)

c sort type "real"

      INTEGER LA
      DOUBLE PRECISION A(LA)

      INTEGER IU(21),IL(21),I,M,J,K,IJ,L
      DOUBLE PRECISION T,TT,R

      M = 1
      I = 1
      J = LA
      R = .375D0
      IF (LA.LE.0) RETURN
   10 IF (I.EQ.J) GO TO 55
   15 IF (R.GT..5898437D0) GO TO 20
      R = R + 3.90625D-2
      GO TO 25
   20 R = R - .21875D0
   25 K = I
c                          select a central element of the
c                          array and save it in location t
      IJ = I + (J-I)*R
      T = A(IJ)
c                          if first element of array is greater
c                          than t, interchange with t
      IF (A(I).LE.T) GO TO 30
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
   30 L = J
c                           if last element of array is less than
c                           t, interchange with t
      IF (A(J).GE.T) GO TO 40
      A(IJ) = A(J)
      A(J) = T
      T = A(IJ)
c                          if first element of array is greater
c                          than t, interchange with t
      IF (A(I).LE.T) GO TO 40
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
      GO TO 40
   35 IF (A(L).EQ.A(K)) GO TO 40
      TT = A(L)
      A(L) = A(K)
      A(K) = TT
c                           find an element in the second half of
c                           the array which is smaller than t
   40 L = L - 1
      IF (A(L).GT.T) GO TO 40
c                           find an element in the first half of
c                           the array which is greater than t
   45 K = K + 1
      IF (A(K).LT.T) GO TO 45
c                           interchange these elements
      IF (K.LE.L) GO TO 35
c                           save upper and lower subscripts of
c                           the array yet to be sorted
      IF (L-I.LE.J-K) GO TO 50
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 60
   50 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 60
c                           begin again on another portion of
c                           the unsorted array
   55 M = M - 1
      IF (M.EQ.0) RETURN
      I = IL(M)
      J = IU(M)
   60 IF (J-I.GE.11) GO TO 25
      IF (I.EQ.1) GO TO 10
      I = I - 1
   65 I = I + 1
      IF (I.EQ.J) GO TO 55
      T = A(I+1)
      IF (A(I).LE.T) GO TO 65
      K = I
   70 A(K+1) = A(K)
      K = K - 1
      IF (T.LT.A(K)) GO TO 70
      A(K+1) = T
      GO TO 65
      END
c ----------------------------------------------------------------------
      SUBROUTINE DISORTU(A,LA)

c sort type "integer"

      INTEGER LA
      INTEGER A(LA)

      INTEGER IU(21),IL(21),I,M,J,K,IJ,L
      INTEGER T,TT
      DOUBLE PRECISION R

      M = 1
      I = 1
      J = LA
      R = .375D0
      IF (LA.LE.0) RETURN
   10 IF (I.EQ.J) GO TO 55
   15 IF (R.GT..5898437D0) GO TO 20
      R = R + 3.90625D-2
      GO TO 25
   20 R = R - .21875D0
   25 K = I
c                                  select a central element of the
c                                  array and save it in location t
      IJ = I + (J-I)*R
      T = A(IJ)
c                                  if first element of array is greater
c                                  than t, interchange with t
      IF (A(I).LE.T) GO TO 30
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
   30 L = J
c                                  if last element of array is less than
c                                  t, interchange with t
      IF (A(J).GE.T) GO TO 40
      A(IJ) = A(J)
      A(J) = T
      T = A(IJ)
c                                  if first element of array is greater
c                                  than t, interchange with t
      IF (A(I).LE.T) GO TO 40
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
      GO TO 40
   35 IF (A(L).EQ.A(K)) GO TO 40
      TT = A(L)
      A(L) = A(K)
      A(K) = TT
c                                  find an element in the second half of
c                                  the array which is smaller than t
   40 L = L - 1
      IF (A(L).GT.T) GO TO 40
c                                  find an element in the first half of
c                                  the array which is greater than t
   45 K = K + 1
      IF (A(K).LT.T) GO TO 45
c                                  interchange these elements
      IF (K.LE.L) GO TO 35
c                                  save upper and lower subscripts of
c                                  the array yet to be sorted
      IF (L-I.LE.J-K) GO TO 50
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 60
   50 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 60
c                                  begin again on another portion of
c                                  the unsorted array
   55 M = M - 1
      IF (M.EQ.0) RETURN
      I = IL(M)
      J = IU(M)
   60 IF (J-I.GE.11) GO TO 25
      IF (I.EQ.1) GO TO 10
      I = I - 1
   65 I = I + 1
      IF (I.EQ.J) GO TO 55
      T = A(I+1)
      IF (A(I).LE.T) GO TO 65
      K = I
   70 A(K+1) = A(K)
      K = K - 1
      IF (T.LT.A(K)) GO TO 70
      A(K+1) = T
      GO TO 65
      END

      SUBROUTINE COLLAPSEXY(X,Y,NPTS,XMSG,YMSG,XX,YY,MPTS)
      IMPLICIT NONE
      INTEGER NPTS,MPTS
      DOUBLE PRECISION X(NPTS),Y(NPTS)
      DOUBLE PRECISION XX(NPTS),YY(NPTS)
      DOUBLE PRECISION XMSG,YMSG

      INTEGER N

      MPTS = 0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG .AND. Y(N).NE.YMSG) THEN
              MPTS = MPTS + 1
              XX(MPTS) = X(N)
              YY(MPTS) = Y(N)
c              PRINT *,'n, mpts, xx(mpts), yy(mpts)=',N,MPTS,XX(MPTS),
c     +          YY(MPTS)
          END IF
      END DO

      RETURN
      END
