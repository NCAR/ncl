c --------------------------------------------------------------
C input
      SUBROUTINE DINDX77(X,Y,NMOS,NYRS,XMSG,IPRNT,WORK,LWORK,ZI,ZNI,IER)

c routine to call indx77x which actually does the calculations
c .   this is just a short calling list
c NOTE: indx77x can also be called directly
c .   DSJ: this is a variant of index routines

C number of "months" (eg: 12, 4, 1)
C number of years of data
C print flag (=0 mean do not print)
C length of work (>= 2*nmos*nyrs + 4*nmos + nyrs)
C error code
      INTEGER NMOS,NYRS,IPRNT,LWORK,IER

C monthly data from station/grid pt 1
C monthly data from station/grid pt 2
C missing code (if any)
C index
C noise index
C work array that will be partitioned
      DOUBLE PRECISION X(NMOS,NYRS),Y(NMOS,NYRS),XMSG,ZI(NMOS,NYRS),
     +                 ZNI(NMOS,NYRS),WORK(LWORK)

C local

C overall st dev of x anomalies
C overall st dev of y anomalies
      DOUBLE PRECISION XASTD,YASTD


      IER = 0
      IF (NMOS.LE.0 .OR. NYRS.LE.0) IER = IER + 1
      IF (LWORK.LT. (2*NMOS*NYRS+4*NMOS+NYRS)) IER = 19

c Check if either input arrays contains all missing values.
      IXMISS = 1
      IYMISS = 1
      DO NYR = 1,NYRS
          DO NMO = 1,NMOS
              IF (X(NMO,NYR).NE.XMSG) THEN
                  IXMISS = 0
              END IF
              IF (Y(NMO,NYR).NE.XMSG) THEN
                  IYMISS = 0
              END IF
          END DO
      END DO
      IF (IXMISS.EQ.1 .OR. IYMISS.EQ.1) THEN
          IER = 2
          RETURN
      END IF

      IF (IER.NE.0) THEN
          DO NYR = 1,NYRS
              DO NMO = 1,NMOS
                  ZI(NMO,NYR) = XMSG
                  ZNI(NMO,NYR) = XMSG
              END DO
          END DO

          RETURN
      END IF

C set up pointers (addresses in work array)

C beginning of "xa"   (see indx77x)
      I1 = 1
C beginning of "ya"   (see indx77x)
      I2 = I1 + NMOS*NYRS
C beginning of "xave" (see indx77x)
      I3 = I2 + NMOS*NYRS
C beginning of "yave" (see indx77x)
      I4 = I3 + NMOS
C beginning of "xstd" (see indx77x)
      I5 = I4 + NMOS
C beginning of "ystd" (see indx77x)
      I6 = I5 + NMOS
C beginning of "wyrs" (see indx77x)
      I7 = I6 + NMOS

C input
C input
C input
C output
      CALL DINDX77X(X,Y,NMOS,NYRS,XMSG,IPRNT,WORK(I1),WORK(I2),WORK(I3),
     +              WORK(I4),WORK(I5),WORK(I6),WORK(I7),XASTD,YASTD,ZI,
     +              ZNI,IER)

      RETURN
      END
c -----------------------------------------------------------
C input
C input/output
C output
      SUBROUTINE DINDX77X(X,Y,NMOS,NYRS,XMSG,IPRNT,XA,YA,XAVE,YAVE,XSTD,
     +                    YSTD,WYRS,XASTD,YASTD,ZI,ZNI,IER)
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION YVAR
      DOUBLE PRECISION XABAR
      DOUBLE PRECISION XAVAR
      DOUBLE PRECISION YABAR
      DOUBLE PRECISION YAVAR

c This is the most commonly used routine.

c given two series of year-month values (eg: slp) calculate
c .  an "index" (eg: Southern Oscillation Index)
c .  the way Kevin Trenberth suggests doing it.
c .  This means using the overall anomaly standard deviation
c .  to normalize the anomalies.

c index is computed as zi = x/sigma(x) - y/sigma(y) (normalized stuff)

c Trenberth (1984), "Signal versus Noise in the Southern Oscillation"
c .  Monthly Weather Review 112:326-332

c code written for clarity

C number of "months" (eg: 12, 4, 1)
C number of years of data
C print flag (=0 means do not print)
C error code
      INTEGER NMOS,NYRS,IPRNT,IER

C monthly data from station/grid pt 1
C monthly data from station/grid pt 2
C missing code (if any)
C index
C noise index
      DOUBLE PRECISION X(NMOS,NYRS),Y(NMOS,NYRS),XMSG,ZI(NMOS,NYRS),
     +                 ZNI(NMOS,NYRS)

C upon exit the following will contain

C monthly anomalies from average
C monthly averages
C monthly st dev (interannual variability)
C work array
C overall st dev of xa anomalies
C overall st dev of ya anomalies
      DOUBLE PRECISION XA(NMOS,NYRS),YA(NMOS,NYRS),XAVE(NMOS),
     +                 YAVE(NMOS),XSTD(NMOS),YSTD(NMOS),WYRS(NYRS),
     +                 XASTD,YASTD

      IER = 0
      IF (NMOS.LE.0 .OR. NYRS.LE.0) IER = IER + 1
      IF (IER.NE.0) RETURN

c initilize index to msg values

      DO NYR = 1,NYRS
          DO NMO = 1,NMOS
              ZI(NMO,NYR) = XMSG
              ZNI(NMO,NYR) = XMSG
          END DO
      END DO

c calculate each months long-term mean and standard deviation
c .   (long-term <==> climatological)

      DO NMO = 1,NMOS
          DO NYR = 1,NYRS
              WYRS(NYR) = X(NMO,NYR)
          END DO
          CALL DSTAT2(WYRS,NYRS,XMSG,XAVE(NMO),XVAR,XSTD(NMO),KNTX,IER)

          DO NYR = 1,NYRS
              WYRS(NYR) = Y(NMO,NYR)
          END DO
          CALL DSTAT2(WYRS,NYRS,XMSG,YAVE(NMO),YVAR,YSTD(NMO),KNTY,IER)
      END DO

c calculate anomalies from climatogical mean

      DO NYR = 1,NYRS
          DO NMO = 1,NMOS
              IF (X(NMO,NYR).NE.XMSG .AND. XAVE(NMO).NE.XMSG) THEN
                  XA(NMO,NYR) = X(NMO,NYR) - XAVE(NMO)
              ELSE
                  XA(NMO,NYR) = XMSG
              END IF
              IF (Y(NMO,NYR).NE.XMSG .AND. YAVE(NMO).NE.XMSG) THEN
                  YA(NMO,NYR) = Y(NMO,NYR) - YAVE(NMO)
              ELSE
                  YA(NMO,NYR) = XMSG
              END IF
          END DO
      END DO

c calculate the overall standard deviation of the anomalies
c .   (ie: use anomalies from all year-months together)

      CALL DSTAT2(XA,NMOS*NYRS,XMSG,XABAR,XAVAR,XASTD,KNTXA,IER)
      CALL DSTAT2(YA,NMOS*NYRS,XMSG,YABAR,YAVAR,YASTD,KNTYA,IER)

c calculate each index

      IF (XASTD.NE.XMSG .AND. XASTD.GT.0.0D0 .AND. YASTD.NE.XMSG .AND.
     +    YASTD.GT.0.0D0) THEN
          DO NYR = 1,NYRS
              DO NMO = 1,NMOS
                  IF (XA(NMO,NYR).NE.XMSG .AND.
     +                YA(NMO,NYR).NE.XMSG) THEN
                      ZI(NMO,NYR) = XA(NMO,NYR)/XASTD -
     +                              YA(NMO,NYR)/YASTD
                      ZNI(NMO,NYR) = XA(NMO,NYR)/XASTD +
     +                               YA(NMO,NYR)/YASTD
                  END IF
              END DO
          END DO
      ELSE
          IER = IER + 10
      END IF

      IF (IPRNT.NE.0) THEN
          DO NYR = 1,NYRS
              WRITE (*,FMT='('' SINDEX: '',i4,12(1x,f7.1))') NYR,
     +          (ZI(NMO,NYR),NMO=1,NMOS)
          END DO
          IF (IPRNT.LT.0) THEN
              DO NYR = 1,NYRS
                  WRITE (*,FMT='('' NINDEX: '',i4,12(1x,f7.1))') NYR,
     +              (ZNI(NMO,NYR),NMO=1,NMOS)
              END DO
          END IF
      END IF

      RETURN
      END
