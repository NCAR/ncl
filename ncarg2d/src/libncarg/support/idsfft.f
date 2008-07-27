C
C $Id: idsfft.f,v 1.6 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDSFFT (MD,NDP,XD,YD,ZD,NXI,NYI,NZI,XI,YI,ZI,IWK,WK)
C
C DIMENSION OF           XD(NDP), YD(NDP), ZD(NDP), XI(NXI),
C ARGUMENTS              YI(NYI), ZI(NZI,NYI), WK(6*NDP),
C                        IWK(31*NDP + NXI*NYI)
C
C PURPOSE                This subroutine performs smooth surface
C                        fitting when the projections of the data
C                        points in the X-Y plane are irregularly
C                        distributed in the plane.
C
C USAGE                  CALL IDSFFT (MD,NDP,XD,YD,ZD,NXI,NYI,NZI,
C                                     XI,YI,ZI,IWK,WK)
C
C ARGUMENTS
C
C ON INPUT               MD
C                          Mode of computation (must be 1, 2, or 3,
C                          else an error return will occur).
C                           = 1 if this is the first call to this
C                               subroutine, or if the value of NDP
C                               has been changed from the previous
C                               call, or if the contents of the XD
C                               or YD arrays have been changed from
C                               the previous call.
C                           = 2 if the values of NDP and the XD and
C                               YD arrays are unchanged from the
C                               previous call, but new values for
C                               XI and YI are being used.  If MD = 2
C                               and NDP has been changed since the
C                               previous call to IDSFFT, an error
C                               return occurs.
C                           = 3 if the values of NDP, NXI, NYI, XD,
C                               YD, XI, and YI are unchanged from the
C                               previous call, i.e., if the only change
C                               on input to IDSFFT is in the ZD array.
C                               If MD = 3 and NDP, NXI, or NYI has been
C                               changed since the previous call to
C                               IDSFFT, an error return occurs.
C
C                           Between the call with MD = 2 or MD = 3 and
C                           the preceding call, the IWK and WK work
C                           arrays should not be disturbed.
C
C                        NDP
C                          Number of data points (must be 4 or
C                          greater, else an error return will occur).
C
C                        XD
C                          Array of dimension NDP containing the X
C                          coordinates of the data points.
C
C                        YD
C                          Array of dimension NDP containing the Y
C                          coordinates of the data points.
C
C                        ZD
C                          Array of dimension NDP containing the Z
C                          coordinates of the data points.
C
C                        NXI
C                          Number of output grid points in the X
C                          direction (must be 1 or greater, else
C                          an error return will occur).
C
C                        NYI
C                          Number of output grid points in the Y
C                          direction (must be 1 or greater, else
C                          an error return will occur).
C
C                        NZI
C                          First dimension of ZI as declared in the
C                          calling program.  NZI must be greater than
C                          or equal to NXI, else an error return will
C                          occur.
C
C                        XI
C                          Array of dimension NXI containing the
C                          X coordinates of the output grid points.
C
C                        YI
C                         Array of dimension NYI containing the
C                         Y coordinates of the output grid points.
C
C                        IWK
C                          Integer work array of dimension at
C                          least 31*NDP + NXI*NYI.
C
C                        WK
C                          Real work array of dimension at least 6*NDP.
C
C ON OUTPUT              ZI
C                           Real, two-dimensional array of dimension
C                           (NZI,NYI), storing the interpolated Z
C                           values at the output grid points.
C
C SPECIAL CONDITIONS     Inadequate work space IWK and WK may cause
C                        incorrect results.
C
C                        The data points must be distinct and their
C                        projections in the X-Y plane must not be
C                        collinear, else an error return occurs.
C
C IDSFFT calls the subroutines IDGRID, IDPDRV, IDPTIP, and IDTANG.
C
C Declaration statements.
C
      DIMENSION XD(NDP),YD(NDP),ZD(NDP),XI(NXI),YI(NYI),ZI(NZI,NYI),
     +          IWK(31*NDP+NXI*NYI),WK(6*NDP)
C
      COMMON /IDPT/ ITPV,DMMY(27)
      SAVE   /IDPT/
C
      COMMON /IDCOMN/ INTY,ITTY,ALSP,BLSP,CLSP,XAVG,YAVG
      SAVE   /IDCOMN/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('IDSFFT (BIVAR) - UNCLEARED PRIOR ERROR',1).NE.0)
     +                                                        RETURN
C
C Check for input errors.
C
      IF (MD.LT.1.OR.MD.GT.3) THEN
        CALL SETER ('IDSFFT (BIVAR) - INPUT VARIABLE MD IS OUT OF RANGE'
     +,2,1)
        RETURN
      END IF
C
      IF (NDP.LT.4) THEN
        CALL SETER ('IDSFFT (BIVAR) - INPUT VARIABLE NDP IS OUT OF RANGE
     +',3,1)
        RETURN
      END IF
C
      IF (NXI.LT.1.OR.NYI.LT.1) THEN
        CALL SETER ('IDSFFT (BIVAR) - INPUT VARIABLE NXI OR NYI IS OUT O
     +F RANGE',4,1)
        RETURN
      END IF
C
      IF (NXI.GT.NZI) THEN
        CALL SETER ('IDSFFT (BIVAR) - INPUT VARIABLE NZI IS LESS THAN NX
     +I',5,1)
        RETURN
      END IF
C
      IF (MD.LE.1) THEN
C
        IWK(1)=NDP
C
      ELSE
C
        NDPPV=IWK(1)
C
        IF (NDP.NE.NDPPV) THEN
          CALL SETER  ('IDSFFT (BIVAR) - MD = 2 OR 3 BUT NDP WAS CHANGED
     + SINCE LAST CALL',6,1)
          RETURN
        END IF
C
      END IF
C
      IF (MD.LE.2) THEN
C
        IWK(2)=INTY
        IWK(3)=NXI
        IWK(4)=NYI
C
      ELSE
C
        INTYPV=IWK(2)
C
        IF (INTY.NE.INTYPV) THEN
          CALL SETER ('IDSFFT (BIVAR) - MD = 3 BUT ITY WAS CHANGED SINCE
     + LAST CALL',7,1)
           RETURN
        END IF
C
        NXIPV=IWK(3)
C
        IF (NXI.NE.NXIPV) THEN
          CALL SETER ('IDSFFT (BIVAR) - MD = 3 BUT NXI WAS CHANGED SINCE
     + LAST CALL',8,1)
           RETURN
        END IF
C
        NYIPV=IWK(4)
C
        IF (NYI.NE.NYIPV) THEN
          CALL SETER ('IDSFFT (BIVAR) - MD = 3 BUT NYI WAS CHANGED SINCE
     + LAST CALL',9,1)
          RETURN
        END IF
C
      END IF
C
C Allocate storage areas in the array IWK.
C
      JWIPT=16
      JWIWL=6*NDP+1
      JWNGP0=JWIWL-1
      JWIPL=24*NDP+1
      JWIWP=30*NDP+1
      JWIGP0=31*NDP
      JWWPD=5*NDP+1
C
C If MD = 1, triangulate the X/Y plane.  (If MD = 2 or 3, this has
C already been done and need not been redone.)
C
      IF (MD.EQ.1) THEN
        CALL IDTANG (NDP,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),IWK(JWIWL),
     +                                                  IWK(JWIWP),WK)
        IF (ICFELL('IDSFFT',10).NE.0) RETURN
        IWK(5)=NT
        IWK(6)=NL
      ELSE
        NT=IWK(5)
        NL=IWK(6)
      END IF
C
      IF (NT.EQ.0) RETURN
C
C If linear interpolation is activated, compute the coefficients of the
C least-squares-fit plane and the mean values of X and Y.
C
      IF (INTY.NE.0) THEN
        CALL IDLSQF (XD,YD,ZD,NDP,ALSP,BLSP,CLSP,XAVG,YAVG)
      END IF
C
C If MD = 1 or 2, sort output grid points in ascending order by the
C number of the containing region (a triangle number or a combined
C pair of border line segment numbers).  (If MD = 3, this has already
C been done and need not be redone.)
C
      IF (MD.LE.2) THEN
        CALL IDGRID (XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),NXI,
     +               NYI,XI,YI,IWK(JWNGP0+1),IWK(JWIGP0+1))
        IF (ICFELL('IDSFFT',11).NE.0) RETURN
      END IF
C
C If quintic interpolation is activated, estimate partial derivatives.
C
      IF (INTY.EQ.0) THEN
        CALL IDPDRV (NDP,XD,YD,ZD,NT,IWK(JWIPT),WK,WK(JWWPD))
      END IF
C
C Interpolate to get ZI values.
C
      ITPV=0
      JIG0MX=0
      JIG1MN=NXI*NYI+1
      NNGP=NT+2*NL
C
      DO 105 JNGP=1,NNGP
C
        ITI=JNGP
        IF (JNGP.LE.NT) GO TO 101
        IL1=(JNGP-NT+1)/2
        IL2=(JNGP-NT+2)/2
        IF (IL2.GT.NL) IL2=1
        ITI=IL1*(NT+NL)+IL2
C
  101   JWNGP=JWNGP0+JNGP
        NGP0=IWK(JWNGP)
        IF (NGP0.EQ.0) GO TO 103
        JIG0MN=JIG0MX+1
        JIG0MX=JIG0MX+NGP0
C
        DO 102 JIGP=JIG0MN,JIG0MX
          JWIGP=JWIGP0+JIGP
          IZI=IWK(JWIGP)
          IYI=(IZI-1)/NXI+1
          IXI=IZI-NXI*(IYI-1)
          CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     +                        ITI,XI(IXI),YI(IYI),ZI(IXI,IYI))
  102   CONTINUE
C
  103   JWNGP=JWNGP0+2*NNGP+1-JNGP
        NGP1=IWK(JWNGP)
        IF (NGP1.EQ.0) GO TO 105
        JIG1MX=JIG1MN-1
        JIG1MN=JIG1MN-NGP1
C
        DO 104 JIGP=JIG1MN,JIG1MX
          JWIGP=JWIGP0+JIGP
          IZI=IWK(JWIGP)
          IYI=(IZI-1)/NXI+1
          IXI=IZI-NXI*(IYI-1)
          CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     +                        ITI,XI(IXI),YI(IYI),ZI(IXI,IYI))
  104   CONTINUE
C
  105 CONTINUE
C
C Done.
C
        RETURN
C
      END
