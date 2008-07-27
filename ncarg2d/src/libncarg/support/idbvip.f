C
C $Id: idbvip.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDBVIP (MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,IWK,WK)
C
C DIMENSION OF            XD(NDP), YD(NDP), ZD(NDP), XI(NIP), YI(NIP)
C ARGUMENTS               ZI(NIP), IWK(31*NDP+NIP),  WK(8*NDP)
C
C PURPOSE                 To perform bivariate interpolation when the
C                         projections of the data points in the X-Y
C                         plane are irregularly distributed.
C
C USAGE                   CALL IDBVIP (MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,
C                                      IWK,WK)
C
C ARGUMENTS
C
C ON INPUT                MD
C                           Mode of computation (must be 1, 2, or 3,
C                           else an error return occurs.)
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
C                               previous call to IDBVIP, an error
C                               return occurs.
C                           = 3 if the values of NDP, NIP, XD, YD
C                               XI, and YI are unchanged from the
C                               previous call, i.e., if the only
C                               change on input to IDBVIP is in the
C                               ZD array.  If MD=3 and NDP or NIP has
C                               been changed since the previous call
C                               to IDBVIP, an error return occurs.
C
C                           Between the call with MD=2 or MD=3 and
C                           the preceding call, the IWK and WK work
C                           arrays should not be disturbed.
C
C                        NDP
C                          Number of data points (must be 4 or
C                          greater, else an error return occurs).
C
C                        XD
C                          Array of dimension NDP containing the
C                          X coordinates of the data points.
C
C                        YD
C                          Array of dimension NDP containing the
C                          Y coordinates of the data points.
C
C                        ZD
C                          Array of dimension NDP containing the
C                          Z coordinates of the data points.
C
C                        NIP
C                          The number of output points at which
C                          interpolation is to be performed (must be
C                          1 or greater, else an error return occurs).
C
C                        XI
C                          Array of dimension NIP containing the X
C                          coordinates of the output points.
C
C                        YI
C                          Array of dimension NIP containing the Y
C                          coordinates of the output points.
C
C                        IWK
C                          Integer work array of dimension at least
C                          31*NDP + NIP
C
C                        WK
C                          Real work array of dimension at least 8*NDP
C
C ON OUTPUT               ZI
C                           Array of dimension NIP where interpolated
C                           Z values are to be stored.
C
C SPECIAL CONDITIONS     Inadequate work space IWK and WK may cause
C                        incorrect results.
C
C                        The data points must be distinct and their
C                        projections in the X-Y plane must not be
C                        collinear, else an error return occurs.
C
C IDBVIP calls the subroutines IDLCTN, IDPDRV, IDPTIP, and IDTANG.
C
C Declaration statements.
C
      DIMENSION XD(NDP),YD(NDP),ZD(NDP),XI(NIP),YI(NIP),ZI(NIP),
     +          IWK(31*NDP+NIP),WK(8*NDP)
C
      COMMON /IDLC/ ITIPV,DMMY1(13)
      SAVE   /IDLC/
C
      COMMON /IDPT/ ITPV,DMMY(27)
      SAVE   /IDPT/
C
      COMMON /IDCOMN/ INTY,ITTY,ALSP,BLSP,CLSP,XAVG,YAVG
      SAVE   /IDCOMN/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('IDBVIP (BIVAR) - UNCLEARED PRIOR ERROR',1).NE.0)
     +                                                        RETURN
C
C Check for input errors.
C
      IF (MD.LT.1.OR.MD.GT.3) THEN
        CALL SETER ('IDBVIP (BIVAR) - INPUT VARIABLE MD IS OUT OF RANGE'
     +,2,1)
        RETURN
      END IF
C
      IF (NDP.LT.4) THEN
        CALL SETER ('IDBVIP (BIVAR) - INPUT VARIABLE NDP IS OUT OF RANGE
     +',3,1)
        RETURN
      END IF
C
      IF (NIP.LT.1) THEN
        CALL SETER ('IDBVIP (BIVAR) - INPUT VARIABLE NIP IS OUT OF RANGE
     +',4,1)
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
          CALL SETER  ('IDBVIP (BIVAR) - MD = 2 OR 3 BUT NDP WAS CHANGED
     + SINCE LAST CALL',5,1)
          RETURN
        END IF
C
      END IF
C
      IF (MD.LE.2) THEN
C
        IWK(2)=INTY
        IWK(3)=NIP
C
      ELSE
C
        INTYPV=IWK(2)
C
        IF (INTY.NE.INTYPV) THEN
          CALL SETER ('IDBVIP (BIVAR) - MD = 3 BUT ITY WAS CHANGED SINCE
     + LAST CALL',6,1)
           RETURN
        END IF
C
        NIPPV=IWK(3)
C
        IF (NIP.NE.NIPPV) THEN
          CALL SETER ('IDBVIP (BIVAR) - MD = 3 BUT NIP WAS CHANGED SINCE
     + LAST CALL',7,1)
          RETURN
        END IF
C
      END IF
C
C Allocate storage areas in the array IWK.
C
      JWIPT=16
      JWIWL=6*NDP+1
      JWIWK=JWIWL
      JWIPL=24*NDP+1
      JWIWP=30*NDP+1
      JWIT0=31*NDP
      JWWPD=5*NDP+1
C
C If MD = 1, triangulate the X-Y plane.  (If MD = 2 or 3, this has
C already been done and need not been redone.)
C
      IF (MD.EQ.1) THEN
        CALL IDTANG (NDP,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),IWK(JWIWL),
     +                                                  IWK(JWIWP),WK)
        IF (ICFELL('IDBVIP',8).NE.0) RETURN
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
C If MD = 1 or 2, locate all points at which interpolation is to be
C performed by getting the number of the containing region (a triangle
C number or a combined pair of border-line-segment numbers).  (If MD
C = 3, this has already been done and need not be redone.)
C
      IF (MD.LE.2) THEN
C
        ITIPV=0
C
        DO 101 IIP=1,NIP
          CALL IDLCTN(NDP,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),XI(IIP),
     +                         YI(IIP),IWK(JWIT0+IIP),IWK(JWIWK),WK)
  101   CONTINUE
C
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
C
      DO 102 IIP=1,NIP
        CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     +               IWK(JWIT0+IIP),XI(IIP),YI(IIP),ZI(IIP))
  102 CONTINUE
C
C Done.
C
      RETURN
C
      END
