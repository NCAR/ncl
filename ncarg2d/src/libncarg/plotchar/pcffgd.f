C
C $Id: pcffgd.f,v 1.6 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCFFGD (IPSS,NASC,CHGT,SIZE,RDGU,LDGU,NDGU)
C
C Get the digitization of the character whose ASCII decimal equivalent
C is NASC and store in RGDU.
C
      include 'pcffme.h'
      include 'pcffdx.h'
      include 'pcffsp.h'
C
      DIMENSION RDGU(LDGU)
      DIMENSION TWINO(4),TVPTO(4)
C
      IF (NASC.GT.CHREND .OR. NASC.LT.CHRSTR) THEN
        NDGU = 0
        GO TO 90
      ENDIF
C
C Get the byte pointers delimiting the digitization for the character.
C
      IADDR1 = CHRPNT(NASC-CHRSTR+1)
      IF (NASC .EQ. CHREND) THEN
        IADDR2 = LSTPNT
      ELSE
        IADDR2 = CHRPNT(NASC-CHRSTR+2)
      ENDIF
      IADDL = IADDR2-IADDR1
C
C Get the character description from the binary fontcap.
C
      NUMPKT = (8*IADDL)/(PKFLWD+XBITWD+YBITWD)
      IF (NUMPKT .GT. ICLEN) THEN
        NDGU = 0  
        GO TO 90
      ENDIF
      CALL PCFFGP(NUMPKT,8*IADDR1,SFLGS,IXC,IYC)
C
C  Create the digitization.
C
      I = 1
   80 CONTINUE
       IF (SFLGS(I) .EQ. 6) THEN
C
C Begin character, get width.
C
         CWIDTH = REAL(IYC(I))
C
C  Subtract off the biases from the coordinate points; translate the
C  font coordinate system to the PLOTCHAR one; scale to the desired
C  height.
C
         XOFF = -0.5*CWIDTH
         YOFF = REAL(FBASE-FHALF)
         DO 20 IC=1,NUMPKT
           IF (SFLGS(IC).EQ.0 .OR. SFLGS(IC).EQ.4) THEN
             XC(IC) = (REAL((IXC(IC)-XBIAS))+XOFF)*SCALE
             YC(IC) = (REAL((IYC(IC)-YBIAS))+YOFF)*SCALE
           ENDIF
   20    CONTINUE
C
C  Set the left and right concatenation points.
C
         JNDX = 1
         RDGU(JNDX) = XOFF*SCALE
         JNDX = JNDX+1
         RDGU(JNDX) = -XOFF*SCALE
         IF (IPSS .EQ. 1) THEN
           NDGU = JNDX
           GO TO 90
         ENDIF
C
C  Define normalization transformation 1 to be consistent with the 
C  Bezier fidelity parameter and select normalization transformation 1.
C
         CALL GQCNTN(IER,NTRO)
         CALL GQNT(1,IER,TWINO,TVPTO)
         YMAX = REAL(FHALF-FBASE)*SCALE/SIZE
         YMIN = -YMAX
         XMAX = YMAX
         XMIN = YMIN
         CALL GSWN(1,XMIN,XMAX,YMIN,YMAX)
         CALL GSVP(1,0.,1.,0.,1.)
         CALL GSELNT(1)
       ELSE IF (SFLGS(I) .EQ. 1) THEN
C
C End a region (or a polyline if an outline font is requested).
C
         IF (OUTLIN .NE. 0) THEN
           JNDX = JNDX+1
           RDGU(JNDX) = -2048.  
           JNDX = JNDX+1
           RDGU(JNDX) = 0.
         ELSE
           JNDX = JNDX+1
           RDGU(JNDX) = -2047.  
           JNDX = JNDX+1
           RDGU(JNDX) = 0.
         ENDIF
         NDGU = JNDX
         IF (NDGU .GT. LDGU) CALL SETER
     +         ('PCFFGD - INTERNAL LOGIC ERROR - SEE CONSULTANT',1,2)
       ELSE IF (SFLGS(I) .EQ. 7) THEN
C
C End a polyline.
C
         JNDX = JNDX+1
         RDGU(JNDX) = -2048.  
         JNDX = JNDX+1
         RDGU(JNDX) = 0.
         NDGU = JNDX
         IF (NDGU .GT. LDGU) CALL SETER
     +         ('PCFFGD - INTERNAL LOGIC ERROR - SEE CONSULTANT',1,2)
       ELSE IF (SFLGS(I) .EQ. 2) THEN
C
C Add the points for a hole to the path, or draw the hole if an
C outline font is requested.
C
         XORIG = RDGU(JNDX-1)
         YORIG = RDGU(JNDX)
         IF (OUTLIN .EQ. 1) THEN
C
C Draw the current line.
C
           JNDX = JNDX+1
           RDGU(JNDX) = -2048.
           JNDX = JNDX+1
           RDGU(JNDX) = 0.
         ENDIF 
         CALL PCFFHL (I,JNDX,XORIG,YORIG,NCO,JNDXO,RDGU(1))
         JNDX = JNDXO
         I = NCO
       ELSE IF (SFLGS(I) .EQ. 0) THEN
C
C Add the point to the path.
C
         JNDX = JNDX+1
         RDGU(JNDX) = XC(I)
         JNDX = JNDX+1
         RDGU(JNDX) = YC(I)
       ELSE IF (SFLGS(I) .EQ. 4) THEN
C
C  Process the Bezier curve.
C
         BCNTLX(1) = RDGU(JNDX-1)
         BCNTLY(1) = RDGU(JNDX)
         DO 30 IB=2,4
           BCNTLX(IB) = XC(I+IB-2)
           BCNTLY(IB) = YC(I+IB-2)
   30    CONTINUE
C
         CALL BCCURV(BCNTLX,BCNTLY,IBZL,BZXC,BZYC,NBP)
C
C  Add the interpolated points to the curve (start at 2 since the
C  original point is already in the curve).
C
         DO 60 K=2,NBP
           JNDX = JNDX+1
           RDGU(JNDX) = BZXC(K) 
           JNDX = JNDX+1
           RDGU(JNDX) = BZYC(K) 
   60    CONTINUE
         I = I+2
       ELSE IF (SFLGS(I) .EQ. 5) THEN
C
C  Re-establish normalization transformation 1 and select the
C  normalization transformation that was in effect at the beginning.
C
         CALL GSWN(1,TWINO(1),TWINO(2),TWINO(3),TWINO(4))
         CALL GSVP(1,TVPTO(1),TVPTO(2),TVPTO(3),TVPTO(4))
         CALL GSELNT(NTRO)
         GO TO 90
       ELSE
         PRINT * , 'PCFFGD -- Invalid fontcap encoding encountered.'
         STOP
       ENDIF
C
      IF (I .GE. NUMPKT) THEN
        GO TO 90
      ELSE
        I = I+1
        GO TO 80
      ENDIF
C
   90 CONTINUE
C
      RETURN
      END
