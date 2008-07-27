C
C $Id: plchlq.f,v 1.12 2008-07-27 00:17:20 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PLCHLQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This is the low-quality character-drawing routine.
C
C
C D E C L A R A T I O N S
C
C
      CHARACTER*(*) CHRS
C
C The COMMON block PCPFLQ contains internal parameters that affect the
C behavior of routines besides PLCHHQ.
C
      COMMON /PCPFLQ/ IMAP,OORV,RHTW
      SAVE   /PCPFLQ/
C
C Define arrays in which to save the current viewport and window.
C
      DIMENSION VPRT(4),WNDW(4)
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL PCBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PLCHLQ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Compute the coordinates of (XPOS,YPOS) in the fractional coordinate
C system (normalized device coordinates XFRA and YFRA).
C
      IF (IMAP.LE.0) THEN
        XFRA=CUFX(XPOS)
        IF (ICFELL('PLCHLQ',2).NE.0) RETURN
        YFRA=CUFY(YPOS)
        IF (ICFELL('PLCHLQ',3).NE.0) RETURN
      ELSE
        CALL PCMPXY (IMAP,XPOS,YPOS,XTMP,YTMP)
        IF (ICFELL('PLCHLQ',4).NE.0) RETURN
        IF (OORV.NE.0..AND.XTMP.EQ.OORV) RETURN
        XFRA=CUFX(XTMP)
        IF (ICFELL('PLCHLQ',5).NE.0) RETURN
        YFRA=CUFY(YTMP)
        IF (ICFELL('PLCHLQ',6).NE.0) RETURN
      END IF
C
C Determine the resolution of the plotter, as declared by default or
C by the user.
C
      CALL GETUSV ('XF',IRSX)
      IF (ICFELL('PLCHLQ',7).NE.0) RETURN
      RSLN=2.**IRSX-1.
C
C Determine a character height which will make the characters have the
C width requested by the user (and decide at what angle to write them).
C First, compute the same multiplier we would use for PLCHHQ (except
C for the adjustment factor SIZA) ...
C
      IF (IMAP.LE.0) THEN
        IF (SIZE.LE.0.) THEN
          SIZM=ABS(SIZE)/1023.
        ELSE IF (SIZE.LT.1.) THEN
          SIZM=SIZE/16.
        ELSE
          SIZM=(SIZE/RSLN)/16.
        END IF
        ANGV=ANGD
      ELSE
        SINA=SIN(.017453292519943*ANGD)
        COSA=COS(.017453292519943*ANGD)
        CALL PCMPXY (IMAP,XPOS-.5*SIZE*COSA,YPOS-.5*SIZE*SINA,XTM1,YTM1)
        IF (ICFELL('PLCHLQ',8).NE.0) RETURN
        CALL PCMPXY (IMAP,XPOS+.5*SIZE*COSA,YPOS+.5*SIZE*SINA,XTM2,YTM2)
        IF (ICFELL('PLCHLQ',9).NE.0) RETURN
        IF (OORV.NE.0..AND.(XTM1.EQ.OORV.OR.XTM2.EQ.OORV)) RETURN
        XTM1=CUFX(XTM1)
        IF (ICFELL('PLCHLQ',10).NE.0) RETURN
        YTM1=CUFY(YTM1)
        IF (ICFELL('PLCHLQ',11).NE.0) RETURN
        XTM2=CUFX(XTM2)
        IF (ICFELL('PLCHLQ',12).NE.0) RETURN
        YTM2=CUFY(YTM2)
        IF (ICFELL('PLCHLQ',13).NE.0) RETURN
        IF (XTM1.EQ.XTM2.AND.YTM1.EQ.YTM2) RETURN
        SIZM=SQRT((XTM2-XTM1)*(XTM2-XTM1)+(YTM2-YTM1)*(YTM2-YTM1))/16.
        ANGV=57.2957795130823*ATAN2(YTM2-YTM1,XTM2-XTM1)
      END IF
C
C ... and then compute from that the desired character height.
C
      CHRH=16.*SIZM
C
C Save the current character height, text path, character up vector,
C and text alignment.
C
      CALL GQCHH (IERR,HGTO)
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHLQ - ERROR EXIT FROM GQCHH',14,1)
        RETURN
      END IF
C
      CALL GQTXP (IERR,ITPO)
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHLQ - ERROR EXIT FROM GQTXP',15,1)
        RETURN
      END IF
C
      CALL GQCHUP (IERR,CUXO,CUYO)
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHLQ - ERROR EXIT FROM GQCHUP',16,1)
        RETURN
      END IF
C
      CALL GQTXAL (IERR,ITAX,ITAY)
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHLQ - ERROR EXIT FROM GQTXAL',17,1)
        RETURN
      END IF
C
C Set the desired character height.
C
      CALL GSCHH (CHRH)
C
C Set the desired text path.
C
      CALL GSTXP (0)
C
C Define the character up vector, being careful to generate exact values
C for exact multiples of 90 degrees.
C
      ANGM=MOD(ANGV,360.)
C
      IF (ANGM.EQ.0.) THEN
        CALL GSCHUP (0.,1.)
      ELSE IF (ANGM.EQ.90.) THEN
        CALL GSCHUP (-1.,0.)
      ELSE IF (ANGM.EQ.180.) THEN
        CALL GSCHUP (0.,-1.)
      ELSE IF (ANGM.EQ.270.) THEN
        CALL GSCHUP (1.,0.)
      ELSE IF (ANGM.GT.0..AND.ANGM.LT.180.) THEN
        CALL GSCHUP (-1.,1./TAN(.017453292519943*ANGM))
      ELSE
        CALL GSCHUP (1.,-1./TAN(.017453292519943*ANGM))
      END IF
C
C Define the text alignment.
C
      IF (CNTR.LT.0.) THEN
        ICNT=1
      ELSE IF (CNTR.EQ.0.) THEN
        ICNT=2
      ELSE
        ICNT=3
      END IF
C
      CALL GSTXAL (ICNT,3)
C
C Flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('PLCHLQ',18).NE.0) RETURN
C
C Save the current window and, if necessary, redefine it so that we can
C use normalized device coordinates.
C
      CALL GQCNTN (IERR,NRMT)
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHLQ - ERROR EXIT FROM GQCNTN',19,1)
        RETURN
      END IF
      IF (NRMT.NE.0) THEN
        CALL GQNT (NRMT,IERR,WNDW,VPRT)
        IF (IERR.NE.0) THEN
          CALL SETER ('PLCHLQ - ERROR EXIT FROM GQNT',20,1)
          RETURN
        END IF
        CALL GSWN (NRMT,VPRT(1),VPRT(2),VPRT(3),VPRT(4))
      END IF
C
C Plot the characters.
C
      CALL GTX (XFRA,YFRA,CHRS)
C
C Restore the window definition.
C
      IF (NRMT.NE.0) THEN
        CALL GSWN (NRMT,WNDW(1),WNDW(2),WNDW(3),WNDW(4))
      END IF
C
C Restore all the original text attributes.
C
      CALL GSCHH  (HGTO)
      CALL GSTXP  (ITPO)
      CALL GSCHUP (CUXO,CUYO)
      CALL GSTXAL (ITAX,ITAY)
C
C Update the pen position.
C
      CALL PLOTIF (XFRA,YFRA,0)
      IF (ICFELL('PLCHLQ',21).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
