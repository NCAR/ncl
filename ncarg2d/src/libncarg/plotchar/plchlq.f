C
C $Id: plchlq.f,v 1.3 1992-11-17 18:47:06 kennison Exp $
C
C
C-----------------------------------------------------------------------
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
C Define arrays in which to save the current viewport and window.
C
      DIMENSION VPRT(4),WNDW(4)
C
C Flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
C
C Compute the coordinates of (XPOS,YPOS) in the fractional coordinate
C system (normalized device coordinates).
C
      XFRA=CUFX(XPOS)
      YFRA=CUFY(YPOS)
C
C Save the current window and, if necessary, redefine it so that we can
C use normalized device coordinates.
C
      CALL GQCNTN (IERR,NRMT)
      IF (NRMT.NE.0) THEN
        CALL GQNT (NRMT,IERR,WNDW,VPRT)
        CALL GSWN (NRMT,VPRT(1),VPRT(2),VPRT(3),VPRT(4))
      END IF
C
C Save current character height, text path, character up vector, and
C text alignment.
C
      CALL GQCHH (IERR,HGTO)
      CALL GQTXP (IERR,ITPO)
      CALL GQCHUP (IERR,CUXO,CUYO)
      CALL GQTXAL (IERR,ITAX,ITAY)
C
C Determine the resolution of the plotter, as declared by default or
C by the user.
C
      CALL GETUSV ('XF',IRSX)
      RSLN=2.**IRSX-1.
C
C Determine a character height which will make the characters have the
C width requested by the user.  First, compute the same multiplier we
C would use for PLCHHQ (except for the adjustment factor SIZA) ...
C
      IF (SIZE.LE.0.) THEN
        SIZM=ABS(SIZE)/1023.
      ELSE IF (SIZE.LT.1.) THEN
        SIZM=SIZE/16.
      ELSE
        SIZM=(SIZE/RSLN)/16.
      END IF
C
C ... and then compute from that the desired character height.
C
      CHRH=16.*SIZM
C
C Set the character height.
C
      CALL GSCHH (CHRH)
C
C Set the text path.
C
      CALL GSTXP (0)
C
C Define the character up vector, being careful to generate exact values
C for exact multiples of 90 degrees.
C
      ANGM=MOD(ANGD,360.)
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
C Plot the characters.
C
      CALL GTX (XFRA,YFRA,CHRS)
C
C Update the pen position.
C
      CALL PLOTIF (XFRA,YFRA,0)
C
C Restore all original text attributes.
C
      CALL GSCHH  (HGTO)
      CALL GSTXP  (ITPO)
      CALL GSCHUP (CUXO,CUYO)
      CALL GSTXAL (ITAX,ITAY)
C
C Restore the window definition.
C
      IF (NRMT.NE.0) THEN
        CALL GSWN (NRMT,WNDW(1),WNDW(2),WNDW(3),WNDW(4))
      END IF
C
C Done.
C
      RETURN
C
      END
