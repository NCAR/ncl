C
C $Id: wtstr.f,v 1.7 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WTSTR (PX,PY,CH,IS,IO,IC)
C
C WTSTR is called to draw a character string in a specified position.
C
C PX and PY specify, in user coordinates, the position of a point
C relative to which a character string is to be positioned.
C
C CH is the character string to be written.
C
C IS is the desired size of the characters to be used, stated as a
C character width in the plotter coordinate system.  The values 0, 1,
C 2, and 3 mean 8, 12, 16, and 24, respectively.
C
C IO is the desired orientation angle, in degrees counterclockwise from
C a horizontal vector pointing to the right.
C
C IC specifies the desired type of centering.  A negative value puts
C (PX,PY) in the center of the left end of the character string, a zero
C puts (PX,PY) in the center of the whole string, and a positive value
C puts (PX,PY) in the center of the right end of the character string.
C
      CHARACTER*(*) CH
C
C Define arrays in which to save the current viewport and window.
C
      DIMENSION VP(4),WD(4)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('WTSTR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('WTSTR',2).NE.0) RETURN
C
C Compute the coordinates of (PX,PY) in the fractional coordinate
C system (normalized device coordinates).
C
      XN=CUFX(PX)
      IF (ICFELL('WTSTR',3).NE.0) RETURN
      YN=CUFY(PY)
      IF (ICFELL('WTSTR',4).NE.0) RETURN
C
C Save the current window and, if necessary, redefine it so that we can
C use normalized device coordinates.
C
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('WTSTR - ERROR EXIT FROM GQCNTN',5,1)
        RETURN
      END IF
      IF (NT.NE.0) THEN
        CALL GQNT (NT,IE,WD,VP)
        IF (IE.NE.0) THEN
          CALL SETER ('WTSTR - ERROR EXIT FROM GQNT',6,1)
          RETURN
        END IF
        CALL GSWN (NT,VP(1),VP(2),VP(3),VP(4))
      END IF
C
C Save current character height, text path, character up vector, and
C text alignment.
C
      CALL GQCHH (IE,OS)
      IF (IE.NE.0) THEN
        CALL SETER ('WTSTR - ERROR EXIT FROM GQCHH',7,1)
        RETURN
      END IF
      CALL GQTXP (IE,IP)
      IF (IE.NE.0) THEN
        CALL SETER ('WTSTR - ERROR EXIT FROM GQTXP',8,1)
        RETURN
      END IF
      CALL GQCHUP (IE,UX,UY)
      IF (IE.NE.0) THEN
        CALL SETER ('WTSTR - ERROR EXIT FROM GQCHUP',9,1)
        RETURN
      END IF
      CALL GQTXAL (IE,IX,IY)
      IF (IE.NE.0) THEN
        CALL SETER ('WTSTR - ERROR EXIT FROM GQTXAL',10,1)
        RETURN
      END IF
C
C Define the character height.  (The final scale factor is derived from
C the default font.)
C
      CALL GETUSV ('YF',MY)
      IF (ICFELL('WTSTR',11).NE.0) RETURN
      YS=REAL(2**MY)
      IF (IS.GE.0.AND.IS.LE.3) THEN
        CS=REAL(8+4*IS+4*(IS/3))/YS
      ELSE
        CS=MIN(REAL(IS),YS)/YS
      ENDIF
C
C     CS=CS*1.0
C
      CALL GSCHH(CS)
C
C Define the text path.
C
      CALL GSTXP (0)
C
C Define the character up vector.
C
      JO=MOD(IO,360)
      IF (JO.EQ.0) THEN
        CALL GSCHUP (0.,1.)
      ELSE IF (JO.EQ.90) THEN
        CALL GSCHUP (-1.,0.)
      ELSE IF (JO.EQ.180) THEN
        CALL GSCHUP (0.,-1.)
      ELSE IF (JO.EQ.270) THEN
        CALL GSCHUP (1.,0.)
      ELSE IF (JO.GT.0.AND.JO.LT.180) THEN
        CALL GSCHUP (-1.,1./TAN(REAL(JO)*3.1415926/180.))
      ELSE
        CALL GSCHUP (1.,-1./TAN(REAL(JO)*3.1415926/180.))
      ENDIF
C
C Define the text alignment.
C
      CALL GSTXAL (MAX(-1,MIN(+1,IC))+2,3)
C
C Plot the characters.
C
      CALL GTX (XN,YN,CH)
C
C Restore the original text attributes.
C
      CALL GSCHH (OS)
      CALL GSTXP (IP)
      CALL GSCHUP (UX,UY)
      CALL GSTXAL (IX,IY)
C
C Restore the window definition.
C
      IF (NT.NE.0) THEN
        CALL GSWN (NT,WD(1),WD(2),WD(3),WD(4))
      END IF
C
C Update the pen position.
C
      CALL FRSTPT (PX,PY)
      IF (ICFELL('WTSTR',12).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
