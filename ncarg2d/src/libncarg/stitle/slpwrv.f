C
C $Id: slpwrv.f,v 1.1 1993-01-14 00:29:57 kennison Exp $
C
      SUBROUTINE SLPWRV (XPOS,YPOS,ITXT,LTXT,IWID,IORI,ICEN)
C
C This routine is used by STITLE to write a message vertically.  The
C arguments are like those for PWRIT.  The characters are centered
C vertically.
C
      CHARACTER*(*) ITXT
C
C Compute the height of the characters used.
C
      IHEI = MAX0(IWID,0)
      IF (IHEI.LE.3) IHEI = 4*(2+(4*IHEI)/3)
      IHEI = 2*IHEI
      FHEI = REAL(IHEI)/1024.
      YTOT = REAL(LTXT)*FHEI
C
C Plot the text characters (if any) one at a time, reducing the Y
C coordinate by the character height each time.
C
      IF (LTXT.GT.0) THEN
C
        YPOST = .5+.5*YTOT
C
        DO 101 JTXT=1,LTXT
          CALL PWRIT(XPOS,YPOST,ITXT(JTXT:JTXT),1,IWID,IORI,ICEN)
          YPOST = YPOST-FHEI
  101   CONTINUE
C
      END IF
C
      RETURN
C
      END
