C
C $Id: slgetr.f,v 1.2 1993-01-14 00:29:48 kennison Exp $
C
      SUBROUTINE SLGETR(PNAM,RVAL)
C
C Get the real value of an STITLE parameter.
C
C Arguments
C     Input
C             PNAM     The three-character name of some parameter.
C
C     Output
C             RVAL     The real value of the specified parameter.
C
      CHARACTER*(*) PNAM
      CHARACTER*3   CTMP
C
C The labeled common block SLCOMN holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SLCOMN/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),IBKG,LND,BGCLR(3),FGCLR(3),IFST,IWK,FIN,
     +                FOU,ISPB,ISPF,IDEN,IWU,IMAP,OORV
      SAVE   /SLCOMN/
C
C Initialize variables if this is the first user call.
C
      IF (IFST .EQ. 0) THEN
        CALL SLINIT
        IFST = 1
      ENDIF
C
      CTMP = PNAM
      LNTHC = LEN(PNAM)
C
      IF (LNTHC .LT. 3) GO TO 901
C
C See what parameter name we have.
C
      IF      (CTMP.EQ.'ALN' .OR. CTMP.EQ.'aln') THEN
C
C  Get the flag controlling the output of the alignment frames.
C
        RVAL = REAL(LND)
C
      ELSE IF (CTMP.EQ.'BGB' .OR. CTMP.EQ.'bgb') THEN
C
C  Get background color, blue component.
C
        RVAL = BGCLR(3)
C
      ELSE IF (CTMP.EQ.'BGG' .OR. CTMP.EQ.'bgg') THEN
C
C  Get background color, green component.
C
        RVAL = BGCLR(2)
C
      ELSE IF (CTMP.EQ.'BGR' .OR. CTMP.EQ.'bgr') THEN
C
C  Get background color, red component.
C
        RVAL = BGCLR(1)
C
      ELSE IF (CTMP.EQ.'FGB' .OR. CTMP.EQ.'fgb') THEN
C
C  Get foreground color, blue component.
C
        RVAL = FGCLR(3)
C
      ELSE IF (CTMP.EQ.'FGG' .OR. CTMP.EQ.'fgg') THEN
C
C  Get foreground color, green component.
C
        RVAL = FGCLR(2)
C
      ELSE IF (CTMP.EQ.'FGR' .OR. CTMP.EQ.'fgr') THEN
C
C  Get foreground color, red component.
C
        RVAL = FGCLR(1)
C
      ELSE IF (CTMP.EQ.'FIN' .OR. CTMP.EQ.'fin') THEN
C
C  Get fade in time.
C
        RVAL = FIN
C
      ELSE IF (CTMP.EQ.'FOU' .OR. CTMP.EQ.'fou') THEN
C
C  Get fade out time.
C
        RVAL = FOU
C
      ELSE IF (CTMP.EQ.'GSZ' .OR. CTMP.EQ.'gsz') THEN
C
C  Get value of interline spacing.
C
        RVAL = GAPSZ
C
      ELSE IF (CTMP.EQ.'ICO' .OR. CTMP.EQ.'ico') THEN
C
C  Get centering parameter.
C
        RVAL = REAL(ICO)
C
      ELSE IF (CTMP.EQ.'ICU' .OR. CTMP.EQ.'icu') THEN
C
C  Get unit number for input.
C
        RVAL = REAL(ICU)
C
      ELSE IF (CTMP.EQ.'INC' .OR. CTMP.EQ.'inc') THEN
C
C  Get interline spacing in practice runs.
C
        RVAL = REAL(ICRTJP)
C
      ELSE IF (CTMP.EQ.'LOG' .OR. CTMP.EQ.'log') THEN
C
C  Get the FORTRAN logical unit number for WISS.
C
        RVAL = REAL(IWU)
C
      ELSE IF (CTMP.EQ.'LX1' .OR. CTMP.EQ.'lx1') THEN
C
C  Get lower left X viewport value.
C
        RVAL = REAL(LIM(1))
C
      ELSE IF (CTMP.EQ.'LX2' .OR. CTMP.EQ.'lx2') THEN
C
C  Get upper right X viewport value.
C
        RVAL = REAL(LIM(2))
C
      ELSE IF (CTMP.EQ.'LY1' .OR. CTMP.EQ.'ly1') THEN
C
C  Get lower left Y viewport value.
C
        RVAL = REAL(LIM(3))
C
      ELSE IF (CTMP.EQ.'LY2' .OR. CTMP.EQ.'ly2') THEN
C
C  Get upper right Y viewport value.
C
        RVAL = REAL(LIM(4))
C
      ELSE IF (CTMP.EQ.'MAP' .OR. CTMP.EQ.'map') THEN
C
C  Get PLOTCHAR mapping flag.
C
        RVAL = REAL(IMAP)
C
      ELSE IF (CTMP.EQ.'NXE' .OR. CTMP.EQ.'nxe') THEN
C
C  Get horizontal scroll end.
C
        RVAL = REAL(NXFIN)
C
      ELSE IF (CTMP.EQ.'NXS' .OR. CTMP.EQ.'nxs') THEN
C
C  Get horizontal scroll start.
C
        RVAL = REAL(NXST)
C
      ELSE IF (CTMP.EQ.'ORV' .OR. CTMP.EQ.'orv') THEN
C
C  Get PLOTCHAR out-of-range flag.
C
        RVAL = OORV
C
      ELSE IF (CTMP.EQ.'PSZ' .OR. CTMP.EQ.'psz') THEN
C
C  Get character size.
C
        RVAL = PCHSZ
C
      ELSE IF (CTMP.EQ.'SBK' .OR. CTMP.EQ.'sbk') THEN
C
C  Get the flag controlling the suppression of background color
C  during a fade in/out.
C
        RVAL = REAL(ISPB)
C
      ELSE IF (CTMP.EQ.'SFG' .OR. CTMP.EQ.'sfg') THEN
C
C  Get the flag controlling the suppression of foreground color
C  during a fade in/out.
C
        RVAL = REAL(ISPF)
C
      ELSE IF (CTMP.EQ.'TM1' .OR. CTMP.EQ.'tm1') THEN
C
C  Get initial blank frame count.
C
        RVAL = T1
C
      ELSE IF (CTMP.EQ.'TM2' .OR. CTMP.EQ.'tm2') THEN
C
C  Get final blank frame count.
C
        RVAL = T2
C
      ELSE IF (CTMP.EQ.'WID' .OR. CTMP.EQ.'wid') THEN
C
C  Get the workstation identifier for WISS.
C
        RVAL = REAL(IDEN)
C
      ELSE
C
C Parameter name not recognized.
C
        GO TO 901
C
      ENDIF
C
      RETURN
C
C Error return.
C
  901 WRITE(I1MACH(4),1001) CTMP
      RVAL=0.
      RETURN
C
 1001 FORMAT (' SLGETI OR SLGETR -- INVALID KEYWORD = ',A3,', ZERO VALUE
     + RETURNED')
C
      END
