C
C $Id: slsetr.f,v 1.2 1993-01-14 00:30:02 kennison Exp $
C
      SUBROUTINE SLSETR(PNAM,RVAL)
C
C Set a parameter of STITLE, using a real value.
C
C Arguments
C     Input
C             PNAM     The three-character name of some parameter.
C
C             RVAL     A real expression which is to become the value
C                      of the specified parameter.
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
C  Set the flag controlling the output of the alignment frames.
C
        LND = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'BGB' .OR. CTMP.EQ.'bgb') THEN
C
C  Set background color, blue component.
C
        BGCLR(3) = RVAL
C
      ELSE IF (CTMP.EQ.'BGG' .OR. CTMP.EQ.'bgg') THEN
C
C  Set background color, green component.
C
        BGCLR(2) = RVAL
C
      ELSE IF (CTMP.EQ.'BGR' .OR. CTMP.EQ.'bgr') THEN
C
C  Set background color, red component.
C
        BGCLR(1) = RVAL
C
      ELSE IF (CTMP.EQ.'FGB' .OR. CTMP.EQ.'fgb') THEN
C
C  Set foreground color, blue component.
C
        FGCLR(3) = RVAL
C
      ELSE IF (CTMP.EQ.'FGG' .OR. CTMP.EQ.'fgg') THEN
C
C  Set foreground color, green component.
C
        FGCLR(2) = RVAL
C
      ELSE IF (CTMP.EQ.'FIN' .OR. CTMP.EQ.'fin') THEN
C
C  Set fade in time.
C
        FIN = RVAL
C
      ELSE IF (CTMP.EQ.'FOU' .OR. CTMP.EQ.'fou') THEN
C
C  Set fade out time.
C
        FOU = RVAL
C
      ELSE IF (CTMP.EQ.'GSZ' .OR. CTMP.EQ.'gsz') THEN
C
C  Set value of interline spacing.
C
        GAPSZ = RVAL
C
      ELSE IF (CTMP.EQ.'FGR' .OR. CTMP.EQ.'fgr') THEN
C
C  Set foreground color, red component.
C
        FGCLR(1) = RVAL
C
      ELSE IF (CTMP.EQ.'ICO' .OR. CTMP.EQ.'ico') THEN
C
C  Set centering parameter.
C
        ICO = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'ICU' .OR. CTMP.EQ.'icu') THEN
C
C  Set unit number for input.
C
        ICU = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'INC' .OR. CTMP.EQ.'inc') THEN
C
C  Set interline spacing in practice runs.
C
        ICRTJP= INT(RVAL)
C
      ELSE IF (CTMP.EQ.'LOG' .OR. CTMP.EQ.'log') THEN
C
C  Set the FORTRAN logical unit number for WISS.
C
        IWU = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'LX1' .OR. CTMP.EQ.'lx1') THEN
C
C  Set lower left X viewport value.
C
        LIM(1) = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'LX2' .OR. CTMP.EQ.'lx2') THEN
C
C  Set upper right X viewport value.
C
        LIM(2) = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'LY1' .OR. CTMP.EQ.'ly1') THEN
C
C  Set lower left Y viewport value.
C
        LIM(3) = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'MAP' .OR. CTMP.EQ.'map') THEN
C
C  Set PLOTCHAR mapping flag.
C
        IMAP = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'LY2' .OR. CTMP.EQ.'ly2') THEN
C
C  Set upper right Y viewport value.
C
        LIM(4) = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'NXE' .OR. CTMP.EQ.'nxe') THEN
C
C  Set horizontal scroll end.
C
        NXFIN = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'NXS' .OR. CTMP.EQ.'nxs') THEN
C
C  Set horizontal scroll start.
C
        NXST = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'ORV' .OR. CTMP.EQ.'orv') THEN
C
C  Set PLOTCHAR out-of-range flag.
C
        RVAL = OORV
C
      ELSE IF (CTMP.EQ.'PSZ' .OR. CTMP.EQ.'psz') THEN
C
C  Set character size.
C
        PCHSZ = RVAL
C
      ELSE IF (CTMP.EQ.'SBK' .OR. CTMP.EQ.'sbk') THEN
C
C  Set the flag controlling the suppression of background color
C  during a fade in/out.
C
        ISPB = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'SFG' .OR. CTMP.EQ.'sfg') THEN
C
C  Set the flag controlling the suppression of foreground color
C  during a fade in/out.
C
        ISPF = INT(RVAL)
C
      ELSE IF (CTMP.EQ.'TM1' .OR. CTMP.EQ.'tm1') THEN
C
C  Set initial blank frame count.
C
        T1 = RVAL
C
      ELSE IF (CTMP.EQ.'TM2' .OR. CTMP.EQ.'tm2') THEN
C
C  Set final blank frame count.
C
        T2 = RVAL
C
      ELSE IF (CTMP.EQ.'WID' .OR. CTMP.EQ.'wid') THEN
C
C  Set the workstation identifier for WISS.
C
        IDEN = INT(RVAL)
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
      RETURN
C
 1001 FORMAT(' SLSETI OR SLSETR -- INVALID KEYWORD = ',A3,', NO ACTION T
     +AKEN')
C
      END
