C
C	$Id: slseti.f,v 1.1.1.1 1992-04-17 22:33:01 ncargd Exp $
C
      SUBROUTINE SLSETI(PA,IVAL)
C
C Set integer-valued parameters for STITLE.
C
C Arguments
C     Input   PA       Character string indicating parameter
C                      to be set.
C             IVAL     An integer value for setting PA.
C

      CHARACTER*(*) PA
      CHARACTER*3   CTMP
C
C
C The labeled common block SCRLDT holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SCRLDT/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),MXOLD,MYOLD,LOLD,IBKG,LND,BGCLR(3),
     +                FGCLR(3),IFST,IWK,FIN,FOU,ISPB,ISPF,IDEN,IWU
      SAVE   /SCRLDT/
C
C Initialize variables if this is the first user call.
C
      IF (IFST .EQ. 0) THEN
        CALL SLINIT
        IFST = 1
      ENDIF
C
      LNTHC = LEN(PA)
      IF (LNTHC .LT. 3) THEN
        WRITE(I1MACH(4), 500) CTMP
        GO TO 100
      ENDIF
C
      CTMP = PA(1:3)
      IF (CTMP.EQ.'ICU' .OR. CTMP.EQ.'icu') THEN
C
C  Set unit number for input.
C
        ICU = IVAL
      ELSE IF (CTMP.EQ.'ICO' .OR. CTMP.EQ.'ico') THEN
C
C  Set centering parameter.
C
        ICO = IVAL
      ELSE IF (CTMP.EQ.'NXS' .OR. CTMP.EQ.'nxs') THEN
C
C  Set horizontal scroll start.
C
        NXST = IVAL
      ELSE IF (CTMP.EQ.'NXE' .OR. CTMP.EQ.'nxe') THEN
C
C  Set horizontal scroll end.
C
        NXFIN = IVAL
      ELSE IF (CTMP.EQ.'INC' .OR. CTMP.EQ.'inc') THEN
C
C  Set interline spacing in practice runs.
C
        ICRTJP = IVAL
      ELSE IF (CTMP.EQ.'LX1' .OR. CTMP.EQ.'lx1') THEN
C
C  Set lower left X viewport value.
C
        LIM(1) = IVAL
      ELSE IF (CTMP.EQ.'LX2' .OR. CTMP.EQ.'lx2') THEN
C
C  Set upper right X viewport value.
C
        LIM(2) = IVAL
      ELSE IF (CTMP.EQ.'LY1' .OR. CTMP.EQ.'ly1') THEN
C
C  Set lower left Y viewport value.
C
        LIM(3) = IVAL
      ELSE IF (CTMP.EQ.'LY2' .OR. CTMP.EQ.'ly2') THEN
C
C  Set upper right Y viewport value.
C
        LIM(4) = IVAL
      ELSE IF (CTMP.EQ.'ALN' .OR. CTMP.EQ.'aln') THEN
C
C  Set the flag controlling the output of the alignment frames.
C
        LND = IVAL
      ELSE IF (CTMP.EQ.'SBK' .OR. CTMP.EQ.'sbk') THEN
C
C  Set the flag controlling the suppression of background color
C  during a fade in/out.
C
        ISPB = IVAL
      ELSE IF (CTMP.EQ.'SFG' .OR. CTMP.EQ.'sfg') THEN
C
C  Set the flag controlling the suppression of foreground color
C  during a fade in/out.
C
        ISPF = IVAL
      ELSE IF (CTMP.EQ.'WID' .OR. CTMP.EQ.'wid') THEN
C
C  Set the workstation ID for opening WISS.
C
        IDEN = IVAL
      ELSE IF (CTMP.EQ.'LOG' .OR. CTMP.EQ.'log') THEN
C
C  Set the FORTRAN logical unit number for opening WISS.
C
        IWU = IVAL
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
  100 CONTINUE
      RETURN
C
  500 FORMAT(' SLSETI -- INVALID KEYWORD = ',A3,', NO ACTION TAKEN')
  501 FORMAT(' SLSETI -- INCORRECT NUMBER OF VALUES SPECIFIED FOR KEYWOR
     -D = ',A3)
C
      END
