C
C	$Id: slgeti.f,v 1.1.1.1 1992-04-17 22:33:01 ncargd Exp $
C
      SUBROUTINE SLGETI(PA,IVAL)
C
C Retrieve integer-valued parameters for STITLE.
C
C Arguments
C     Input
C             PA       Character string indicating parameter.
C
C     Output
C             IVAL     An integer value giving the current setting
C                      of the specified parameter.
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
C  Get unit number for input.
C
        IVAL = ICU
      ELSE IF (CTMP.EQ.'ICO' .OR. CTMP.EQ.'ico') THEN
C
C  Get centering parameter.
C
        IVAL = ICO
      ELSE IF (CTMP.EQ.'NXS' .OR. CTMP.EQ.'nxs') THEN
C
C  Get horizontal scroll start.
C
        IVAL = NXST
      ELSE IF (CTMP.EQ.'NXE' .OR. CTMP.EQ.'nxe') THEN
C
C  Get horizontal scroll end.
C
        IVAL = NXFIN
      ELSE IF (CTMP.EQ.'INC' .OR. CTMP.EQ.'inc') THEN
C
C  Get interline spacing in practice runs.
C
        IVAL = ICRTJP
      ELSE IF (CTMP.EQ.'LX1' .OR. CTMP.EQ.'lx1') THEN
C
C  Get lower left X viewport value.
C
        IVAL = LIM(1)
      ELSE IF (CTMP.EQ.'LX2' .OR. CTMP.EQ.'lx2') THEN
C
C  Get upper right X viewport value.
C
        IVAL = LIM(2)
      ELSE IF (CTMP.EQ.'LY1' .OR. CTMP.EQ.'ly1') THEN
C
C  Get lower left Y viewport value.
C
        IVAL = LIM(3)
      ELSE IF (CTMP.EQ.'LY2' .OR. CTMP.EQ.'ly2') THEN
C
C  Get upper right Y viewport value.
C
        IVAL = LIM(4)
      ELSE IF (CTMP.EQ.'ALN' .OR. CTMP.EQ.'aln') THEN
C
C  Get the flag controlling the output of the alignment frames.
C
        IVAL = LND
      ELSE IF (CTMP.EQ.'SBK' .OR. CTMP.EQ.'sbk') THEN
C
C  Get the flag controlling the suppression of background color
C  during a fade in/out.
C
        IVAL = ISPB
      ELSE IF (CTMP.EQ.'SFG' .OR. CTMP.EQ.'sfg') THEN
C
C  Get the flag controlling the suppression of foreground color
C  during a fade in/out.
C
        IVAL = ISPF
      ELSE IF (CTMP.EQ.'WID' .OR. CTMP.EQ.'wid') THEN
C
C  Get the workstation identifier for WISS.
C
        IVAL = IDEN
      ELSE IF (CTMP.EQ.'LOG' .OR. CTMP.EQ.'log') THEN
C
C  Get the FORTRAN logical unit number for WISS.
C
        IVAL = IWU
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
  100 CONTINUE
      RETURN
C
  500 FORMAT(' SLGETI -- INVALID KEYWORD = ',A3,', NO ACTION TAKEN')
  501 FORMAT(' SLGETI -- INCORRECT NUMBER OF VALUES SPECIFIED FOR KEYWOR
     -D = ',A3)
C
      END
