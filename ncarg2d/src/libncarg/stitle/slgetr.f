C
C	$Id: slgetr.f,v 1.1.1.1 1992-04-17 22:33:05 ncargd Exp $
C
      SUBROUTINE SLGETR(PA,RVAL)
C
C Get real-vlaued parameters for STITLE.
C
C Arguments
C     Input
C             PA       Character string indicating the parameter.
C
C     Output
C             RVAL     A floating-point number representing the
C                      current setting for the specified parameter.
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
      IF (CTMP.EQ.'BGR' .OR. CTMP.EQ.'bgr') THEN
C
C  Get background color, red component.
C
        RVAL = BGCLR(1)
      ELSE IF (CTMP.EQ.'BGG' .OR. CTMP.EQ.'bgg') THEN
C
C  Get background color, green component.
C
        RVAL = BGCLR(2)
      ELSE IF (CTMP.EQ.'BGB' .OR. CTMP.EQ.'bgb') THEN
C
C  Get background color, blue component.
C
        RVAL = BGCLR(3)
      ELSE IF (CTMP.EQ.'FGR' .OR. CTMP.EQ.'fgr') THEN
C
C  Get foreground color, red component.
C
        RVAL = FGCLR(1)
      ELSE IF (CTMP.EQ.'FGG' .OR. CTMP.EQ.'fgg') THEN
C
C  Get foreground color, green component.
C
        RVAL = FGCLR(2)
      ELSE IF (CTMP.EQ.'FGB' .OR. CTMP.EQ.'fgb') THEN
C
C  Get foreground color, blue component.
C
        RVAL = FGCLR(3)
      ELSE IF (CTMP.EQ.'PSZ' .OR. CTMP.EQ.'psz') THEN
C
C  Get character size.
C
        RVAL = PCHSZ
      ELSE IF (CTMP.EQ.'GSZ' .OR. CTMP.EQ.'gsz') THEN
C
C  Get value of interline spacing.
C
        RVAL = GAPSZ
      ELSE IF (CTMP.EQ.'TM1' .OR. CTMP.EQ.'tm1') THEN
C
C  Get initial blank frame count.
C
        RVAL = T1
      ELSE IF (CTMP.EQ.'TM2' .OR. CTMP.EQ.'tm2') THEN
C
C  Set final blank frame count.
C
        RVAL = T2
      ELSE IF (CTMP.EQ.'FIN' .OR. CTMP.EQ.'fin') THEN
C
C  Get fade in time.
C
        RVAL = FIN
      ELSE IF (CTMP.EQ.'FOU' .OR. CTMP.EQ.'fou') THEN
C
C  Get fade out time.
C
        RVAL = FOU
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
  100 CONTINUE
      RETURN
C
  500 FORMAT(' SLGETR -- INVALID KEYWORD = ',A3,', NO ACTION TAKEN')
  501 FORMAT(' SLGETR -- INCORRECT NUMBER OF VALUES SPECIFIED FOR KEYWOR
     -D = ',A3)
C
      END
