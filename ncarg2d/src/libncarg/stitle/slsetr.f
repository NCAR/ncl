C
C	$Id: slsetr.f,v 1.1.1.1 1992-04-17 22:33:05 ncargd Exp $
C
      SUBROUTINE SLSETR(PA,RVAL)
C
C Set real-vlaued parameters for STITLE.
C
C Arguments
C     Input   PA       Character string indicating parameter to be set.
C             RVAL     A real number for setting PA.
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
C  Set background color, red component.
C
        RVAL = AMAX1(0.,RVAL)
        RVAL = AMIN1(1.,RVAL)
        BGCLR(1) = RVAL
        IBKG = 2
        CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
      ELSE IF (CTMP.EQ.'BGG' .OR. CTMP.EQ.'bgg') THEN
C
C  Set background color, green component.
C
        RVAL = AMAX1(0.,RVAL)
        RVAL = AMIN1(1.,RVAL)
        BGCLR(2) = RVAL
        IBKG = 2
        CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
      ELSE IF (CTMP.EQ.'BGB' .OR. CTMP.EQ.'bgb') THEN
C
C  Set background color, blue component.
C
        RVAL = AMAX1(0.,RVAL)
        RVAL = AMIN1(1.,RVAL)
        BGCLR(3) = RVAL
        IBKG = 2
        CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
      ELSE IF (CTMP.EQ.'FGR' .OR. CTMP.EQ.'fgr') THEN
C
C  Set foreground color, red component.
C
        RVAL = AMAX1(0.,RVAL)
        RVAL = AMIN1(1.,RVAL)
        FGCLR(1) = RVAL
        CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
      ELSE IF (CTMP.EQ.'FGG' .OR. CTMP.EQ.'fgg') THEN
C
C  Set foreground color, green component.
C
        RVAL = AMAX1(0.,RVAL)
        RVAL = AMIN1(1.,RVAL)
        FGCLR(2) = RVAL
        CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
      ELSE IF (CTMP.EQ.'FGB' .OR. CTMP.EQ.'fgb') THEN
C
C  Set foreground color, blue component.
C
        RVAL = AMAX1(0.,RVAL)
        RVAL = AMIN1(1.,RVAL)
        FGCLR(3) = RVAL
        CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
      ELSE IF (CTMP.EQ.'PSZ' .OR. CTMP.EQ.'psz') THEN
C
C  Set character size.
C
        PCHSZ = RVAL
      ELSE IF (CTMP.EQ.'GSZ' .OR. CTMP.EQ.'gsz') THEN
C
C  Set value of interline spacing.
C
        GAPSZ = RVAL
      ELSE IF (CTMP.EQ.'TM1' .OR. CTMP.EQ.'tm1') THEN
C
C  Set initial blank frame count.
C
        T1 = RVAL
      ELSE IF (CTMP.EQ.'TM2' .OR. CTMP.EQ.'tm2') THEN
C
C  Set final blank frame count.
C
        T2 = RVAL
      ELSE IF (CTMP.EQ.'FIN' .OR. CTMP.EQ.'fin') THEN
C
C  Set fade in time.
C
        FIN = RVAL
      ELSE IF (CTMP.EQ.'FOU' .OR. CTMP.EQ.'fou') THEN
C
C  Set fade out time.
C
        FOU = RVAL
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
  100 CONTINUE
      RETURN
C
  500 FORMAT(' SLSETR -- INVALID KEYWORD = ',A3,', NO ACTION TAKEN')
  501 FORMAT(' SLSETR -- INCORRECT NUMBER OF VALUES SPECIFIED FOR KEYWOR
     -D = ',A3)
C
      END
