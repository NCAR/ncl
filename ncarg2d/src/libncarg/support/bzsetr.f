C
C	$Id: bzsetr.f,v 1.2 1992-11-17 19:10:14 fred Exp $
C
      SUBROUTINE BZSETR(PA,RVAL)
C
C Set real-valued parameters for the Bezier curve package.
C
C Arguments
C     Input   PA       Character string indicating parameter to be set.
C             RVAL     A real number for setting PA.
C

      CHARACTER*(*) PA
      CHARACTER*3   CTMP
C
      include 'bzcom.h'
C
      CTMP = PA(1:3)
C
      IF (CTMP.EQ.'FTL' .OR. CTMP.EQ.'ftl') THEN
C
C  Set the flatness tolerance limit (fraction of maximum screen height).
C
        FRATIO = RVAL 
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
      RETURN
C
  500 FORMAT(' BZSETR -- Invalid keyword = ',A3,', no action taken')
C
      END
