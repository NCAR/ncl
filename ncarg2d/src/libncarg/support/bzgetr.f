      SUBROUTINE BZGETR(PA,RVAL)
C
C Retrieve real-valued parameters for the Bezier curve package.
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
      include 'bzcom.h'
C
      CTMP = PA(1:3)
C
      IF (CTMP.EQ.'FLT' .OR. CTMP.EQ.'flt') THEN
C
C  Get the flatness tolerance limit.
C
        RVAL = FRATIO
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
      RETURN
C
  500 FORMAT(' BZGETR -- Invalid keyword = ',A3,', no action taken')
C
      END
