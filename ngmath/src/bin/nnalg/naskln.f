      SUBROUTINE NASKLN(IUNIT,LINE)
C
C  Return the next non-comment line on IUNIT in LINE.
C
      CHARACTER*80 LINE
C
    5 CONTINUE
      READ(IUNIT,100,END=200) LINE
  100 FORMAT(A80)
      IF (LINE(1:2) .EQ. '/*') GO TO 5
C
      RETURN
  200 CONTINUE
      WRITE(6,500)
  500 FORMAT(' Unexpected end of file on input')
      STOP
C
      END
