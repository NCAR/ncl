C
C	$Id: isoscr.f,v 1.1.1.1 1992-04-17 22:32:09 ncargd Exp $
C
      SUBROUTINE ISOSCR()
C
C OPEN SCRATCH FILE : ./tmp.isosrfhr
C
      COMMON /UNITS/ IUNIT
      IUNIT = 4
      OPEN(UNIT=4,STATUS='SCRATCH',FORM='UNFORMATTED')
      END
