      SUBROUTINE DPSETI (PNAM,IVAL)
C
C This routine, given an integer value, sets the value of an internal
C parameter of type INTEGER or REAL to that value.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pass the buck to DPSETR.
C
        CALL DPSETR (PNAM,REAL(IVAL))
        IF (ICFELL('DPSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
