C
C       $Id:
C
      SUBROUTINE NGMFTC(WKID)
C
C Effects a temporaty close in metafile output allowing for subsequent 
C reopening for appending using NGREOP.  The metafile output may be 
C suspended at any time, and not necessarily at a picture break.
C
C ARGUMENTS
C
C ON INPUT  
C    
C   WKID  An integer argument indicating the workstation ID of the
C         workstation to temporarily close.
C
      INTEGER WKID
      CHARACTER*80 IDR(1),ODR(1)
C
      WRITE(IDR(1)(1:5),500) WKID
  500 FORMAT(I5)
      CALL SFLUSH
      CALL GESC(-1387,1,IDR,1,1,ODR)
C
      RETURN
      END
