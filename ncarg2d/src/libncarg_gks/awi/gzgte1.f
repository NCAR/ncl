      SUBROUTINE GZGTE1(INDEX,NUM,ERMSG)
C
C  Given an index INDEX into the error table, this subroutine
C  returns the NCAR GKS error number in NUM and the error
C  message string in ERMSG (the length of ERMSG will be a
C  maximum of 90 characters).
C
      CHARACTER*(*) ERMSG
C
      include 'gkscom.h'
C
      IF (INDEX.LE.NUMERS .AND. INDEX.GE.1) THEN
        NUM = IERNMS(INDEX)
        ERMSG = ERMSGS(INDEX)
      ELSE
        NUM = -100
        ERMSG = ERMSGS(NUM)
      ENDIF
C
      RETURN
      END
