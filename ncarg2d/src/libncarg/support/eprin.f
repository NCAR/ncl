C
C $Id: eprin.f,v 1.2 1993-09-23 17:21:20 kennison Exp $
C
      SUBROUTINE EPRIN
C
C This routine just prints the current error message, if any.
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDA.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*113 ERMSG
        SAVE   /SECOMC/
C
C Do it.
C
        IF (IERRF.NE.0) WRITE (IERRU,'('' ERROR '',I4,'' IN '',A)')
     +                                              IERRF,ERMSG(1:LOMSG)
C
C Done.
C
        RETURN
C
      END
