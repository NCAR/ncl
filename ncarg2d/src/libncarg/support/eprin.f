C
C $Id: eprin.f,v 1.3 1994-03-16 00:42:40 kennison Exp $
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
          CHARACTER*256 ERMSG
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
