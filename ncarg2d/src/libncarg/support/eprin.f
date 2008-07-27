C
C $Id: eprin.f,v 1.7 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE EPRIN
C
C This routine just prints the current error message, if any.
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDAX.
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
