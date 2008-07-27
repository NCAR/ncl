C
C	$Id: pcbdff.f,v 1.7 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCBDFF
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA PCBDFFX
C
C BLOCK DATA for filled fonts.
C
      include 'pcffme.doc'
      include 'pcffme.h'
      include 'pcffdx.doc'
      include 'pcffdx.h'
      include 'pcffsp.doc'
      include 'pcffsp.h'
C
C Initialize a single variable to avoid a loader error on
C some systems.  This blockdata subroutine is here as a
C dummy in case it should ever be needed in the future.
C
      DATA TYPFLG / 0 /
C
      END
