C
C $Id: gznume.f,v 1.4 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION GZNUME()
C
C  This function returns the total number of legal errors available
C  in NCAR GKS.
C
      include 'gkscom.h'
C
      GZNUME = NUMERS
      RETURN
      END

