C
C $Id: frstc.f,v 1.5 2008-07-27 00:17:15 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FRSTC (MXN,MYN,IENT)
C
C This routine provides an interface to ISPLTF for codes still using
C coordinates in the metacode coordinate system.
C
        CALL ISPLTF (REAL(MXN)/32767.,REAL(MYN)/32767.,IENT)
C
C Done.
C
        RETURN
C
      END
