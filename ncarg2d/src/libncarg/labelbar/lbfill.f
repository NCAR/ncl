C
C $Id: lbfill.f,v 1.5 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LBFILL (IFTP,XCRA,YCRA,NCRA,INDX)
        DIMENSION XCRA(*),YCRA(*)
        CALL GSFACI (INDX)
        CALL GFA (NCRA-1,XCRA,YCRA)
        RETURN
      END
