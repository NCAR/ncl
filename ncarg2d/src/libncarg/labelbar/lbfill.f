C
C $Id: lbfill.f,v 1.2 1994-03-17 20:19:44 kennison Exp $
C
      SUBROUTINE LBFILL (IFTP,XCRA,YCRA,NCRA,INDX)
        DIMENSION XCRA(*),YCRA(*)
        CALL GSFACI (INDX)
        CALL GFA (NCRA-1,XCRA,YCRA)
        RETURN
      END
