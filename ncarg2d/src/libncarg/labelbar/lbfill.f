C
C	$Id: lbfill.f,v 1.1.1.1 1992-04-17 22:32:57 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LBFILL (IFTP,XCRA,YCRA,NCRA,INDX)
        DIMENSION XCRA(*),YCRA(*)
        CALL GSFACI (INDX)
        CALL GFA (NCRA-1,XCRA,YCRA)
        RETURN
      END
