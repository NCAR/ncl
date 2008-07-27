C
C	$Id: wmw2nx.f,v 1.4 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMW2NX(NPT,P,Q)
C
C  This subroutine takes the NPT points in the real array P,
C  which are assumed to be in world coordinates, and, using
C  the current normalization transformation, converts them
C  into NDC coordinates and stores them in Q.  This subroutine
C  operates on X-coordinates.
C
      INTEGER NPT
      REAL P(NPT),Q(NPT)
      REAL WINDOW(4),VIEWPT(4)
C
      CALL GQCNTN(IER,NTR)
      CALL GQNT(NTR,IER,WINDOW,VIEWPT)
      SCALE = (VIEWPT(2)-VIEWPT(1))/(WINDOW(2)-WINDOW(1))
      DO 20 I=1,NPT
        Q(I) = VIEWPT(1)+(P(I)-WINDOW(1))*SCALE
   20 CONTINUE
C
      RETURN
      END
