C
C	$Id: wmcext.f,v 1.4 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMCEXT(ISN,NF,X,Y,STR,SIZ,EXTBOX)
C
C  Use Plotchar to determine a text extent box for the station
C  model label numbered ISN from font number NF with the characters
C  in STR, at size SIZ, at position (X,Y).  The extent box is returned
C  in EXTBOX in the order XLL,YLL,XUR,YUR.
C
      CHARACTER*(*) STR
      INTEGER WMGTLN
      DIMENSION EXTBOX(4)
C
      CALL PCGETI('TE',ITEOLD)
      CALL PCGETI('FN',NFOLD)
      CALL PCSETI('TE',1)
      CALL PCSETI('FN',NF)
      IF (ISN.EQ.1 .OR. ISN.EQ.2 .OR. ISN.EQ.6 .OR. ISN.EQ.10) THEN
        ASIZ = 1.5*SIZ
      ELSE
        ASIZ = SIZ
      ENDIF
      LL = WMGTLN(STR,LEN(STR),0)
      CALL PLCHHQ(X,Y,STR(1:LL),ASIZ,360.,0.)       
      CALL PCGETR ('DL',XL)
      CALL PCGETR ('DR',XR)
      CALL PCGETR ('DB',YB)
      CALL PCGETR ('DT',YT)
      EXTBOX(1) = X-XL
      EXTBOX(2) = Y-YB
      EXTBOX(3) = X+XR
      EXTBOX(4) = Y+YT
C
      CALL PCSETI('TE',ITEOLD)
      CALL PCSETI('FN',NFOLD)
C
      RETURN
      END
