C
C	$Id: mskrv2.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MSKRV2 (T,XS,YS,N,X,Y,XP,YP,S,SIGMA,ICS,SLP)
C
      DIMENSION X(N),Y(N),XP(N),YP(N),S(N)
C
C ---------------------------------------------------------------------
C Note:  This routine comes from a proprietary package called FITPACK.
C It is used in the NCAR graphics package by permission of the author,
C Alan Cline.
C ---------------------------------------------------------------------
C
C                                            CODED BY ALAN KAYLOR CLINE
C                                         FROM FITPACK -- JUNE 22, 1986
C                                   A CURVE AND SURFACE FITTING PACKAGE
C                                 A PRODUCT OF PLEASANT VALLEY SOFTWARE
C                             8603 ALTUS COVE, AUSTIN, TEXAS 78759, USA
C
C ---------------------------------------------------------------------
C
C THIS SUBROUTINE PERFORMS THE MAPPING OF POINTS IN THE
C INTERVAL (0.,1.) ONTO A CURVE IN THE PLANE. THE SUBROUTINE
C MSKRV1 SHOULD BE CALLED EARLIER TO DETERMINE CERTAIN
C NECESSARY PARAMETERS. THE RESULTING CURVE HAS A PARAMETRIC
C REPRESENTATION BOTH OF WHOSE COMPONENTS ARE SPLINES UNDER
C TENSION AND FUNCTIONS OF THE POLYGONAL ARCLENGTH
C PARAMETER.
C
C ON INPUT--
C
C   T CONTAINS A REAL VALUE TO BE MAPPED TO A POINT ON THE
C   CURVE. THE INTERVAL (0.,1.) IS MAPPED ONTO THE ENTIRE
C   CURVE, WITH 0. MAPPING TO (X(1),Y(1)) AND 1. MAPPING
C   TO (X(N),Y(N)). VALUES OUTSIDE THIS INTERVAL RESULT IN
C   EXTRAPOLATION.
C
C   N CONTAINS THE NUMBER OF POINTS WHICH WERE SPECIFIED
C   TO DETERMINE THE CURVE.
C
C   X AND Y ARE ARRAYS CONTAINING THE X- AND Y-COORDINATES
C   OF THE SPECIFIED POINTS.
C
C   XP AND YP ARE THE ARRAYS OUTPUT FROM MSKRV1 CONTAINING
C   CURVATURE INFORMATION.
C
C   S IS AN ARRAY CONTAINING THE POLYGONAL ARCLENGTHS OF
C   THE CURVE.
C
C   SIGMA CONTAINS THE TENSION FACTOR (ITS SIGN IS IGNORED).
C
C   ICS IS A FLAG, NON-ZERO IF THE ANGLE OF THE SLOPE IS TO
C   BE COMPUTED AND RETURNED AS WELL.
C
C THE PARAMETERS N, X, Y, XP, YP, S, AND SIGMA SHOULD BE
C INPUT UNALTERED FROM THE OUTPUT OF MSKRV1.
C
C ON OUTPUT--
C
C   XS AND YS CONTAIN THE X- AND Y-COORDINATES OF THE IMAGE
C   POINT ON THE CURVE.
C
C   IF ICS IS NON-ZERO, SLP CONTAINS THE ANGLE (IN DEGREES)
C   OF THE SLOPE AT THE POINT.
C
C NONE OF THE INPUT PARAMETERS ARE ALTERED.
C
C THIS SUBROUTINE REFERENCES PACKAGE MODULES MSINTR AND
C MSSHCH.
C
C-----------------------------------------------------------
C
C THE INDEX I IS SAVED FROM CALL TO CALL FOR THE SAKE OF
C EFFICIENCY
C
      SAVE I
      DATA I /1/
C
C DETERMINE INTERVAL
C
      TN = S(N)*T
C
      IF (I .GE. N) I = N/2
C
      IF (TN .LT. S(I)) THEN
        IF (TN .LE. S(2)) THEN
          I = 1
          GO TO 2
        ELSE
          IL = 2
          IH = I
        END IF
      ELSE IF (TN .LE. S(I+1)) THEN
        GO TO 2
      ELSE IF (TN .GE. S(N-1)) THEN
        I = N-1
        GO TO 2
      ELSE
        IL = I+1
        IH = N-1
      END IF
C
    1 I = (IL+IH)/2
      IF (TN .LT. S(I)) THEN
        IH = I
      ELSE IF (TN .GT. S(I+1)) THEN
        IL = I+1
      ELSE
        GO TO 2
      END IF
      GO TO 1
C
    2 IP1 = I+1
C
C DENORMALIZE TENSION FACTOR
C
      SIGMAP = ABS(SIGMA)*REAL(N-1)/S(N)
C
C SET UP AND PERFORM INTERPOLATION
C
      DEL1 = TN-S(I)
      DEL2 = S(IP1)-TN
      DELS = S(IP1)-S(I)
      SUMX = (X(IP1)*DEL1+X(I)*DEL2)/DELS
      SUMY = (Y(IP1)*DEL1+Y(I)*DEL2)/DELS
      IF (SIGMAP .EQ. 0.) THEN
        D = DEL1*DEL2/(6.*DELS)
        C1 = (DEL1+DELS)*D
        C2 = (DEL2+DELS)*D
        XS = SUMX-XP(IP1)*C1-XP(I)*C2
        YS = SUMY-YP(IP1)*C1-YP(I)*C2
        IF (ICS.NE.0) THEN
          DD = (DEL2-DEL1)/(6.*DELS)
          DC1 = (DEL1+DELS)*DD+D
          DC2 = (DEL2+DELS)*DD-D
          XT = (X(IP1)-X(I))/DELS-XP(IP1)*DC1-XP(I)*DC2
          YT = (Y(IP1)-Y(I))/DELS-YP(IP1)*DC1-YP(I)*DC2
          SLP = 57.2957795130823*ATAN2(YT,XT)
        END IF
      ELSE
        SIGDEL = SIGMAP*DELS
        CALL MSSHCH(SS,DUMMY,SIGDEL,-1)
        CALL MSSHCH(S1,DUMMY,SIGMAP*DEL1,-1)
        CALL MSSHCH(S2,DUMMY,SIGMAP*DEL2,-1)
        D = SIGDEL*SIGMAP*(1.+SS)
        C1 = DEL1*(S1-SS)/D
        C2 = DEL2*(S2-SS)/D
        XS = SUMX+XP(IP1)*C1+XP(I)*C2
        YS = SUMY+YP(IP1)*C1+YP(I)*C2
        IF (ICS.NE.0) THEN
          CALL MSSHCH (DUMMY,HC1,SIGMAP*DEL1,1)
          CALL MSSHCH (DUMMY,HC2,SIGMAP*DEL2,1)
          DC1 = (HC1-SS)/D
          DC2 = (SS-HC2)/D
          XT = (X(IP1)-X(I))/DELS+XP(IP1)*DC1+XP(I)*DC2
          YT = (Y(IP1)-Y(I))/DELS+YP(IP1)*DC1+YP(I)*DC2
          SLP = 57.2957795130823*ATAN2(YT,XT)
        END IF
      END IF
      RETURN
      END
