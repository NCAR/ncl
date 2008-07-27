C
C	$Id: condet.f,v 1.4 2008-07-27 00:16:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONDET (NDP,XD,YD,NCP,IPC)
C
C******************************************************************
C*                                                                *
C*   THIS FILE IS A PACKAGE OF SUPPORT ROUTINES FOR THE ULIB      *
C*   FILES CONRAN , CONRAQ AND CONRAS.  SEE THOSE FILES FOR AN    *
C*   EXPLAINATION OF THE ENTRY POINTS.                            *
C*                                                                *
C******************************************************************
C
C THIS SUBROUTINE SELECTS SEVERAL DATA POINTS THAT ARE CLOSEST
C TO EACH OF THE DATA POINT.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD,YD = ARRAYS CONTAINING THE X AND Y COORDINATES
C             OF DATA POINTS,
C     NCP = NUMBER OF DATA POINTS CLOSEST TO EACH DATA
C           POINTS.
C THE OUTPUT PARAMETER IS
C     IPC = INTEGER ARRAY OF DIMENSION NCP*NDP, WHERE THE
C           POINT NUMBERS OF NCP DATA POINTS CLOSEST TO
C           EACH OF THE NDP DATA POINTS ARE TO BE STORED.
C THIS SUBROUTINE ARBITRARILY SETS A RESTRICTION THAT NCP MUST
C NOT EXCEED 25 WITHOUT MODIFICATION TO THE ARRAYS DSQ0 AND IPC0.
C DECLARATION STATEMENTS
C
      COMMON /CONRA3/ IREC
      DIMENSION       XD(NDP)    ,YD(NDP)    ,IPC(1)
      DIMENSION       DSQ0(25)   ,IPC0(25)
C
        SAVE
C
C STATEMENT FUNCTION
C
      DSQF(U1,V1,U2,V2) = (U2-U1)**2+(V2-V1)**2
C
C CALCULATION
C
      DO  220 IP1=1,NDP
C
C - SELECTS NCP POINTS.
C
         X1 = XD(IP1)
         Y1 = YD(IP1)
         J1 = 0
         DSQMX = 0.0
         DO  110 IP2=1,NDP
            IF (IP2 .EQ. IP1) GO TO  110
            DSQI = DSQF(X1,Y1,XD(IP2),YD(IP2))
            J1 = J1+1
            DSQ0(J1) = DSQI
            IPC0(J1) = IP2
            IF (DSQI .LE. DSQMX) GO TO  100
            DSQMX = DSQI
            JMX = J1
  100       IF (J1 .GE. NCP) GO TO  120
  110    CONTINUE
  120    IP2MN = IP2+1
         IF (IP2MN .GT. NDP) GO TO  150
         DO  140 IP2=IP2MN,NDP
            IF (IP2 .EQ. IP1) GO TO  140
            DSQI = DSQF(X1,Y1,XD(IP2),YD(IP2))
            IF (DSQI .GE. DSQMX) GO TO  140
            DSQ0(JMX) = DSQI
            IPC0(JMX) = IP2
            DSQMX = 0.0
            DO  130 J1=1,NCP
               IF (DSQ0(J1) .LE. DSQMX) GO TO  130
               DSQMX = DSQ0(J1)
               JMX = J1
  130       CONTINUE
  140    CONTINUE
C
C - CHECKS IF ALL THE NCP+1 POINTS ARE COLLINEAR.
C
  150    IP2 = IPC0(1)
         DX12 = XD(IP2)-X1
         DY12 = YD(IP2)-Y1
         DO  160 J3=2,NCP
            IP3 = IPC0(J3)
            DX13 = XD(IP3)-X1
            DY13 = YD(IP3)-Y1
            IF ((DY13*DX12-DX13*DY12) .NE. 0.0) GO TO  200
  160    CONTINUE
C
C - SEARCHES FOR THE CLOSEST NONCOLLINEAR POINT.
C
         NCLPT = 0
         DO  190 IP3=1,NDP
            IF (IP3 .EQ. IP1) GO TO  190
            DO  170 J4=1,NCP
               IF (IP3 .EQ. IPC0(J4)) GO TO  190
  170       CONTINUE
            DX13 = XD(IP3)-X1
            DY13 = YD(IP3)-Y1
            IF ((DY13*DX12-DX13*DY12) .EQ. 0.0) GO TO  190
            DSQI = DSQF(X1,Y1,XD(IP3),YD(IP3))
            IF (NCLPT .EQ. 0) GO TO  180
            IF (DSQI .GE. DSQMN) GO TO  190
  180       NCLPT = 1
            DSQMN = DSQI
            IP3MN = IP3
  190    CONTINUE
         DSQMX = DSQMN
         IPC0(JMX) = IP3MN
C
C - REPLACES THE LOCAL ARRAY FOR THE OUTPUT ARRAY.
C
  200    J1 = (IP1-1)*NCP
         DO  210 J2=1,NCP
            J1 = J1+1
            IPC(J1) = IPC0(J2)
  210    CONTINUE
  220 CONTINUE
      RETURN
      END
