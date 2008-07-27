C
C	$Id: gdsg.f,v 1.9 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GDSG(SGNA)
C
C  DELETE SEGMENT
C
      INTEGER EDSG
      PARAMETER (EDSG=59)
C
      include 'gkscom.h'
C
      INTEGER SGNA
      CHARACTER*80 CSNAME
C
C  This subroutine is here solely as support for the SPPS GFLASn
C  entries.  Full segmentation is not a part of the NCAR GKS
C  package at this time.  The NCAR package is non-standard to the
C  extent that certain segmentation functions are supported, but
C  not all level 1 functions are supported.  This subroutine should
C  be considered a user entry point only by way of the GFLASn
C  calls--it should never be called directly be the user.
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,EDSG,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the segment name is valid.
C
      IF (SGNA.LT.0 .OR. SGNA.GT.99) THEN
        ERS = 1
        CALL GERHND(120,EDSG,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the segment exists.
C
      DO 200 I=1,NUMSEG
        IF (SEGS(I) .EQ. SGNA) GO TO 210
  200 CONTINUE
      ERS = 1
      CALL GERHND(122,EDSG,ERF)
      ERS = 0
      RETURN
  210 CONTINUE
C
C  Check if the segment is open.
C
      IF (SGNA .EQ. CURSEG) THEN
        ERS = 1
        CALL GERHND(125,EDSG,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Remove the segment name from the list of segment names in use 
C  and readjust the associated segment description arrays.
C
      IF (NUMSEG .GT. 0) THEN
        DO 201 I=1,NUMSEG
          IF (SEGS(I) .EQ. SGNA) THEN
C
            CSNAME = ' '
            CSNAME = SEGNAM(I)
            IP1 = I+1
            DO 202 J=IP1,NUMSEG
              SEGS(J-1) = SEGS(J)
              SEGNAM(J-1) = SEGNAM(J)
              SEGLEN(J-1) = SEGLEN(J)
              DO 205 IR=1,2
                DO 206 JC=1,3
                  SEGT(J-1,IR,JC) = SEGT(J,IR,JC)
  206           CONTINUE
  205         CONTINUE
  202       CONTINUE
            SEGS(NUMSEG) = 0
            SEGNAM(NUMSEG) = ' '
            SEGLEN(NUMSEG) = 0
            DO 203 IR=1,2
              DO 204 JC=1,3
                SEGT(NUMSEG,IR,JC) = 0.
  204         CONTINUE
  203       CONTINUE
            SEGT(NUMSEG,1,1) = 1.
            SEGT(NUMSEG,2,2) = 1.
            NUMSEG = NUMSEG-1
            GO TO 10
          ENDIF
  201   CONTINUE
      ENDIF
   10 CONTINUE
C
C  Make the interface call.
C
      FCODE = 79
      CALL GZROI(0)
      CONT  = 0
      STRL1 = 80
      STRL2 = 80
      STR = CSNAME
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EDSG,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
