C
C	$Id: gcelcd.f,v 1.1 1993-01-09 01:57:49 fred Exp $
C
        SUBROUTINE GCELCD(WKID,IOS,STATUS)
C
C  Process the control elements.
C
C  At present the working control elements include:
C     VDC INTEGER PRECISION
C     CLIP RECTANGLE
C     CLIP INDICATOR
C  All other control elements will be ignored.
C
      include 'trinst.h'
      include 'trpars.h'
      include 'trstat.h'
      include 'trcode.h'
C
      INTEGER WKID, IOS, STATUS
      INTEGER TMP, XMIN, XMAX, YMIN, YMAX, CLPDAT(4)
C
      STATUS = 0
C
      IF (OPID .EQ. CELMVI) THEN
C
C  VDC integer precision.
C
        CALL GOPDEC(MOPLEN,MWHCPR,1,IOS,STATUS)
      ELSE IF (OPID .EQ. CELMCR) THEN
C
C  Set the clip rectangle for normalization transformation 1 that
C  we are currently using.
C
        CALL GOPDEC(CLPDAT,MOPLEN,4,IOS,STATUS)
        XMIN = CLPDAT(1)
        XMIN = MIN(XMIN,CLPDAT(3))
        XMAX = CLPDAT(3)
        XMAX = MAX(XMAX,CLPDAT(1))
        YMIN = CLPDAT(2)
        YMIN = MIN(YMIN,CLPDAT(4))
        YMAX = CLPDAT(4)
        YMAX = MAX(YMAX,CLPDAT(2))
        CALL GSWN(1,REAL(XMIN)/32767.,REAL(XMAX)/32767.,
     +                 REAL(YMIN)/32767.,REAL(YMAX)/32767.)
        CALL GSVP(1,REAL(XMIN)/32767.,REAL(XMAX)/32767.,
     +                 REAL(YMIN)/32767.,REAL(YMAX)/32767.)
C
      ELSE IF (OPID.EQ.CELMCI) THEN
C
C  Set the clipping indicator.
C
        CALL GOPDEC(TMP,MENCPR,1,IOS,STATUS)
        CALL GSCLIP(TMP)
      END IF
C
C  Check the instruction status.
C
      IF (STATUS .NE. 0) RETURN
C
C  If the instruction is continued, then loop to get the 
C  full operand list.
C
  10  CONTINUE
      IF (CNTINU) THEN
        CALL GNPART(IOS,STATUS) 
        IF (STATUS .NE. 0) RETURN
        GO TO 10
      END IF 
C
      RETURN
      END
