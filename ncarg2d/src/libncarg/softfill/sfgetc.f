C
C $Id: sfgetc.f,v 1.4 1994-03-17 20:58:34 kennison Exp $
C
      SUBROUTINE SFGETC (CNP,CVP)
C
      CHARACTER*(*) CNP,CVP
C
C This subroutine is called to retrieve the character value of a
C specified parameter.
C
C CNP is the name of the parameter whose value is to be retrieved.
C
C CVP is a character variable in which the desired value is to be
C returned by SFGETC.
C
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,LCH,LDP(8,8)
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SFBLDA
C
C Declare a local character variable in which to form an error message.
C
      CHARACTER*38 CTM
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFGETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (.NOT.(LEN(CNP).LT.2)) GO TO 10001
      CTM(1:36)='SFGETC - PARAMETER NAME TOO SHORT - '
      CTM(37:36+LEN(CNP))=CNP
      CALL SETER (CTM(1:36+LEN(CNP)),2,1)
      RETURN
10001 CONTINUE
C
C Get the appropriate parameter value.
C
      IF (.NOT.(CNP(1:2).EQ.'CH'.OR.CNP(1:2).EQ.'ch')) GO TO 10002
      IF (.NOT.(LCH.GT.0)) GO TO 10003
      CVP=CHAR(LCH)
      GO TO 10004
10003 CONTINUE
      CVP=' '
10004 CONTINUE
      GO TO 10005
10002 CONTINUE
      CTM(1:36)='SFGETC - PARAMETER NAME NOT KNOWN - '
      CTM(37:38)=CNP(1:2)
      CALL SETER (CTM(1:38),3,1)
      RETURN
10005 CONTINUE
C
C Done.
C
      RETURN
C
      END
