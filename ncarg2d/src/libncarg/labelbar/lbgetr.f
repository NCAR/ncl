C
C	$Id: lbgetr.f,v 1.1.1.1 1992-04-17 22:32:57 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LBGETR (WHCH,RVAL)
C
        CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C RVAL is a real variable in which the desired value is to be returned
C by LBGETR.
C
C
C Declare the common block where internal parameters are stored.
C
        COMMON /LBCOMN/ ICBL,ICFL,ICLB,WOBL,WOFL,WOLB
        SAVE   /LBCOMN/
C
C Declare the block data routine external to force it to load.
C
        EXTERNAL LBBLDA
C
C Define a character temporary for use in forming error messages.
C
        CHARACTER*50 CTMP
C
C Check for a parameter name that is too short.
C
        IF (LEN(WHCH).LT.3) THEN
          CTMP(1:46)='LBGETI OR LBGETR - PARAMETER NAME TOO SHORT - '
          CTMP(47:46+LEN(WHCH))=WHCH
          CALL SETER (CTMP(1:46+LEN(WHCH)),1,2)
          STOP
        END IF
C
C Get the appropriate parameter value.
C
        IF (WHCH(1:3).EQ.'CBL') THEN
          RVAL=REAL(ICBL)
        ELSE IF (WHCH(1:3).EQ.'CFL') THEN
          RVAL=REAL(ICFL)
        ELSE IF (WHCH(1:3).EQ.'CLB') THEN
          RVAL=REAL(ICLB)
        ELSE IF (WHCH(1:3).EQ.'WBL') THEN
          RVAL=WOBL
        ELSE IF (WHCH(1:3).EQ.'WFL') THEN
          RVAL=WOFL
        ELSE IF (WHCH(1:3).EQ.'WLB') THEN
          RVAL=WOLB
        ELSE
          CTMP(1:46)='LBGETI OR LBGETR - PARAMETER NAME NOT KNOWN - '
          CTMP(47:49)=WHCH(1:3)
          CALL SETER (CTMP(1:49),3,2)
          STOP
        END IF
C
C Done.
C
        RETURN
C
      END
