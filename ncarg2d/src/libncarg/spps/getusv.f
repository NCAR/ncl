C
C	$Id: getusv.f,v 1.1.1.1 1992-04-17 22:32:30 ncargd Exp $
C
      SUBROUTINE GETUSV (VN,IV)
      CHARACTER*(*) VN
C
C This subroutine retrieves the current values of the utility state
C variables.  VN is the character name of the variable and IV is
C its value.
C
C The labelled common block IUTLCM contains all of the utility state
C variables.
C
      COMMON /IUTLCM/IU(100)
      SAVE /IUTLCM/
C
C Check for the linear-log scaling variable.
C
      IF (VN(1:2).EQ.'LS') THEN
        IV=IU(1)
C
C Check for the variable specifying the mirror-imaging of the axes.
C
      ELSE IF (VN(1:2).EQ.'MI') THEN
        IV=IU(2)
C
C Check for the variable specifying the resolution of the plotter in x.
C
      ELSE IF (VN(1:2).EQ.'XF') THEN
        IV=IU(3)
C
C Check for the variable specifying the resolution of the plotter in x.
C
      ELSE IF (VN(1:2).EQ.'YF') THEN
        IV=IU(4)
C
C Check for the variable specifying the size of the pen-move buffer.
C
      ELSE IF (VN(1:2).EQ.'PB') THEN
        IV=IU(5)
C
C Check for the variable specifying the metacode unit.
C
      ELSE IF (VN(1:2).EQ.'MU') THEN
        IV=IABS(IU(6))
C
C Check for one of the variables specifying color and intensity.
C
      ELSE IF (VN(1:2).EQ.'IR') THEN
        IV=IU(7)
C
      ELSE IF (VN(1:2).EQ.'IG') THEN
        IV=IU(8)
C
      ELSE IF (VN(1:2).EQ.'IB') THEN
        IV=IU(9)
C
      ELSE IF (VN(1:2).EQ.'IN') THEN
        IV=IU(10)
C
C Check for the variable specifying the current color index.
C
      ELSE IF (VN(1:2).EQ.'II') THEN
        IV=IU(11)
C
C Check for the variable specifying the maximum color index.
C
      ELSE IF (VN(1:2).EQ.'IM') THEN
        IV=IU(12)
C
C Check for the variable specifying the line width scale factor.
C
      ELSE IF (VN(1:2).EQ.'LW') THEN
        IV=IU(13)
C
C Check for the variable specifying the marker size scale factor.
C
      ELSE IF (VN(1:2).EQ.'MS') THEN
        IV=IU(14)
C
C Otherwise, the variable name is unknown.
C
      ELSE
        CALL SETER ('GETUSV - UNKNOWN VARIABLE NAME IN CALL',1,2)
C
      ENDIF
C
      RETURN
C
      END
