C
C	$Id: vvsetr.f,v 1.1 1992-10-12 15:32:51 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVSETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to set the real value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be set.
C
C RVAL is a real variable containing the new value of the parameter.
C
C Declare all the VELVCT common blocks
C
      PARAMETER (IPLVLS = 64)
C
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB       ,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY        ,
     +                RMN        ,RMX        ,SIDE       ,SIZE       ,
     +                XLT        ,YBT        ,ZMN        ,ZMX        ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON /VEC2/   BIG        ,INCX       ,INCY
C
C Character variable declartions
C
      CHARACTER*140 CSTR
C
      COMMON /VVCHAR/ CSTR
C 
      SAVE /VEC1/, /VEC2/, /VVCHAR/
C
C
C Check for a parameter name that is too short.
C
      IF (LEN(WHCH).LT.3) THEN
        CSTR(1:46)='VVSETI OR VVSETR - PARAMETER NAME TOO SHORT - '
        CSTR(47:46+LEN(WHCH))=WHCH
        CALL SETER (CSTR(1:46+LEN(WHCH)),1,2)
        STOP
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr'
     +    .OR.WHCH(1:3).EQ.'TVL'.OR.WHCH(1:3).EQ.'tvl') THEN
         IF (IPAI.LT.1.OR.IPAI.GT.NLVL) THEN
            CSTR(1:46)='VVSETI OR VVSETR - SETTING XXX - PAI INCORRECT'
            CSTR(28:30)=WHCH(1:3)
            CALL SETER (CSTR(1:46),2,2)
            STOP
         END IF
      END IF
C
C Set the appropriate parameter value.
C
      IF (WHCH(1:3).EQ.'PXT'.OR. WHCH(1:3).EQ.'pxt') THEN
         IF (RVAL .LE. 0.0) GO TO 9800
         EXT=RVAL
      ELSE IF (WHCH(1:3).EQ.'VPO'.OR. WHCH(1:3).EQ.'vpo') THEN
         ICTRFG=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'LBL'.OR. WHCH(1:3).EQ.'lbl') THEN
         ILAB=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'DPF'.OR. WHCH(1:3).EQ.'dpf') THEN
         IOFFD=INT(RVAL) 
      ELSE IF (WHCH(1:3).EQ.'MGF'.OR.WHCH(1:3).EQ.'mgf') THEN
         IOFFM=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'AMN'.OR.WHCH(1:3).EQ.'amn') THEN
         IF (RVAL .LE. 0.0) GO TO 9800
         RMN=RVAL
      ELSE IF (WHCH(1:3).EQ.'AMX'.OR.WHCH(1:3).EQ.'amx') THEN
         IF (RVAL .LE. 0.0) GO TO 9800
         RMX=RVAL
      ELSE IF (WHCH(1:3).EQ.'PSZ'.OR.WHCH(1:3).EQ.'psz') THEN
         IF (RVAL .LE. 0.0) GO TO 9800
         SIDE=RVAL
      ELSE IF (WHCH(1:3).EQ.'CSZ'.OR.WHCH(1:3).EQ.'csz') THEN
         IF (RVAL .LE. 0.0) GO TO 9800
         SIZE=RVAL
      ELSE IF (WHCH(1:3).EQ.'PLF'.OR.WHCH(1:3).EQ.'plf') THEN
         IF (RVAL .LT. 0.0) GO TO 9800
         XLT=RVAL
      ELSE IF (WHCH(1:3).EQ.'PBT'.OR.WHCH(1:3).EQ.'pbt') THEN
         IF (RVAL .LT. 0.0) GO TO 9800
         YBT=RVAL
      ELSE IF (WHCH(1:3).EQ.'NLV'.OR.WHCH(1:3).EQ.'nlv') THEN
         NLVL=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'PAI'.OR.WHCH(1:3).EQ.'pai') THEN
         IF (RVAL .LT. 1.0 .OR. RVAL .GT. NLVL) GO TO 9800
         IPAI=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'CTV'.OR.WHCH(1:3).EQ.'ctv') THEN
         ICTV=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'LWD'.OR.WHCH(1:3).EQ.'lwd') THEN
         IF (RVAL .LE. 0.0) GO TO 9800
         WDLV=RVAL
      ELSE IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr') THEN
         ICLR(IPAI)=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'TVL'.OR.WHCH(1:3).EQ.'tvl') THEN
         TVLU(IPAI)=RVAL
      ELSE IF (WHCH(1:3).EQ.'GIX'.OR.WHCH(1:3).EQ.'gix') THEN
        IF (RVAL .LT. 1.0) GO TO 9800
        INCX=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'GIY'.OR.WHCH(1:3).EQ.'giy') THEN
        IF (RVAL .LT. 1.0) GO TO 9800
        INCY=INT(RVAL)
      ELSE
        CSTR(1:46)='VVGETI OR VVGETR - PARAMETER NAME NOT KNOWN - '
        CSTR(47:49)=WHCH(1:3)
        CALL SETER (CSTR(1:49),3,2)
        STOP
      END IF
C
      GOTO 9900
C
 9800 CONTINUE
C
      CSTR(1:46)='VVGETI OR VVGETR - PARAMETER VALUE OUT OF RANGE - '
      CSTR(50:52)=WHCH(1:3)
      CALL SETER (CSTR(1:50),3,2)
      STOP
C      
 9900 CONTINUE
C
C Done.
C
      RETURN
C
      END
