C
C	$Id: vvgetr.f,v 1.1 1992-10-12 15:31:51 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVGETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C RVAL is a real variable in which the desired value is to be returned
C by VVGETR.
C
C
C Declare all of the VELVCT common blocks.
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
C Check for a parameter name that is too short.
C
      IF (LEN(WHCH).LT.3) THEN
        CSTR(1:46)='VVGETI OR VVGETR - PARAMETER NAME TOO SHORT - '
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
            CSTR(1:46)='VVGETI OR VVGETR - GETTING XXX - PAI INCORRECT'
            CSTR(28:30)=WHCH(1:3)
            CALL SETER (CSTR(1:46),2,2)
            STOP
         END IF
      END IF
C
C Get the appropriate parameter value.
C
      IF (WHCH(1:3).EQ.'PXT'.OR.WHCH(1:3).EQ.'pxt') THEN
        RVAL=REAL(EXT)
      ELSE IF (WHCH(1:3).EQ.'VPO'.OR.WHCH(1:3).EQ.'vpo') THEN
        RVAL=REAL(ICTRFG)
      ELSE IF (WHCH(1:3).EQ.'LBL'.OR.WHCH(1:3).EQ.'lbl') THEN
        RVAL=REAL(ILAB)
      ELSE IF (WHCH(1:3).EQ.'DPF'.OR.WHCH(1:3).EQ.'dpf') THEN
        RVAL=REAL(IOFFD)
      ELSE IF (WHCH(1:3).EQ.'MGF'.OR.WHCH(1:3).EQ.'mgf') THEN
        RVAL=REAL(IOFFM)
      ELSE IF (WHCH(1:3).EQ.'AMN'.OR.WHCH(1:3).EQ.'amn') THEN
        RVAL=RMN
      ELSE IF (WHCH(1:3).EQ.'AMX'.OR.WHCH(1:3).EQ.'amx') THEN
        RVAL=RMX
      ELSE IF (WHCH(1:3).EQ.'PSZ'.OR.WHCH(1:3).EQ.'psz') THEN
        RVAL=SIDE
      ELSE IF (WHCH(1:3).EQ.'CSZ'.OR.WHCH(1:3).EQ.'csz') THEN
        RVAL=SIZE
      ELSE IF (WHCH(1:3).EQ.'PLF'.OR.WHCH(1:3).EQ.'plf') THEN
        RVAL=XLT
      ELSE IF (WHCH(1:3).EQ.'PBT'.OR.WHCH(1:3).EQ.'pbt') THEN
        RVAL=YBT
      ELSE IF (WHCH(1:3).EQ.'NLV'.OR.WHCH(1:3).EQ.'nlv') THEN
        RVAL=REAL(NLVL)
      ELSE IF (WHCH(1:3).EQ.'PAI'.OR.WHCH(1:3).EQ.'pai') THEN
        RVAL=REAL(IPAI)
      ELSE IF (WHCH(1:3).EQ.'CTV'.OR.WHCH(1:3).EQ.'ctv') THEN
        RVAL=REAL(ICTV)
      ELSE IF (WHCH(1:3).EQ.'LWD'.OR.WHCH(1:3).EQ.'lwd') THEN
        RVAL=WDLV
      ELSE IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr') THEN
        RVAL=REAL(ICLR(IPAI))
      ELSE IF (WHCH(1:3).EQ.'TVL'.OR.WHCH(1:3).EQ.'tvl') THEN
        RVAL=TVLU(IPAI)
      ELSE IF (WHCH(1:3).EQ.'GIX'.OR.WHCH(1:3).EQ.'gix') THEN
        RVAL=REAL(INCX)
      ELSE IF (WHCH(1:3).EQ.'GIY'.OR.WHCH(1:3).EQ.'giy') THEN
        RVAL=REAL(INCY)
      ELSE
        CSTR(1:46)='VVGETI OR VVGETR - PARAMETER NAME NOT KNOWN - '
        CSTR(47:49)=WHCH(1:3)
        CALL SETER (CSTR(1:49),3,2)
        STOP
      END IF
C
 9900 CONTINUE
C
C Done.
C
      RETURN
C
      END
