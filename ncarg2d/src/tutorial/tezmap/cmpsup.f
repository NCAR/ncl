C
C	$Id: cmpsup.f,v 1.2 1994-07-08 21:39:49 stautler Exp $
C
	PROGRAM CMPSUP
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)

	REAL PLIM1(2),PLIM2(2),PLIM3(2),PLIM4(2)
	DATA PLIM1 /0.0, 0.0/
	DATA PLIM2 /0.0, 0.0/
	DATA PLIM3 /0.0, 0.0/
	DATA PLIM4 /0.0, 0.0/

C
C Open GKS, open and activate a workstation.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)

	CALL SUPMAP(7,0.,0.,0.,PLIM1,PLIM2,PLIM3,PLIM4,1,5,0,0,IERR)
	CALL FRAME
C
C Deactivate and close the workstation, close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS

	STOP
	END
