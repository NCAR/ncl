
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                COPYRIGHT (C,IERR)  1995                              C
C        UNIVERSITY CORPORATION FOR ATMOSPHERIC RESEARCH               C
C                ALL RIGHTS RESERVED                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      FILE:           TI01C.C
C
C      AUTHOR:         BOB LACKMAN
C          NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
C          PO 3000, BOULDER, COLORADO
C
C      DATE:           FRI JAN 06 18:31:18 MDT 1995
C
C      DESCRIPTION:    DEMONSTRATES THE TITLE OBJECT
C                      DEFAULTS.
C

	PROGRAM TI01
	IMPLICIT NONE

	EXTERNAL NHLFAPPLAYERCLASS
        EXTERNAL NHLFRESLISTLAYERCLASS
        EXTERNAL NHLFAPPLAYERCLASS
        EXTERNAL NHLFTITLELAYERCLASS
        EXTERNAL NHLFXWORKSTATIONLAYERCLASS

	INTEGER APPID, WID, PID
	INTEGER RLIST, IERR

C
C INITIALIZE THE HIGH LEVEL UTILITY LIBRARY
C

	CALL NHLFINITIALIZE

C
C CREATE AN APPLICATION CONTEXT. SET THE APP DIR TO THE CURRENT DIRECTORY
C SO THE APPLICATION LOOKS FOR A RESOURCE FILE IN THE WORKING DIRECTORY.
C IN THIS EXAMPLE THE RESOURCE FILE SUPPLIES THE PLOT TITLE ONLY.
C
        CALL NHLFRLCREATE(RLIST,'SETRL')
        CALL NHLFRLCLEAR(RLIST)
	CALL NHLFRLSETSTRING(RLIST,'appUsrDir','./',IERR)
	CALL NHLFCREATE(APPID,'ti01',NHLFAPPLAYERCLASS,
     $       0,RLIST,IERR)


C
C CREATE AN XWORKSTATION OBJECT.
C
	CALL NHLFRLCLEAR(RLIST)
	CALL NHLFRLSETINTEGER(RLIST,'wkPause','TRUE',IERR)
	CALL NHLFCREATE(WID,'ti01Work',NHLFXWORKSTATIONLAYERCLASS,
     $       0,RLIST,IERR)
C
C SPECIFY THE VIEWPORT EXTENT OF THE OBJECT.
C

        CALL NHLFRLCLEAR(RLIST)
	CALL NHLFRLSETFLOAT(RLIST,'vpXF',.2,IERR)
	CALL NHLFRLSETFLOAT(RLIST,'vpYF',.8,IERR)
	CALL NHLFRLSETFLOAT(RLIST,'vpWidthF',.6,IERR)
	CALL NHLFRLSETFLOAT(RLIST,'vpHeightF',.6,IERR)

	CALL NHLFCREATE(PID,'Titles',
     $	     NHLFTITLELAYERCLASS,WID,RLIST,IERR)

	CALL NHLFDRAW(PID,IERR)
	CALL NHLFFRAME(WID,IERR)
	CALL NHLFDESTROY(PID,IERR)
	CALL NHLFDESTROY(WID,IERR)
	CALL NHLFDESTROY(APPID,IERR)
	CALL NHLFCLOSE

	STOP
	END
