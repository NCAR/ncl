C
C	$Id: cezmap1.f,v 1.1 1993-01-13 17:59:22 haley Exp $
C
C
C Open GKS, Turn Clipping off
C
      CALL OPNGKS 
C
C INVOKE DEMO DRIVER
C
      CALL CEZMAP('SV',40.,-50.,0.,'PO')
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL CLSGKS

	STOP
	END

      SUBROUTINE CEZMAP(PROJ,PLAT,PLON,ROTA)

	CHARACTER*2 PROJ, OUTLN
	REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)

	DATA PLIM1 /0.,0./
	DATA PLIM2 /0.,0./
	DATA PLIM3 /0.,0./
	DATA PLIM4 /0.,0./
C
C CMPLOT demonstrates MAPLOT drawing continental and political outlines
C
C Set up Maps.
C
C
C Draw Continental, political outlines 
C
        CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PO')
C
C Set up projection
C
	CALL MAPROJ (PROJ,PLAT,PLON,ROTA)
C
C If it's a satellite projection, choose a satellite distance
C
	IF (PROJ.EQ.'SV') CALL MAPSTR ('SA - SATELLITE DISTANCE',5.)
C
C Set limits of map
C
        CALL MAPSET ('MA',PLIM1,PLIM2,PLIM3,PLIM4)
C
C Draw map
C
	CALL MAPDRW
C
C Advance the frame.
C
        CALL FRAME
C
C Done.
C
        RETURN
	END
