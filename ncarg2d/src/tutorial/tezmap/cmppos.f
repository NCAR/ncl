C
C	$Id: cmppos.f,v 1.2 1994-07-08 21:39:48 stautler Exp $
C
        PROGRAM CMPPOS
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Draw map in lower lefthand corner of the window
C
        CALL MAPPOS (0.5, 1.0, 0.0, 0.5)
        CALL MAPDRW
C
C Draw a perimeter around the viewport
C
        CALL GSELNT(0)
        CALL PLOTIF (0.0,0.0,0)
        CALL PLOTIF (1.0,0.0,1)
        CALL PLOTIF (1.0,1.0,1)
        CALL PLOTIF (0.0,1.0,1)
        CALL PLOTIF (0.0,0.0,1)

        CALL FRAME
C
C Deactivate and close the workstation, close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS

        STOP
        END
