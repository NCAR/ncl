C
C	$Id: cmppos.f,v 1.1 1992-09-29 16:10:16 ncargd Exp $
C
        PROGRAM CMPPOS
C
C Open GKS.
C
        CALL OPNGKS
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
        CALL CLSGKS

        STOP
        END
