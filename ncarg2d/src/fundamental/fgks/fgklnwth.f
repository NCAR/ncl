
      PROGRAM FGKLNWTH
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL LINEEX (IWKID)
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
      SUBROUTINE LINEEX (IWKID)
C
C PURPOSE                To provide a simple demonstration of 
C                        how to change line width.
C
C USAGE                  CALL LINEEX (IWKID) 
C
C ARGUMENTS
C
C ON INPUT               IWKID
C                          Workstation id
C
C LANGUAGE               FORTRAN
C
C PORTABILITY            FORTRAN 77
C
C NOTE                   The call to GOPWK will have to be modified
C                        when using a non-NCAR GKS package.  The third
C                        argument must be the workstation type for WISS.
C
C

C  Data for the graphical objects.
C
      DATA    TWOPI/6.283185/
      DATA    X0P, Y0P, RP, NPTP/ 0.500, 0.5, 0.40 ,  16/
C
C Declare the constant for converting from degrees to radians.
C
      DATA DTR / .017453292519943 /
C
C Establish the viewport and window.
C
      CALL SET(.01,.99,0.01,.99,0.,1.,0.,1.,1)
C
C Turn buffering off
C
      CALL SETUSV('PB',2)
C
C Set up a color table
C
C
C White background
C
      CALL GSCR (IWKID,0,1.,1.,1.)
C
C Black foreground
C
      CALL GSCR (IWKID,1,0.,0.,0.)
C
C Red
C
      CALL GSCR (IWKID,2,1.,0.,0.)
C
C Green
C
      CALL GSCR (IWKID,3,0.,1.,0.)
C
C Blue
C
      CALL GSCR (IWKID,4,0.,0.,1.)
C
C Create a polygonal fan 
C
      DTHETA = TWOPI/NPTP
      DO 10 I=1,16
         ANG = DTHETA*REAL(I) + .19625
         XC = RP*COS(ANG)
         YC = RP*SIN(ANG)
C
C Set the line color
C 
         CALL GSPLCI (1)
C
C Set line width 
C 
         CALL GSLWSC(REAL(I)) 
C
C Draw a line
C
         CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
 10   CONTINUE
      
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Set the line color to black
C
      CALL GSPLCI (1)
C
C Create a background perimeter 
C
      CALL FRSTPT( 0.0, 0.0)
      CALL VECTOR( 1.0, 0.0)
      CALL VECTOR( 1.0, 1.0)
      CALL VECTOR( 0.0, 1.0)
      CALL VECTOR( 0.0, 0.0)
C
C Label the plot
C
      CALL PLCHLQ(0.5,0.91,'Changing Line Width',25.,0.,0.)
C
C Advance the frame
C
      CALL FRAME
C
      END
