
      PROGRAM TEZMAP
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
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL EZMAP(IERR)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE EZMAP (IERROR)
C
C PURPOSE                To provide a simple demonstration of
C                        the mapping utility, EZMAP.
C
C USAGE                  CALL EZMAP (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, the test was not successful.
C
C I/O                    The map dataset must be connected to a
C                        FORTRAN unit.  See the open statement in
C                        this test deck.
C
C                        If the test is successful, the message
C
C              EZMAP TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 10
C                        frames are produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plots.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      EZMAP, GRIDAL
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              TEZMAP calls routine MAPROJ once for each of
C                        the 10 types of projections:
C                          stereographic,
C                          orthographic,
C                          Lambert conformal conic with 2 standard
C                            parallels,
C                          Lambert equal area,
C                          gnomonic,
C                          azimuthal equidistant,
C                          cylindrical equidistant,
C                          Mercator,
C                          Mollweide type, and
C                          satellite view.
C
C HISTORY                Written October, 1976, main entry SUPMAP.
C                        Updated September, 1985, main entry EZMAP.
C
      DIMENSION PLM1(2),PLM2(2),PLM3(2),PLM4(2)
      COMMON /ERROR/ IFRAME, IERRR
C
      DATA PLM1,PLM2,PLM3,PLM4 /8*0.0/
C
C Define the center of a plot title string on a square grid of size
C 0. to 1.
C
      DATA TX/0.5/, TY/0.9765/
C
C Initialize the error parameter.
C
      IERRR = 0
C
C Turn on the error recovery mode.
C
      CALL ENTSR(IDUM,1)
C
C
C     Frame 1 -- The stereographic projection.
C
       IFRAME = 1
C
C Set the projection-type parameter.
C
      CALL MAPROJ('ST',80.0,-160.0,0.0)
C
C Set the limit parameters.
C
      CALL MAPSET('MA',PLM1,PLM2,PLM3,PLM4)
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC('OU','PS')
C
C Draw the map.
C
      CALL MAPDRW
C
C Report any errors encountered.
C
       IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,'EZMAP DEMONSTRATION:  STEREOGRAPHIC PROJECTION'
     +           ,16.,0.,0.)
      CALL FRAME
 10   CONTINUE
C
C
C     Frame 2 -- The orthographic projection.
C
      IFRAME = 2
C
C
C Set the projection-type parameter.
C
      CALL MAPROJ('OR',60.0,-120.0,0.0)
C
C Draw the map.
C
      CALL MAPDRW
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Write the title.
C
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,
     +     'EZMAP DEMONSTRATION:  ORTHOGRAPHIC PROJECTION',16.,0.,0.)
      CALL FRAME
 20   CONTINUE
C
C
C     Frame 3 -- The Lambert conformal conic projection.
C
      IFRAME = 3
C
C
C Set the projection-type, limits, and outline-dataset parameters.
C
      CALL MAPROJ('LC',45.0,-100.0,45.0)
      CALL MAPSET('CO',50.0,-130.0,20.0,-75.0)
      CALL MAPSTC('OU','US')
C
C Draw the map.
C
      CALL MAPDRW
C
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,
     +     'EZMAP DEMONSTRATION: LAMBERT CONFORMAL CONIC PROJECTION',
     +           16.,0.,0.)
      CALL FRAME
 30   CONTINUE
C
C
C     Frame 4 -- The Lambert equal area projection.
C
      IFRAME = 4
C
C Set the projection-type, limits, and outline-dataset parameters.
C
      CALL MAPROJ('LE',20.0,-40.0,0.0)
      CALL MAPSET('MA',PLM1,PLM2,PLM3,PLM4)
      CALL MAPSTC('OU','CO')
C
C Draw the map.
C
      CALL MAPDRW
C
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,
     +     'EZMAP DEMONSTRATION:  LAMBERT EQUAL AREA PROJECTION',
     +     16.,0.,0.)
      CALL FRAME
 40   CONTINUE
C
C
C     Frame 5 -- The gnomonic projection.
C
      IFRAME = 5
C
C Set the projection-type parameter.
C
      CALL MAPROJ('GN',0.0,0.0,0.0)
C
C Draw the map.
C
      CALL MAPDRW
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,'EZMAP DEMONSTRATION:  GNOMONIC PROJECTION',
     +           16.,0.,0.)
      CALL FRAME
 50   CONTINUE
C
C
C     Frame 6 -- The azimuthal equidistant projection.
C
      IFRAME = 6
C
C Set the projection-type parameter.
C
      CALL MAPROJ('AE',-20.0,40.0,0.0)
C
C Set the grid spacing.
C
      CALL MAPSTR('GR',5.0)
C
C Draw the map.
C
      CALL MAPDRW
C
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,
     +     'EZMAP DEMONSTRATION:  AZIMUTHAL EQUIDISTANT PROJECTION',
     +           16.,0.,0.)
      CALL FRAME
 60   CONTINUE
C
C
C     Frame 7 -- The cylindrical equidistant projection.
C
      IFRAME = 7
C
C Set the map projection type parameter.
C
      CALL MAPROJ('CE',-40.0,80.0,0.0)
C
C Draw the map.
C
      CALL MAPDRW
C
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,
     +     'EZMAP DEMONSTRATION:  CYLINDRICAL EQUIDISTANT PROJECTION',
     +           16.,0.,0.)
      CALL FRAME
 70   CONTINUE
C
C
C     Frame 8 -- The mercator projection.
C
      IFRAME = 8
C
C Set the map projection type parameter.
C
      CALL MAPROJ('ME',-60.0,120.0,0.0)
C
C Draw the map.
C
      CALL MAPDRW
C
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,'EZMAP DEMONSTRATION: MERCATOR PROJECTION',
     +           16.,0.,0.)
      CALL FRAME
 80   CONTINUE
C
C
C     Frame 9 -- The Mollyweide-type projection.
C
      IFRAME = 9
C
C Set the map projection type parameter.
C
      CALL MAPROJ('MO',-80.0,160.0,0.0)
C
C Draw the map.
C
      CALL MAPDRW
C
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,
     +     'EZMAP DEMONSTRATION:  MOLLWEIDE-TYPE PROJECTION',
     +     16.,0.,0.)
      CALL FRAME
 90   CONTINUE
C
C
C     Frame 10 -- The satellite view projection.
C
      IFRAME = 10
C
C Set the map projection type parameter.
C
      CALL MAPROJ('SV',0.0,-135.0,0.0)
C
C Set the satellite distance and supress grid lines.
C
      CALL MAPSTR('SA',6.631)
      CALL MAPSTI('GR',0)
C
C Draw the map.
C
      CALL MAPDRW
C
C Report any errors encountered.
C
      IF( NERRO(IERR).NE.0) CALL RPTERR
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(TX,TY,
     +     'EZMAP DEMONSTRATION: SATELLITE VIEW PROJECTION',16.,0.,0.)
      CALL FRAME
 100  CONTINUE
C
      IF(IERRR.EQ.0) WRITE(6,1000)
      IF(IERRR.EQ.1) WRITE(6,1001)
      IERROR = IERR
      RETURN
C
 1000 FORMAT(' EZMAP TEST EXECUTED--SEE PLOTS TO CERTIFY')
 1001 FORMAT(' EZMAP TEST UNSUCCESSFUL')
      END
      SUBROUTINE RPTERR
C
C ROUTINE TO REPORT ERROR MESSEGES
C
      COMMON /ERROR/ IFRAME,IERRR
C
      WRITE(6,1000) IFRAME
      CALL EPRIN
      WRITE(6,1001)
      CALL ERROF
      IERRR = 1
      RETURN
C
 1000 FORMAT(' ERROR IN FRAME ',I2,5X,'ERROR MESSAGE FOLLOWS:')
 1001 FORMAT(' ******',//)
      END
