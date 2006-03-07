
        PROGRAM TPPACK
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Open GKS, open a workstation of type 1, activate the workstation.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Invoke the demo driver.
C
        CALL PPACK(IERR)
C
C Deactivate and close the workstation and close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END

      SUBROUTINE PPACK (IERR)
C
C PURPOSE                To provide a simple demonstration of the use
C                        of a couple of the POLYPACK routines.
C
C USAGE                  CALL PPACK (IERR)
C
C ARGUMENTS
C
C ON OUTPUT              IERR
C
C                          an error parameter
C                          = 0, if the test is successful.
C
C I/O                    If the test is successful, the message
C
C                          POLYPACK TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is written on unit 6.
C
C PRECISION              Single.
C
C REQUIRED LIBRARY       POLYPACK, SPPS
C FILES
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN
C
C HISTORY                Written in June, 1994.
C
C ALGORITHM              TPPACK defines a simple clip polygon and a
C                        simple subject polygon, displays them both,
C                        and uses the POLYPACK routines PPINPO and
C                        PPINTR to fill the intersection.
C
C PORTABILITY            FORTRAN 77
C
C Declare arrays in which to define the clip polygon and the subject
C polygon.
C
        DIMENSION XCCP(5),YCCP(5),XCSP(11),YCSP(11)
C
C Declare the required work arrays.
C
        PARAMETER (NWRK=999)
C
        DIMENSION RWRK(NWRK),IWRK(NWRK)
C 
C The EQUIVALENCE line is commented out below. If memory storage is an
C issue for you, *and* RWRK is not a DOUBLE PRECISION variable, then you
C can uncomment this line.
C
C        EQUIVALENCE (RWRK(1),IWRK(1))
C
C Tell the compiler that the fill routines for polygons and trapezoids
C and the merge routine for polygons are EXTERNALs, not REALs.
C
        EXTERNAL FILLPO,FILLTR,MERGPO
C
C Merge polygons are formed in the common block MERGCM:
C
        COMMON /MERGCM/ XCMP(999),YCMP(999),NCMP
        SAVE   /MERGCM/
C
C Define the clip polygon to be a small square.
C
        DATA NCCP / 5 /
C
        DATA XCCP( 1),YCCP( 1) / -5. , -5. /
        DATA XCCP( 2),YCCP( 2) /  5. , -5. /
        DATA XCCP( 3),YCCP( 3) /  5. ,  5. /
        DATA XCCP( 4),YCCP( 4) / -5. ,  5. /
        DATA XCCP( 5),YCCP( 5) / -5. , -5. /
C
C Define the subject polygon to be a diamond with a hole in it.
C
        DATA NCSP / 11 /
C
        DATA XCSP( 1),YCSP( 1) /  0. ,  9. /
        DATA XCSP( 2),YCSP( 2) /  0. ,  6. /
        DATA XCSP( 3),YCSP( 3) /  6. ,  0. /
        DATA XCSP( 4),YCSP( 4) /  0. , -6. /
        DATA XCSP( 5),YCSP( 5) / -6. ,  0. /
        DATA XCSP( 6),YCSP( 6) /  0. ,  6. /
        DATA XCSP( 7),YCSP( 7) /  0. ,  9. /
        DATA XCSP( 8),YCSP( 8) / -9. ,  0. /
        DATA XCSP( 9),YCSP( 9) /  0. , -9. /
        DATA XCSP(10),YCSP(10) /  9. ,  0. /
        DATA XCSP(11),YCSP(11) /  0. ,  9. /
C
C Initialize the error flag to zero.
C
        IERR=0
C
C Enable solid fill instead of the default hollow fill.
C
        CALL GSFAIS (1)
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Put a label on the whole plot.
C
        CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL PLCHHQ (.5,.975,'DEMONSTRATING THE USE OF POLYPACK',
     +                                                .015,0.,0.)
C
C In the upper left-hand corner, draw just the clip polygon and the
C subject polygon.
C
        CALL SET (.05,.475,.525,.95,-10.,10.,-10.,10.,1)
        CALL PLCHHQ (0.,-9.5,'The subject polygon (hollow diamond) and c
     +lip polygon (square).',.008,0.,0.)
        CALL GPL (NCCP,XCCP,YCCP)
        CALL GPL (NCSP,XCSP,YCSP)
C
C In the upper right-hand corner, fill the difference polygon, using
C PPDIPO and FILLPO.
C
        CALL SET (.525,.95,.525,.95,-10.,10.,-10.,10.,1)
        CALL PLCHHQ (0.,-9.5,'The difference (subject polygon minus clip
     + polygon).',.008,0.,0.)
        CALL GPL (NCCP,XCCP,YCCP)
        CALL GPL (NCSP,XCSP,YCSP)
        CALL PPDIPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,FILLPO,IERR)
        IF (IERR.NE.0) THEN
          WRITE (6,*) 'POLYPACK ROUTINE PPDIPO RETURNS IERR = ',IERR
          RETURN
        END IF
C
C In the lower left-hand corner, fill the intersection polygon, using
C PPINTR and FILLTR.
C
        CALL SET (.05,.475,.05,.475,-10.,10.,-10.,10.,1)
        CALL PLCHHQ (0.,-9.5,'The intersection of the subject and clip p
     +olygons.',.008,0.,0.)
        CALL GPL (NCCP,XCCP,YCCP)
        CALL GPL (NCSP,XCSP,YCSP)
        CALL PPINTR (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,FILLTR,IERR)
        IF (IERR.NE.0) THEN
          WRITE (6,*) 'POLYPACK ROUTINE PPINTR RETURNS IERR = ',IERR
          RETURN
        END IF
C
C In the lower right-hand corner, fill the union polygon, using PPUNPO
C and MERGPO.
C
        CALL SET (.525,.95,.05,.475,-10.,10.,-10.,10.,1)
        CALL PLCHHQ (0.,-9.5,'The union of the subject and clip polygons
     +.',.008,0.,0.)
        CALL GPL (NCCP,XCCP,YCCP)
        CALL GPL (NCSP,XCSP,YCSP)
        NCMP=0
        CALL PPUNPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,MERGPO,IERR)
        IF (IERR.NE.0) THEN
          WRITE (6,*) 'POLYPACK ROUTINE PPUNPO RETURNS IERR = ',IERR
          RETURN
        END IF
        IF (NCMP.EQ.0) THEN
          WRITE (6,*) 'MERGE POLYGON IS NULL'
          RETURN
        ELSE IF (NCMP.EQ.1000) THEN
          WRITE (6,*) 'MERGE POLYGON WAS TOO BIG TO HANDLE'
          RETURN
        ELSE
          CALL GFA (NCMP-1,XCMP,YCMP)
        END IF
C
C Advance the frame.
C
        CALL FRAME
C
C Write the appropriate message.
C
        WRITE (6,*) 'POLYPACK TEST EXECUTED--SEE PLOTS TO CERTIFY'
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE FILLPO (XCRA,YCRA,NCRA)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
C This routine processes polygons generated by the routines PPDIPO,
C PPINPO, and PPUNPO.
C
C Fill the polygon.
C
        CALL GFA (NCRA-1,XCRA,YCRA)
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE FILLTR (XCBL,XCBR,YCOB,DXLE,DXRE,YCOT)
C
        DIMENSION XCRA(5),YCRA(5)
C
C This routine fills trapezoids generated by the routines PPDITR,
C PPINTR, and PPUNTR.
C
C If the trapezoid is not degenerate, fill it and outline it.
C
        IF (YCOT.GT.YCOB) THEN
          XCRA(1)=XCBL
          YCRA(1)=YCOB
          XCRA(2)=XCBR
          YCRA(2)=YCOB
          XCRA(3)=XCBR+DXRE*(YCOT-YCOB)
          YCRA(3)=YCOT
          XCRA(4)=XCBL+DXLE*(YCOT-YCOB)
          YCRA(4)=YCOT
          XCRA(5)=XCBL
          YCRA(5)=YCOB
          CALL GFA (4,XCRA,YCRA)
          CALL GPL (5,XCRA,YCRA)
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE MERGPO (XCRA,YCRA,NCRA)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
C This routine merges the polygons generated by one of the routines
C PPDIPO, PPINPO, and PPUNPO into a single polygon with holes.
C
C Merge polygons are formed in the common block MERGCM:
C
        COMMON /MERGCM/ XCMP(999),YCMP(999),NCMP
        SAVE   /MERGCM/
C
C Copy the coordinates of the latest polygon into the merge polygon
C coordinate arrays and, if the polygon is not the first of the group,
C repeat the first point of the first polygon.  (Actually, the code
C below does something a little more complicated: if necessary, it
C interpolates points to ensure that the connecting lines between
C polygons consist of horizontal and/or vertical steps; this tends
C to prevent problems caused by deficiencies in the fill algorithms
C on some devices.)
C
        NTMP=NCMP
C
        IF (NTMP+NCRA+4.LE.999) THEN
          IF (NCMP.NE.0) THEN
            IF (XCMP(NTMP).NE.XCRA(1).AND.YCMP(NTMP).NE.YCRA(1)) THEN
              IF (YCMP(NTMP).LT.YCRA(1)) THEN
                NTMP=NTMP+1
                XCMP(NTMP)=XCRA(1)
                YCMP(NTMP)=YCMP(NTMP-1)
              ELSE
                NTMP=NTMP+1
                XCMP(NTMP)=XCMP(NTMP-1)
                YCMP(NTMP)=YCRA(1)
              END IF
            END IF
            NTMP=NTMP+1
            XCMP(NTMP)=XCRA(1)
            YCMP(NTMP)=YCRA(1)
          END IF
          DO 101 ICRA=1,NCRA
            XCMP(NTMP+ICRA)=XCRA(ICRA)
            YCMP(NTMP+ICRA)=YCRA(ICRA)
  101     CONTINUE
          NTMP=NTMP+NCRA
          IF (NCMP.NE.0) THEN
            IF (XCMP(NTMP).NE.XCMP(1).AND.YCMP(NTMP).NE.YCMP(1)) THEN
              IF (YCMP(NTMP).LT.YCMP(1)) THEN
                NTMP=NTMP+1
                XCMP(NTMP)=XCMP(1)
                YCMP(NTMP)=YCMP(NTMP-1)
              ELSE
                NTMP=NTMP+1
                XCMP(NTMP)=XCMP(NTMP-1)
                YCMP(NTMP)=YCMP(1)
              END IF
            END IF
            NTMP=NTMP+1
            XCMP(NTMP)=XCMP(1)
            YCMP(NTMP)=YCMP(1)
          END IF
        ELSE
          NTMP=1000
        END IF
C
        NCMP=NTMP
C
C Done.
C
        RETURN
C
      END
