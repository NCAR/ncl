        PROGRAM CCPMVI
        PARAMETER (NLON=361,NLAT=181,N=5)
        REAL ZDAT(NLON,NLAT)
        INTEGER IASF(13)
        DATA IASF / 13*1 /
C
C Print out a warning about how time consuming this example is
C
        WRITE (6,*) ' WARNING: This example may take 20 minutes or'
        WRITE (6,*) '          more to execute on some machines.'
C
C Open GKS, turn clipping off, and set up GKS flags.
C Use workstation type 3 for use with Gflash.
C
        CALL GOPKS(6,IDUM)
        CALL GOPWK(1,2,1)
        CALL GACWK(1)
        CALL GSCLIP (0)
        CALL GSASF (IASF)
C
C Set up a color table
C
        CALL COLOR
C
C Initialize the Gflash package.
C
        CALL GOPWK(9,1,3)
C
C Begin storing the map information
C
        CALL GFLAS1(0)
C
C Draw a map
C
        CALL DRWMAP ('SV',40.,-105.)
C
C End storing the map information
C
        CALL GFLAS2
C
C Set up conpack options
C
        CALL SETCTR (0,1,-180.,180.,-90.,90.,1.E36)
C
C Set up a loop to do more than one contour plot
C
        DO 30, I=1,N
C
C Create some data
C
           CALL MKDAT (I,ZDAT,NLON,NLAT)
C
C Draw contours
C
	   CALL DRWCTR (ZDAT,NLON,NLAT)
C
C Draw Map on top of contours and close frame
C
           CALL GFLAS3 (0)
           CALL FRAME
 30     CONTINUE
C
C Close Gflash workstation
C
        CALL GCLWK(9)
C
C Close GKS 
C
        CALL GDAWK(1)
        CALL GCLWK(1)
        CALL GCLKS

        STOP
        END

        SUBROUTINE MKDAT(NCTFR,ZDAT,NLON,NLAT)
C
C Create some data to contour
C
        REAL ZDAT(NLON,NLAT)
C
C NCTFR is used to generate a random number, ZDAT is the data array
C
        CALL GGDINI (0.,1.,NCTFR,.9)
        ZMIN= 1.E36
        ZMAX=-1.E36
        DO 10 I=1,NLON
           RLON=.017453292519943*(-180.+360.*REAL(I-1)/REAL(NLON-1))
           DO 20 J=1,NLAT
              RLAT=.017453292519943*(-90.+180.*REAL(J-1)/REAL(NLAT-1))
              ZDAT(I,J)=GGDPNT(RLAT,RLON)+.5*COS(4.*RLAT)
              ZMIN=MIN(ZMIN,ZDAT(I,J))
              ZMAX=MAX(ZMAX,ZDAT(I,J))
 20        CONTINUE
 10     CONTINUE
        DO 30 I=1,NLON
           DO 40 J=1,NLAT
              ZDAT(I,J)=((ZDAT(I,J)-ZMIN)/(ZMAX-ZMIN))*130.-10.
 40        CONTINUE
 30     CONTINUE
        DO 50 I=175,185
           DO 60 J=85,95
              ZDAT(I,J)=1.E36
 60        CONTINUE
 50     CONTINUE
        
        RETURN
        END

        SUBROUTINE DRWMAP (PTYPE,PLAT,PLON)
C
C Set up a map projection with desired options, but don't draw it.
C
        CHARACTER*2 PTYPE

C
C Set up a map
C
        CALL MAPPOS (.05,.90,.05,.90)
        CALL MAPROJ (PTYPE,PLAT,PLON,0.)
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
        CALL MAPINT
C
C Draw map and grid lines
C
        CALL GSPLCI (1)
        CALL GSLWSC (3.)
        CALL MAPLOT
        CALL GSLWSC (1.)
        CALL MAPGRD
        CALL SFLUSH

        RETURN
        END

        SUBROUTINE SETCTR (ISET,MAPFLG,XMIN,XMAX,YMIN,YMAX,SPVAL)
C
C Set up Conpack options, but don't do anything
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',ISET)
        CALL CPSETI ('MAP - MAPPING FLAG',MAPFLG)
        CALL CPSETR ('XC1 - X COORDINATE AT I=1',XMIN)
        CALL CPSETR ('XCM - X COORDINATE AT I=M',XMAX)
        CALL CPSETR ('YC1 - Y COORDINATE AT J=1',YMIN)
        CALL CPSETR ('YCN - Y COORDINATE AT J=N',YMAX)
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',1)
        CALL CPSETR ('CMN - CONTOUR LEVEL MINIMUM',0.)
        CALL CPSETR ('CMX - CONTOUR LEVEL MAXIMUM',110.)
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',10.)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
        CALL CPSETR ('SPV - SPECIAL VALUE',SPVAL)
        CALL CPSETI ('CAF - CELL ARRAY FLAG',2)
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('AIA - AREA IDENTIFIER OUTSIDE THE GRID',-100)
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
        CALL CPSETI ('AIA - AREA IDENTIFIER - SPECIAL-VALUE AREAS',-100)
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',0)
        CALL CPSETC ('HLT - HIGH/LOW TEXT',' '' ')
	CALL CPSETC ('ILT - INFORMATIONAL LABEL TEXT',' ')

        RETURN
        END
	SUBROUTINE DRWCTR (ZDAT,NLON,NLAT)

        PARAMETER (LRWK=5000,LIWK=5000,ICAM=512,ICAN=512,N=5)
        REAL ZDAT(NLON,NLAT),RWRK(LRWK)
        INTEGER IWRK(LIWK),ICRA(ICAM,ICAN)
C
C Zero cell array
C
        DO 10 I=1,ICAM
           DO 20 J=1,ICAN
              ICRA(I,J)=0
 20        CONTINUE
 10     CONTINUE
C
C Initialize Conpack
C
	CALL CPRECT (ZDAT,NLON,NLON,NLAT,RWRK,LRWK,IWRK,LIWK)
C
C Set cell array values and map it to user coordinates
C
	CALL CPCICA (ZDAT,RWRK,IWRK,ICRA,ICAM,ICAM,ICAN,0.,0.,1.,1.)
C
C Draw cell array and flush buffer
C
        CALL GCA (CFUX(0.),CFUY(0.),CFUX(1.),CFUY(1.),ICAM,ICAN,1,1,
     +                                               ICAM,ICAN,ICRA)
        CALL SFLUSH
C
C Draw contour lines
C
        CALL GSLWSC (2.)
        CALL GSPLCI (0)
        CALL CPCLDR (ZDAT,RWRK,IWRK)
        CALL SFLUSH

	RETURN
	END
        SUBROUTINE COLOR
        CALL GSCR (1, 0,0.000,0.000,0.000)
        CALL GSCR (1, 1,1.000,1.000,1.000)
        CALL GSCR (1, 2,0.500,1.000,1.000)
        DO 10 I=3,15
          CALL GSCR (1,I,MAX(0.,MIN(1.,1.-REAL(ABS(I- 3)/10.))),
     +                   MAX(0.,MIN(1.,1.-REAL(ABS(I- 9)/10.))),
     +                   MAX(0.,MIN(1.,1.-REAL(ABS(I-15)/10.))))
 10     CONTINUE
        CALL GSCR (1,16,1.,0.,0.)
        RETURN
        END
