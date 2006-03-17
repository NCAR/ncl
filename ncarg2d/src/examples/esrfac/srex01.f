
      PROGRAM SREX01
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
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX       ,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT     ,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI        ,
     3                CLO        ,CINC       ,ISPVAL
C
C Define the required arrays.
C
      DIMENSION XDAT(100),YDAT(100),ODAT(40,40),ZDAT(100,100),
     +                                          QDAT(100,100),
     +          WORK(20000),STLN(6)
C
C Define the data for the label on top of the graph.
C
      CHARACTER*30 PLBL
      DATA PLBL / 'Longs Peak relief using SRFACE' /
C 
C Define the line of sight (viewpoint and point looked at).
C
      DATA STLN / 5247.5 , 5247.5 , 2530. , 247.5 , 247.5 , 1280. /
C
C Open GKS, open workstation of type 1, activate workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Generate x-coordinate values.
C
      DO 10 I=1,100
         XDAT(I)=5.*REAL(I-1)
 10   CONTINUE
C
C Generate y-coordinate values.
C
      DO 15 J=1,100
         YDAT(J)=5.*REAL(J-1)
 15   CONTINUE
C
C Put the original Long's Peak data in the array ODAT.
C
      DO 20 J=1,40
         READ (5,1001)  (ODAT(I,J),I=1,40)
 20   CONTINUE
C
C Interpolate to get more closely-spaced data in the array QDAT.
C
      DO 30 J=1,100
         FL=1.+39.*REAL(J-1)/99.
         L=MAX(1,MIN(39,INT(FL)))
         FL=FL-REAL(L)
         DO 40 I=1,100
            FK=1.+39.*REAL(I-1)/99.
            K=MAX(1,MIN(39,INT(FK)))
            FK=FK-REAL(K)
            QDAT(I,J)=(1.-FL)*((1.-FK)*ODAT(K,L  )+FK*ODAT(K+1,L  ))+
     +                    FL *((1.-FK)*ODAT(K,L+1)+FK*ODAT(K+1,L+1))
 40      CONTINUE
 30   CONTINUE
C
C Apply a nine-point smoother to get smoother data in the array ZDAT.
C
      DO 50 J=1,100
         JM1=MAX(J-1,1)
         JP1=MIN(J+1,100)
         DO 60 I=1,100
            IM1=MAX(I-1,1)
            IP1=MIN(I+1,100)
            ZDAT(I,J)=.2500*QDAT(I,J)+
     +                .1250*(QDAT(IM1,J)+QDAT(IP1,J)+
     +                       QDAT(I,JM1)+QDAT(I,JP1))+
     +                .0625*(QDAT(IM1,JM1)+QDAT(IP1,JM1)+
     +                       QDAT(IM1,JP1)+QDAT(IP1,JP1))
 60      CONTINUE
 50   CONTINUE
C
C Plot the data four times.
C
      DO 70 NPLT=1,4
C
C Before the 2nd, 3rd, and 4th plots, rotate the data by 90 degrees.
C
         IF (NPLT.NE.1) THEN
            DO 80 J=1,100
               DO 90 I=1,100
                  QDAT(I,J)=ZDAT(I,J)
 90            CONTINUE
 80         CONTINUE
            DO 100 J=1,100
               K=101-J
               DO 110 I=1,100
                  L=I
                  ZDAT(I,J)=QDAT(K,L)
 110           CONTINUE
 100        CONTINUE
         END IF
C
C Call LOGO to add NCAR and SCD logos.
C
         CALL LOGO
C
C Set common variables for skirt and frame calls.
C
         ISKIRT = 1
         HSKIRT = 1100
         IFR    = 0
C
C Use SRFACE to draw a representation of the surface.
C
         CALL SRFACE (XDAT,YDAT,ZDAT,WORK,100,100,100,STLN,0.)
C
C Put in label on the top of the map.
C
         CALL PWRIT (.5,.850,PLBL,30,3,0,0)
C
C Advance to the next frame.
C
         CALL FRAME
C
C End of loop.
C
 70   CONTINUE
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
C
C Formats.
C
 1001 FORMAT (1X,15F5.0)
C
      END
C
      SUBROUTINE LOGO
C
      CALL NCAR(.55,.92)
      CALL SCD(.04,.04)
      CALL IRONS(0.09,0.090)
C
      RETURN
      END
C
      SUBROUTINE NCAR(XPOS,YPOS)
C
      CALL PCSETI ('CD', 1)
      CALL PLCHHQ ( XPOS,YPOS, 'NCAR  GRAPHICS',
     + .038, 0.0, 0.0 )
C
      CALL PCSETI ('CD', 0)
C
      RETURN
      END
C
      SUBROUTINE SCD(XBOT, YBOT)
C
      REAL XBOT, YBOT
C
      CALL PCSETI ('CD', 1)
      CALL PLCHHQ ( XBOT, YBOT, 'SCD', .04, 0.0, -1.0 )
C
      CALL PLCHHQ ( XBOT + .145, YBOT + .008,
     + 'SCIENTIFIC COMPUTING DIVISION', .012, 0.0, -1.0)
C
      CALL PLCHHQ ( XBOT + .145, YBOT - .016,
     + 'NATIONAL CENTER FOR ATMOSPHERIC RESEARCH', .012, 0.0, -1.0)
C
      CALL PCSETI ('CD', 0)
C
      CALL LINE (0.93,0.07,.18,0.07)
C
      RETURN
      END
C
      SUBROUTINE IRONS(X1,Y1)
C
      CALL LINE (X1+.039,Y1+.050,X1+0.015,Y1+0.054)
      CALL LINE (X1+.015,Y1+.054,X1-.032,Y1+0.013)
C
C Devil's Thumb.
C
      CALL LINE (X1+.010,Y1+.048,X1+.007,Y1+0.054)
      CALL LINE (X1+.007,Y1+.054,X1+.007,Y1+0.059)
      CALL LINE (X1+.007,Y1+.059,X1+.005,Y1+0.053)
      CALL LINE (X1+.005,Y1+.053,X1+.006,Y1+0.044)
C
C Bear Mountain.
C
      CALL LINE (X1+0.026,Y1+0.03,X1+.038,Y1+.05)
      CALL LINE (X1+.038,Y1+.05,X1+.053,Y1+.07)
      CALL LINE (X1+.053,Y1+.07,X1+.078,Y1+.03)
      CALL LINE (X1+.078,Y1+.03,X1+.098,Y1+.05)
      CALL LINE (X1+.098,Y1+.05,X1+.1,Y1+.04)
      CALL LINE (X1+.1,Y1+.04,X1+.13,Y1+.065)
      CALL LINE (X1+.13,Y1+.065,X1+.18,Y1+.029)
      CALL LINE (X1+.18,Y1+.029,X1+.25,Y1+.039)
      CALL LINE (X1+.25,Y1+.039,X1+.27,Y1+.026)
C
      CALL LINE (X1+.27,Y1+.026,X1+.81,Y1+.026)
      RETURN
      END
