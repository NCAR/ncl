        PROGRAM CCPEZ

        PARAMETER (M=50, N=50)
        REAL ZREG(M,N)

C Get some data
        CALL MKDAT (3,ZREG, M, N)
C Open GKS
        CALL OPNGKS
C Call CPEZCT
        CALL CPEZCT (ZREG, M, N)
C Close GKS
        CALL CLSGKS

C Quit
        STOP
        END

        SUBROUTINE MKDAT(NCTFR,ZDAT,M,N)
C
C Create some data to contour
C
        REAL ZDAT(M,N)
C
C NCTFR is used to generate a random number, ZDAT is the data array
C
        CALL GGDINI (0.,1.,NCTFR,.9)
        ZMIN= 1.E36
        ZMAX=-1.E36
        DO 10 I=1,M
           RLON=.017453292519943*(-180.+360.*REAL(I-1)/REAL(M-1))
           DO 20 J=1,N
              RLAT=.017453292519943*(-90.+180.*REAL(J-1)/REAL(N-1))
              ZDAT(I,J)=GGDPNT(RLAT,RLON)+.5*COS(4.*RLAT)
              ZMIN=MIN(ZMIN,ZDAT(I,J))
              ZMAX=MAX(ZMAX,ZDAT(I,J))
 20        CONTINUE
 10     CONTINUE
        DO 30 I=1,M
           DO 40 J=1,N
              ZDAT(I,J)=((ZDAT(I,J)-ZMIN)/(ZMAX-ZMIN))*130.-50.
 40        CONTINUE
 30     CONTINUE
        
        RETURN
        END

