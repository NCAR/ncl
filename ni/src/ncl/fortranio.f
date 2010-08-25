        SUBROUTINE NCLPFORTRANREAD(PPATH,ARR,N,NCERR)
        CHARACTER* (*)  PPATH
        CHARACTER ARR(N)
        INTEGER NCERR
        LOGICAL OPN

        DO 5 I=10,50
                INQUIRE(I,OPENED=OPN)
                IF (OPN.eqv..FALSE.) THEN       
                        GOTO 7
                END IF
  5     CONTINUE

  7     OPEN(I,STATUS='old',FILE=PPATH ,IOSTAT=ios,form='unformatted',
     +  ERR=10)
        READ(I,ERR=20) ARR
        CLOSE(I)

        NCERR = -1
        RETURN
 10     NCERR = -4
        CALL NHLFPERROR('FATAL',1000,'fbinread: An error occured '
     +  //'opening the requested file')
        RETURN
 20     NCERR = -4
        CALL NHLFPERROR('FATAL',1000,'fbinread: An error occured while '
     +  //'reading the requested file check dimension size information')
        RETURN
        END

        SUBROUTINE NCLPFORTRANWRITE(PPATH,ARR,N,NCERR)
        CHARACTER* (*) PPATH
        CHARACTER ARR(N)
        INTEGER NCERR
        LOGICAL OPN

        DO 5 I=10,50
                INQUIRE(I,OPENED=OPN)
                IF (OPN.eqv..FALSE.) THEN       
                        GOTO 7
                END IF
  5     CONTINUE

  7     OPEN(I,STATUS='new',FILE=PPATH,IOSTAT=ios,FORM='unformatted',
     +  ERR=30)
        WRITE(I,ERR=40) ARR
        CLOSE(I)

        NCERR = -1
        RETURN
 30     NCERR = -4
        CALL NHLFPERROR('FATAL',1000,'fbinwrite: An error occured '
     +  //'opening the requested file')
        RETURN
 40     NCERR = -4
        CALL NHLFPERROR('FATAL',1000,'fbinwrite: An error occured '
     +  //'while writing the requested file')
        RETURN
        END
