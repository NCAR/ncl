C
C $Id: ngpict.f,v 1.7 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGPICT(WKID,ACTION)
      COMMON /GFLASH/MODEF,IOPWKS(100),IOACT(100),NUMOP,IWISSI
C
C  NGPICT is designed to effect a break in the picture drawing
C  sequence depending upon whether the workstation type is 
C  MO, or whether it is an OUTPUT or OUTIN workstation.
C  An option also exists for prompting the user when an OUTPUT
C  or OUTIN workstation is ready and waiting.
C  
C  Arguments
C
C    INPUT
C
C      WKID    --  An integer specifying the workstation ID
C                  as was specified in a GOPWK call.
C      ACTION  --  The desired action.
C
C        = 0  Execute an UPDATE WORKSTATION on WKID.
C        = 1  Execute an UPDATE WORKSTATION followed by a
C             CLEAR WORKSTATION.
C        = 2  Execute an UPDATE WORKSTATION followed by a pause
C             waiting for a mouse click or a key click.
C        = 3  Execute an UPDATE WORKSTATION followed by a pause
C             followed by a CLEAR WORKSTATION after the pause is
C             terminated.
C        = 4  Same as 3 except a "<READY>" prompt is issued after
C             the UPDATE WORKSTATION.
C
C  If a metafile, the a CLEAR WORKSTATION is done which inserts an
C  END PICTURE into the metafile.  The only valid actions for a
C  metafile workstation are 1 and 2.
C  
      INTEGER WKID,ACTION
      CHARACTER*80 DATREC,STR,ISTR
      DIMENSION NASFO(13),NASF(13),LSACWK(15)
C
C  If no workstations are open, return.
C
      CALL GQOPWK (1,IER,NO,ID)
      IF (NO .EQ. 0) RETURN
C
C  Check to see if the specified workstation is open.  
C  If not we are done.
C
      DO 101 I=1,NO
        CALL GQOPWK (I,IER,NDUM,ID)
        IF (ID .EQ. WKID) THEN
          GO TO 102
        ENDIF
  101 CONTINUE
      RETURN
  102 CONTINUE
C
C  Check to see if ACTION is in range.
C
      IF (ACTION.LT.0 .OR. ACTION.GT.4) THEN
          CALL SETER
     -  ('NGPICT - VALUE FOR ACTION IS OUT OF RANGE',14,2)
      ENDIF
C
C  Get the workstation type.
C
      CALL GQWKC (WKID,IER,ICON,ITYPE)
C
C  Get the workstation category (0=OUTPUT; 2=OUTIN; 4=MO).
C
      CALL GQWKCA (ITYPE,IER,ICAT)
C
C  Flush the SPPS pen move buffer and update the workstation.
C
      CALL PLOTIF(0.,0.,2)
      CALL GUWK(WKID,1)      
C
      IF (ICAT .EQ. 4) THEN
C
C  Check to see if the ACTION is allowed for an MO workstation.
C
        IF (ACTION.NE.0 .AND. ACTION.NE.1) THEN
          CALL SETER
     +      ('NGPICT - ILLEGAL VALUE FOR ACTION ON A METAFILE',15,2)
        ENDIF
C
C  Illegal to call NGPICT while a FLASH buffer is open.
C
        IF (MODEF .EQ. 1) THEN
          CALL SETER 
     +  ('NGPICT - ILLEGAL TO CALL NGPICT WHILE A FLASH BUFFER IS OPEN',       
     +      16,2)
        ENDIF
C
        IF (ACTION .EQ. 0) THEN
          RETURN
        ELSE IF (ACTION .EQ. 1) THEN
          CALL GCLRWK(WKID,1)
        ENDIF
      ELSE IF (ICAT.EQ.0 .OR. ICAT.EQ.2) THEN
        IF (ACTION .EQ. 0) THEN
          RETURN
        ELSE IF (ACTION .EQ. 1) THEN
          CALL GCLRWK(WKID,1)
        ELSE IF (ACTION .EQ. 2) THEN
          ISTR(1:1) = CHAR(0)
          CALL GINST(WKID,1,0,ISTR,1,0.,1279.,0.,1023.,1,1,1,DATREC)
          CALL GSSTM(WKID,1,0,0)
          CALL GRQST(WKID,1,ISTAT,LOSTR,STR)
        ELSE IF (ACTION .EQ. 3) THEN
          ISTR(1:1) = CHAR(0)
          CALL GINST(WKID,1,0,ISTR,1,0.,1279.,0.,1023.,1,1,1,DATREC)
          CALL GSSTM(WKID,1,0,0)
          CALL GRQST(WKID,1,ISTAT,LOSTR,STR)
          CALL GCLRWK(WKID,1)
        ELSE IF (ACTION.EQ.4) THEN
C
C  Deactivate all workstations except WKID so that the prompt
C  does not go to them; store the list of deactivated wirkstations
C  in LSACWK.
C
          CALL GQACWK(1,IER,NUMACT,IDUM) 
          NN = 0
          DO 30 I=1,NUMACT
            CALL GQACWK(I,IER,NDUM,IWKID)
            LSACWK(I) = IWKID
   30     CONTINUE
          DO 40 I=1,NUMACT
            IF (LSACWK(I) .NE. WKID) CALL GDAWK(LSACWK(I))
   40     CONTINUE
C
C  Save the current normalization transformation number and the 
C  appropriate current attributes.
C
          CALL GQCNTN(IER,NTRO)
          CALL GQTXFP(IER,NFONTO,NPRECO)
          CALL GQCHXP(IER,XEXPO)
          CALL GQCHSP(IER,XSPCO)
          CALL GQTXCI(IER,NCOLO)
          CALL GQASF(IER,NASFO)
          CALL GQCHH(IER,XHTO)
          CALL GQCHUP(IER,XUPXO,XUPYO)
          CALL GQTXP(IER,NTXPO)
          CALL GQTXAL(IER,NALNHO,NALNVO)
C
C  Write out the prompt.
C
          CALL GSELNT(0)
          DO 20 I=1,13
            NASF(I) = NASFO(I)
   20     CONTINUE
          NASF( 7) = 1
          NASF( 8) = 1
          NASF( 9) = 1
          NASF(10) = 1
          CALL GSASF(NASF)
          CALL GSTXFP(1,2)
          CALL GSCHXP(1.)
          CALL GSCHSP(0.)
          CALL GSTXCI(1)
          CHGT = .0175
          CALL GSCHH(CHGT)
          CALL GSCHUP(0.,1.)
          CALL GSTXP(0)
          CALL GSTXAL(1,3)
          CALL GTX(0.005,.5*CHGT+.005,'<READY>')
C
C  Restore all settings.
C
          CALL GSELNT(NTRO)
          CALL GSASF(NASFO)
          CALL GSTXFP(NFONTO,NPRECO)
          CALL GSCHXP(XEXPO)
          CALL GSCHSP(XSPCO)
          CALL GSTXCI(NCOLO)
          CALL GSCHH(XHTO)
          CALL GSCHUP(XUPXO,XUPYO)
          CALL GSTXP(NTXPO)
          CALL GSTXAL(NALNHO,NALNVO)
C
C  Reactivate the workstations that were deactivated.
C
          DO 50 I=1,NUMACT
            IF (LSACWK(I) .NE. WKID) CALL GACWK(LSACWK(I))
   50     CONTINUE
C
C  Pause.
C
          ISTR(1:1) = CHAR(0)
          CALL GINST(WKID,1,0,ISTR,1,0.,1279.,0.,1023.,1,1,1,DATREC)
          CALL GSSTM(WKID,1,0,0)
          CALL GRQST(WKID,1,ISTAT,LOSTR,STR)
C
          CALL GCLRWK(WKID,1)
        ENDIF
      ENDIF
C
      RETURN
      END
