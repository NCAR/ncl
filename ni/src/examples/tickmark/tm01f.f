	program tm01f
	implicit none

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tm01f.f
C
C      Author:         Ed Stautler
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 06 18:31:18 MDT 1995
C
C      Description:    Demonstrates the TickMark Object
C                      defaults.
C

	external nhlfHlulayerclass
        external nhlfResListlayerclass
        external nhlfApplayerclass
        external nhlfTickMarklayerclass
        external nhlfXWorkstationlayerclass
		
	integer appid, wid, pid
	integer rlist, ierr

C
C Initialize the high level utility library
C

	call NhlFInitialize

C
C Create an application context. Set the app dir to the current directory
C so the application looks for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
        call NhlFRLCreate(rlist,'SETRL')
        call NhlFRLClear(rlist)
	call NhlFRLSetString(rlist,'appUsrDir',"./",ierr)
	call NhlFCreate(appid,"tm01",NhlFappLayerClass,0,rlist,ierr)

C
C Create an XWorkstation object.
C
	call NhlFRLClear(rlist)
	call NhlFRLSetInteger(rlist,'wkPause','True',ierr)
	call NhlFCreate(wid,"tm01Work",NhlFxWorkstationLayerClass,0,
     $       rlist,ierr)
C
C Specify the viewport extent of the object.
C

        call NhlFRLClear(rlist)
	call NhlFRLSetFloat(rlist,'vpXF',.2,ierr)
	call NhlFRLSetFloat(rlist,'vpYF',.8,ierr)
	call NhlFRLSetFloat(rlist,'vpWidthF',.6,ierr)
	call NhlFRLSetFloat(rlist,'vpHeightF',.6,ierr)

	call NhlFCreate(pid,"TickMarks",NhlFtickMarkLayerClass,wid,
     $       rlist,ierr)

	call NhlFDraw(pid,ierr)
	call NhlFFrame(wid,ierr)
	call NhlFDestroy(pid,ierr)
	call NhlFDestroy(wid,ierr)
	call NhlFDestroy(appid,ierr)
	call NhlFClose

	stop
	end
