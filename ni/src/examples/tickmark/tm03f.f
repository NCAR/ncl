        program tm03f
        implicit none

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tm03f.f
C
C      Author:         Ed Stautler
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 06 18:31:18 MDT 1995
C
C      Description:    Demonstrates the TickMark Object
C                      with reversed and log y axis.
C

        external nhlfHlulayerclass
        external nhlfResListlayerclass
        external nhlfApplayerclass
        external nhlfTickMarklayerclass
        external nhlfXWorkstationlayerclass


	real level(10)
	data level / 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100 / 

C
C Define label strings for EXPLICIT mode tick mark placement
C
	character*10 labels(7)
	data labels /'90:S:o:N:S', '60:S:o:N:S', '30:S:o:N:S', 'EQ', 
     $		     '30:S:o:N:N', '60:S:o:N:N', '90:S:o:N:N' /

C
C Specify data locations for above labels
c
	real labellocs(7)
	data labellocs / -90.0, -60.0, -30.0, 0.0, 30.0, 60.0, 90.0 /
		
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
        call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
        call NhlFCreate(appid,'tm03',NhlFappLayerClass,0,rlist,ierr)

C
C Create an XWorkstation object.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetInteger(rlist,'wkPause','True',ierr)
        call NhlFCreate(wid,'tm03Work',NhlFxWorkstationLayerClass,0,
     $       rlist,ierr)

C
C Specify the viewport extent of the object.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetFloat(rlist,'vpXF',.2,ierr)
        call NhlFRLSetFloat(rlist,'vpYF',.8,ierr)
        call NhlFRLSetFloat(rlist,'vpWidthF',.6,ierr)
        call NhlFRLSetFloat(rlist,'vpHeightF',.6,ierr)
	call NhlFRLSetFloat(rlist,'tmYLDataTopF',100.0,ierr)
	call NhlFRLSetFloat(rlist,'tmYLDataBottomF',1000.0,ierr)
	call NhlFRLSetFloat(rlist,'tmXBDataRightF',90.0,ierr)
	call NhlFRLSetFloat(rlist,'tmXBDataLeftF',-90.0,ierr)
 	call NhlFRLSetString(rlist,'tmYLStyle','Irregular',ierr)
 	call NhlFRLSetString(rlist,'tmXBMode','Explicit',ierr)
	call NhlFRLSetString(rlist,'tmXBMinorOn','False',ierr)
	call NhlFRLSetFloatArray(rlist,'tmXBValues',labellocs,
     $       7,ierr)

C
C Array 'level' contains original grid point data locations in Y direction.
C Providing the grid points to the TickMark object as the control points
C for the IRREGULAR style transformation, means that these points will be
C evenly spaced along the Y axis. Since this is how CONPACK thinks the
C points are spaced, the tick marks will correctly correspond with the 
C data coordinates. See the HLU User's Guide for a complete
C discussion of IRREGULAR style transformations.
C
	call NhlFRLSetStringArray(rlist,'tmXBLabels',labels,
     $       7,ierr)
	call NhlFRLSetFloatArray(rlist,'tmYLIrregularPoints',level,
     $       10,ierr)
        call NhlFCreate(pid,'TickMarks',NhlFtickMarkLayerClass,wid,
     $       rlist,ierr)

	call NhlFDraw(pid,ierr)
	call NhlFFrame(wid,ierr)
	call NhlFDestroy(pid,ierr)
	call NhlFDestroy(wid,ierr)
	call NhlFDestroy(appid,ierr)
	call NhlFClose

	stop
	end
