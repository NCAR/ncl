cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                      c
c                Copyright (C,ierr)  1995                                   c
c        University Corporation for Atmospheric Research               c
c                all rights reserved                                   c
c                                                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      File:           tx03c.c
c
c      Author:         Bob Lackman
c          National Center for Atmospheric Research
c          PO 3000, Boulder, Colorado
c
c      Date:           Fri Jan 06 18:31:18 mdt 1995
c
c      Description:    Demonstrates the TextItem Object
c                      Writes "NCAR Graphics" in a series of
c                      114 different colors. (The default colormap.)
c

        external nhlfhlulayerclass
        external nhlfreslistlayerclass
        external nhlfapplayerclass
        external nhlfxworkstationlayerclass
	external nhlftextitemlayerclass

		
	integer appid, wid, pid
	integer rlist, ierr
	integer m,i

	parameter(m=114)

c
c Initialize the high level utility library
c

	call nhlfinitialize

c
c Create an application context. Set the app dir to the current directory
c so the application looks for a resource file in the working directory.
c In this example the resource file supplies the plot title only.
c
        call nhlfrlcreate(rlist,'setrl')
        call nhlfrlclear(rlist)
	call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
	call nhlfcreate(appid,'tx03',nhlfapplayerclass,0,rlist,ierr)

c
c Create an xworkstation object.
c
	call nhlfrlclear(rlist)
	call nhlfrlsetinteger(rlist,'wkPause',true,ierr)
	call nhlfcreate(wid,'tx03Work',nhlfxworkstationlayerclass,
     $       0,rlist,ierr)
/*
 * Create 114 plots varying the fill color of the text bounding box
 * to all entries of the default workstation color map.
 */

	do 10, i=1,m
		call nhlfrlclear(rlist)
		call nhlfrlsetinteger(rlist,'txBackgroundFillColor',
     $               i,ierr)

		call nhlfcreate(pid,'TextItems',nhlftextitemlayerclass,
     $               wid,rlist,ierr)

		call nhlfdraw(pid,ierr)
		call nhlfframe(wid,ierr)
 10    	continue

	call nhlfdestroy(pid,ierr)
	call nhlfdestroy(wid,ierr)
	call nhlfdestroy(appid,ierr)
	call nhlfclose

	stop
	end
