
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                      c
c                Copyright (C) 1995                                    c
c        University Corporation for Atmospheric Research               c
c                all rights reserved                                   c
c                                                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      File:           ti02c.c
c
c      Author:         Bob Lackman
c          National Center for Atmospheric Research
c          PO 3000, Boulder, Colorado
c
c      Date:           Fri Jan 06 18:31:18 mdt 1995
c
c      Description:    Demonstrates the title object
c                      defaults.
c

	program ti02
	implicit none

	external nhlfapplayerclass
        external nhlfreslistlayerclass
        external nhlfapplayerclass
        external nhlftitlelayerclass
        external nhlfxworkstationlayerclass

	integer appid, wid, pid
	integer rlist, ierr

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
	call nhlfcreate(appid,'ti02',nhlfapplayerclass,
     $       0,rlist,ierr)


c
c Create an xworkstation object.
c
	call nhlfrlclear(rlist)
	call nhlfrlsetinteger(rlist,'wkPause','true',ierr)
	call nhlfcreate(wid,'ti02Work',nhlfxworkstationlayerclass,
     $       0,rlist,ierr)
c
c Specify the viewport extent of the object.
c

        call nhlfrlclear(rlist)
	call nhlfrlsetfloat(rlist,'vpXF',.2,ierr)
	call nhlfrlsetfloat(rlist,'vpYF',.8,ierr)
	call nhlfrlsetfloat(rlist,'vpWidthF',.6,ierr)
	call nhlfrlsetfloat(rlist,'vpHeightF',.6,ierr)

	call nhlfcreate(pid,'Titles',
     $	     nhlftitlelayerclass,wid,rlist,ierr)

	call nhlfdraw(pid,ierr)
	call nhlfframe(wid,ierr)
	call nhlfdestroy(pid,ierr)
	call nhlfdestroy(wid,ierr)
	call nhlfdestroy(appid,ierr)
	call nhlfclose

	stop
	end
