!
!      $Id: ngi.res,v 1.6 1997-02-27 20:20:35 boote Exp $
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
!			   Copyright (C)  1996				!
!	     University Corporation for Atmospheric Research		!
!			   All Rights Reserved				!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	File:		ngi.res
!
!	Author:		Jeff W. Boote
!			National Center for Atmospheric Research
!			PO 3000, Boulder, Colorado
!
!	Date:		Fri Sep 27 16:25:40 MDT 1996
!
!	Description:	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! These resources are compiled into the application as seen.		!
! This file is only installed for reference purposes.			!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! These resources will *hopefully* be replaced by a configuration dialog!
! in and "Options" menu.						!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

NgNGO*loadfileMGR.pattern:	*.ncl
NgNGO*loadfileMGR.directory:	.
NgNGO*addfileMGR.pattern:	*.{cdf,nc,grb}
NgNGO*addfileMGR.directory:	.

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! Basic color scheme for entire app					!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

NgNGO*background:	#b2b2b2
NgNGO*foreground:	black
NgNGO*XmList*background:	#bebebe
NgNGO*XmList*foreground:	black
NgNGO*XmText*background:	#bebebe
NgNGO*XmText*foreground:	black
NgNGO*XmTextField*background:	#bebebe
NgNGO*XmTextField*foreground:	black

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! Basic Font scheme for entire app					!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

NgNGO*FontList:		-*-helvetica-bold-r-normal-*-*-120-*-*-*-*-iso8859-*
NgNGO*menubar*FontList:	-*-helvetica-bold-r-normal-*-*-120-*-*-*-*-iso8859-*
NgNGO*XmText*FontList:	-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-iso8859-*
NgNGO*XmTextField*FontList:	-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-iso8859-*
NgNGO*XmFrame*XmLabel.FontList:	-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-iso8859-*

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! Menubar controls and labels for entire app				!
! (*includes translations/accelerators/actions*)			!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

NgNGO*globalTranslations:	\
		Meta ~Ctrl<Key>C:	closeWindow()		\n\
		 Alt ~Ctrl<Key>C:	closeWindow()		\n\
		Meta ~Ctrl<Key>Q:	quitApplication()	\n\
		 Alt ~Ctrl<Key>Q:	quitApplication()	\n\
		Meta ~Ctrl<Key>A:	addFile()		\n\
		 Alt ~Ctrl<Key>A:	addFile()		\n\
		Meta ~Ctrl<Key>L:	loadScript()		\n\
		 Alt ~Ctrl<Key>L:	loadScript()		\n\
		Meta ~Ctrl<Key>N:	nclWindow()		\n\
		 Alt ~Ctrl<Key>N:	nclWindow()
!
! Menubar strings
!
*menubar.file.labelString:		File
*menubar.file.mnemonic:			F

*menubar.edit.labelString:		Edit
*menubar.edit.mnemonic:			E
*menubar.edit.sensitive:		False

*menubar.view.labelString:		View
*menubar.view.mnemonic:			V
*menubar.view.sensitive:		False

*menubar.options.labelString:		Options
*menubar.options.mnemonic:		O
*menubar.options.sensitive:		False

*menubar.window.labelString:		Window
*menubar.window.mnemonic:		W

*menubar.help.labelString:		Help
*menubar.help.mnemonic:			H
*menubar.help.sensitive:		False

*fmenu.addFile.labelString:		Add File
*fmenu.addFile.mnemonic:		A
*fmenu.addFile.acceleratorText:		Alt+A

*fmenu.loadScript.labelString:		Load Script
*fmenu.loadScript.mnemonic:		L
*fmenu.loadScript.acceleratorText:	Alt+L

*fmenu.closeWindow.labelString:		Close
*fmenu.closeWindow.mnemonic:		C
*fmenu.closeWindow.acceleratorText:	Alt+C

*fmenu.quitApplication.labelString:	Exit
*fmenu.quitApplication.mnemonic:	x
*fmenu.quitApplication.acceleratorText:	Alt+Q

*wmenu.nclWindow.labelString:		Ncl Editor
*wmenu.nclWindow.mnemonic:		N
*wmenu.nclWindow.acceleratorText:	Alt+N

!
! menubar geometry
!
NgNGO*XmForm.menubar.bottomAttachment:	ATTACH_NONE
NgNGO*XmForm.menubar.topOffset:		0
NgNGO*XmForm.menubar.bottomOffset:	0
NgNGO*XmForm.menubar.leftOffset:	0
NgNGO*XmForm.menubar.rightOffset:	0

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! Default geometry for entire app					!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*topAttachment:				ATTACH_FORM
*bottomAttachment:			ATTACH_FORM
*leftAttachment:			ATTACH_FORM
*rightAttachment:			ATTACH_FORM

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! Main Window								!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*mainMGR*topOffset:			5
*mainMGR*bottomOffset:			5
*mainMGR*leftOffset:			5
*mainMGR*rightOffset:			5

*mainMGR*XmPushButton.width:		32
*mainMGR*XmPushButton.height:		32

*mainMGR*pane.topOffset:		0
*mainMGR*pane.leftOffset:		0
*mainMGR*pane.rightOffset:		0
*mainMGR*pane.bottomOffset:		0
*mainMGR*pane.marginHeight:		0

*mainMGR*pane*ptbform*rightAttachment:	ATTACH_NONE
*mainMGR*pane*ptbform*labelString:	I

*mainMGR*otree.highlightRowMode:	True
*mainMGR*otree.horizontalSizePolicy:	CONSTANT
*mainMGR*otree.hsbDisplayPolicy:	STATIC
*mainMGR*otree.verticalSizePolicy:	CONSTANT
*mainMGR*otree.vsbDisplayPolicy:	STATIC

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! NCL EDITOR								!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ncledit*nclcmd.translations:		#override			\
		<Select>:		activate()			\n\
		<Key>Return:		process-return()		\
					activate()

! Geometry
*ncleditMGR*XmList.listSizePolicy:	CONSTANT
*ncleditMGR*XmList.visibleItemCount:	4

*ncleditMGR*XmFrame.marginWidth:	5
*ncleditMGR*XmFrame.marginHeight:	5
*ncleditMGR*XmFrame*childHorizontalAlignment:	ALIGNMENT_CENTER

! default XmForm constraints for all widgets in the ncledit window
*ncleditMGR*topOffset:			5
*ncleditMGR*bottomOffset:		5
*ncleditMGR*leftOffset:			5
*ncleditMGR*rightOffset:		5

*ncleditMGR*pane.topOffset:		0
*ncleditMGR*pane.leftOffset:		0
*ncleditMGR*pane.rightOffset:		0
*ncleditMGR*pane.bottomOffset:		0
*ncleditMGR*pane.marginHeight:		0

*ncleditMGR*slabel.bottomAttachment:	ATTACH_NONE

*ncleditMGR*sform*bottomOffset:		10
*ncleditMGR*sform*bottomOffset:		10

*ncleditMGR*nclprompt.topOffset:	12
*ncleditMGR*nclprompt.leftOffset:	4
*ncleditMGR*nclprompt*rightAttachment:	ATTACH_NONE
*ncleditMGR*nclprompt*bottomAttachment:	ATTACH_NONE
*ncleditMGR*nclprompt*highlightThickness:0
*ncleditMGR*nclprompt*borderWidth:	0
*ncleditMGR*nclprompt.marginWidth:	1

*ncleditMGR*nclcmd*highlightThickness:0
*ncleditMGR*nclcmd*borderWidth:		0
*ncleditMGR*nclcmd.marginWidth:		4

*ncleditMGR*scroll*topOffset:		12
*ncleditMGR*scroll*leftOffset:		0

*ncleditMGR*hoframe.rightPosition:	23
*ncleditMGR*vframe.leftPosition:	23
*ncleditMGR*vframe.rightPosition:	46
*ncleditMGR*fframe.leftPosition:	46
*ncleditMGR*fframe.rightPosition:	69
*ncleditMGR*fuframe.leftPosition:	69

*ncleditMGR*reset.topAttachment:	ATTACH_NONE
*ncleditMGR*reset.rightAttachment:	ATTACH_NONE

*ncleditMGR*ilabel.topAttachment:	ATTACH_NONE
*ncleditMGR*ilabel.alignment:		ALIGNMENT_BEGINNING

ncledit*nclprompt.editable:		False
ncledit*nclprompt.cursorPositionVisible:False
ncledit*nclprompt.traversalOn:		False
ncledit*nclcmd.columns:			40
ncledit*nclprompt.rows:			15
ncledit*nclcmd.rows:			15

! titles/labels and such
ncledit.title:				NCL Editor
ncledit*slabel.labelString:		Global State
ncledit*holabel.labelString:		Graphic Variables
ncledit*vlabel.labelString:		Regular Variables
ncledit*flabel.labelString:		File Variables
ncledit*fulabel.labelString:		Functions
ncledit*ilabel.labelString:
ncledit*reset.labelString:		Reset
ncledit*reset.sensitive:		False

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! addfile window							!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

addfile*vname.resizeWidth:		True
addfile*vname.value:			\ 
addfile*midtxt.labelString:		= addfile(\"[ ]\",\"
addfile*endtxt.labelString:		\")

addfile*vname.rightAttachment:		ATTACH_NONE
addfile*midtxt.rightAttachment:		ATTACH_NONE
addfile*rwoptMenu.rightAttachment:	ATTACH_NONE

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! Xwk (X Workstation Window)						!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
! This should probably move down into the hlu/gks level so users can
! specify the size of the output frame...
!
*xwork.ngxwkSize:			500

!
! TEMPORARY - UNTIL COLOR MANAGEMENT IS DONE
!
*xworkMGR*graphics.background:	black
*xworkMGR*graphics.foreground:	white

*xworkMGR*topOffset:			5
*xworkMGR*bottomOffset:			5
*xworkMGR*leftOffset:			5
*xworkMGR*rightOffset:			5

*xworkMGR.mgr.topOffset:		0
*xworkMGR.mgr.bottomOffset:		0
*xworkMGR.mgr.leftOffset:		0
*xworkMGR.mgr.rightOffset:		0
