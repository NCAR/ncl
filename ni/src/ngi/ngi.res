!
!      $Id: ngi.res,v 1.5 1997-01-17 18:58:37 boote Exp $
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
!
!

! These resources will *hopefully* be replaced by a configuration dialog
! in and "Options" menu.
NgNGO*loadfileMGR.pattern:	*.ncl
NgNGO*loadfileMGR.directory:	.
NgNGO*addfileMGR.pattern:	*.{cdf,nc,grb}
NgNGO*addfileMGR.directory:	.

NgNGO*background:	#b2b2b2
NgNGO*foreground:	black
NgNGO*XmList*background:	#bebebe
NgNGO*XmList*foreground:	black
NgNGO*XmText*background:	#bebebe
NgNGO*XmText*foreground:	black
NgNGO*XmTextField*background:	#bebebe
NgNGO*XmTextField*foreground:	black


NgNGO*FontList:		-*-helvetica-bold-r-normal-*-*-120-*-*-*-*-iso8859-*
NgNGO*menubar*FontList:	-*-helvetica-bold-r-normal-*-*-120-*-*-*-*-iso8859-*
NgNGO*XmText*FontList:	-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-iso8859-*
NgNGO*XmTextField*FontList:	-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-iso8859-*
NgNGO*XmFrame*XmLabel.FontList:	-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-iso8859-*

*globalTranslations:	\
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

!!!!!!!!!!!!!!!!!
! Main Window	!
!!!!!!!!!!!!!!!!!
main.mainMGR*topOffset:			5
main.mainMGR*bottomOffset:		5
main.mainMGR*leftOffset:		5
main.mainMGR*rightOffset:		5

main.mainMGR*XmPushButton.width:	32
main.mainMGR*XmPushButton.height:	32

main.mainMGR*menubar.bottomAttachment:	ATTACH_NONE
main.mainMGR*menubar.topOffset:		0
main.mainMGR*menubar.bottomOffset:	0
main.mainMGR*menubar.leftOffset:	0
main.mainMGR*menubar.rightOffset:	0

main.mainMGR*pane.topOffset:		0
main.mainMGR*pane.leftOffset:		0
main.mainMGR*pane.rightOffset:		0
main.mainMGR*pane.bottomOffset:		0
main.mainMGR*pane.marginHeight:		0

main.mainMGR*pane*ptbform*rightAttachment:	ATTACH_NONE
main.mainMGR*pane*ptbform*labelString:	I

main.mainMGR*otree.highlightRowMode:		True
main.mainMGR*otree.horizontalSizePolicy:	CONSTANT
main.mainMGR*otree.hsbDisplayPolicy:		STATIC
main.mainMGR*otree.verticalSizePolicy:		CONSTANT
main.mainMGR*otree.vsbDisplayPolicy:		STATIC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NCL EDITOR			!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


ncledit*nclcmd.translations:	#override				\
	<Select>:		activate()				\n\
	<Key>Return:		process-return()			\
				activate()

! Geometry
ncledit.ncleditMGR*XmList.listSizePolicy:	CONSTANT
ncledit.ncleditMGR*XmList.visibleItemCount:	4

ncledit.ncleditMGR*XmFrame.marginWidth:	5
ncledit.ncleditMGR*XmFrame.marginHeight:	5
ncledit.ncleditMGR*XmFrame*childHorizontalAlignment:	ALIGNMENT_CENTER

! default XmForm constraints for all widgets in the ncledit window
*topAttachment:			ATTACH_FORM
*bottomAttachment:		ATTACH_FORM
*leftAttachment:		ATTACH_FORM
*rightAttachment:		ATTACH_FORM
ncledit.ncleditMGR*topOffset:		5
ncledit.ncleditMGR*bottomOffset:	5
ncledit.ncleditMGR*leftOffset:		5
ncledit.ncleditMGR*rightOffset:		5

ncledit.ncleditMGR*menubar.bottomAttachment:	ATTACH_NONE
ncledit.ncleditMGR*menubar.topOffset:		0
ncledit.ncleditMGR*menubar.bottomOffset:	0
ncledit.ncleditMGR*menubar.leftOffset:		0
ncledit.ncleditMGR*menubar.rightOffset:		0

ncledit.ncleditMGR*pane.topOffset:		0
ncledit.ncleditMGR*pane.leftOffset:		0
ncledit.ncleditMGR*pane.rightOffset:		0
ncledit.ncleditMGR*pane.bottomOffset:		0
ncledit.ncleditMGR*pane.marginHeight:		0

ncledit.ncleditMGR*slabel.bottomAttachment:	ATTACH_NONE

ncledit.ncleditMGR*sform*bottomOffset:		10
ncledit.ncleditMGR*sform*bottomOffset:		10

ncledit.ncleditMGR*nclprompt.topOffset:		12
ncledit.ncleditMGR*nclprompt.leftOffset:	4
ncledit.ncleditMGR*nclprompt*rightAttachment:	ATTACH_NONE
ncledit.ncleditMGR*nclprompt*bottomAttachment:	ATTACH_NONE
ncledit.ncleditMGR*nclprompt*highlightThickness:0
ncledit.ncleditMGR*nclprompt*borderWidth:	0
!ncledit.ncleditMGR*nclprompt.shadowThickness:	0
ncledit.ncleditMGR*nclprompt.marginWidth:	1

ncledit.ncleditMGR*nclcmd*highlightThickness:0
ncledit.ncleditMGR*nclcmd*borderWidth:		0
!ncledit.ncleditMGR*nclcmd.shadowThickness:	0
ncledit.ncleditMGR*nclcmd.marginWidth:		4

ncledit.ncleditMGR*scroll*topOffset:		12
ncledit.ncleditMGR*scroll*leftOffset:		0

ncledit.ncleditMGR*hoframe.rightPosition:	23
ncledit.ncleditMGR*vframe.leftPosition:		23
ncledit.ncleditMGR*vframe.rightPosition:	46
ncledit.ncleditMGR*fframe.leftPosition:		46
ncledit.ncleditMGR*fframe.rightPosition:	69
ncledit.ncleditMGR*fuframe.leftPosition:	69

ncledit.ncleditMGR*reset.topAttachment:		ATTACH_NONE
ncledit.ncleditMGR*reset.rightAttachment:	ATTACH_NONE

ncledit.ncleditMGR*ilabel.topAttachment:	ATTACH_NONE
ncledit.ncleditMGR*ilabel.alignment:		ALIGNMENT_BEGINNING

ncledit*nclprompt.editable:		False
ncledit*nclprompt.cursorPositionVisible:False
ncledit*nclprompt.traversalOn:		False
ncledit*nclcmd.columns:			40
ncledit*nclprompt.rows:			15
ncledit*nclcmd.rows:			15

!
! Geometry for addfile window
!
addfile*vname.rightAttachment:		ATTACH_NONE
addfile*midtxt.rightAttachment:		ATTACH_NONE
addfile*rwoptMenu.rightAttachment:	ATTACH_NONE

!
! Menubar strings
!
*menubar.file.labelString:		File
*menubar.file.mnemonic:			F

*menubar.edit.labelString:		Edit
*menubar.edit.mnemonic:			E
*menubar.edit.sensitive:		False

*menubar.config.labelString:		Config
*menubar.config.mnemonic:		C
*menubar.config.sensitive:		False

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

! titles/labels and such
!ngi.ncledit.gotitle:		NCL Editor
ncledit.title:	NCL Editor
ncledit*slabel.labelString:	Global State
ncledit*holabel.labelString:	Graphic Variables
ncledit*vlabel.labelString:	Regular Variables
ncledit*flabel.labelString:	File Variables
ncledit*fulabel.labelString:	Functions
ncledit*ilabel.labelString:
ncledit*reset.labelString:	Reset
ncledit*reset.sensitive:	False
ncledit*close.labelString:	Close

addfile*vname.resizeWidth:	True
addfile*vname.value:	\ 
addfile*midtxt.labelString:	= addfile(\"[ ]\",\"
addfile*endtxt.labelString:	\")

!ncledit*loadscript_fsb_popup.title:	Load NCL Script
!ncledit*loadscript_fsb.selectionLabelString:	Select NCL Script:
!ncledit*addfile_fsb_popup.title:	Add Data File
!ncledit*addfile_fsb.selectionLabelString:	Select Data file:
!ncledit*ncl_prompt_popup.title:	Add Data File
!ncledit*ncl_prompt.selectionLabelString:	Enter variable name for file
