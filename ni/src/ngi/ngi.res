!
!      $Id: ngi.res,v 1.1 1996-10-10 18:55:51 boote Exp $
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
		Meta ~Ctrl<Key>W:	closeWindow()		\n\
		 Alt ~Ctrl<Key>W:	closeWindow()		\n\
		Meta ~Ctrl<Key>Q:	quitApplication()	\n\
		 Alt ~Ctrl<Key>Q:	quitApplication()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NCL EDITOR			!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ncledit*nclcmd.translations:	#override				\
	<Select>:		activate()				\n\
	<Key>Return:		process-return()			\
				activate()

! Geometry
ncledit*XmList.listSizePolicy:	CONSTANT
ncledit*XmList.visibleItemCount:	4

ncledit*XmFrame.marginWidth:	5
ncledit*XmFrame.marginHeight:	5
ncledit*XmFrame*childHorizontalAlignment:	ALIGNMENT_CENTER

! default XmForm constraints for all widgets in the ncledit window
ncledit*topAttachment:		ATTACH_FORM
ncledit*bottomAttachment:	ATTACH_FORM
ncledit*leftAttachment:		ATTACH_FORM
ncledit*rightAttachment:	ATTACH_FORM
ncledit*topOffset:		5
ncledit*bottomOffset:		5
ncledit*leftOffset:		5
ncledit*rightOffset:		5

ncledit*menubar.bottomAttachment:	ATTACH_NONE
ncledit*menubar.topOffset:		0
ncledit*menubar.bottomOffset:		0
ncledit*menubar.leftOffset:		0
ncledit*menubar.rightOffset:		0

ncledit*pane.topOffset:			0
ncledit*pane.leftOffset:		0
ncledit*pane.rightOffset:		0
ncledit*pane.bottomOffset:		0
ncledit*pane.marginHeight:		0

ncleidt*slabel.topOffset:		0
ncledit*slabel.bottomAttachment:	ATTACH_NONE

ncledit*sform*bottomOffset:		10
ncledit*sform*bottomOffset:		10
ncledit*scroll*topOffset:		10

ncledit*holabel.bottomAttachment:	ATTACH_NONE
ncledit*vlabel.bottomAttachment:	ATTACH_NONE
ncledit*flabel.bottomAttachment:	ATTACH_NONE
ncledit*fulabel.bottomAttachment:	ATTACH_NONE

ncledit*reset.topAttachment:		ATTACH_NONE
ncledit*reset.rightAttachment:		ATTACH_NONE
ncledit*ilabel.topAttachment:		ATTACH_NONE
ncledit*ilabel.alignment:		ALIGNMENT_BEGINNING

ncledit*nclcmd.columns:			40
ncledit*nclcmd.rows:			15

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

*menubar.help.labelString:		Help
*menubar.help.mnemonic:			H
*menubar.help.sensitive:		False

*fmenu.closeWindow.labelString:		Close
*fmenu.closeWindow.mnemonic:		C
*fmenu.closeWindow.acceleratorText:	Alt+W

*fmenu.quitApplication.labelString:	Quit
*fmenu.quitApplication.mnemonic:	Q
*fmenu.quitApplication.acceleratorText:	Alt+Q

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

!ncledit*loadscript_fsb.pattern:	*.ncl
!ncledit*loadscript_fsb_popup.title:	Load NCL Script
!ncledit*loadscript_fsb.selectionLabelString:	Select NCL Script:
!ncledit*addfile_fsb_popup.title:	Add Data File
!ncledit*addfile_fsb.selectionLabelString:	Select Data file:
!ncledit*ncl_prompt_popup.title:	Add Data File
!ncledit*ncl_prompt.selectionLabelString:	Enter variable name for file
