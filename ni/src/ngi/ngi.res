!
!      $Id: ngi.res,v 1.9 1997-06-20 18:23:20 dbrown Exp $
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

!!! DEBUGING !!!
NgNGO*synchronous:	True
*dragInitiatorProtocolStyle:	XmDRAG_NONE
*dragReceiverProtocolStyle:	XmDRAG_NONE


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! These resources will *hopefully* be replaced by a configuration dialog!
! in an "Options" menu.						!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

NgNGO*loadfileMGR.pattern:	*.ncl
NgNGO*loadfileMGR.directory:	.
NgNGO*addfileMGR*pattern:	*.{cdf,nc,grb,ccm}
NgNGO*addfileMGR*directory:	.

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! Basic color scheme for entire app					!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Change the following line to "XcbVisual" to get the best visual
! for the display (in this programmers humble opinion...).  However,
! this may cause some colormap flashing on some displays.
!NgNGO*visual:	XcbVisual
ngi.NgNGO*visual:	PseudoColor
NgNGO*visual:	PseudoColor
NgNGO*depth:	8

NgNGO*colorMode:	mixedcmap

NgNGO*maxColorCells:	0
NgNGO*rgbError:		5

NgNGO*background:	#b2b2b2
NgNGO*foreground:	black
NgNGO*topShadowColor:	#e1e1e1
NgNGO*bottomShadowColor:	#656565
NgNGO*highlightColor:	black
NgNGO*troughColor:	#a2a2a2
NgNGO*armColor:		#a2a2a2
NgNGO*selectColor:	#a2a2a2

NgNGO*XmList.background:	#bfbfbf
NgNGO*XmText.background:	#bfbfbf

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

!
! Motif 2.0 versions of above...(Not tested.)
!
!NgNGO*renderTable: bold, medium
!NgNGO*renderTable*bold.fontName:	-*-helvetica-bold-r-normal-*-*-120-*-*-*-*-iso8859-*
!NgNGO*renderTable*medium.fontName:	-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-iso8859-*
!NgNGO*renderTable*renditionForeground:	UNSPECIFIED_PIXEL
!NgNGO*renderTable*renditionBackground:	UNSPECIFIED_PIXEL
!NgNGO*renderTable*fontType:		FONT_IS_FONT
!NgNGO*renderTable*underlineType:	AS_IS

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
		 Alt ~Ctrl<Key>N:	nclWindow()		\n\
		Meta ~Ctrl<Key>D:	browseWindow()		\n\
		 Alt ~Ctrl<Key>D:	browseWindow()
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
*wmenu.browseWindow.labelString:	Data Browser
*wmenu.browseWindow.mnemonic:		D
*wmenu.browseWindow.acceleratorText:	Alt+D

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

addfile*vname.rightAttachment:		ATTACH_NONE
addfile*rwoptMenu.rightAttachment:	ATTACH_NONE

addfile*varform.topOffset: 10
addfile*varform.bottomOffset: 10
addfile*varform.rightOffset: 10
addfile*varlabel.labelString: Variables
addfile*addfile_fsb*OK.shadowThickness: 0
addfile*filterform.rightOffset: 10
addfile*filterform.leftOffset: 10
addfile*SelectText.leftOffset: 10
addfile*SelectText.rightOffset: 10
addfile*sizelabel.leftOffset: 0
addfile*sizelabel.bottomOffset: 10
addfile*sizeframe.bottomOffset: 10
addfile*datelabel.leftOffset: 0
addfile*datelabel.bottomOffset: 10
addfile*dateframe.bottomOffset: 10
addfile*dateframe.rightOffset: 10
addfile*sep.bottomOffset: 5
addfile*sep.leftOffset: 0
addfile*sep.rightOffset: 0
addfile*applyform.bottomOffset: 5
addfile*workareaform.bottomOffset: 0
addfile*workareaform.leftOffset: 0
addfile*workareaform.topOffset: 0
addfile*workareaform.rightOffset: 0

addfile*ItemsList.translations:  #override     \
        Button1<Motion>:        ListBeginSelect() \n\
        <Btn1Down>:             ListBeginSelect() \
                                ListEndSelect() \
                                SelectFileAction() \n\
        <Btn1Up>:               ListEndSelect() \
                                SelectFileAction() \n\
        <Btn1Up>(2+):           ListEndSelect() \
                                OpenDataFileAction() \n\
        <Key>Return:            OpenDataFileAction() \n\
        <Select>:               SelectFileAction() \n\
        <Key>osfUp:             ListPrevItem() \
                                SelectFileAction() \n\
        <Key>osfDown:           ListNextItem() \
                                SelectFileAction() \n\
        <Key>osfPageUp:         ListPrevPage() \
                                SelectFileAction() \n\
        <Key>osfPageDown:       ListNextPage() \
                                SelectFileAction() \n\
        <Btn3Down>:             ListBeginSelect() \
                                ListEndSelect() \
                                SelectFileAction() \
                                InfoPopupAction(2) \n\
        <Btn3Up>:               ListEndSelect() \
                                InfoPopdownAction(2)


addfile*DirList.translations:  #override       \
        <Key>Return:            FilterAction() \n\
        <Select>:               FilterTextAction() \n\
        <Btn1Up>(2+):           ListBeginSelect() \
                                ListEndSelect() \
                                FilterAction() \n\
        <Btn1Down>,<Btn1Up>:    ListBeginSelect() \
                                ListEndSelect() \
                                FilterTextAction() \n\
        <Key>osfUp:             ListPrevItem() \
                                FilterTextAction() \n\
        <Key>osfDown:           ListNextItem() \
                                FilterTextAction() \n\
        <Key>osfPageUp:         ListPrevPage() \
                                FilterTextAction() \n\
        <Key>osfPageDown:       ListNextPage() \
                                FilterTextAction()

addfile*FilterText.translations:  #override    \
        <Key>Return:            FilterAction() \n\
        <Key>osfUp:             ListUpOrDownAction(0) \n\
        <Key>osfDown:           ListUpOrDownAction(1) \n\
        <Key>osfPageUp:         ListUpOrDownAction(2) \n\
        <Key>osfPageDown:       ListUpOrDownAction(3)

addfile*SelectText.translations:  #override    \
        <Key>osfUp:             ListUpOrDownAction(0)\n\
        <Key>osfDown:           ListUpOrDownAction(1) \n\
        <Key>osfPageUp:         ListUpOrDownAction(2) \n\
        <Key>osfPageDown:       ListUpOrDownAction(3)

addfile*VarList.translations:  #override       \
        Button3<Motion>:        ListButtonMotion() \
                                CheckInfoPopupAction(1) \n\
        <Btn1Down>:             ListBeginSelect() \
                                InfoPopupAction(0) \n\
        <Btn1Up>:               ListEndSelect() \
                                InfoPopdownAction(0) \n\
        Button1<Motion>:        ListButtonMotion() \
                                CheckInfoPopupAction(0) \n\
        <Btn3Down>:             ListBeginSelect() \
                                InfoPopupAction(1) \n\
        <Btn3Up>:               ListEndSelect() \
                                InfoPopdownAction(1)

addfile*Start_Stop.translations: #override \
        <Btn1Down>,<Btn1Up>: ArmAndActivate() VcrToggleAction(0)

! for development
!addfile*directory:     /traver/dev/IRIS_IRIX_6_2_/lib/ncarg/data/cdf
!addfile*directory:     /fs/scd/home1/dbrown/src/data
addfile*directory:     /fs/scd/home1/ncargd/dev/IRIS_IRIX_6_2_/lib/ncarg/data/cdf/

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

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! DATA BROWSER								!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!*browseMGR*Folder*tab.translations:  #override     \
!	<FocusIn>:	XmLFolderPrimFocusIn() \
!			TabFocusAction() \n\
!	<FocusOut>:	XmLFolderPrimFocusOut()
*browseMGR*FileTree*translations: #override \
        <Btn3Up>:               Button3Action()

*browseMGR*DataGrid*translations: #override \
        <Key>osfDown:	XmLGridTraverse(DOWN) \
			GridTraverseAction()

! Geometry
browse.title:				Data Browser
*browseMGR*XmList.listSizePolicy:	CONSTANT
*browseMGR*XmList.visibleItemCount:	4

*browseMGR*XmFrame.marginWidth:	5
*browseMGR*XmFrame.marginHeight:	5
*browseMGR*XmFrame*childHorizontalAlignment:	ALIGNMENT_CENTER

! default XmForm constraints for all widgets in the browse window
*browseMGR*topOffset:		0
*browseMGR*bottomOffset:	0
*browseMGR*leftOffset:		0
*browseMGR*rightOffset:		0
*browseMGR*spacing: 0
*browseMGR*vbutton*marginHeight: 0

*browseMGR*pane.topOffset:		0
*browseMGR*pane.leftOffset:		0
*browseMGR*pane.rightOffset:		0
*browseMGR*pane.bottomOffset:		0
*browseMGR*pane.marginHeight:		0

*browseMGR*Folder.topOffset:		5
*browseMGR*Folder.leftOffset:		5
*browseMGR*Folder.rightOffset:		5
*browseMGR*Folder.bottomOffset:		5
*browseMGR*Folder.width: 500
*browseMGR*Folder.height: 500
*browseMGR*topform.width: 500
*browseMGR*topform.height: 500
