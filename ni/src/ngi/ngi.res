!
!      $Id: ngi.res,v 1.23 1999-03-05 01:15:20 dbrown Exp $
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
NgNGO*addfileMGR*pattern:	*.{cdf,nc,grb,hdf,ccm}
NgNGO*addfileMGR*directory:	.

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! Basic color scheme for entire app					!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! It is not recommended to set the "visual" resource, however, if you
! feel you must, then remember to set the "depth" resource with it
! or it will be ignored. DefaultVisual is a special visual name that
! means to use the DefaultVisual of the display, even if it isn't optimal.
! XcbVisual is a special visual name that means to use the "best" possible
! visual (based on an internal algorithm tuned for ngi).
! The possible values for the "visual" resource string are:
!	StaticGray,StaticColor,TrueColor,GrayScale,
!	PseudoColor,DirectColor,DefaultVisual,XcbVisual
!			or
!	the actual hex id of the visual (Server dependent).
!
NgNGO*visual:		XcbVisual
!NgNGO*visual:		PseudoColor
!NgNGO*depth:		8

!NgNGO*colorMode:	privatecmap
NgNGO*colorMode:	sharedcmap
NgNGO*colorMode:	mixedcmap

!NgNGO*maxColorCells:	0
NgNGO*rgbError:		5
ngi.minColorCells:	20

NgNGO*background:	#b2b2b2
NgNGO*foreground:	black
NgNGO*topShadowColor:	#e1e1e1
NgNGO*bottomShadowColor:	#656565
NgNGO*highlightColor:	black
NgNGO*troughColor:	#a2a2a2
NgNGO*armColor:		#a2a2a2
NgNGO*selectColor:	#a2a2a2

! XmL widget color resources
NgNGO*blankBackground:	#b2b2b2
NgNGO*inactiveBackground:	#b2b2b2
NgNGO*inactiveForeground:	black
NgNGO*selectBackground:		black
NgNGO*selectForeground:		#b2b2b2
NgNGO*cellBackground:		#b2b2b2
NgNGO*cellBottomBorderColor:	#656565
NgNGO*cellForeground:		black
NgNGO*cellLeftBorderColor:	#e1e1e1
NgNGO*cellRightBorderColor:	#656565
NgNGO*cellTopBorderColor:	#e1e1e1
NgNGO*connectingLineColor:	black
NgNGO*plusMinusColor:		black

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
		Ctrl ~Alt<Key>N:	nclWindow("new")	\n\
		Meta ~Ctrl<Key>B:	browseWindow()		\n\
		 Alt ~Ctrl<Key>B:	browseWindow()		\n\
		Ctrl ~Alt<Key>B:	browseWindow("new")	\n\
		Ctrl ~Alt~Meta<Key>C:	colorMapEditor()

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

*fmenu.nclWindow.labelString:		Ncl Editor
*fmenu.nclWindow.mnemonic:		N
*fmenu.nclWindow.acceleratorText:	Ctrl+N

*fmenu.browseWindow.labelString:	Browser
*fmenu.browseWindow.mnemonic:		B
*fmenu.browseWindow.acceleratorText:	Ctrl+B

*fmenu.addFile.labelString:		Add File
*fmenu.addFile.mnemonic:		A
*fmenu.addFile.acceleratorText:		Alt+A

*fmenu.loadScript.labelString:		Load Script
*fmenu.loadScript.mnemonic:		L
*fmenu.loadScript.acceleratorText:	Alt+L

*fmenu.printPlot.labelString:		Print/Output Plot
*fmenu.printPlot.mnemonic:		P
*fmenu.printPlot.acceleratorText:	Ctrl+P

*fmenu.closeWindow.labelString:		Close
*fmenu.closeWindow.mnemonic:		C
*fmenu.closeWindow.acceleratorText:	Alt+C

*fmenu.quitApplication.labelString:	Exit
*fmenu.quitApplication.mnemonic:	x
*fmenu.quitApplication.acceleratorText:	Alt+Q

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
*mainMGR*otree.translations: #override \
        <Btn1Down>:               RowMoveAction() \n\
	<Btn1Up>:		  RowSetPosAction()

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

ncledit*errorPopupOption.labelString:	Editor popups on error
ncledit*errorBellOption.labelString:	Error bell on

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! addfile window							!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!addfile*vname.resizeWidth:		True
!addfile*vname.value:			\ 

!addfile*vname.rightAttachment:		ATTACH_NONE
!addfile*rwoptMenu.rightAttachment:	ATTACH_NONE

addfile*varform.topOffset: 10
addfile*varform.bottomOffset: 10
addfile*varform.rightOffset: 10
addfile*varlabel.labelString: Variables
addfile*addfile_fsb.shadowThickness: 0
addfile*addfile_fsb*OK.shadowThickness: 0
addfile*filterform.rightOffset: 10
addfile*filterform.leftOffset: 10
!addfile*vname.leftOffset: 10
addfile*vname.rightOffset: 10
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
addfile*ItemsList*XmNscrollBarDisplayPolicy: STATIC

addfile*FilterText.translations:  #override    \
        <Key>Return:            FilterAction() \n\
        <Key>osfUp:             ListUpOrDownAction(0) \n\
        <Key>osfDown:           ListUpOrDownAction(1) \n\
        <Key>osfPageUp:         ListUpOrDownAction(2) \n\
        <Key>osfPageDown:       ListUpOrDownAction(3)

addfile*vname.translations:  #override    \
        <Key>Return:            ApplyAction() \n\
        <Key>osfUp:             ListUpOrDownAction(0)\n\
        <Key>osfDown:           ListUpOrDownAction(1) \n\
        <Key>osfPageUp:         ListUpOrDownAction(2) \n\
        <Key>osfPageDown:       ListUpOrDownAction(3)

addfile*Start_Stop.translations: #override \
        <Btn1Down>,<Btn1Up>: ArmAndActivate() VcrToggleAction(0)

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

*xwork*emenu*colorMapEditor.labelString:		Colormap
*xwork*emenu*colorMapEditor.mnemonic:			C
*xwork*emenu*colorMapEditor.acceleratorText:		Ctrl+C

*xwork*omenu*drawSingleViewOption.labelString:		Draw Selected View Only
*xwork*omenu*autoRefreshOption.labelString:		Auto Update

*xwork*vmenu*clearAllViews.labelString:                 Clear All Views
*xwork*vmenu*drawAllViews.labelString:                  Draw All Views



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
! colorMapEditor							!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*colorMapEditor.title:			Colormap Editor
*colorMapEditor*Offset:			0

*colorMapEditor*cframe.topAttachment:	ATTACH_FORM
*colorMapEditor*cframe.leftAttachment:	ATTACH_FORM
*colorMapEditor*cframe.rightAttachment:	ATTACH_FORM
*colorMapEditor*cframe.bottomAttachment:	ATTACH_NONE
*colorMapEditor*cframe.topOffset:	10
*colorMapEditor*cframe.leftOffset:	10
*colorMapEditor*cframe.rightOffset:	10

*colorMapEditor*cform.mbar.background:		red
*colorMapEditor*cform.mbar.topAttachment:	ATTACH_FORM
*colorMapEditor*cform.mbar.leftAttachment:	ATTACH_FORM
*colorMapEditor*cform.mbar.rightAttachment:	ATTACH_NONE
*colorMapEditor*cform.mbar.bottomAttachment:	ATTACH_FORM
*colorMapEditor*cform.mbar.adjustLast:		True
*colorMapEditor*cform.mbar.spacing:		0
*colorMapEditor*cform.mbar.borderWidth:	0
*colorMapEditor*cform.mbar.marginHeight:	0
*colorMapEditor*cform.mbar.marginWidth:	0

*colorMapEditor*pal.labelString:		Select a Palette

*colorMapEditor*cform.clabel.topAttachment:	ATTACH_FORM
*colorMapEditor*cform.clabel.leftAttachment:	ATTACH_WIDGET
*colorMapEditor*cform.clabel.rightAttachment:	ATTACH_NONE
*colorMapEditor*cform.clabel.bottomAttachment:	ATTACH_FORM
*colorMapEditor*cform.clabel.leftOffset:	5
*colorMapEditor*cform.clabel.labelString:	Colormap Size:

*colorMapEditor*cform.csize.topAttachment:	ATTACH_FORM
*colorMapEditor*cform.csize.leftAttachment:	ATTACH_WIDGET
*colorMapEditor*cform.csize.rightAttachment:	ATTACH_FORM
*colorMapEditor*cform.csize.bottomAttachment:	ATTACH_FORM

*colorMapEditor*cform.csize.orientation:	HORIZONTAL
*colorMapEditor*cform.csize.showValue:		True
*colorMapEditor*cform.csize.maximum:		256
*colorMapEditor*cform.csize.minimum:		2
*colorMapEditor*cform.csize.decimalPoints:	0

*colorMapEditor*ok.labelString:		\ Ok 
*colorMapEditor*ok.topAttachment:	ATTACH_NONE
*colorMapEditor*ok.leftAttachment:	ATTACH_FORM
*colorMapEditor*ok.rightAttachment:	ATTACH_POSITION
*colorMapEditor*ok.bottomAttachment:	ATTACH_FORM
*colorMapEditor*ok.rightPosition:	25
*colorMapEditor*ok.topOffset:		5
*colorMapEditor*ok.leftOffset:		5
*colorMapEditor*ok.rightOffset:		5
*colorMapEditor*ok.bottomOffset:		5

*colorMapEditor*apply.labelString:	\ Apply 
*colorMapEditor*apply.topAttachment:	ATTACH_NONE
*colorMapEditor*apply.leftAttachment:	ATTACH_POSITION
*colorMapEditor*apply.rightAttachment:	ATTACH_POSITION
*colorMapEditor*apply.bottomAttachment:	ATTACH_FORM
*colorMapEditor*apply.leftPosition:	25
*colorMapEditor*apply.rightPosition:	50
*colorMapEditor*apply.topOffset:		5
*colorMapEditor*apply.leftOffset:	5
*colorMapEditor*apply.rightOffset:	5
*colorMapEditor*apply.bottomOffset:	5

*colorMapEditor*cancel.labelString:	\ Cancel 
*colorMapEditor*cancel.topAttachment:	ATTACH_NONE
*colorMapEditor*cancel.leftAttachment:	ATTACH_POSITION
*colorMapEditor*cancel.rightAttachment:	ATTACH_POSITION
*colorMapEditor*cancel.bottomAttachment:	ATTACH_FORM
*colorMapEditor*cancel.leftPosition:	50
*colorMapEditor*cancel.rightPosition:	75
*colorMapEditor*cancel.topOffset:	5
*colorMapEditor*cancel.leftOffset:	5
*colorMapEditor*cancel.rightOffset:	5
*colorMapEditor*cancel.bottomOffset:	5

*colorMapEditor*help.labelString:	\ Help 
*colorMapEditor*help.topAttachment:	ATTACH_NONE
*colorMapEditor*help.leftAttachment:	ATTACH_POSITION
*colorMapEditor*help.rightAttachment:	ATTACH_FORM
*colorMapEditor*help.bottomAttachment:	ATTACH_FORM
*colorMapEditor*help.leftPosition:	75
*colorMapEditor*help.topOffset:		5
*colorMapEditor*help.leftOffset:		5
*colorMapEditor*help.rightOffset:	5
*colorMapEditor*help.bottomOffset:	5

*colorMapEditor*bottom.topAttachment:	ATTACH_NONE
*colorMapEditor*bottom.leftAttachment:	ATTACH_FORM
*colorMapEditor*bottom.rightAttachment:	ATTACH_FORM
*colorMapEditor*bottom.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*bottom.bottomOffset:	3

*colorMapEditor*curframe.topAttachment:	ATTACH_NONE
*colorMapEditor*curframe.leftAttachment:	ATTACH_FORM
*colorMapEditor*curframe.rightAttachment:ATTACH_FORM
*colorMapEditor*curframe.bottomAttachment:ATTACH_WIDGET
*colorMapEditor*curframe.topOffset:	10
*colorMapEditor*curframe.leftOffset:	10
*colorMapEditor*curframe.rightOffset:	10
*colorMapEditor*curframe.bottomOffset:	10

*colorMapEditor*XmTextField.cursorPositionVisible:	False
*colorMapEditor*XmTextField.editable:			False
*colorMapEditor*XmTextField.marginHeight:		2
*colorMapEditor*XmTextField.marginWidth:		2

*colorMapEditor*indxt.topAttachment:	ATTACH_NONE
*colorMapEditor*indxt.leftAttachment:	ATTACH_NONE
*colorMapEditor*indxt.rightAttachment:	ATTACH_POSITION
*colorMapEditor*indxt.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*indxt.bottomOffset:	3
*colorMapEditor*indxt.rightPosition:	24
*colorMapEditor*indxt.columns:		3
*colorMapEditor*indxt.maxLength:		3
*colorMapEditor*indxt.alignment:		ALIGNMENT_BEGINNING

*colorMapEditor*indxl.topAttachment:	ATTACH_OPPOSITE_WIDGET
*colorMapEditor*indxl.leftAttachment:	ATTACH_FORM
*colorMapEditor*indxl.rightAttachment:	ATTACH_WIDGET
*colorMapEditor*indxl.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*indxl.bottomOffset:	3
*colorMapEditor*indxl.alignment:		ALIGNMENT_END
*colorMapEditor*indxl.labelString:	Index:

*colorMapEditor*redt.topAttachment:	ATTACH_NONE
*colorMapEditor*redt.leftAttachment:	ATTACH_NONE
*colorMapEditor*redt.rightAttachment:	ATTACH_POSITION
*colorMapEditor*redt.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*redt.bottomOffset:	3
*colorMapEditor*redt.rightPosition:	48
*colorMapEditor*redt.columns:		5
*colorMapEditor*redt.maxLength:		5
*colorMapEditor*redt.alignment:		ALIGNMENT_BEGINNING

*colorMapEditor*redl.topAttachment:	ATTACH_OPPOSITE_WIDGET
*colorMapEditor*redl.leftAttachment:	ATTACH_POSITION
*colorMapEditor*redl.rightAttachment:	ATTACH_WIDGET
*colorMapEditor*redl.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*redl.bottomOffset:	3
*colorMapEditor*redl.leftPosition:	24
*colorMapEditor*redl.alignment:		ALIGNMENT_END
*colorMapEditor*redl.labelString:	Red:

*colorMapEditor*greent.topAttachment:	ATTACH_NONE
*colorMapEditor*greent.leftAttachment:	ATTACH_NONE
*colorMapEditor*greent.rightAttachment:	ATTACH_POSITION
*colorMapEditor*greent.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*greent.bottomOffset:	3
*colorMapEditor*greent.rightPosition:	75
*colorMapEditor*greent.columns:		5
*colorMapEditor*greent.maxLenth:		5
*colorMapEditor*greent.alignment:	ALIGNMENT_BEGINNING

*colorMapEditor*greenl.topAttachment:	ATTACH_OPPOSITE_WIDGET
*colorMapEditor*greenl.leftAttachment:	ATTACH_POSITION
*colorMapEditor*greenl.rightAttachment:	ATTACH_WIDGET
*colorMapEditor*greenl.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*greenl.bottomOffset:	3
*colorMapEditor*greenl.leftPosition:	48
*colorMapEditor*greenl.alignment:	ALIGNMENT_END
*colorMapEditor*greenl.labelString:	Green:

*colorMapEditor*bluet.topAttachment:	ATTACH_NONE
*colorMapEditor*bluet.leftAttachment:	ATTACH_NONE
*colorMapEditor*bluet.rightAttachment:	ATTACH_FORM
*colorMapEditor*bluet.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*bluet.bottomOffset:	3
*colorMapEditor*bluet.columns:		5
*colorMapEditor*bluet.maxLenth:		5
*colorMapEditor*bluet.rightOffset:	5
*colorMapEditor*bluet.alignment:		ALIGNMENT_BEGINNING

*colorMapEditor*bluel.topAttachment:	ATTACH_OPPOSITE_WIDGET
*colorMapEditor*bluel.leftAttachment:	ATTACH_POSITION
*colorMapEditor*bluel.rightAttachment:	ATTACH_WIDGET
*colorMapEditor*bluel.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*bluel.leftPosition:	75
*colorMapEditor*bluel.topOffset:		0
*colorMapEditor*bluel.rightOffset:	0
*colorMapEditor*bluel.bottomOffset:	3
*colorMapEditor*bluel.alignment:		ALIGNMENT_END
*colorMapEditor*bluel.labelString:	Blue:

*colorMapEditor*cur_def.topAttachment:	ATTACH_FORM
*colorMapEditor*cur_def.leftAttachment:	ATTACH_POSITION
*colorMapEditor*cur_def.rightAttachment:	ATTACH_FORM
*colorMapEditor*cur_def.bottomAttachment:ATTACH_WIDGET
*colorMapEditor*cur_def.topOffset:	3
*colorMapEditor*cur_def.rightOffset:	3
*colorMapEditor*cur_def.bottomOffset:	3
*colorMapEditor*cur_def.leftPosition:	50
*colorMapEditor*cur_def.labelString:	\ 

*colorMapEditor*cdefl.topAttachment:	ATTACH_FORM
*colorMapEditor*cdefl.leftAttachment:	ATTACH_FORM
*colorMapEditor*cdefl.rightAttachment:	ATTACH_WIDGET
*colorMapEditor*cdefl.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*cur_def.topOffset:	3
*colorMapEditor*cur_def.leftOffset:	3
*colorMapEditor*cur_def.rightOffset:	3
*colorMapEditor*cur_def.bottomOffset:	3
!colorMapEditor*cdefl.alignment:	ALIGNMENT_END
*colorMapEditor*cdefl.labelString:	Current Color:

*colorMapEditor*eframe.topAttachment:	ATTACH_WIDGET
*colorMapEditor*eframe.leftAttachment:	ATTACH_FORM
*colorMapEditor*eframe.rightAttachment:	ATTACH_FORM
*colorMapEditor*eframe.bottomAttachment:	ATTACH_WIDGET
*colorMapEditor*eframe.topOffset:	10
*colorMapEditor*eframe.leftOffset:	10
*colorMapEditor*eframe.rightOffset:	10
*colorMapEditor*eframe.bottomOffset:	10

*colorMapEditor*sform.topAttachment:	ATTACH_FORM
*colorMapEditor*sform.leftAttachment:	ATTACH_NONE
*colorMapEditor*sform.rightAttachment:	ATTACH_FORM
*colorMapEditor*sform.bottomAttachment:	ATTACH_FORM

*colorMapEditor*elabel.topAttachment:	ATTACH_FORM
*colorMapEditor*elabel.leftAttachment:	ATTACH_FORM
*colorMapEditor*elabel.rightAttachment:	ATTACH_FORM
*colorMapEditor*elabel.bottomAttachment:	ATTACH_NONE
*colorMapEditor*elabel.topOffset:	3
*colorMapEditor*elabel.leftOffset:	3
*colorMapEditor*elabel.rightOffset:	3
*colorMapEditor*elabel.bottomOffset:	3
*colorMapEditor*elabel.labelString:	\ 

*colorMapEditor*XmScale.decimalPoints:	3
*colorMapEditor*XmScale.maximum:		1000
*colorMapEditor*XmScale.minimum:		0
*colorMapEditor*XmScale.showValue:		True

*colorMapEditor*rsframe.topAttachment:	ATTACH_WIDGET
*colorMapEditor*rsframe.leftAttachment:	ATTACH_FORM
*colorMapEditor*rsframe.rightAttachment:	ATTACH_WIDGET
*colorMapEditor*rsframe.bottomAttachment:ATTACH_FORM
*colorMapEditor*rsframe.topOffset:	3
*colorMapEditor*rsframe.leftOffset:	3
*colorMapEditor*rsframe.rightOffset:	3
*colorMapEditor*rsframe.bottomOffset:	3

*colorMapEditor*rscalel.topAttachment:	ATTACH_FORM
*colorMapEditor*rscalel.leftAttachment:	ATTACH_FORM
*colorMapEditor*rscalel.rightAttachment:	ATTACH_FORM
*colorMapEditor*rscalel.bottomAttachment:ATTACH_NONE
*colorMapEditor*rscalel.labelString:	Red

*colorMapEditor*rscale.topAttachment:	ATTACH_WIDGET
*colorMapEditor*rscale.leftAttachment:	ATTACH_FORM
*colorMapEditor*rscale.rightAttachment:	ATTACH_FORM
*colorMapEditor*rscale.bottomAttachment:	ATTACH_FORM

*colorMapEditor*gsframe.topAttachment:	ATTACH_WIDGET
*colorMapEditor*gsframe.leftAttachment:	ATTACH_NONE
*colorMapEditor*gsframe.rightAttachment:	ATTACH_WIDGET
*colorMapEditor*gsframe.bottomAttachment:ATTACH_FORM
*colorMapEditor*gsframe.topOffset:	3
*colorMapEditor*gsframe.leftOffset:	3
*colorMapEditor*gsframe.rightOffset:	3
*colorMapEditor*gsframe.bottomOffset:	3

*colorMapEditor*gscalel.topAttachment:	ATTACH_FORM
*colorMapEditor*gscalel.leftAttachment:	ATTACH_FORM
*colorMapEditor*gscalel.rightAttachment:	ATTACH_FORM
*colorMapEditor*gscalel.bottomAttachment:ATTACH_NONE
*colorMapEditor*gscalel.labelString:	Green

*colorMapEditor*gscale.topAttachment:	ATTACH_WIDGET
*colorMapEditor*gscale.leftAttachment:	ATTACH_FORM
*colorMapEditor*gscale.rightAttachment:	ATTACH_FORM
*colorMapEditor*gscale.bottomAttachment:	ATTACH_FORM

*colorMapEditor*bsframe.topAttachment:	ATTACH_WIDGET
*colorMapEditor*bsframe.leftAttachment:	ATTACH_NONE
*colorMapEditor*bsframe.rightAttachment:	ATTACH_FORM
*colorMapEditor*bsframe.bottomAttachment:ATTACH_FORM
*colorMapEditor*bsframe.topOffset:	3
*colorMapEditor*bsframe.leftOffset:	3
*colorMapEditor*bsframe.rightOffset:	3
*colorMapEditor*bsframe.bottomOffset:	3

*colorMapEditor*bscalel.topAttachment:	ATTACH_FORM
*colorMapEditor*bscalel.leftAttachment:	ATTACH_FORM
*colorMapEditor*bscalel.rightAttachment:	ATTACH_FORM
*colorMapEditor*bscalel.bottomAttachment:ATTACH_NONE
*colorMapEditor*bscalel.labelString:	Blue

*colorMapEditor*bscale.topAttachment:	ATTACH_WIDGET
*colorMapEditor*bscale.leftAttachment:	ATTACH_FORM
*colorMapEditor*bscale.rightAttachment:	ATTACH_FORM
*colorMapEditor*bscale.bottomAttachment:	ATTACH_FORM

*colorMapEditor*cmapdpy.topAttachment:	ATTACH_FORM
*colorMapEditor*cmapdpy.leftAttachment:	ATTACH_FORM
*colorMapEditor*cmapdpy.rightAttachment:	ATTACH_WIDGET
*colorMapEditor*cmapdpy.bottomAttachment:ATTACH_FORM
*colorMapEditor*cmapdpy.topOffset:	3
*colorMapEditor*cmapdpy.leftOffset:	3
*colorMapEditor*cmapdpy.rightOffset:	5
*colorMapEditor*cmapdpy.bottomOffset:	3
*colorMapEditor*cmapdpy.spacing:		0
*colorMapEditor*cmapdpy.borderWidth:	0
*colorMapEditor*cmapdpy.marginHeight:	0
*colorMapEditor*cmapdpy.marginWidth:	0
*colorMapEditor*cmapdpy.adjustLast:	False

*colorMapEditor*indx.highlightThickness:	0
*colorMapEditor*indx.shadowThickness:	0
*colorMapEditor*indx.marginHeight:	0
*colorMapEditor*indx.marginWidth:	0
*colorMapEditor*indx.borderWidth:	1
*colorMapEditor*indx.width:		13
*colorMapEditor*indx.height:		10
*colorMapEditor*indx.translations:	#override		\
		<Btn2Down>:		Arm()			\n\
		<Btn2Down>,<Btn2Up>:	Activate() Disarm()

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
*browse*helpBtn.labelString: Getting Started

!*browseMGR*EnumMenu*translations: #override \
!        <Btn1Up>:               EnumButtonUpAction()

*VarPage*ShaperTgl.labelString: Select a Subregion
*VarPage*DatagridTgl.labelString: Data
*VarPage*OutputMenuLbl.labelString: Output Data:

*CreateDialog*okLabelString : Create
*CreateDialog*ConfigurePB.labelString : Configure

*HluVarsBtn.labelString: Graphic Vars
*RegVarsBtn.labelString: Regular Vars
*FilesBtn.labelString: Files
*FileVarsBtn.labelString: File Vars
*FunctionsBtn.labelString: Functions

!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! PRINT DIALOG								!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


*printPlot*title:				Print/Output Plot
*printPlot*printDestinationLbl.labelString:	Print To:
*printPlot*printerTgl.labelString:		Printer
*printPlot*fileTgl.labelString:			File
*printPlot*printCommandLbl.labelString:		Print Command:
*printPlot*fileNameLbl.labelString:		File:
*printPlot*fileTypeLbl.labelString:		File Type:
*printPlot*overwriteTgl.labelString:		Confirm Overwrite
*printPlot*orientationLbl.labelString:		Orientation:
*printPlot*portraitPb.labelString:		Portrait
*printPlot*landscapePb.labelString:		Landscape
*printPlot*autoOrientPb.labelString:		Auto Orient
*printPlot*plotBoundsLbl.labelString:		Extent:
*printPlot*maximizeBBTgl.labelString:		Maximal BBox
*printPlot*fullViewspaceTgl.labelString:	Full Viewspace
*printPlot*paperSizeLbl.labelString:		Paper Size:
*printPlot*resolutionLbl.labelString:		Resolution:
*printPlot*workstationLbl.labelString: 		Workstation:
*printPlot*plotViewsLbl.labelString:		Plot Views:
*printPlot*selectAllTgl.labelString:		All
*printPlot*selectedViewsTgl.labelString: 	Selected


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
! DEVELOPMENT STUFF
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! for development
addfile*directory: /fs/scd/home1/ncargd/dev/sun4_SunOS_5_5_1/lib/ncarg/data/cdf
addfile*directory: /usr/local/ncarg/lib/ncarg/data/cdf/
addfile*directory: /fs/scd/home1/ncargd/dev/IRIS_IRIX_6_2_/lib/ncarg/data/cdf/


!htmlview: display is sgi,linux
!*html*fontSizeList: 12,8,22,18,14,12,10,8
!htmlview: display is sun
!*html*fontSizeList: 10,8,20,16,12,10,8,6
*enableBadHTMLWarnings : HTML_NONE
!*highlightColor: white
*html*highlightOnEnter: False
!*xResolution: 50
!*yResolution: 50

!*anchorButtons: False
*XmCascadeButtonGadget*shadowThickness: 2
*XmPushButtonGadget*shadowThickness: 2
*XmPushButton*shadowThickness: 2
*PaneInc*marginHeight: 0
*PaneInc*bottomMargin: 0
*PaneInc*topMargin: 0
*PaneInc*topOffset: 0
*PaneInc*bottomOffset: 0



!*contourPlotClass*cnMinLevelValF : 1.0
!*contourPlotClass*cnMaxLevelValF : 7.0
!*contourPlotClass*cnLevelSpacingF : 1.0

!*contourPlotClass*cnLevelSelectionMode : ManualLevels
!*wkColorMap : psgcap
!*wkColorMap : temp1
*cnHighLabelsOn : False
*cnLowLabelsOn : False
!*mpShapeMode: FixedAspectNoFitBB
!*mapPlotClass*vpWidthF : 1.0
!*mapPlotClass*vpHeightF : 1.0
!*mapPlotClass*vpYF : 1.0
!*mapPlotClass*vpXF : 0.0
!*mapPlotClass*mpDumpAreaMap : True
*mpDataBaseVersion : NCARG4_1
*vpUseSegments: True
*logLinPlotClass*pmTickMarkDisplayMode: always
*irregularPlotClass*pmTickMarkDisplayMode: always
*wkColorMap: temp1
