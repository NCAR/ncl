/*
 *      $Id: xy12c.c,v 1.10 1996-08-26 20:54:39 boote Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       xy12c.c
 *
 *  Author:     Jeff W. Boote
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Thu Apr 22 16:25:55 MDT 1993
 *
 *  Description:    This program demonstrates how to incorporate the
 *                  hlu library with your own X interface.  It also
 *                  demonstrates how to copy specific graphics that
 *                  are displayed to the X Workstation to an Ncgm
 *                  Workstation.
 */
#include <stdio.h>
#include <errno.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrTable.h>
#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/TextF.h>
#include <Xm/ToggleBG.h>
#include <Xm/CascadeB.h>
#include <Xm/PushBG.h>
#include <Xm/PanedW.h>
#include <Xm/Label.h>
#include <Xm/List.h>

/*
 * Declare Global Vars
 */

/* hlu objects  */
int xworkid,ncgmwid,dataid,appid;
int xyplotid = NhlDEFAULT_APP;

/* X vars needed    */
XtAppContext    app_con;
Display     *dpy = NULL;
Widget      top_level;
Widget      hardCopy;
Widget      nextFrame;
Widget      XframeCountText;
Widget      NcgmframeCountText;

/*
 * Function:    QuitCB
 *
 * Description: This function is called to exit the program.
 *
 * In Args: Not Used.
 *
 * Out Args:    
 *
 * Scope:   static
 * Returns: void
 * Side Effect: 
 */
/*ARGSUSED*/
void
QuitCB
#if __STDC__
(
    Widget      w,      /* widget that called       */
    XtPointer   clientd,    /* registered w/callback    */
    XtPointer   calld       /* callback specific data   */
)
#else
(w,clientd,calld)
    Widget      w;      /* widget that called       */
    XtPointer   clientd;    /* registered w/callback    */
    XtPointer   calld;      /* callback specific data   */
#endif
{

    /*
     * This is called so the ncgm will be a valid ncgm file even if
     * no frames were ever copied to it.
     */
    NhlFrame(ncgmwid);

    /*
     * NhlDestroy destroy's the given id and all of it's children
     * so destroying the workstation objects also destroy's plotid.
     */
    NhlDestroy(ncgmwid);
    NhlDestroy(xworkid);
    NhlDestroy(appid);

    NhlClose();
    exit(0);
}

/*
 * Function:    NextFrameCB
 *
 * Description: This function is called to display the next frame.
 *
 * In Args: Not Used.
 *
 * Out Args:    
 *
 * Scope:   static
 * Returns: void
 * Side Effect: 
 */
/*ARGSUSED*/
static void
NextFrameCB
#if __STDC__
(
    Widget      w,      /* widget that called       */
    XtPointer   clientd,    /* registered w/callback    */
    XtPointer   calld       /* callback specific data   */
)
#else
(w,clientd,calld)
    Widget      w;      /* widget that called       */
    XtPointer   clientd;    /* registered w/callback    */
    XtPointer   calld;      /* callback specific data   */
#endif
{
    static  int FirstTime = True;
    static int  count = 0;
    static int  rlist;

    static FILE *fp = NULL;

    float   explicit_values[] =
                {2,6,10,14,18,22,26,30,34};

    char    *explicit_labels[] =
                {"0800GMT 03/13/93",
                "1200GMT 03/13/93",
                "1600GMT 03/13/93",
                "2000GMT 03/13/93",
                "0000GMT 03/14/93",
                "0400GMT 03/14/93",
                "0800GMT 03/14/93",
                "1200GMT 03/14/93",
                "1600GMT 03/14/93"
                };


    char    title[80],sid[80];
    char    data[1024];
    float   pvalues[36];
    float   *pvalptr = pvalues;
    int tint;
    int len=36;



    /*
     * If this is the first time Open the data file
     */
    if(FirstTime){
        rlist = NhlRLCreate(NhlSETRL);
        fp = fopen("xy12c.asc","r");
        if(fp == (FILE*)NULL){
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                "Unable to open data file \"xy12c.asc\"");
            return;
        }
    }

    /*
     * Read the next line of data
     */
    if(fgets(data,sizeof(data),fp) == (char*)NULL){
        NhlPError(NhlFATAL,errno,"No More Data");
        fclose(fp);
        fp = NULL;
        XtSetSensitive(nextFrame,False);
        return;
    }

    /*
     * copy the ascii values into the variables
     */
    tint = sscanf(data,
"%s%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f",
            &sid,
            &pvalues[0],
            &pvalues[1],
            &pvalues[2],
            &pvalues[3],
            &pvalues[4],
            &pvalues[5],
            &pvalues[6],
            &pvalues[7],
            &pvalues[8],
            &pvalues[9],
            &pvalues[10],
            &pvalues[11],
            &pvalues[12],
            &pvalues[13],
            &pvalues[14],
            &pvalues[15],
            &pvalues[16],
            &pvalues[17],
            &pvalues[18],
            &pvalues[19],
            &pvalues[20],
            &pvalues[21],
            &pvalues[22],
            &pvalues[23],
            &pvalues[24],
            &pvalues[25],
            &pvalues[26],
            &pvalues[27],
            &pvalues[28],
            &pvalues[29],
            &pvalues[30],
            &pvalues[31],
            &pvalues[32],
            &pvalues[33],
            &pvalues[34],
            &pvalues[35]);

    /*
     * If it didn't convert correctly disable nextFrame and return
     */
    if(tint != 37){
        NhlPError(NhlFATAL,errno,"Unable to Read data");
        XtSetSensitive(nextFrame,False);
        return;
    }

    sprintf(title,"Pressure vs. Time @ Station %s",sid);

    /*
     * If it's the first time Create the Plot object
     */
    if(FirstTime){
        int grlist;
        int *dspec;
        int num_dspec;

        FirstTime = False;

        NhlRLClear(rlist);
        NhlRLSetIntegerArray(rlist,NhlNctYTableLengths,&len,1);
        NhlRLSetString(rlist,NhlNctYTableType,NhlTFloat);
        NhlRLSetArray(rlist,NhlNctYTable,&pvalptr,NhlTPointer,
                            sizeof(NhlPointer),1);
        NhlRLSetFloat(rlist,NhlNctYMissingV,-9999.0);
        NhlCreate(&dataid,"xy_data",NhlcoordArrTableClass,
                            NhlDEFAULT_APP,rlist);

        NhlRLClear(rlist);
        NhlRLSetFloat(rlist,NhlNvpXF,.25);
        NhlRLSetFloat(rlist,NhlNvpYF,.85);
        NhlRLSetFloat(rlist,NhlNvpWidthF,.5);
        NhlRLSetFloat(rlist,NhlNvpHeightF,.5);

        NhlRLSetStringArray(rlist,NhlNtmXBLabels,explicit_labels,
                        NhlNumber(explicit_labels));
        NhlRLSetFloatArray(rlist,NhlNtmXBValues,explicit_values,
                        NhlNumber(explicit_values));
        NhlRLSetInteger(rlist,NhlNtmXBMode,NhlEXPLICIT);
        NhlRLSetInteger(rlist,NhlNtmXBMinorOn,False);
        NhlRLSetInteger(rlist,NhlNtmXBLabelJust,0);
        NhlRLSetInteger(rlist,NhlNtmXBLabelAngleF,330);

        NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
        NhlRLSetInteger(rlist,NhlNtrYMaxF,1030);
        NhlRLSetInteger(rlist,NhlNtrYMinF,960);

        NhlRLSetString(rlist,NhlNtiMainString,title);
        NhlRLSetString(rlist,NhlNtiXAxisString,"Time");
        NhlRLSetString(rlist,NhlNtiYAxisString,
                        "Sealevel Pressure (mb)");

        NhlCreate(&xyplotid,"xy_plot",NhlxyPlotClass,xworkid,
                                    rlist);
        grlist = NhlRLCreate(NhlGETRL);
        NhlRLClear(grlist);
        NhlRLGetIntegerArray(grlist,NhlNxyCoordDataSpec,&dspec,&num_dspec);
        NhlGetValues(xyplotid,grlist);

        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNxyMonoLineColor,True);
        NhlRLSetInteger(rlist,NhlNxyMonoDashPattern,True);
        NhlRLSetInteger(rlist,NhlNxyLineColor,4);
        NhlRLSetInteger(rlist,NhlNxyDashPattern,1);
        NhlSetValues(*dspec,rlist);
	NhlFree(dspec);


        XtSetSensitive(hardCopy,True);
    }
    /*
     * Otherwise just change the values to update the plot object
     * to the new data.
     */
    else{

        NhlRLClear(rlist);
        NhlRLSetArray(rlist,NhlNctYTable,&pvalptr,NhlTPointer,
                            sizeof(NhlPointer),1);
        NhlSetValues(dataid,rlist);

        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNtiMainString,title);
        NhlSetValues(xyplotid,rlist);

        NhlRLClear(rlist);
    }

    /*
     * Clear the Window
     */
    NhlClearWorkstation(xworkid);

    /*
     * Draw the plot into the window
     */
    NhlDraw(xyplotid);

    /*
     * Make sure the graphics isn't buffered
     */
    NhlUpdateWorkstation(xworkid);

    /*
     * Update the Frame counter
     */
    count++;
    sprintf(data,"%d",count);
    XmTextFieldSetString(XframeCountText,data);
}

/*
 * Function:    HardCopyCB
 *
 * Description: This function is called to copy the current Frame to
 *      the NCGM file.
 *
 * In Args: Not Used.
 *
 * Out Args:    
 *
 * Scope:   static
 * Returns: void
 * Side Effect: 
 */
/*ARGSUSED*/
void
HardCopyCB
#if __STDC__
(
    Widget      w,      /* widget that called       */
    XtPointer   clientd,    /* registered w/callback    */
    XtPointer   calld       /* callback specific data   */
)
#else
(w,clientd,calld)
    Widget      w;      /* widget that called       */
    XtPointer   clientd;    /* registered w/callback    */
    XtPointer   calld;      /* callback specific data   */
#endif
{
    static int FirstTime = True;
    static int count = 0;
    char    data[10];

    /*
     * This call re-parents plotid to a new parent (ncgmwid).
     * (Used to copy xyplot from it's current Workstation to the
     * new workstation.
     */
    NhlChangeWorkstation(xyplotid,ncgmwid);

    /*
     * If it is the first time there are no previous plots displayed in
     * the NcgmWorkstation so we shouldn't call NhlFrame.
     */
    if(FirstTime){
        FirstTime = False;
    }
    else{
        NhlFrame(ncgmwid);
    }

    /*
     * Draw the plot to the meta file.
     */
    NhlDraw(xyplotid);

    /*
     * Update the Hardcopy Frame count.
     */
    count++;
    sprintf(data,"%d",count);
    XmTextFieldSetString(NcgmframeCountText,data);

    /*
     * Now put the plot back to the X Workstation so the next draw
     * will display to the X Window.
     */
    NhlChangeWorkstation(xyplotid,xworkid);
}

/*
 * Function:    CreateGUIinterface
 *
 * Description: This function is used to create the GUI interface for this
 *      example.  It is a very simple interface.
 *
 * In Args: 
 *      int *argc,  arg count
 *      char    **argv  arg value
 *
 * Out Args:    
 *
 * Scope:   static
 * Returns: Window - the window id of the window to display the graphics
 *      in.
 * Side Effect: 
 */
Window
CreateGUIinterface
#if __STDC__
(
    Cardinal    *argc,  /* arg count    */
    char        **argv  /* arg value    */
)
#else
(argc,argv)
    Cardinal    *argc;  /* arg count    */
    char        **argv; /* arg value    */
#endif
{
    int     nargs;
    Arg     args[20];
    Widget          topform;
    Widget          graphics;
    Widget          commandframe;
    Widget          commandform;
    Widget          graphicsSW;
    Widget          graphicsVP;
    Widget          quitButton;
    Widget          XframeCount;
    Widget          NcgmframeCount;
    int     arg_count = (int)*argc;

    /*
     * Init Xt library
     */
    XtToolkitInitialize();

    /*
     * Open the Display Connection
     */
    app_con = XtCreateApplicationContext();
    dpy = XtOpenDisplay(app_con,NULL,argv[0],"NhlExample",NULL,0,&arg_count,
                                    argv);

    if(dpy == NULL){
        NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to open display %s",
                            getenv("DISPLAY"));
        exit(3);
    }

    /*
     * Create the Top-level Window
     */
    nargs = 0;
    XtSetArg(args[nargs], XmNdeleteResponse, XmDO_NOTHING); nargs++;
    XtSetArg(args[nargs], XmNallowShellResize, True); nargs++;
    XtSetArg(args[nargs], XmNmappedWhenManaged, False); nargs++;
    top_level = XtAppCreateShell(argv[0], "NhlExample",
                applicationShellWidgetClass, dpy, args, nargs);

    /*
     * Catch the "f.delete" function from WindowManager - and call QuitCB
     * instead of being killed.
     */
    XmAddWMProtocolCallback(top_level,
            XmInternAtom(dpy,"WM_DELETE_WINDOW",False),QuitCB,NULL);

    nargs = 0;
    topform = XtCreateWidget("topform",xmFormWidgetClass,top_level,args,
                                    nargs);
    XtManageChild(topform);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNshadowThickness,7); nargs++;
    XtSetArg(args[nargs],XmNshadowType,XmSHADOW_ETCHED_OUT); nargs++;
    commandframe = XtCreateWidget("commandframe",xmFrameWidgetClass,
                            topform,args,nargs);
    XtManageChild(commandframe);

    nargs = 0;
    commandform = XtCreateWidget("commandform",xmFormWidgetClass,
                        commandframe,args,nargs);
    XtManageChild(commandform);

    /*
     * Using the Widget Name to set the labelString resource
     */
    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    quitButton = XmCreatePushButtonGadget(commandform,"  QUIT  ",args,
                                    nargs);
    XtManageChild(quitButton);
    XtAddCallback(quitButton,XmNactivateCallback,QuitCB,NULL);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,quitButton); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    nextFrame = XmCreatePushButtonGadget(commandform," Next Frame ",args,
                                    nargs);
    XtManageChild(nextFrame);
    XtAddCallback(nextFrame,XmNactivateCallback,NextFrameCB,NULL);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,nextFrame); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNsensitive,False); nargs++;
    hardCopy = XmCreatePushButtonGadget(commandform," Hard Copy ",args,
                                    nargs);
    XtManageChild(hardCopy);
    XtAddCallback(hardCopy,XmNactivateCallback,HardCopyCB,NULL);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,hardCopy); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    XframeCount = XmCreateLabel(commandform,"Displayed Frame",args,nargs);
    XtManageChild(XframeCount);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,XframeCount); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNvalue,"0"); nargs++;
    XtSetArg(args[nargs],XmNeditable,False); nargs++;
    XtSetArg(args[nargs],XmNcursorPositionVisible,False); nargs++;
    XtSetArg(args[nargs],XmNtraversalOn,False); nargs++;
    XframeCountText = XmCreateTextField(commandform,"Displayed Frame",args,
                                    nargs);
    XtManageChild(XframeCountText);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,XframeCountText); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    NcgmframeCount = XmCreateLabel(commandform,"Copied Frames",args,nargs);
    XtManageChild(NcgmframeCount);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,NcgmframeCount); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNvalue,"0"); nargs++;
    XtSetArg(args[nargs],XmNeditable,False); nargs++;
    XtSetArg(args[nargs],XmNcursorPositionVisible,False); nargs++;
    XtSetArg(args[nargs],XmNtraversalOn,False); nargs++;
    NcgmframeCountText = XmCreateTextField(commandform,"Frames Copied",args,
                                    nargs);
    XtManageChild(NcgmframeCountText);

    /*
     * Graphics Window
     */
    nargs = 0;
    XtSetArg(args[nargs], XmNtopAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNbottomAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNleftAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNrightAttachment, XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs], XmNrightOffset, 5); nargs++;
    XtSetArg(args[nargs], XmNrightWidget, commandframe); nargs++;
    XtSetArg(args[nargs], XmNleftWidget, commandframe); nargs++;
    XtSetArg(args[nargs], XmNscrollingPolicy, XmAUTOMATIC); nargs++;
    XtSetArg(args[nargs], XmNscrollBarDisplayPolicy, XmSTATIC); nargs++;
    graphicsSW = XtCreateWidget("graphicsSW",xmScrolledWindowWidgetClass,
                        topform,args,nargs);
    XtManageChild(graphicsSW);

    /*
     * These sizes shouldn't be hard-coded - they should be setable in
     * an X resource file - Or they should be configurable interactively
     * by the user.
     */
    nargs = 0;
    XtSetArg(args[nargs], XmNwidth, 700); nargs++;
    XtSetArg(args[nargs], XmNheight, 700); nargs++;
    XtSetArg(args[nargs], XmNbackground,
                BlackPixel(dpy,DefaultScreen(dpy))); nargs++;
    graphics = XtCreateWidget("graphics",xmDrawingAreaWidgetClass,
                        graphicsSW,args,nargs);
    XtManageChild(graphics);

    /*
     * DO ALL GEOMETRY HACKS AFTER REALIZATION
     */

    XtRealizeWidget(top_level);

    {
        /*
         * Set Backing-Store so graphics don't get erased by
         * other windows.
         */
        XSetWindowAttributes    xswa;

        xswa.backing_store = WhenMapped;
        XChangeWindowAttributes(dpy,XtWindow(graphics),CWBackingStore,
                                    &xswa);
    }

    /*
     * This section is used to set the Scrolled window so the viewport
     * is the same size as the drawingArea.
     */
    {
        Dimension SWwidth, SWheight;
        Dimension VPwidth, VPheight;
        Dimension DAwidth, DAheight;

        /*
         * Retrieve the width and height of the clipping window
         * and the Scrolled Window.
         */
        nargs = 0;
        XtSetArg(args[nargs], XmNclipWindow, &graphicsVP); nargs++;
        XtSetArg(args[nargs], XmNwidth, &SWwidth); nargs++;
        XtSetArg(args[nargs], XmNheight, &SWheight); nargs++;
        XtGetValues(graphicsSW,args,nargs);

        nargs = 0;
        XtSetArg(args[nargs], XmNwidth, &VPwidth); nargs++;
        XtSetArg(args[nargs], XmNheight, &VPheight); nargs++;
        XtGetValues(graphicsVP,args,nargs);

        /*
         * Retrieve the width and height of the graphics window
         */
        nargs = 0;
        XtSetArg(args[nargs], XmNwidth, &DAwidth); nargs++;
        XtSetArg(args[nargs], XmNheight, &DAheight); nargs++;
        XtGetValues(graphics,args,nargs);

        /*
         * Set the SW to the correct size so the entire graphics window
         * is visible but nothing more.
         */
        nargs = 0;
        XtSetArg(args[nargs],XmNwidth,(SWwidth+DAwidth-VPwidth));
                                    nargs++;
        XtSetArg(args[nargs],XmNheight,(SWheight+DAheight-VPheight));
                                    nargs++;
        XtSetValues(graphicsSW,args,nargs);

        /*
         * Set the max size of top_level so the viewport window
         * is never larger than the graphics window
         * (reusing DA vars for Main size)
         */
        nargs = 0;
        XtSetArg(args[nargs], XmNheight, &DAheight); nargs++;
        XtSetArg(args[nargs], XmNwidth, &DAwidth); nargs++;
        XtGetValues(top_level,args,nargs);

        nargs = 0;
        XtSetArg(args[nargs], XmNmaxHeight, DAheight); nargs++;
        XtSetArg(args[nargs], XmNmaxWidth, DAwidth); nargs++;
        XtSetValues(top_level,args,nargs);

    }

    return (XtWindow(graphics));
}

/*
 * Function:    main
 *
 * Description: The main function of this application.
 *
 * In Args: 
 *      int argc,
 *      char    **argv
 *
 * Out Args:    
 *
 * Returns: int
 * Side Effect: 
 */
int
main
#if __STDC__
(
    Cardinal    argc,
    char        **argv
)
#else
(argc,argv)
    Cardinal    argc;
    char        **argv;
#endif
{
    Window  graphicsWin;

    /*
     * Call this before X calls so I can use NhlPError stuff
     */
    NhlInitialize();
    NhlCreate(&appid,"xy12",NhlappClass,NhlDEFAULT_APP,0);

    /*
     * Create GUI interface and retrieve the window to display the graphics
     * in.
     */
    graphicsWin = CreateGUIinterface(&argc,argv);

    /*
     * Create the Workstation Class objects
     */
    NhlVACreate(&xworkid,"xy12xWork",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,
        NhlNwkWindowId,     graphicsWin,
        NhlNwkPause,        False,
        NULL);

    NhlVACreate(&ncgmwid,"xy12ncgmWork",NhlncgmWorkstationClass,
                                NhlDEFAULT_APP,
        NhlNwkMetaName,     "xy12c.ncgm",
        NULL);

    /*
     * Make the main window visible
     */
    XtMapWidget(top_level);

    /*
     * start event looping
     */
    XtAppMainLoop(app_con);

    exit(0);
}
