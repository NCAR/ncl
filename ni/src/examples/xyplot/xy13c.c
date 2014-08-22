/*
 *      $Id: xy13c.c,v 1.12 1995-06-15 01:42:13 dbrown Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       xy13c.c
 *
 *  Author:     Jeff W. Boote
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Fri Apr 16 11:34:37 MDT 1993
 *
 *  Description:    This is an example showing how to interactively retrieve
 *          data values from a plot by pointing and clicking
 *          on the plot.
 */
#include <stdio.h>
#include <errno.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/CoordArrTable.h>
#include <ncarg/hlu/XyPlot.h>
#include <Xm/Xm.h>
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

#include "xy13c.h"
/*
 * Size for the Graphics window in pixels
 */
#define WIDTH   700
#define HEIGHT  700

/*
 * global HLU objects
 */
int appid = NhlDEFAULT_APP;
int xworkid = NhlDEFAULT_APP;
int xyplotid = NhlDEFAULT_APP;
int grlist=0,srlist=0;

XtAppContext    app_con;
Display     *dpy = NULL;
Widget      top_level;
Widget      XText;
Widget      YText;
Widget      graphics;

/*
 * Function:    QuitCB
 *
 * Description: This function is called to exit the program.
 *
 * In Args: 
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
#if NhlNeedProto
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
     * NhlDestroy destroy's the given id and all of it's children
     * so destroying the workstation objects also destroy's plotid.
     */
    NhlDestroy(xworkid);

    NhlClose();
    exit(0);
}

/*
 * Function:    XcoordToNDC
 *
 * Description: This function is used to determine NDC coord given the
 *      x, and y offset from the origin of the X window.
 *      (remember X coord's origin is upper-left and NDC
 *      origin is lower-left)
 *
 * In Args: 
 *
 * Out Args:    
 *
 * Scope:   static
 * Returns: void
 * Side Effect: 
 */
static void
XcoordToNDC
#if NhlNeedProto
(
    int x,  /* x coord  */
    int y,  /* y coord  */
    float   *ndcx,  /* return x */
    float   *ndcy   /* return y */
)
#else
(x,y,ndcx,ndcy)
    int x;  /* x coord  */
    int y;  /* y coord  */
    float   *ndcx;  /* return x */
    float   *ndcy;  /* return y */
#endif
{
    *ndcx = (float)x / (float)WIDTH;
    *ndcy = (float)1.0 - ((float)y / (float)HEIGHT);

    return;
}

/*
 * Function:    SelectionEH
 *
 * Description: This function is used to make a selection on the frame.
 *      It is important to note that the origin of the NDC
 *      coord-system is the lower left corner, but the origin
 *      of each objects viewport is the upper left corner.
 *
 * In Args: 
 *      Widget      w,  widget
 *      XtPointer   data,   data installed with EH
 *      XEvent      *ev event
 *
 * Out Args:    
 *
 * Scope:   static
 * Returns: void
 * Side Effect: 
 */
/*ARGSUSED*/
static void
SelectionEH
#if NhlNeedProto
(
    Widget      w,  /* widget           */
    XtPointer   d,  /* data installed with EH   */
    XEvent      *ev,    /* event            */
    Boolean     *cont   /* cont to dispatch     */
)
#else
(w,d,ev,cont)
    Widget      w;  /* widget           */
    XtPointer   d;  /* data installed with EH   */
    XEvent      *ev;    /* event            */
    Boolean     *cont;  /* cont to dispatch     */
#endif
{
    XButtonEvent        *event = (XButtonEvent*)ev;
    float           x,y,ndcx,ndcy,ndcwidth,ndcheight;
    char            data[1024];

    if(event->type != ButtonPress)
        return;

    /*
     * This event handler is only interested in button 1
     */
    if(event->button != Button1)
        return;

    XcoordToNDC(event->x,event->y,&x,&y);


    /*
     * Find the Viewport of the XyPlot
     */
    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNvpXF,&ndcx);
    NhlRLGetFloat(grlist,NhlNvpYF,&ndcy);
    NhlRLGetFloat(grlist,NhlNvpWidthF,&ndcwidth);
    NhlRLGetFloat(grlist,NhlNvpHeightF,&ndcheight);
    (void)NhlGetValues(xyplotid,grlist);

    /*
     * If the button click was not in the viewport - clear the
     * text windows
     */
    if( (x < ndcx) ||
        (x > ndcx + ndcwidth) ||
        (y > ndcy) ||
        ( y < ndcy - ndcheight)){

        XmTextFieldSetString(XText,"");
        XmTextFieldSetString(YText,"");
    }
    /*
     * Otherwise, put the data coords in the text windows
     */
    else{
        float   oor = 0.0;
        int status = 0;
        (void)NhlNDCToData(xyplotid,&x,&y,1,&x,&y,NULL,NULL,&status,
                                    &oor);

        (void)sprintf(data,"%f",x);
        XmTextFieldSetString(XText,data);
        (void)sprintf(data,"%f",y);
        XmTextFieldSetString(YText,data);
    }

    return;
}

/*
 * Function:    CreateGUIinterface
 *
 * Description: This function is used to create the GUI interface for this
 *      example.  It is a very simple interface.
 *
 * In Args: 
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
#if NhlNeedProto
(
    int *argc,  /* arg count    */
    char    **argv  /* arg value    */
)
#else
(argc,argv)
    int *argc;  /* arg count    */
    char    **argv; /* arg value    */
#endif
{
    int     nargs;
    Arg     args[20];
    Widget          topform;
    Widget          commandframe;
    Widget          commandform;
    Widget          graphicsSW;
    Widget          graphicsVP;
    Widget          quitButton;
    Widget          directions;
    Widget          Xvalue;
    Widget          Yvalue;

    XtToolkitInitialize();

    app_con = XtCreateApplicationContext();
    dpy = XtOpenDisplay(app_con,NULL,argv[0],"NhlExample",NULL,0,argc,argv);

    if(dpy == NULL){
        NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to open display %s",
                            getenv("DISPLAY"));
        exit(3);
    }

    nargs = 0;
    XtSetArg(args[nargs], XmNdeleteResponse, XmDO_NOTHING); nargs++;
    XtSetArg(args[nargs], XmNallowShellResize, True); nargs++;
    XtSetArg(args[nargs], XmNmappedWhenManaged, False); nargs++;
    top_level = XtAppCreateShell(argv[0], "NhlExample",
                applicationShellWidgetClass, dpy, args, nargs);

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
    directions = XmCreateLabel(commandform,"Click to Probe Data",args,
                                    nargs);
    XtManageChild(directions);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,directions); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    Xvalue = XmCreateLabel(commandform,"X Data Value",args,nargs);
    XtManageChild(Xvalue);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,Xvalue); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNeditable,False); nargs++;
    XtSetArg(args[nargs],XmNcursorPositionVisible,False); nargs++;
    XtSetArg(args[nargs],XmNtraversalOn,False); nargs++;
    XText = XmCreateTextField(commandform,"Frames Copied",args,nargs);
    XtManageChild(XText);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,XText); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    Yvalue = XmCreateLabel(commandform,"Y Data Value",args,nargs);
    XtManageChild(Yvalue);

    nargs = 0;
    XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs],XmNtopWidget,Yvalue); nargs++;
    XtSetArg(args[nargs],XmNbottomAttachment,XmATTACH_NONE); nargs++;
    XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs],XmNeditable,False); nargs++;
    XtSetArg(args[nargs],XmNcursorPositionVisible,False); nargs++;
    XtSetArg(args[nargs],XmNtraversalOn,False); nargs++;
    YText = XmCreateTextField(commandform,"Frames Copied",args,nargs);
    XtManageChild(YText);

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
     * by the user - but hey, what do you want from an example?
     */
    nargs = 0;
    XtSetArg(args[nargs], XmNwidth, WIDTH); nargs++;
    XtSetArg(args[nargs], XmNheight, HEIGHT); nargs++;
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
        XSetWindowAttributes    xswa;

        xswa.backing_store = WhenMapped;
        XChangeWindowAttributes(dpy,XtWindow(graphics),CWBackingStore,
                                    &xswa);
    }

    /*
     * This section is used to set the Scrolled window so the viewport
     * is the same size as the drawingArea
     */
    {
        Dimension SWwidth, SWheight;
        Dimension VPwidth, VPheight;
        Dimension DAwidth, DAheight;

        /*
         * Retrieve the width and height of the clipping window
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
 *
 * Out Args:    
 *
 * Scope:   global
 * Returns: int
 * Side Effect: 
 */
int
main
#if NhlNeedProto
(
    int argc,
    char    **argv
)
#else
(argc,argv)
    int argc;
    char    **argv;
#endif
{
    Window  graphicsWin;
    XEvent  event;

    float *xdata[31];
    float *ydata[31];
    int lengths[31];
    char *line_labels[31];
    int dataid;
    int datadepid[1];
    int *dspec = datadepid;
    int num_dspec;

    /*
     * Initialize data values
     */
    xdata[0] = dry_minusfiftyT;
    ydata[0] = dry_minusfiftyP;
    lengths[0] = sizeof(dry_minusfiftyT)/sizeof(float);
    line_labels[0] = "-50";
    xdata[1] = dry_minusfourtyT;
    ydata[1] = dry_minusfourtyP;
    lengths[1] = sizeof(dry_minusfourtyT)/sizeof(float);
    line_labels[1] = "-40";
    xdata[2] = dry_minusthirtyT;
    ydata[2] = dry_minusthirtyP;
    lengths[2] = sizeof(dry_minusthirtyT)/sizeof(float);
    line_labels[2] = "-30";
    xdata[3] = dry_minustwentyT;
    ydata[3] = dry_minustwentyP;
    lengths[3] = sizeof(dry_minustwentyT)/sizeof(float);
    line_labels[3] = "-20";
    xdata[4] = dry_minustenT;
    ydata[4] = dry_minustenP;
    lengths[4] = sizeof(dry_minustenT)/sizeof(float);
    line_labels[4] = "-10";
    xdata[5] = dry_zeroT;
    ydata[5] = dry_zeroP;
    lengths[5] = sizeof(dry_zeroT)/sizeof(float);
    line_labels[5] = "0";
    xdata[6] = dry_tenT;
    ydata[6] = dry_tenP;
    lengths[6] = sizeof(dry_tenT)/sizeof(float);
    line_labels[6] = "10";
    xdata[7] = dry_twentyT;
    ydata[7] = dry_twentyP;
    lengths[7] = sizeof(dry_twentyT)/sizeof(float);
    line_labels[7] = "20";
    xdata[8] = dry_thirtyT;
    ydata[8] = dry_thirtyP;
    lengths[8] = sizeof(dry_thirtyT)/sizeof(float);
    line_labels[8] = "30";
    xdata[9] = dry_fourtyT;
    ydata[9] = dry_fourtyP;
    lengths[9] = sizeof(dry_fourtyT)/sizeof(float);
    line_labels[9] = "40";
    xdata[10] = dry_fiftyT;
    ydata[10] = dry_fiftyP;
    lengths[10] = sizeof(dry_fiftyT)/sizeof(float);
    line_labels[10] = "50";
    xdata[11] = dry_sixtyT;
    ydata[11] = dry_sixtyP;
    lengths[11] = sizeof(dry_sixtyT)/sizeof(float);
    line_labels[11] = "60";
    xdata[12] = dry_seventyT;
    ydata[12] = dry_seventyP;
    lengths[12] = sizeof(dry_seventyT)/sizeof(float);
    line_labels[12] = "70";
    xdata[13] = dry_eightyT;
    ydata[13] = dry_eightyP;
    lengths[13] = sizeof(dry_eightyT)/sizeof(float);
    line_labels[13] = "80";
    xdata[14] = dry_ninetyT;
    ydata[14] = dry_ninetyP;
    lengths[14] = sizeof(dry_ninetyT)/sizeof(float);
    line_labels[14] = "90";
    xdata[15] = iso_160T;
    ydata[15] = iso_160P;
    lengths[15] = sizeof(iso_160T)/sizeof(float);
    line_labels[15] = "160g";
    xdata[16] = iso_80T;
    ydata[16] = iso_80P;
    lengths[16] = sizeof(iso_80T)/sizeof(float);
    line_labels[16] = "80g";
    xdata[17] = iso_40T;
    ydata[17] = iso_40P;
    lengths[17] = sizeof(iso_40T)/sizeof(float);
    line_labels[17] = "40g";
    xdata[18] = iso_20T;
    ydata[18] = iso_20P;
    lengths[18] = sizeof(iso_20T)/sizeof(float);
    line_labels[18] = "20g";
    xdata[19] = iso_10T;
    ydata[19] = iso_10P;
    lengths[19] = sizeof(iso_10T)/sizeof(float);
    line_labels[19] = "10g";
    xdata[20] = iso_5T;
    ydata[20] = iso_5P;
    lengths[20] = sizeof(iso_5T)/sizeof(float);
    line_labels[20] = "5g";
    xdata[21] = iso_2_5T;
    ydata[21] = iso_2_5P;
    lengths[21] = sizeof(iso_2_5T)/sizeof(float);
    line_labels[21] = "2.5g";
    xdata[22] = iso_1_25T;
    ydata[22] = iso_1_25P;
    lengths[22] = sizeof(iso_1_25T)/sizeof(float);
    line_labels[22] = "1.25g";
    xdata[23] = iso_0_625T;
    ydata[23] = iso_0_625P;
    lengths[23] = sizeof(iso_0_625T)/sizeof(float);
    line_labels[23] = "0.625g";
    xdata[24] = wet_40gT;
    ydata[24] = wet_40gP;
    lengths[24] = sizeof(wet_40gT)/sizeof(float);
    line_labels[24] = NULL;
    xdata[25] = wet_20gT;
    ydata[25] = wet_20gP;
    lengths[25] = sizeof(wet_20gT)/sizeof(float);
    line_labels[25] = NULL;
    xdata[26] = wet_10gT;
    ydata[26] = wet_10gP;
    lengths[26] = sizeof(wet_10gT)/sizeof(float);
    line_labels[26] = NULL;
    xdata[27] = wet_5gT;
    ydata[27] = wet_5gP;
    lengths[27] = sizeof(wet_5gT)/sizeof(float);
    line_labels[27] = NULL;
    xdata[28] = wet_5gT;
    ydata[28] = wet_5gP;
    lengths[28] = sizeof(wet_5gT)/sizeof(float);
    line_labels[28] = NULL;
    xdata[29] = wet_2_5gT;
    ydata[29] = wet_2_5gP;
    lengths[29] = sizeof(wet_2_5gT)/sizeof(float);
    line_labels[29] = NULL;
    xdata[30] = wet_1_25gT;
    ydata[30] = wet_1_25gP;
    lengths[30] = sizeof(wet_1_25gT)/sizeof(float);
    line_labels[30] = NULL;

    /*
     * Call this before X calls so I can use NhlPError stuff
     */
    NhlInitialize();

    srlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlCreate(&appid,"xy13",NhlappClass,NhlDEFAULT_APP,0);

    /*
     * Create GUI interface and retrieve the window to display the graphics
     * in.
     */
    graphicsWin = CreateGUIinterface(&argc,argv);

    /*
     * NhlNwkPause will default to False if NhlNwkWindowId is
     * set - If the user provides the window they must manually
     * control interaction with it.
     */
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNwkWindowId,graphicsWin);
    NhlCreate(&xworkid,"xy13Work",NhlcairoWindowWorkstationClass,appid,srlist);

    NhlRLClear(srlist);

    NhlRLSetString(srlist,NhlNctXTableType,NhlTFloat);
    NhlRLSetArray(srlist,NhlNctXTable,xdata,NhlTPointer,sizeof(float*),31);
    NhlRLSetIntegerArray(srlist,NhlNctXTableLengths,lengths,31);

    NhlRLSetString(srlist,NhlNctYTableType,NhlTFloat);
    NhlRLSetArray(srlist,NhlNctYTable,ydata,NhlTPointer,sizeof(float*),31);
    NhlRLSetIntegerArray(srlist,NhlNctYTableLengths,lengths,31);
    NhlCreate(&dataid,"dataid",NhlcoordArrTableClass,NhlDEFAULT_APP,
                                srlist);

    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNxyCoordData,dataid);
    NhlRLSetInteger(srlist,NhlNxyYStyle,NhlIRREGULAR);
    NhlRLSetFloatArray(srlist,NhlNxyYIrregularPoints,dry_zeroP,
                    (sizeof(dry_zeroP)/sizeof(float)));

    NhlCreate(&xyplotid,"xy_plot",NhlxyPlotClass,xworkid,srlist);

    NhlRLClear(grlist);
    NhlRLGetIntegerArray(grlist,NhlNxyCoordDataSpec,&dspec,&num_dspec);
    NhlGetValues(xyplotid,grlist);

    NhlRLClear(srlist);
    NhlRLSetStringArray(srlist,NhlNxyExplicitLabels,line_labels,31);
    NhlSetValues(dspec[0],srlist);

    XtAddEventHandler(graphics,ButtonPressMask,False,
                (XtEventHandler)SelectionEH,(XtPointer)NULL);

    XtMapWidget(top_level);

    /*
     * Loop until the Window has been exposed before Calling NhlDraw
     */
    for(XtAppNextEvent(app_con,&event);event.type != Expose;
                        XtAppNextEvent(app_con,&event)){
        XtDispatchEvent(&event);
    }
    XtDispatchEvent(&event);

    /*
     * Now Draw the Plot
     */
    NhlDraw(xyplotid);

    /*
     * Flush the Graphics
     */
    NhlUpdateWorkstation(xworkid);

    XtAppMainLoop(app_con);

    exit(0);
}
