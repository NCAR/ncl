/*
 * $Id: c_fcoord2.c,v 1.3 1995-06-14 14:00:03 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

main()
{
#ifdef NeedFuncProto
	extern void coord2(int *);
#else
	extern void coord2();
#endif
	int ierr;
/*
 * Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	coord2(&ierr);
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void coord2
#ifdef NeedFuncProto
(int *ierror)
#else
(ierror)
int *ierror;
#endif
{
	int i;
	float xclw, xcrw, ycbw, yctw, vpl, vpr, vpb, vpt;
	float xclv, xcrv, ycbv, yctv;
/*
 *  This example shows the mapping from NCAR Grapics user coordinates to
 *   GKS normalized device coordinates.
 */
	float xcra[101], ycra[101];
	float dsl = .59, dsr = .99, dsb = .4, dst = .7;
	float pfl = .72, pfr = .98, pfb = .42, pft = .69;
	gset_clip_ind (GIND_NO_CLIP);
	gset_linewidth(2.);
/*
 * Employ the new high quality filled fonts in PLOTCHAR
 */
	c_pcsetc("FN","times-roman");

	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);

	c_pcsetc("FC","%");
	c_plchhq (.50,.98, "Mapping from a window in the user coordinate system",.02,0.,0.);
	c_plchhq (.50,.94,"to a viewport in the normalized device coordinate system",.02,0.,0.);
	c_plchhq (.50,.90,"using a transformation defined by calling SET",.02,0.,0.);
	c_pcseti("FN",21);
	c_plchhq (.50,.86,"(Used in calls to most NCAR Graphics routines)",.016,0.,0.);
	c_pcseti("FN",29);
	c_plchhq (.50,.22,"Assume a CALL SET (.15,.95,.10,.90,1000.,100.,100.,1000.,2).",.013,0.,0.);
	c_plchhq (.50,.185,"The GKS viewport is:  .15, .95, .10, .90",.013,0.,0.);
	c_plchhq (.50,.15,"The GKS window is:  100., 1000., 2., 3.",.013,0.,0.);
	c_plchhq (.50,.115,"The value of 'MI' is 3 (mirror-imaging of X's).",.013,0.,0.);
	c_plchhq (.50,.08,"The value of 'LS' is 2 (log scaling of Y's).",.013,0.,0.);
	c_pcseti("FN",22);
	c_set    (.01,.55,.29,.83,0.,1100.,0.,1100.,1);
	c_line   (   0.,   0.,1100.,   0.);
	c_line   (1100.,   0.,1050.,  25.);
	c_line   (1100.,   0.,1050., -25.);
	c_plchhq (c_cfux(c_cufx(1100.)+.015),0.,"X",.015,0.,0.);
	c_line   (   0.,   0.,   0.,1100.);
	c_line   (   0.,1100.,  25.,1050.);
	c_line   (   0.,1100., -25.,1050.);
	c_plchhq (0.,c_cfuy(c_cufy(1100.)+.015),"Y",.015,0.,0.);
	c_line   ( 100., 100.,1000., 100.);
	c_line   (1000., 100.,1000.,1000.);
	c_line   (1000.,1000., 100.,1000.);
	c_line   ( 100.,1000., 100., 100.);
	c_plchhq (c_cfux(c_cufx(100.)+.005),c_cfuy(c_cufy(1000.)-.01),"WINDOW",.01,0.,-1.);
	c_plchhq (100.,c_cfuy(c_cufy(100.)-.01),"100",.008,0.,0.);
	c_plchhq (1000.,c_cfuy(c_cufy(100.)-.01),"1000",.008,0.,0.);
	c_plchhq (c_cfux(c_cufx(100.)-.005),100.,"100",.008,0.,1.);
	c_plchhq (c_cfux(c_cufx(100.)-.005),1000.,"1000",.008,0.,1.);
	for( i = 0; i < 101; i++ ) {
		xcra[i] = 200.+7.*(float)i;
		ycra[i] = 200.+700.*(pow(10.,((xcra[i]-200.)/700.))-1.)/9.+100.*sin((xcra[i]-200.)/30.);
	}
	c_curve  (xcra,ycra,101);
	xclw=c_cufx(100.);
	xcrw=c_cufx(1000.);
	ycbw=c_cufy(100.);
	yctw=c_cufy(1000.);
	c_plotit (0,0,0);
	gset_linewidth (1.);
	c_line (100.,200.,1000.,200.);
	c_line (100.,300.,1000.,300.);
	c_line (100.,400.,1000.,400.);
	c_line (100.,500.,1000.,500.);
	c_line (100.,600.,1000.,600.);
	c_line (100.,700.,1000.,700.);
	c_line (100.,800.,1000.,800.);
	c_line (100.,900.,1000.,900.);
	c_line (200.,100.,200.,1000.);
	c_line (300.,100.,300.,1000.);
	c_line (400.,100.,400.,1000.);
	c_line (500.,100.,500.,1000.);
	c_line (600.,100.,600.,1000.);
	c_line (700.,100.,700.,1000.);
	c_line (800.,100.,800.,1000.);
	c_line (900.,100.,900.,1000.);
	c_plotit (0,0,0);
	gset_linewidth (2.);
	c_set    (dsl,dsr,dsb,dst,0.,1.,0.,1.,1);
	c_line   (0.,0.,1.,0.);
	c_line   (1.,0.,1.,1.);
	c_line   (1.,1.,0.,1.);
	c_line   (0.,1.,0.,0.);
	c_plchhq (c_cfux(c_cufx(0.)+.005),c_cfuy(c_cufy(1.)-.01),"DEVICE",.01,0.,-1.);
	c_set    (pfl,pfr,pfb,pft,0.,1.,0.,1.,1);
	c_line   (0.,0.,1.,0.);
	c_line   (1.,0.,1.,1.);
	c_line   (1.,1.,0.,1.);
	c_line   (0.,1.,0.,0.);
	c_plchhq (c_cfux(c_cufx(0.)+.005),c_cfuy(c_cufy(1.)-.01), "PLOTTER FRAME",.01,0.,-1.);
	c_plchhq (0.,c_cfuy(c_cufy(0.)-.01),"0",.008,0.,0.);
	c_plchhq (1.,c_cfuy(c_cufy(0.)-.01),"1",.008,0.,0.);
	c_plchhq (c_cfux(c_cufx(0.)-.005),0.,"0",.008,0.,1.);
	c_plchhq (c_cfux(c_cufx(0.)-.005),1.,"1",.008,0.,1.);
	vpl=pfl+.15*(pfr-pfl);
	vpr=pfl+.95*(pfr-pfl);
	vpb=pfb+.10*(pft-pfb);
	vpt=pfb+.90*(pft-pfb);
	c_set    (vpl,vpr,vpb,vpt,1000., 100., 100.,1000.,2);
	c_line   (1000., 100., 100., 100.);
	c_line   ( 100., 100., 100.,1000.);
	c_line   ( 100.,1000.,1000.,1000.);
	c_line   (1000.,1000.,1000., 100.);
	c_plchhq (c_cfux(c_cufx(1000.)+.005),c_cfuy(c_cufy(1000.)-.01),"VIEWPORT",.01,0.,-1.);
	c_plchhq (1000.,c_cfuy(c_cufy(100.)-.01),".15",.008,0.,0.);
	c_plchhq (100.,c_cfuy(c_cufy(100.)-.01),".95",.008,0.,0.);
	c_plchhq (c_cfux(c_cufx(1000.)-.005),100.,".10",.008,0.,1.);
	c_plchhq (c_cfux(c_cufx(1000.)-.005),1000.,".90",.008,0.,1.);
	c_curve  (xcra,ycra,101);
	c_plotit (0,0,0);
	gset_linewidth (1.);
	c_line (100.,200.,1000.,200.);
	c_line (100.,300.,1000.,300.);
	c_line (100.,400.,1000.,400.);
	c_line (100.,500.,1000.,500.);
	c_line (100.,600.,1000.,600.);
	c_line (100.,700.,1000.,700.);
	c_line (100.,800.,1000.,800.);
	c_line (100.,900.,1000.,900.);
	c_line (200.,100.,200.,1000.);
	c_line (300.,100.,300.,1000.);
	c_line (400.,100.,400.,1000.);
	c_line (500.,100.,500.,1000.);
	c_line (600.,100.,600.,1000.);
	c_line (700.,100.,700.,1000.);
	c_line (800.,100.,800.,1000.);
	c_line (900.,100.,900.,1000.);
	c_plotit (0,0,0);
	gset_linewidth(2.);
	xclv=c_cufx(1000.);
	xcrv=c_cufx(100.);
	ycbv=c_cufy(100.);
	yctv=c_cufy(1000.);
	c_set    (0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_plotit (0,0,0);
	gset_linewidth (1.);
	c_dashdc ("$'",3,1);
	c_lined  (xclw,ycbw,xcrv,ycbv);
	c_lined  (xcrw,ycbw,xclv,ycbv);
	c_lined  (xcrw,yctw,xclv,yctv);
	c_lined  (xclw,yctw,xcrv,yctv);
	c_frame();
	printf( "COORD2 TEST SUCCESSFUL\nSEE PLOTS TO VERIFY PERFORMANCE\n");
	return;
}
