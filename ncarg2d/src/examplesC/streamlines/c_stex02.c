/*
 *  $Id: c_stex02.c,v 1.1 1994-07-28 14:48:14 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define M  25
#define N  25

main()
{
	int ierr;
	extern void stex02();
/*
 * Example STEX02 demonstrates how both the field flow utilities -
 * Vectors and Streamlines - map into the non-uniform coordinate
 * systems supported by the NCAR Graphics SET call. Each of the
 * five frames display a uniform 45 degree field using a Streamlines
 * representation overlaying a Vectors representation. The first four
 * frames cycle through all possible settings of the Linear-Log (LL)
 * argument to the SET call. The fifth frame shows a user coordinate
 * system where the unit X is twice the size of the unit Y.
 *
 * The example also illustrates use of the compatibility mode parameter
 * to allow use of the older interfaces (VELVCT and STRMLN), while
 * still accessing post-Version 3.2 capabilities of the field flow
 * utilities. Note that use of the old entry-points is discouraged
 * other than on a transitional basis. Therefore the comments show the
 * code required to create an identical plot using the Version 3.2 
 * interfaces.
 *
 *  Open gks, open workstation, activate workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	stex02(&ierr);
/*
 * Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void stex02 (ierror)
int *ierror;
{
	int i, j, ll, ier, icn, ierr;
	float u[N][M], v[N][M];
	float wrk[M*N*2], xmx;
	float x, y;
/*
 * Specify the NDC coordinates for a plot title.
 */
	float fx = 0.090909, fy = 0.976540;
/*
 * Specify VELVCT arguments.
 */
	float flo = 0., hi = 0., spv = 0.;
	int nset = 0, length = 0, ispv = 0;
/*
 * Initialize the error parameter.
 */
	*ierror = 1;
/*
 * Specify velocity field component arrays U and V.
 */
	for( i = 0; i < M; i++ ) {
		for( j = 0; j < N; j++ ) {
            u[j][i] = 1.0;
            v[j][i] = 1.0;
		}
	}
/*
 * Generate five frames:
 * Frame 1: X-Axis linear, Y-Axis linear, equal units
 * Frame 2: X-Axis linear, Y-Axis log
 * Frame 3: X-Axis log, Y-Axis linear
 * Frame 4: X-Axis log, Y-Axis log
 * Frame 5: X-Axis linear, Y-Axis linear, 1 X unit = 2 Y units
 */
	for( i = 1; i <= 5; i++ ) {
		if (i == 5) {
            xmx = 50.0;
            ll = 1;
		}
		else {
            xmx = 100.0;
            ll = i;
		}
/*
 * Set up the user coordinate system and the data coordinate
 * boundaries.
 */
		c_set(0.05,0.95,0.05,0.95,1.0,xmx,1.0,100.0,ll);
		c_vvsetr("XC1 -- Lower X Bound", 1.0);
		c_vvsetr("XCM -- Upper X Bound", xmx);
		c_vvsetr("YC1 -- Lower X Bound", 1.0);
		c_vvsetr("YCN -- Upper Y Bound", 100.0);
		c_stsetr("XC1 -- Lower X Bound", 1.0);
		c_stsetr("XCM -- Upper X Bound", xmx);
		c_stsetr("YC1 -- Lower X Bound", 1.0);
		c_stsetr("YCN -- Upper Y Bound", 100.0);
/*
 * Set the compatibility mode parameters: 
 * (1) negative to allow use of Version 3.2 mapping routines 
 * (the old mapping routines, FX and FY, do not support non-uniform 
 * coordinates, and in addition, must be custom coded to support the
 * data coordinate to user coordinate mapping); 
 * (2) to absolute value less than 3 to cause the option input 
 * arguments for VELVCT and STRMLN (FLO,HI,NSET,LENGTH,ISPV, and SPV) 
 * to override the equivalent Version 3.2 parameters; 
 * (3) to an even value, specifying that old common blocks be ignored.
 *
 * This setting causes the value of the NSET parameter to
 * determine whether the utilities perform a SET call. If NSET = 1 
 * the utilities do not perform the set call. 
 */
		c_vvseti("CPM -- Compatibility Mode", -2) ;
		c_stseti("CPM -- Compatibility Mode", -2) ;
		nset = 1;
		gset_line_colr_ind(3);
		c_velvct ((float *)u,M,(float *)v,M,M,N,flo,hi,nset,length,ispv,&spv);
		gset_line_colr_ind(7);
		c_strmln ((float *)u,(float *)v,wrk,M,M,N,nset,&ier);
/*
 * To produce the same plot using Version 3.2 interfaces, comment out
 * the code between this comment and the preceeding one, and uncomment
 * the following code (Note that here CPM is left at its default value
 * of 0 and the SET parameter is given the value of 0 to specify that
 * the utilities should not perform a SET call):
 *
 * You could try setting the transformation type parameter, TRT, to 0 
 * to see the effect it has on the plot frames.
 *
 *         CALL VVSETI("TRT - Transfomation Type", 0)
 *         CALL STSETI("TRT - Transfomation Type", 0)
 *
 * ====================================================================
 *$$$         IDM=0
 *$$$         RDM=0
 *$$$         CALL VVSETI("SET - Do-SET-Call Flag", 0)
 *$$$         CALL STSETI("SET - Do-SET-Call Flag", 0)
 *$$$         gset_line_colr_ind(3)
 *$$$         CALL VVINIT(U,M,V,M,RDM,IDM,M,N,RDM,IDM)
 *$$$         CALL VVECTR(U,V,RDM,IDM,IDM,RDM)
 *$$$         gset_line_colr_ind(7)
 *$$$         CALL STINIT(U,M,V,M,RDM,IDM,M,N,WRK,2*M*N)
 *$$$         CALL STREAM(U,V,RDM,IDM,IDM,WRK)
 * ====================================================================
 *
 * Save the current normalization transformation then set it to 0
 */
		ginq_cur_norm_tran_num(&ierr,&icn);
		gsel_norm_tran(0);
		x = c_cfux(fx);
		y = c_cfuy(fy);
/*
 * Call PLCHLQ to write the plot title.
 */
		c_plchlq (x,y,"Streamlines Plotted Over a Uniform Vector Field",16.,0.,-1.);
/*
 * Restore the normalization transformation
 */
		gsel_norm_tran(icn);
		c_frame();
	}
	*ierror = 0;
	return;
}
