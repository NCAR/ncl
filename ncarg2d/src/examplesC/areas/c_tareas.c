/*
 *	$Id: c_tareas.c,v 1.5 1999-07-27 20:14:54 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <stdlib.h>

#define IWTYPE 1
#define WKID   1

main()
{
    int i, idum, ierr;
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
    tareas(&ierr);
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

tareas(ierror)
int *ierror;
{
    int i,j,ing,isu,iam[5000],iai[2],iag[2],ie;
    float xca[73],yca[73],ang;
    float xcs[150],ycs[150],rad,xcd,ycd;
    float dtr,xcn,ycn;
    int ioc[16],icf,nai,itm,it1,it2;
    int ncd, mcs, mai;
    Gcolr_rep rgb[16];
    Gasfs if1;
    extern int colram(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );

    extern int colrln(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );

    ncd = 6;
    mcs = 150;
    mai = 2;
    rgb[0].rgb.red = 0.00; rgb[0].rgb.green = 0.00; rgb[0].rgb.blue = 0.00;
    rgb[1].rgb.red = 0.70; rgb[1].rgb.green = 0.70; rgb[1].rgb.blue = 0.70;
    rgb[2].rgb.red = 0.75; rgb[2].rgb.green = 0.50; rgb[2].rgb.blue = 1.00;
    rgb[3].rgb.red = 0.50; rgb[3].rgb.green = 0.00; rgb[3].rgb.blue = 1.00;
    rgb[4].rgb.red = 0.00; rgb[4].rgb.green = 0.00; rgb[4].rgb.blue = 1.00;
    rgb[5].rgb.red = 0.00; rgb[5].rgb.green = 0.50; rgb[5].rgb.blue = 1.00;
    rgb[6].rgb.red = 0.00; rgb[6].rgb.green = 1.00; rgb[6].rgb.blue = 1.00;
    rgb[7].rgb.red = 0.00; rgb[7].rgb.green = 1.00; rgb[7].rgb.blue = 0.60;
    rgb[8].rgb.red = 0.00; rgb[8].rgb.green = 1.00; rgb[8].rgb.blue = 0.00;
    rgb[9].rgb.red = 0.70; rgb[9].rgb.green = 1.00; rgb[9].rgb.blue = 0.00;
    rgb[10].rgb.red = 1.00; rgb[10].rgb.green = 1.00; rgb[10].rgb.blue = 0.00;
    rgb[11].rgb.red = 1.00; rgb[11].rgb.green = 0.75; rgb[11].rgb.blue = 0.00;
    rgb[12].rgb.red = 1.00; rgb[12].rgb.green = 0.38; rgb[12].rgb.blue = 0.38;
    rgb[13].rgb.red = 1.00; rgb[13].rgb.green = 0.00; rgb[13].rgb.blue = 0.38;
    rgb[14].rgb.red = 1.00; rgb[14].rgb.green = 0.00; rgb[14].rgb.blue = 0.00;
    rgb[15].rgb.red = 1.00; rgb[15].rgb.green = 1.00; rgb[15].rgb.blue = 1.00;

    ioc[0] = 0;
    ioc[1] = 6;
    ioc[2] = 2;
    ioc[3] = 5;
    ioc[4] = 12;
    ioc[5] = 10;
    ioc[6] = 11;
    ioc[7] = 1;
    ioc[8] = 3;
    ioc[9] = 4;
    ioc[10] = 8;
    ioc[11] = 9;
    ioc[12] = 7;
    ioc[13] = 13;
    ioc[14] = 14;
    ioc[15] = 15;

    dtr = .017453292519943;
    ginq_asfs(&ie,&if1);
    if1.fill_int_style=GASF_INDIV;
    if1.fill_style_ind=GASF_INDIV;
    gset_asfs(&if1);
    gset_fill_int_style(GSTYLE_SOLID);
    for( j = 0; j <= 15; j++ ) {
        i=ioc[j];
        gset_colr_rep(WKID,j,&rgb[i]);
    }
    c_arinam(iam,5000);
    c_set(.01,.99,.01,.99,-1.,1.,-1.,1.,1);
    for( ing = 1; ing <= 73; ing++ ) {
        ang=dtr*(float)(5*(ing-1));
        xca[ing-1]=cos(ang);
        yca[ing-1]=sin(ang);
    }
    c_aredam(iam,xca,yca,73,1,0,-1);
    xca[0]=0.;
    yca[0]=0.;
    for( ing = 1; ing <= 15; ing++ ) {
        ang=dtr*(float)(24*(ing-1));
        xca[1]=cos(ang);
        yca[1]=sin(ang);
        c_aredam(iam,xca,yca,2,1,ing,((ing+13)%15)+1);
    }
    for( ing = 1; ing <= 73; ing++ ) {
        ang=dtr*(float)(5*(ing-1));
        xca[ing-1]=.25+.5*cos(ang);
        yca[ing-1]=.25+.5*sin(ang);
    }
    c_aredam(iam,xca,yca,73,2,1,2);
    c_arpram(iam,0,0,0);
    isu=5000-(iam[5]-iam[4]-1);
    printf( "\n   SPACE USED IN AREA MAP IS %d\n",isu);
    c_arscam(iam,xcs,ycs,150,iai,iag,2,colram);
    for( i = 1; i <= 3; i++ ) {
        if(i==1) {
            xcn = -0.5;
            ycn = .5;
        }
        else if(i==2) {
            xcn = -0.5;
            ycn = -0.5;
        }
        else if(i==3) {
            xcn = 0.5;
            ycn = -0.5;
        }
        xca[0]=xcn+.25*cos( 162.*dtr);
        yca[0]=ycn+.25*sin( 162.*dtr);
        xca[1]=xcn+.25*cos(  18.*dtr);
        yca[1]=ycn+.25*sin(  18.*dtr);
        xca[2]=xcn+.25*cos(-126.*dtr);
        yca[2]=ycn+.25*sin(-126.*dtr);
        xca[3]=xcn+.25*cos(  90.*dtr);
        yca[3]=ycn+.25*sin(  90.*dtr);
        xca[4]=xcn+.25*cos( -54.*dtr);
        yca[4]=ycn+.25*sin( -54.*dtr);
        xca[5]=xcn+.25*cos( 162.*dtr);
        yca[5]=ycn+.25*sin( 162.*dtr);
        c_ardrln(iam,xca,yca,ncd,xcs,ycs,mcs,iai,iag,mai,colrln);
    }
    icf=1;
    for( ing = 1; ing <= 1500; ing++ ) {
        rad=(float)(ing)/1000.;
        ang=dtr*(float)(ing-1);
        xcd=.25+.5*rad*cos(ang);
        ycd=.25+.5*rad*sin(ang);
        c_argtai(iam,xcd,ycd,iai,iag,2,&nai,icf);
        itm=1;
        for( i = 0; i < nai; i++ ) {
            if(iai[i]<0) itm=0;
        }
        if(itm) {
            it1=0;
            it2=0;
            for( i = 0; i < nai; i++ ) {
                if(iag[i]==1) it1=iai[i];
                if(iag[i]==2) it2=iai[i];
            }
            if(it1 > 0 && it2 == 1) {
                c_plotit(0,0,0);
                gset_line_colr_ind(it1);
                c_point(xcd,ycd);
            }
        }
        icf=0;
    }
    c_frame();
    *ierror=0;
    printf( "\n  AREAS TEST EXECUTED--SEE PLOTS TO CERTIFY\n");
    return(1);
}

#ifdef __STDC__
int colram(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else 
int colram(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
    int i, itm, it1, it2;
    Gpoint_list fill_area;

    itm=1;
    for( i = 0; i < *nai; i++ ) {
        if ( iai[i] < 0 ) itm=0;
    }
    if (itm) {
        it1=0;
        for( i = 0; i < *nai; i++ ) {
            if (iag[i] == 1) it1=iai[i];
            if (iag[i] == 2) it2=iai[i];
        }
        if (it1 > 0 && it2 != 1) {
/*
 * Set fill area color index.
 */
            gset_fill_colr_ind(it1);
/*
 * Create structure to pass to gfill_area
 */
			fill_area.num_points = *ncs-1;
			fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
			if( !fill_area.points ) {
				fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
				gemergency_close_gks();
				exit(1);
			}
			for( i = 0; i < *ncs-1; i++ ) {
				fill_area.points[i].x = xcs[i];
				fill_area.points[i].y = ycs[i];
			}
/*
 * Fill area
 */
            gfill_area (&fill_area);
			free(fill_area.points);
        }
    }
    return(1);
}

#ifdef __STDC__
int colrln(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else
int colrln(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
    int i, itm, it1, it2;
    Gpoint_list line;

    itm=1;
    for( i = 0; i < *nai; i++ ) {
        if (iai[i] < 0) itm=0;
    }
    if (itm != 0) {
        it1=0;
        it2=0;
        for( i = 0; i < *nai; i++ ) {
            if (iag[i] == 1) it1=iai[i];
            if (iag[i] == 2) it2=iai[i];
        }
        if (it1 > 0 && it2 != 1) {
/*
 * Flush PLOTIT's buffers and set polyline color index.
 */
            c_sflush();
            gset_line_colr_ind( ((it1+3) % 15) + 1);

/*
 * Create structure to pass to gpolyline
 */
			line.num_points = *ncs;
			line.points = (Gpoint *) malloc(line.num_points*sizeof(Gpoint));
			if( !line.points ) {
				fprintf( stderr, "colrln: Not enough memory to create fill area structure\n" );
				gemergency_close_gks();
				exit(1);
			}
			for( i = 0; i < *ncs; i++ ) {
				line.points[i].x = xcs[i];
				line.points[i].y = ycs[i];
			}
/*
 * Draw line
 */
            gpolyline(&line);
        }
    }
    return(1);
}
