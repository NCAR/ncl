#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

#define N 40        /* data rows */
#define M 40        /* data columns */
#define LRWK 3500   /* length of real work array */
#define LIWK 4000   /* length of integer work array */

int main()
{
    int i;          /* counter */
    int iwrk[LIWK];     /* integer work array */
    int ncons;      /* number of contour lines */

    float z[N][M];      /* data */
    float rwrk[LRWK];   /* real work array */
    float size;     /* title character size */
    float y;        /* title verticle position */

    float vpl,vpr,vpb,vpt,wl,wr,wb,wt;  /* c_getset parameters */
    int ll;

    extern void getdat (float*);  /* get input data */
    extern void color();            /* Define color table */


    getdat (&z[0][0]);

/* Open GKS */
    gopen_gks(IERRF, ISZDM);
    gopen_ws(IWKID, LUNIT, IWTYPE);
    gactivate_ws(IWKID);
    gset_clip_ind (0);

/* Set up color table */
    color();

/* Set up plot annotation color and text options */
    c_cpseti("HLC - HIGH/LOW LABEL COLOR INDEX",9);
    c_cpsetc("HLT - HIGH/LOW LABEL TEXT","High'Low");
    c_cpseti("ILC - INFORMATION LABEL COLOR INDEX",2);
    c_cpsetc("ILT - INFORMATION LABEL TEXT","Informational Label");

/* Initialize Conpack */
    c_cprect(&z[0][0], N, N, M, rwrk, LRWK, iwrk, LIWK);

/* Turn on contour labeling for every line */
    c_cppkcl(&z[0][0], rwrk, iwrk);
    c_cpgeti("NCL - NUMBER OF CONTOUR LINES",&ncons);
    c_cpseti("LLP - LINE LABEL POSITIONING",3);
    c_cpseti("LLO - LINE LABEL ORIENTATION",1);

    for(i=0; i<ncons; ++i)
    {
        c_cpseti("PAI - PARAMETER ARRAY INDEX",i+1);
        c_cpseti("CLU - CONTOUR LEVEL USE FLAG",2);
        c_cpseti("LLC - LINE LABEL COLOR INDEX",4);
    }

/* Draw Perimeter */
    c_cpback(&z[0][0], rwrk, iwrk);
    
/* Draw Contours */
    c_cplbdr(&z[0][0],rwrk,iwrk);
    c_cpcldr(&z[0][0],rwrk,iwrk);

/* Draw a Title */
    gset_line_colr_ind(6);
    c_getset(&vpl,&vpr,&vpb,&vpt,&wl,&wr,&wb,&wt,&ll);
    size = .66 * (1.0 - vpt);
    y = 1.0 - .5 * (1.0 - vpt);
    c_set (0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1);
    c_pchiqu (.5, y, "This is the Title",size, 0., 0.);

/* Close frame and close GKS */
    c_frame();
    gdeactivate_ws(IWKID);
    gclose_ws(IWKID);
    gclose_gks();
    return (0);
}

void getdat (float* z)
/*
** Get input data.
**
** z    - data array
*/
{
    int i,j;    /* counters */

    FILE* fp;   /* data file pointer */

/* Open input file, read data. */
    fp = fopen("ccpex.dat","r");
    for(j=0; j<M; ++j)
    {
        for(i=0; i<N; ++i)
        {
            fscanf(fp,"%f",&z[((i*M)+j)]);
        }
    }
    fclose(fp);
}


void color()
/*
** Initialize color table.
*/
{
    Gcolr_rep colr_rep;     /* red, green, blue color values */

/*
**    BACKGROUND color
** Black
*/
    colr_rep.rgb.red = 0.0;
    colr_rep.rgb.green = 0.0;
    colr_rep.rgb.blue = 0.0;
    gset_colr_rep(1,0,&colr_rep);
/*
**     FOREGROUND color
** White
*/
    colr_rep.rgb.red = 1.0;
    colr_rep.rgb.green = 1.0;
    colr_rep.rgb.blue = 1.0;
    gset_colr_rep(1,1,&colr_rep);
/*
** Aqua
*/
    colr_rep.rgb.red = 0.0;
    colr_rep.rgb.green = 0.9;
    colr_rep.rgb.blue = 1.0;
    gset_colr_rep(1,2,&colr_rep);
/*
** Red
*/
    colr_rep.rgb.red = 0.9;
    colr_rep.rgb.green = 0.25;
    colr_rep.rgb.blue = 0.0;
    gset_colr_rep(1,3,&colr_rep);
/*
** OrangeRed
*/
    colr_rep.rgb.red = 1.0;
    colr_rep.rgb.green = 0.0;
    colr_rep.rgb.blue = 0.2;
    gset_colr_rep(1,4,&colr_rep);
/*
** Orange
*/
    colr_rep.rgb.red = 1.0;
    colr_rep.rgb.green = 0.65;
    colr_rep.rgb.blue = 0.0;
    gset_colr_rep(1,5,&colr_rep);
/*
** Yellow
*/
    colr_rep.rgb.red = 1.0;
    colr_rep.rgb.green = 1.0;
    colr_rep.rgb.blue = 0.0;
    gset_colr_rep(1,6,&colr_rep);
/*
** GreenYellow
*/
    colr_rep.rgb.red = 0.7;
    colr_rep.rgb.green = 1.0;
    colr_rep.rgb.blue = 0.2;
    gset_colr_rep(1,7,&colr_rep);
/*
** Chartreuse
*/
    colr_rep.rgb.red = 0.5;
    colr_rep.rgb.green = 1.0;
    colr_rep.rgb.blue = 0.0;
    gset_colr_rep(1,8,&colr_rep);
/*
** Celeste
*/
    colr_rep.rgb.red = 0.2;
    colr_rep.rgb.green = 1.0;
    colr_rep.rgb.blue = 0.5;
    gset_colr_rep(1,9,&colr_rep);
/*
** Green
*/
    colr_rep.rgb.red = 0.2;
    colr_rep.rgb.green = 0.8;
    colr_rep.rgb.blue = 0.2;
    gset_colr_rep(1,10,&colr_rep);
/*
** DeepSkyBlue
*/
    colr_rep.rgb.red = 0.0;
    colr_rep.rgb.green = 0.75;
    colr_rep.rgb.blue = 1.0;
    gset_colr_rep(1,11,&colr_rep);
/*
** RoyalBlue
*/
    colr_rep.rgb.red = 0.25;
    colr_rep.rgb.green = 0.45;
    colr_rep.rgb.blue = 0.95;
    gset_colr_rep(1,12,&colr_rep);
/*
** SlateBlue
*/
    colr_rep.rgb.red = 0.4;
    colr_rep.rgb.green = 0.35;
    colr_rep.rgb.blue = 0.8;
    gset_colr_rep(1,13,&colr_rep);
/*
** DarkViolet
*/
    colr_rep.rgb.red = 0.6;
    colr_rep.rgb.green = 0.0;
    colr_rep.rgb.blue = 0.8;
    gset_colr_rep(1,14,&colr_rep);
/*
** Orchid
*/
    colr_rep.rgb.red = 1.0;
    colr_rep.rgb.green = 0.0;
    colr_rep.rgb.blue = 1.0;
    gset_colr_rep(1,15,&colr_rep);
/*
** Lavender
*/
    colr_rep.rgb.red = 0.8;
    colr_rep.rgb.green = 0.8;
    colr_rep.rgb.blue = 1.0;
    gset_colr_rep(1,16,&colr_rep);
/*
** Gray
*/
    colr_rep.rgb.red = 0.7;
    colr_rep.rgb.green = 0.7;
    colr_rep.rgb.blue = 0.7;
    gset_colr_rep(1,17,&colr_rep);
/*
** Done.
*/
}
