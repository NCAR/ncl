/*
 *	$Id: c_elblba.c,v 1.2 1992-11-04 15:50:27 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/ncarg_gksC.h>

char *llb1[20] = {
    "  0 to 5:H2Q"," 5 to 10:H1Q","10 to 15:H1Q","15 to 20:H1Q","20 to 25:H1Q",
    "25 to 30:H1Q","30 to 35:H1Q","35 to 40:H1Q","40 to 45:H1Q","45 to 50:H1Q",
    "50 to 55:H1Q","55 to 60:H1Q","60 to 65:H1Q","65 to 70:H1Q","70 to 75:H1Q",
    "75 to 80:H1Q","80 to 85:H1Q","85 to 90:H1Q","90 to 95:H1Q","   95 to 100"
};

char *llb2[17] = 
{
    "-2000 feet"," Sea Level"," 2000 feet"," 4000 feet"," 6000 feet",
    " 8000 feet","10000 feet","12000 feet","14000 feet","16000 feet",
    "18000 feet","20000 feet","22000 feet","24000 feet","26000 feet",
    "28000 feet","30000 feet"
};

char *llb3[4] = {"M","N","O","P"};
char *llb4[4] = {"I","J","K","L"};
char *llb5[4] = {"E","F","G","H"};
char *llb6[4] = {"A","B","C","D"};

main()
{
    int i,iasf[13];
    int lnd1[20],lnd2[16],lnd3[4],lnd4[4],lnd5[4],lnd6[4];

    for( i = 0; i < 13; i++ ) iasf[i] = 1;
              
    for( i = 0; i < 20; i++ ) lnd1[i] = i+1;
    for( i = 0; i < 16; i++ ) lnd2[i] = i;
    for( i = 0; i < 4; i++ ) {
        lnd3[i] = i+12;
        lnd4[i] = i+8;
        lnd5[i] = i+4;
        lnd6[i] = i;
    }

    c_opngks();
    c_gsasf(iasf);
    c_gsfais(1);
    c_gsclip(0);
    dfclrs();
    c_pcsetr("CS - CONSTANT SPACING",1.25);
    c_lbsetr("WBL - WIDTH OF BOX LINES",4.);
    c_lbsetr("WFL - WIDTH OF FILL LINES",2.);
    c_lbsetr("WLB - WIDTH OF LABEL LINES",2.);
    c_sfseti("ANGLE OF FILL LINES",15);
    c_sfseti ("TYPE OF FILL",-4);
    c_lblbar(1,.05,.30,.05,.95,20,.3333,1.,lnd1,0,llb1,20,2);
    c_sfseti("TYPE OF FILL",0);
    c_lblbar(1,.70,.95,.05,.95,16,.3333,1.,lnd2,0,llb2,17,1);
    c_lblbar(0,.35,.65,.05,.20,4,.5,.5,lnd3,1,llb3,4,1);
    c_lblbar(0,.35,.65,.20,.35,4,.5,.5,lnd4,1,llb4,4,1);
    c_lblbar(0,.35,.65,.35,.50,4,.5,.5,lnd5,1,llb5,4,1);
    c_lblbar(0,.35,.65,.50,.65,4,.5,.5,lnd6,1,llb6,4,1);
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pcsetr("CS - CONSTANT SPACING",0.);
    c_plchhq(.5,.90,"THREE",.025,0.,0.);
    c_plchhq(.5,.85,"LABELBAR",.025,0.,0.);
    c_plchhq(.5,.80,"EXAMPLES",.025,0.,0.);
    
    c_frame();
    c_clsgks();
}
dfclrs()
{
    int i;
    float rgbv[4][16];

    rgbv[1][1] = 1.00;
    rgbv[2][1] = 1.00;
    rgbv[3][1] = 1.00;
    rgbv[1][2] = 0.70;
    rgbv[2][2] = 0.70;
    rgbv[3][2] = 0.70;
    rgbv[1][3] = 0.75;
    rgbv[2][3] = 0.50;
    rgbv[3][3] = 1.00;
    rgbv[1][4] = 0.50;
    rgbv[2][4] = 0.00;
    rgbv[3][4] = 1.00;
    rgbv[1][5] = 0.00;
    rgbv[2][5] = 0.00;
    rgbv[3][5] = 1.00;
    rgbv[1][6] = 0.00;
    rgbv[2][6] = 0.50;
    rgbv[3][6] = 1.00;
    rgbv[1][7] = 0.00;
    rgbv[2][7] = 1.00;
    rgbv[3][7] = 1.00;
    rgbv[1][8] = 0.00;
    rgbv[2][8] = 1.00;
    rgbv[3][8] = 0.60;
    rgbv[1][9] = 0.00;
    rgbv[2][9] = 1.00;
    rgbv[3][9] = 0.00;
    rgbv[1][10] = 0.70;
    rgbv[2][10] = 1.00;
    rgbv[3][10] = 0.00;
    rgbv[1][11] = 1.00;
    rgbv[2][11] = 1.00;
    rgbv[3][11] = 0.00;
    rgbv[1][12] = 1.00;
    rgbv[2][12] = 0.75;
    rgbv[3][12] = 0.00;
    rgbv[1][13] = 1.00;
    rgbv[2][13] = 0.38;
    rgbv[3][13] = 0.38;
    rgbv[1][14] = 1.00;
    rgbv[2][14] = 0.00;
    rgbv[3][14] = 0.38;
    rgbv[1][15] = 1.00;
    rgbv[2][15] = 0.00;
    rgbv[3][15] = 0.00;
    c_gscr(1,0,0.,0.,0.);
    for( i = 1; i <= 15; i++ ) {
        c_gscr(1,i,rgbv[1][i],rgbv[2][i],rgbv[3][i]);
    }
    return(1);
}
