/*
 *  $Id: c_fpcfonts.c,v 1.1 1994-08-02 20:08:02 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define mod(i,j)   ((i)%(j))

/*
 * Define arrays for column labels and row labels for the plots showing
 * the complex and duplex character sets.
 */
char *clbl[12] = { "PRU(0000)", "PRL(0100)", "IRU(0200)", "IRL(0300)", 
                   "KRU(0400)", "KRL(0500)", "PGU(0600)", "PGL(0700)", 
                   "IGU(1000)", "IGL(1100)", "KGU(1200)", "KGL(1300)" };

char  *rlbl[48] = { "A(01)", "B(02)", "C(03)", "D(04)", "E(05)", "F(06)", 
                    "G(07)", "H(10)", "I(11)", "J(12)", "K(13)", "L(14)", 
                    "M(15)", "N(16)", "O(17)", "P(20)", "Q(21)", "R(22)", 
                    "S(23)", "T(24)", "U(25)", "V(26)", "W(27)", "X(30)", 
                    "Y(31)", "Z(32)", "0(33)", "1(34)", "2(35)", "3(36)", 
                    "4(37)", "5(40)", "6(41)", "7(42)", "8(43)", "9(44)", 
                    "+(45)", "-(46)", "*(47)", "/(50)", "((51)", ")(52)", 
                    "$(53)", "=(54)", " (55)", ",(56)", ".(57)", "     " };
/*
 * Define an array in which to put the numbers of the filled fonts.
 */
    int iffn[23] = { 1, 21, 22, 25, 26, 29, 30, 33, 34, 35, 36, 37, 
                       121,122,125,126,129,130,133,134,135,136,137 };

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    int i, j, k, l, icmp, ichr, ifns, c_ifnt;
    float csmu, xpos, ypos, xcen, ycen, xrgt, ybot;
    float wdth, xlft, ytop, xcrd, ycrd;
    Gcolr_rep rgb;
/*
 * --- D E C L A R A T I O N S -----------------------------------------
 *
 * Define a couple of temporary variables of type CHARACTER.
 */
	char ctmp[7], chrs[9], stmp[10];
/*
 * Define the column and row labels.  The character string ':c:r', where
 * "c" is the first three characters of a column label and "r" is the
 * first character of a row label, is used to select the character to
 * be written in that column of that row.
 *
 * Define a flag which says, if 0, that the first eight plots are to
 * occupy eight separate frames and, if 1, that those plots are to be
 * compressed onto two frames.
 */
	icmp = 1;
/*
 * Open GKS.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set up the background and foreground colors
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,1,&rgb);
/*
 * Set the "fill area interior style" to "solid".
 */
	gset_fill_int_style (GSTYLE_SOLID);
/*
 * Do a call to SET which allows us to use fractional coordinates.
 */
	c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Produce examples of the complex and duplex character sets.
 *
 * Compute a character-size multiplier, depending on whether the first
 * eight plots are being put on eight frames or two.
 */
	csmu=(float)(2-icmp);
/*
 * Each pass through the loop on I produces four plots - the first four
 * for the complex set, and the second four for the duplex set.
 */
    for( i = 1; i <= 2; i++ ) {
/*
 * Change to the appropriate character set.
 */
        c_pcseti ("CD",i-1);
/*
 * Each pass through the following loop produces a single plot.
 */
        for( j = 0; j < 4; j++ ) {
/*
 * If the first eight plots are to be compressed, re-do the SET call to
 * put the plot in a particular quadrant of the frame.
 */
            if(icmp != 0) {
                c_set(.5*(float)(j%2),.5*(float)(j%2)+.5,.5-.5*(float)(j/2),1.-.5*(float)(j/2),0.,1.,0.,1.,1);
            }
/*
 * Put labels at the top of the frame and along the left edge.
 */
            if (i == 1) {
                c_plchhq (.5,.98,"PLCHHQ - COMPLEX CHARACTER SET",csmu*.01,0.,0.);
            }
            else {
                c_plchhq (.5,.98,"PLCHHQ - DUPLEX CHARACTER SET",csmu*.01,0.,0.);
            }
		
            c_plchhq (.58,.9267,"FUNCTION CODES SPECIFYING SIZE, FONT, AND CASE",csmu*.00735,0.,0.);
            c_plchhq (.035,.445,":D:STANDARD FORTRAN CHARACTERS",csmu*.00735,0.,0.);
/*
 * Force constant spacing of the characters used for the column and row
 * labels, so that they will line up better with each other.
 */
            c_pcsetr ("CS",1.25);
/*
 * Label the columns.
 */
            for( k=0; k < 12; k++ ) {
                xpos=.125+.07*(float)(k+1);
                strncpy( stmp, clbl[k], 3 );
                stmp[3] = '\0';
                c_plchhq (xpos,.90,stmp,csmu*.006,0.,0.);
                strncpy( stmp, clbl[k], 9 );
                stmp[9] = '\0';
                *stmp+=3;
                c_plchhq (xpos,.88,stmp,csmu*.004,0.,0.);
                *stmp-=3;
            }
/*
 * Each pass through the following loop produces a single row.
 */
			for( k=0; k < 12; k++ ) {
/*
 * Compute the Y coordinate of the row.
 */
                ypos=.9-.07*(float)(k+1);
/*
 * Label the row.
 */
                stmp[0] = rlbl[12*j+k][0];
                stmp[1] = '\0';
                c_plchhq (.085,ypos,stmp,csmu*.006,0.,-1.);
                strncpy( stmp, rlbl[12*j+k], 5 );
                stmp[5] = '\0';
                c_plchhq (.105,ypos,stmp,csmu*.004,0.,-1.);
/*
 * Each pass through the following loop produces a single character.
 */
                for( l = 0; l < 12; l++ ) {
                    xpos=.125+.07*(float)(l+1);
                    ctmp[0] = ':';
                    ctmp[1] = clbl[l][0];
                    ctmp[2] = clbl[l][1];
                    ctmp[3] = clbl[l][2];
                    ctmp[4] = ':';
                    ctmp[5] = rlbl[12*j+k][0];
                    ctmp[6] = '\0';
                    c_plchhq (xpos,ypos,ctmp,csmu*.01,0.,0.);
                }
            }
/*
 * Return to variable spacing.
 */
            c_pcsetr ("CS",0.);
/*
 * If eight frames are being produced, advance the frame here.
 */
            if (icmp == 0) c_frame();
        }
/*
 * If two frames are being produced, advance the frame here.
 */
        if (icmp != 0) c_frame();
	}
/*
 * Return to the complex character set.
 *
    c_pcseti ("CD",0);
/*
 * If two frames were produced, re-do the call to SET which allows us to
 * use fractional coordinates.
 */
    if(icmp != 0) c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Do a single frame showing all the characters in the fontcap databases,
 * access to which was added in June of 1990.
 *
 * Double the line width.
 */
	c_plotif (0.,0.,2);
	gset_linewidth (2.);
/*
 * Put a label at the top of the plot.
 */
	c_plchhq (.5,.98,"PLCHHQ - FONTCAP DATABASES ADDED 6/90",.02,0.,0.);
/*
 * Temporarily use the slash as a function code character.
 */
	c_pcsetc ("FC - FUNCTION CODE CHARACTER","/");
/*
 * Put an explanatory note on the plot.
 */
      c_plchhq (.5,.945,":F1:c selects the ASCII character 'c', as shown in the first two lines.",.01,0.,0.);

    c_plchhq (.5,.925,":Fn:c (2/F18/K/F0/n/F18/K/F0/20) selects the corresponding character from font n.",.01,0.,0.);
/*
 * Return to a colon as the function code character.
 */
    c_pcsetc ("FC - FUNCTION CODE CHARACTER",":");
/*
 * Loop through all the available fonts.
 */
    for( c_ifnt = 1; c_ifnt <= 20; c_ifnt++ ) {
        ycen=.945-.045*(float)(c_ifnt);
        sprintf( chrs, "FONT %d", c_ifnt );
        c_pcseti ("FN - FONTCAP NUMBER",7);
        c_plchhq (.005,ycen,chrs,.012,0.,-1.);
        c_pcseti ("FN - FONTCAP NUMBER",c_ifnt);
/*
 * Draw all the meaningful characters from the font.
 */
        for( ichr = 33; ichr <= 126; ichr++ ) {
            if (ichr <= 79) {
                xcen=.125+.0183*(float)(ichr-32);
            }
            else {
                xcen=.125+.0183*(float)(ichr-79);
            }
            if (ichr == 80) ycen=ycen-.0225;
            if ('0' - ichr == ':') c_pcsetc ("FC","!");
            sprintf( stmp, "%c", ichr );
            c_plchhq (xcen,ycen,stmp,.01,0.,0.);
            if('0' - ichr == ':') c_pcsetc ("FC",":");
        }
/*
 * End of loop through fonts.
 */
	}
/*
 * Restore the fontcap number to 0 to select the PWRITX database.
 */
    c_pcseti ("FN - FONTCAP NUMBER",0);
/*
 * Go back to normal line width.
 */
    c_plotif (0.,0.,2);
    gset_linewidth (1.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Do a single frame showing all the characters in the fontcap databases,
 * access to which was added in October of 1992.
 *
 * Put a label at the top of the plot.
 */
	c_plchhq (.5,.98,"PLCHHQ - FONTCAP DATABASES ADDED 10/92",.02,0.,0.);
/*
 * Temporarily use the slash as a function code character.
 */
    c_pcsetc ("FC - FUNCTION CODE CHARACTER","/");
/*
 * Put an explanatory note on the plot.
 */
	c_plchhq (.5,.945,":F21:c selects the ASCII character 'c', as shown in the first two lines.",.01,0.,0.);
	c_plchhq (.5,.925,":Fn:c selects the corresponding character from font n.",.01,0.,0.);
/*
 * Return to a colon as the function code character.
 */
    c_pcsetc ("FC - FUNCTION CODE CHARACTER",":");
/*
 * Loop through all the new filled fonts.
 */
    for( ifns=1; ifns <= 23; ifns++ ) {
        ycen=.945-.0391304*(float)(ifns);
        sprintf( chrs, "FONT%4d", iffn[ifns-1] );
        c_pcseti ("FN - FONTCAP NUMBER",7);
        c_plchhq (.005,ycen,chrs,.012,0.,-1.);

        c_pcseti ("FN - FONTCAP NUMBER",iffn[ifns-1]);
/*
 * Draw all the meaningful characters from the font.
 */
        for( ichr=33; ichr <= 126; ichr++ ) {
            if (ichr <= 79)  {
                xcen=.125+.0183*(float)(ichr-32);
            }
            else {
                xcen=.125+.0183*(float)(ichr-79);
            }
            if (ichr == 80) ycen=ycen-.0195652;
            if ((char)ichr == ':') c_pcsetc ("FC","!");
            sprintf( stmp, "%c", ichr );
            c_plchhq (xcen,ycen,stmp,.01,0.,0.);
            if ((char)ichr == ':') c_pcsetc ("FC",":");
        }
/*
 * End of loop through fonts.
 */
}
/*
 * Restore the fontcap number to 0 to select the PWRITX database.
 */
    c_pcseti ("FN - FONTCAP NUMBER",0);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}
