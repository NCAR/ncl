/*
 *  $Id: c_epltch.c,v 1.4 1997-04-21 14:38:30 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <string.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * Define the column and row labels.  The character string ':c:r', where
 * "c" is the first three characters of a column label and "r" is the
 * first character of a row label, is used to select the character to
 * be written in that column of that row.
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
 * Define the font numbers for the filled fonts.
 */
    int iffn[23] = { 1, 21, 22, 25, 26, 29, 30, 33, 34, 35, 36, 37, 
                       121,122,125,126,129,130,133,134,135,136,137 };
#define IWTYPE 1
#define WKID   1

main()
{
    extern void drawbx();
/*
 * Define arrays for column labels and row labels for the plots showing
 * the complex and duplex character sets.
 */
    char ctmp[7], chrs[9], stmp[10];
    int i, j, k, l, icmp, c_ifnt, ichr, ifns;
    float csmu, xpos, ypos, xcen, ycen, xrgt, ybot;
    float wdth, xlft, ytop, xcrd, ycrd;
    Gcolr_rep rgb[5];
/*
 * Define a flag which says, if 0, that the first eight plots are to
 * occupy eight separate frames and, if 1, that those plots are to be
 * compressed onto two frames.
 */
    icmp = 1;
/*
 * Open GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Do a call to SET which allows us to use fractional coordinates.
 */
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Produce examples of the complex and duplex character sets.
 *
 * Compute a character-size multiplier, depending on whether the first
 * eight plots are being put on eight frames or two.
 */
    csmu = (float)(2-icmp);
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
 * If eight frames are being produced, advance the frames here.
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
 */
    c_pcseti ("CD",0);
/*
 * If two frames were produced, re-do the call to SET which allows us to
 * use fractional coordinates.
 */
    if(icmp != 0) c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Do a single frame showing various capabilities of PLCHHQ.
 *
 *
 * Put labels at the top of the plot.
 */
    c_plchhq (.5,.98,"PLCHHQ - VARIOUS CAPABILITIES",.02,0.,0.);
/*
 * First, write characters at various sizes.
 */
    c_plchhq (.225,.900,"SIZE is -1.0",-1.0,0.,0.);
    c_plchhq (.225,.873,"SIZE is -.75",-.75,0.,0.);
    c_plchhq (.225,.846,"SIZE is .015",.015,0.,0.);
    c_plchhq (.225,.811,"SIZE is .020",.020,0.,0.);
    c_plchhq (.225,.776,"SIZE is 15.0",15.0,0.,0.);
    c_plchhq (.225,.742,"SIZE is 20.0",20.0,0.,0.);
/*
 * Next, write characters at various angles.
 */
    c_plchhq (.225,.453,"   ANGD is   0.",.012,  0.,-1.);
    c_plchhq (.225,.453,"   ANGD is  45.",.012, 45.,-1.);
    c_plchhq (.225,.453,"   ANGD is  90.",.012, 90.,-1.);
    c_plchhq (.225,.453,"   ANGD is 135.",.012,135.,-1.);
    c_plchhq (.225,.453,"   ANGD is 180.",.012,180.,-1.);
    c_plchhq (.225,.453,"   ANGD is 225.",.012,225.,-1.);
    c_plchhq (.225,.453,"   ANGD is 270.",.012,270.,-1.);
    c_plchhq (.225,.453,"   ANGD is 315.",.012,315.,-1.);
/*
 * Next, use various values of the centering option.
 */
    c_plchhq (.225,.164,"CNTR is -1.5",.012,0.,-1.5);
    c_plchhq (.225,.140,"CNTR is -1.0",.012,0.,-1.0);
    c_plchhq (.225,.116,"CNTR is -0.5",.012,0.,-0.5);
    c_plchhq (.225,.092,"CNTR is  0.0",.012,0., 0.0);
    c_plchhq (.225,.068,"CNTR is +0.5",.012,0., 0.5);
    c_plchhq (.225,.044,"CNTR is +1.0",.012,0., 1.0);
    c_plchhq (.225,.020,"CNTR is +1.5",.012,0., 1.5);
/*
 * Turn on the computation of text-extent-vector magnitudes and use
 * them to draw a box around a label.  (DRAWBX is not part of PLOTCHAR;
 * the code for it appears at the end of this example.)
 */
    c_pcseti ("TE - TEXT EXTENT FLAG",1);
    
    c_plchhq (.130,.140,"TEXT EXTENT BOX",.012,33.,0.);
    drawbx (.130,.140,33.,.01);
    
    c_pcseti ("TE - TEXT EXTENT FLAG",0);
/*
 * On the right side of the frame create examples of the various kinds
 * of function codes.  First, do them using high-quality characters.
 */
    c_plchhq (.715,.900,"HIGH-QUALITY CHARACTERS USED BELOW",.012,0.,0.);
    
    c_pcsetc ("FC","$");
    c_plchhq (.625,.870,"INPUT STRING",.012,0.,0.);
    c_plchhq (.625,.840,"------------",.012,0.,0.);
    c_plchhq (.625,.810,":L:A",.012,0.,0.);
    c_plchhq (.625,.780,":IGL:A",.012,0.,0.);
    c_plchhq (.625,.750,"A:S:2:N:+B:S:2:N:",.012,0.,0.);
    c_plchhq (.625,.720,"A:S:B",.012,0.,0.);
    c_plchhq (.625,.690,"A:SPU:B",.012,0.,0.);
    c_plchhq (.625,.660,":GIU:+",.012,0.,0.);
    c_plchhq (.625,.630,":1045:",.012,0.,0.);
    c_plchhq (.625,.600,"10:S:10:S:100",.012,0.,0.);
    c_plchhq (.625,.570,"X:B1:2:S1:3",.012,0.,0.);
    c_plchhq (.625,.540,"X:B1:2:S:3:N:Y:S:2",.012,0.,0.);
    c_plchhq (.625,.510,"X:S:A:B:1:NN:ABC",.012,0.,0.);
    c_plchhq (.625,.480,"1.3648:L1:410:S:-13",.012,0.,0.);
    
    c_pcsetc ("FC",":");
    c_plchhq (.875,.870,"RESULT",.012,0.,0.);
    c_plchhq (.875,.840,"------",.012,0.,0.);
    c_plchhq (.875,.810,":L:A",.012,0.,0.);
    c_plchhq (.875,.780,":IGL:A",.012,0.,0.);
    c_plchhq (.875,.750,"A:S:2:N:+B:S:2:N:",.012,0.,0.);
    c_plchhq (.875,.720,"A:S:B",.012,0.,0.);
    c_plchhq (.875,.690,"A:SPU:B",.012,0.,0.);
    c_plchhq (.875,.660,":GIU:+",.012,0.,0.);
    c_plchhq (.875,.630,":1045:",.012,0.,0.);
    c_plchhq (.875,.600,"10:S:10:S:100",.012,0.,0.);
    c_plchhq (.875,.570,"X:B1:2:S1:3",.012,0.,0.);
    c_plchhq (.875,.540,"X:B1:2:S:3:N:Y:S:2",.012,0.,0.);
    c_plchhq (.875,.510,"X:S:A:B:1:NN:ABC",.012,0.,0.);
    c_plchhq (.875,.480,"1.3648:L1:410:S:-13",.012,0.,0.);
/*
 * Now, do the same examples using medium-quality characters.
 */
    c_plchhq (.715,.440,"MEDIUM-QUALITY CHARACTERS USED BELOW",.012,0.,0.);
    
    c_pcseti ("QU",1);
    
    c_pcsetc ("FC","$");
    c_plchhq (.625,.410,"INPUT STRING",.012,0.,0.);
    c_plchhq (.625,.380,"------------",.012,0.,0.);
    c_plchhq (.625,.350,":L:A",.012,0.,0.);
    c_plchhq (.625,.320,":IGL:A",.012,0.,0.);
    c_plchhq (.625,.290,"A:S:2:N:+B:S:2:N:",.012,0.,0.);
    c_plchhq (.625,.260,"A:S:B",.012,0.,0.);
    c_plchhq (.625,.230,"A:SPU:B",.012,0.,0.);
    c_plchhq (.625,.200,":GIU:+",.012,0.,0.);
    c_plchhq (.625,.170,":1045:",.012,0.,0.);
    c_plchhq (.625,.140,"10:S:10:S:100",.012,0.,0.);
    c_plchhq (.625,.110,"X:B1:2:S1:3",.012,0.,0.);
    c_plchhq (.625,.080,"X:B1:2:S:3:N:Y:S:2",.012,0.,0.);
    c_plchhq (.625,.050,"X:S:A:B:1:NN:ABC",.012,0.,0.);
    c_plchhq (.625,.020,"1.3648:L1:410:S:-13",.012,0.,0.);
    
    c_pcsetc ("FC",":");
    c_plchhq (.875,.410,"RESULT",.012,0.,0.);
    c_plchhq (.875,.380,"------",.012,0.,0.);
    c_plchhq (.875,.350,":L:A",.012,0.,0.);
    c_plchhq (.875,.320,":IGL:A",.012,0.,0.);
    c_plchhq (.875,.290,"A:S:2:N:+B:S:2:N:",.012,0.,0.);
    c_plchhq (.875,.260,"A:S:B",.012,0.,0.);
    c_plchhq (.875,.230,"A:SPU:B",.012,0.,0.);
    c_plchhq (.875,.200,":GIU:+",.012,0.,0.);
    c_plchhq (.875,.170,":1045:",.012,0.,0.);
    c_plchhq (.875,.140,"10:S:10:S:100",.012,0.,0.);
    c_plchhq (.875,.110,"X:B1:2:S1:3",.012,0.,0.);
    c_plchhq (.875,.080,"X:B1:2:S:3:N:Y:S:2",.012,0.,0.);
    c_plchhq (.875,.050,"X:S:A:B:1:NN:ABC",.012,0.,0.);
    c_plchhq (.875,.020,"1.3648:L1:410:S:-13",.012,0.,0.);

    c_pcseti ("QU",0);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Example 1-10. -------------------------------------------------------
 *
 * Do a single frame showing the medium-quality characters with various
 * aspect ratios.
 *
 * Put labels at the top of the plot.
 */
    c_plchmq (.5,.98,"PLCHMQ - ALL CHARACTERS - VARIOUS ASPECT RATIOS",.02,0.,0.);

    c_plchmq (.5,.95,"(Ratio of height to width varies from 2 in the top group down to .5 in the bottom group.)",.01,0.,0.);
/*
 * Produce five groups of characters.
 */
    for( i = 1; i <= 5; i++ ) {
        ypos=1.-.18*(float)(i);
        c_pcsetr ("HW",2.-1.5*(float)(i-1)/4.);
        c_plchmq (.5,ypos+.04,"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",.02,0.,0.);
        c_plchmq (.5,ypos    ,"abcdefghijklmnopqrstuvwxyz0123456789",.02,0.,0.);
        c_plchmq (.5,ypos-.04,"!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~",.02,0.,0.);
    }
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Do a single frame showing all the characters in the fontcap databases,
 * access to which was added in June of 1990.
 */
  
/*
 * Double the line width.
 */
    gset_linewidth (2.);
/*
 * Do a single frame showing all the characters in the fontcap databases,
 * access to which was added in June of 1990.
 */

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
 * Example 1-12. -------------------------------------------------------
 */
/*
 * Do a single frame showing the use of fontcap databases and some of
 * new features added in June of 1990.
 *
 * Double the line width.
 */
    gset_linewidth (2.);
/*
 * Put a label at the top of the plot.
 */
    c_plchhq (.5,.98,"PLCHHQ - FEATURES ADDED 6/90",.02,0.,0.);
/*
 * Temporarily use the slash as a function code character.
 */
    c_pcsetc ("FC - FUNCTION CODE CHARACTER","/");
/*
 * Combine characters from several different fonts to produce a single
 * line.
 */
    c_plchhq (.5,.910,"/F13/A line containing characters from several fonts:  /F8/P/BF13/0/N/=/F5/g/SF13/2/N/+/F5/j/SF13/2/N/",.012,0.,0.);

/*
 * Reset the internal parameter "FN" to 4 and write a line illustrating
 * the effect of function codes "Fn", "F", and "F0".  Then reset "FN"
 * to 0.
 */
    c_pcseti ("FN - FONT NUMBER",4);
    c_plchhq (.5,.844,"Set 'FN' (Font Number) to 4 and write a line using 'F' function codes:",.012,0.,0.);
    c_plchhq (.5,.820,"Before an F10 - /F10/after an F10 - /F/after an F - /F0/after an F0.",.012,0.,0.);
    c_pcseti ("FN - FONT NUMBER",0);
/*
 * Write lines illustrating various kinds of zooming.
 */
    c_plchhq (.5,.754,"/F13/Unzoomed characters from font 13.",.012,0.,0.);
    c_plchhq (.5,.730,"/F13X150Q/Characters zoomed in width, using X150Q.",.012,0.,0.);
    c_plchhq (.5,.700,"/F13Y150Q/Characters zoomed in height, using Y150Q.",.012,0.,0.);
    c_plchhq (.5,.664,"/F13Z150Q/Characters zoomed both ways, using Z150Q.",.012,0.,0.);
/*
 * Write a line illustrating non-aligned zooming in height.
 */
    c_plchhq (.5,.630,"/F13/Unaligned zoom of selected characters: /F16Y200/S/Y/cientific /Y200/V/Y/isualization /Y200/G/Y/roup",.012,0.,0.);
/*
 * Write lines illustrating the use of "AS" and "SS".
 */
    c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",.125);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",  0.);
    c_plchhq (.5,.564,"/F14/Line written with 'AS' = .125 and 'SS' = 0.",.012,0.,0.);
    c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",  0.);
    c_plchhq (.5,.540, "/F14/Line written with 'AS' = 0. and 'SS' = 0.",.012,0.,0.);
    c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",.125);
    c_plchhq (.5,.516, "/F14/Line written with 'AS' = 0. and 'SS' = .125",.012,0.,0.);;
    c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",  0.);
/*
 * Illustrate the difference between inexact centering and exact
 * centering of a single character.
 */
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",.128);
    
    c_plchhq (.1,.455,"/F7/This 'g' is centered on the cross using CNTR = 0. and 'CE' = 0:",.012,0.,-1.);
    c_line (.880,.455,.920,.455);
    c_line (.900,.435,.900,.475);
    c_pcseti ("CE - CENTERING OPTION",0);
    c_plchhq (.9,.455,"/F7/g",.025,0.,0.);
    c_pcseti ("CE - CENTERING OPTION",0);
    
    c_plchhq (.1,.405,"/F7/This 'g' is centered on the cross using CNTR = 0. and 'CE' = 1:",.012,0.,-1.);
    c_line (.880,.405,.920,.405);
    c_line (.900,.385,.900,.425);
    c_pcseti ("CE - CENTERING OPTION",1);
    c_plchhq (.9,.405,"/F7/g",.025,0.,0.);
    c_pcseti ("CE - CENTERING OPTION",0);
    
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",0.);
/*
 * Put some large characters on a grid to show the digitization.
 */
    c_plchhq (.5,.312,"Large characters on digitization grid.  X's mark edge points of the characters.",.01,0.,0.);

    wdth=.150;
    xlft=.500-48.*(wdth/16.);
    xrgt=.500+48.*(wdth/16.);
    ybot=.150-11.*(wdth/16.);
    ytop=.150+14.*(wdth/16.);
    
    c_plotif (0.,0.,2);
    gset_linewidth (1.);
    
    for( i = -48; i <= 48; i++ ) {
        xcrd=.500+(float)(i)*(wdth/16.);
        c_line (xcrd,ybot,xcrd,ytop);
    }
    
    for( j = -11; j <= 14; j++ ) {
        ycrd=.150+(float)(j)*(wdth/16.);
        c_line (xlft,ycrd,xrgt,ycrd);
    }
    
    c_plotif (0.,0.,2);
    gset_linewidth (2.);

    xcrd=.500-45.*(wdth/16.);
    ycrd=.150;
    c_line (xcrd-wdth/32.,ycrd-wdth/32.,xcrd+wdth/32.,ycrd+wdth/32.);
    c_line (xcrd-wdth/32.,ycrd+wdth/32.,xcrd+wdth/32.,ycrd-wdth/32.);
    c_plchhq (xcrd,ycrd,"/F9/A",wdth,0.,-1.);
    c_pcgetr ("XE - X COORDINATE AT END OF STRING",&xcrd);
    c_line (xcrd-wdth/32.,ycrd-wdth/32.,xcrd+wdth/32.,ycrd+wdth/32.);
    c_line (xcrd-wdth/32.,ycrd+wdth/32.,xcrd+wdth/32.,ycrd-wdth/32.);
    c_plchhq (xcrd,ycrd,"/F9/B",wdth,0.,-1.);
    c_pcgetr ("XE - X COORDINATE AT END OF STRING",&xcrd);
    c_line (xcrd-wdth/32.,ycrd-wdth/32.,xcrd+wdth/32.,ycrd+wdth/32.);
    c_line (xcrd-wdth/32.,ycrd+wdth/32.,xcrd+wdth/32.,ycrd-wdth/32.);
    c_plchhq (xcrd,ycrd,"/F9/C",wdth,0.,-1.);
    c_pcgetr ("XE - X COORDINATE AT END OF STRING",&xcrd);
    c_line (xcrd-wdth/32.,ycrd-wdth/32.,xcrd+wdth/32.,ycrd+wdth/32.);
    c_line (xcrd-wdth/32.,ycrd+wdth/32.,xcrd+wdth/32.,ycrd-wdth/32.);
    c_plchhq (xcrd,ycrd,"/F9/D",wdth,0.,-1.);
    c_pcgetr ("XE - X COORDINATE AT END OF STRING",&xcrd);
    c_line (xcrd-wdth/32.,ycrd-wdth/32.,xcrd+wdth/32.,ycrd+wdth/32.);
    c_line (xcrd-wdth/32.,ycrd+wdth/32.,xcrd+wdth/32.,ycrd-wdth/32.);
/*
 * Return to a colon as the function code character.
 */
    c_pcsetc ("FC - FUNCTION CODE CHARACTER",":");
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
 * --- Example 1-13 -----------------------------------------
 *
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
    c_plchhq (.5,.945,":F1:c selects the ASCII character 'c', as shown in the first two lines.",.01,0.,0.);

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
 * --- Example 1-14 -----------------------------------------
 * 
 * Do a single frame showing some of the new features added in December
 * of 1992.
 *
 * Put a label at the top of the plot and, below that, an explanatory
 * note.
 */
    c_plchhq (.5,.975,":F25:PLCHHQ - FEATURES ADDED 12/92",.025,0.,0.);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",.275);
    c_plchhq (.5,.938,":F13:(Use idt's 'zoom' to view some of this in detail, especially stacking.",.017,0.,0.);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",0.);
/*
 * Illustrate the use of filled fonts with shadows and outlines.  First,
 * define some colors to use.
 */
    rgb[0].rgb.red = 0.;  rgb[0].rgb.green = .5;  rgb[0].rgb.blue = .5;    
    rgb[1].rgb.red = .9;  rgb[1].rgb.green = .9;  rgb[1].rgb.blue = 0.;
    rgb[2].rgb.red = 1.;  rgb[2].rgb.green = .3;  rgb[2].rgb.blue = .3;    
    rgb[3].rgb.red = 0.;  rgb[3].rgb.green = 0.;  rgb[3].rgb.blue = 1.;    
    rgb[4].rgb.red = .2;  rgb[4].rgb.green = .2;  rgb[4].rgb.blue = .2;    
    for( i = 0; i < 5; i++ ) gset_colr_rep(WKID,i+2,&rgb[i]);
/*
 * Write a line.
 */
    c_plchhq (.5,.900,":F26:By default, the current foreground color is used.",.024,0.,0.);
/*
 * Define the principal color to be used for characters.
 */
    c_pcseti ("CC - CHARACTER COLOR",4);
/*
 * Write another line.
 */
    c_plchhq (.5,.850,":F26:A non-negative 'CC' requests a different color.",.026,0.,0.);
/*
 * Turn on character shadows and define various characteristics of the
 * shadow.
 */
    c_pcseti ("SF - SHADOW FLAG",1);
    c_pcsetr ("SX - SHADOW OFFSET IN X",-.1);
    c_pcsetr ("SY - SHADOW OFFSET IN Y",-.1);
    c_pcseti ("SC - SHADOW COLOR",2);
/*
 * Write another line.
 */
    c_plchhq (.5,.796,":F26:'SF', 'SC', 'SX', and 'SY' create shadows.",.028,0.,0.);
/*
 * Turn on character outlines and define the color of the outline.
 */
    c_pcseti ("OF - OUTLINE FLAG",1);
    c_pcseti ("OC - OUTLINE COLOR",3);
    c_pcseti ("OL - OUTLINE LINE WIDTH",1);
/*
 * Write another line.
 */
    c_plchhq (.5,.738,":F26:'OF', 'OC', and 'OL' add outlines.",.030,0.,0.);
/*
 * Turn on the drawing of boxes and define characteristics of them.
 */
    c_pcseti ("BF - BOX FLAG",7);
    c_pcseti ("BL - BOX LINE WIDTH",2);
    c_pcsetr ("BM - BOX MARGIN",.15);
    c_pcsetr ("BX - BOX SHADOW X OFFSET",-.1);
    c_pcsetr ("BY - BOX SHADOW Y OFFSET",-.1);
    c_pcseti ("BC(1) - BOX COLOR - BOX OUTLINE    ",5);
    c_pcseti ("BC(2) - BOX COLOR - BOX FILL       ",6);
    c_pcseti ("BC(3) - BOX COLOR - BOX SHADOW FILL",2);
/*
 * Write another line.
 */
    c_plchhq (.5,.672,":F26:'BF', 'BC', 'BL', 'BM', 'BX', and 'BY' add a box.",.026,0.,0.);
/*
 * Get rid of the box shadow, which doesn't add much.
 */
    c_pcseti ("BF - BOX FLAG",3);
/*
 * Write another line.
 */
    c_pcsetc ("FC - FUNCTION-CODE CHARACTER","/");
    c_plchhq (.5,.592,"/F26/'MA' and 'OR' are used for mapping:",.030,0.,0.);
    c_pcsetc ("FC - FUNCTION CODE CHARACTER",":");
/*
 * Write a couple of headers for the plots that follow.
 */
    c_plchhq (.28,.528,":F25:(EZMAP)",.024,0.,0.);
    c_plchhq (.72,.528,":F33:(r:F25: and :F33:q)",.024,0.,0.);
/*
 * Initialize EZMAP and draw a background.
 */
    c_mapstc ("OU","CO");
    c_mapsti ("GR",5);
    c_mappos (.065,.495,.065,.495);
    c_mapstr ("SA",8.5);
    c_maproj ("SV",0.,-25.,0.);
    c_mapint();
    c_maplot();
    c_mapgrd();
/*
 * Tell PLOTCHAR to map characters through EZMAP.
 */
    c_pcseti ("MA - MAPPING FLAG",1);
    c_pcsetr ("OR - OUT-OF-RANGE FLAG",1.E12);
/*
 * Write a line across the surface of the globe.
 */
    c_plchhq (-25.,0.,":F25Y200:NCAR GRAPHICS",8.,30.,0.);
/*
 * Do an appropriate SET call for a rho-theta mapping.
 */
    c_set(.505,.935,.065,.495,-27.5,27.5,-27.5,27.5,1);
/*
 * Tell PLOTCHAR to use a rho-theta mapping.
 */
    c_pcseti ("MA - MAPPING FLAG",2);
    c_pcsetr ("OR - OUT-OF-RANGE FLAG",0.);
/*
 * Write three lines in rho/theta space, orienting them so they come out
 * in a circle after mapping.
 */
    c_plchhq (20., 90.,":F25Y125:NCAR GRAPHICS",8.,-90.,0.);
    c_plchhq (20.,210.,":F25Y125:NCAR GRAPHICS",8.,-90.,0.);
    c_plchhq (20.,-30.,":F25Y125:NCAR GRAPHICS",8.,-90.,0.);
/*
 * Turn off mapping and recall SET to allow fractional coordinates again.
 */
    c_pcseti ("MA - MAPPING FLAG",0);

    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Change the drawing order to allow for "stacking" characters from
 * right to left.
 */
    c_pcseti ("DO - DRAWING ORDER",-2);
/*
 * Reduce the space between characters so the "stacking" is visible.
 */
    c_pcsetr ("SS - SUBTRACT-SPACE FLAG",.3);
/*
 * Turn off the box.  Make the shadows black and position them so they
 * help make the stacked characters readable.
 */
    c_pcseti ("BF - BOX FLAG",0);
    c_pcseti ("SC - SHADOW COLOR",0);
    c_pcsetr ("SX - SHADOW OFFSET IN X",.1);
    c_pcsetr ("SY - SHADOW OFFSET IN Y",0.);
/*
 * Write a final line demonstrating "stacking".
 */
    c_plchhq (.5,.030,":F26:Use    'DO'    and    'SS'    to   'stack'    characters    in    either    direction.",.026,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * --- E N D   O F   E X A M P L E S -----------------------------------
 *
 *
 * Close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws (WKID);
    gclose_gks();
}

void drawbx(xcen,ycen,angd,xtra)
float xcen,ycen,angd,xtra;
{
    float dstl, dstr, dstb, dstt;
    float angr, sina, cosa, xfra, yfra, xalb, yalb, xarb, yarb;
    float xart, yart, xalt, yalt;
    c_pcgetr ("DL - DISTANCE LEFT  ",&dstl);
    c_pcgetr ("DR - DISTANCE RIGHT ",&dstr);
    c_pcgetr ("DB - DISTANCE BOTTOM",&dstb);
    c_pcgetr ("DT - DISTANCE TOP   ",&dstt);
    angr=.017453292519943*angd;
    sina=sin(angr);
    cosa=cos(angr);
    xfra=c_cufx(xcen);
    yfra=c_cufy(ycen);
    xalb=xfra-(dstl+xtra)*cosa+(dstb+xtra)*sina;
    yalb=yfra-(dstl+xtra)*sina-(dstb+xtra)*cosa;
    xarb=xfra+(dstr+xtra)*cosa+(dstb+xtra)*sina;
    yarb=yfra+(dstr+xtra)*sina-(dstb+xtra)*cosa;
    xart=xfra+(dstr+xtra)*cosa-(dstt+xtra)*sina;
    yart=yfra+(dstr+xtra)*sina+(dstt+xtra)*cosa;
    xalt=xfra-(dstl+xtra)*cosa-(dstt+xtra)*sina;
    yalt=yfra-(dstl+xtra)*sina+(dstt+xtra)*cosa;
    c_plotif (xalb,yalb,0);
    c_plotif (xarb,yarb,1);
    c_plotif (xart,yart,1);
    c_plotif (xalt,yalt,1);
    c_plotif (xalb,yalb,1);
    c_plotif (0.,0.,2);
    return;
}
