/*
 *	$Id: c_epltch.c,v 1.1 1992-10-26 19:51:00 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/Cbind.h>

main()
{
/*
 * Define arrays for column labels and row labels for the plots showing
 * the complex and duplex character sets.
 */
    char clbl[12][10], rlbl[48][6], ctmp[7], chrs[9], stmp[10];
    int i, j, k, l, icmp, c_ifnt, ichr;
    float csmu, xpos, ypos, xcen, ycen, xrgt, ybot;
    float wdth, xlft, ytop, xcrd, ycrd;
/*
 * Define the column and row labels.  The character string ':c:r', where
 * "c" is the first three characters of a column label and "r" is the
 * first character of a row label, is used to select the character to
 * be written in that column of that row.
 */
    strcpy( clbl[0],  "PRU(0000)" );
    strcpy( clbl[1],  "PRL(0100)" );
    strcpy( clbl[2],  "IRU(0200)" );
    strcpy( clbl[3],  "IRL(0300)" );
    strcpy( clbl[4],  "KRU(0400)" );
    strcpy( clbl[5],  "KRL(0500)" );
    strcpy( clbl[6],  "PGU(0600)" );
    strcpy( clbl[7],  "PGL(0700)" );
    strcpy( clbl[8],  "IGU(1000)" );
    strcpy( clbl[9],  "IGL(1100)" );
    strcpy( clbl[10], "KGU(1200)" );
    strcpy( clbl[11], "KGL(1300)" );

    strcpy( rlbl[0],  "A(01)" );
    strcpy( rlbl[1],  "B(02)" );
    strcpy( rlbl[2],  "C(03)" );
    strcpy( rlbl[3],  "D(04)" );
    strcpy( rlbl[4],  "E(05)" );
    strcpy( rlbl[5],  "F(06)" );
    strcpy( rlbl[6],  "G(07)" );
    strcpy( rlbl[7],  "H(10)" );
    strcpy( rlbl[8],  "I(11)" );
    strcpy( rlbl[9],  "J(12)" );
    strcpy( rlbl[10], "K(13)" );
    strcpy( rlbl[11], "L(14)" );
    strcpy( rlbl[12], "M(15)" );
    strcpy( rlbl[13], "N(16)" );
    strcpy( rlbl[14], "O(17)" );
    strcpy( rlbl[15], "P(20)" );
    strcpy( rlbl[16], "Q(21)" );
    strcpy( rlbl[17], "R(22)" );
    strcpy( rlbl[18], "S(23)" );
    strcpy( rlbl[19], "T(24)" );
    strcpy( rlbl[20], "U(25)" );
    strcpy( rlbl[21], "V(26)" );
    strcpy( rlbl[22], "W(27)" );
    strcpy( rlbl[23], "X(30)" );
    strcpy( rlbl[24], "Y(31)" );
    strcpy( rlbl[25], "Z(32)" );
    strcpy( rlbl[26], "0(33)" );
    strcpy( rlbl[27], "1(34)" );
    strcpy( rlbl[28], "2(35)" );
    strcpy( rlbl[29], "3(36)" );
    strcpy( rlbl[30], "4(37)" );
    strcpy( rlbl[31], "5(40)" );
    strcpy( rlbl[32], "6(41)" );
    strcpy( rlbl[33], "7(42)" );
    strcpy( rlbl[34], "8(43)" );
    strcpy( rlbl[35], "9(44)" );
    strcpy( rlbl[36], "+(45)" );
    strcpy( rlbl[37], "-(46)" );
    strcpy( rlbl[38], "*(47)" );
    strcpy( rlbl[39], "/(50)" );
    strcpy( rlbl[40], "((51)" );
    strcpy( rlbl[41], ")(52)" );
    strcpy( rlbl[42], "$(53)" );
    strcpy( rlbl[43], "=(54)" );
    strcpy( rlbl[44], " (55)" );
    strcpy( rlbl[45], ",(56)" );
    strcpy( rlbl[46], ".(57)" );
    strcpy( rlbl[47], "     " );

/*
 * Define a flag which says, if 0, that the first eight plots are to
 * occupy eight separate frames and, if 1, that those plots are to be
 * compressed onto two frames.
 */
    icmp = 1;
/*
 * Open GKS.
 */
    c_opngks();
/*
 * Do a call to SET which allows us to use fractional coordinates.
 */
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Produce examples of the complex and duplex character sets.
 */

/*
 * Compute a character-size multiplier, depending on whether the first
 * eight plots are being put on eight frames or two.
 */
    csmu=(float)(2-icmp);
/*
 * Each pass through the loop on I produces four plots - the first four
 * for the complex set, and the second four for the duplex set.
 */
    for( i=1; i <= 2; i++ ) {
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
 */

/*
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
 */

/*
 * Do a single frame showing the medium-quality characters with various
 * aspect ratios.
 */

/*
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
        c_plchmq (.5,ypos-.04,"!\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~",.02,0.,0.);
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
    c_gslwsc (2.);
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
    c_gslwsc (1.);
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
    c_gslwsc (2.);
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
    c_plchhq (.5,.844,"Set ''FN'' (Font Number) to 4 and write a line using 'F' function codes:",.012,0.,0.);
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
    c_plchhq (.5,.564,"/F14/Line written with ''AS'' = .125 and ''SS'' = 0.",.012,0.,0.);
    c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",  0.);
    c_plchhq (.5,.540, "/F14/Line written with ''AS'' = 0. and ''SS'' = 0.",.012,0.,0.);
    c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",.125);
    c_plchhq (.5,.516, "/F14/Line written with ''AS'' = 0. and ''SS'' = .125",.012,0.,0.);;
    c_pcsetr ("AS - ADD SPACE BETWEEN CHARACTERS     ",  0.);
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",  0.);
/*
 * Illustrate the difference between inexact centering and exact
 * centering of a single character.
 */
    c_pcsetr ("SS - SUBTRACT SPACE BETWEEN CHARACTERS",.128);
    
    c_plchhq (.1,.455,"/F7/This 'g' is centered on the cross using CNTR = 0. and ''CE'' = 0:",.012,0.,-1.);
    c_line (.880,.455,.920,.455);
    c_line (.900,.435,.900,.475);
    c_pcseti ("CE - CENTERING OPTION",0);
    c_plchhq (.9,.455,"/F7/g",.025,0.,0.);
    c_pcseti ("CE - CENTERING OPTION",0);
    
    c_plchhq (.1,.405,"/F7/This 'g' is centered on the cross using CNTR = 0. and ''CE'' = 1:",.012,0.,-1.);
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
    c_gslwsc (1.);
    
    for( i = -48; i <= 48; i++ ) {
        xcrd=.500+(float)(i)*(wdth/16.);
        c_line (xcrd,ybot,xcrd,ytop);
    }
    
    for( j = -11; j <= 14; j++ ) {
        ycrd=.150+(float)(j)*(wdth/16.);
        c_line (xlft,ycrd,xrgt,ycrd);
    }
    
    c_plotif (0.,0.,2);
    c_gslwsc (2.);

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
    c_gslwsc (1.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
    c_clsgks();
/*
 * Done.
 */
}

drawbx(xcen,ycen,angd,xtra)
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
    return(1);
}
