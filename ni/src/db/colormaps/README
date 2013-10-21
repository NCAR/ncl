======================================================================
This directory contains a few different styles of color maps that
end in ".rgb", ".gp", or ".ncmap".
======================================================================
Adding a new colormap to NCL.

 1. Decide what you are going to call this color table and create a
    file called "xxxx.rgb", where "xxxx" is the color table name. This
    will be the name used in the call to "gsn_define_colormap" or
    as the value for the "wkColorMap" resource. (The ".rgb" extension
    needs to be part of the name, but you drop this when referencing 
    the colormap in an NCL script).

    Add "xxxx.rgb" to the yMakefile in this directory.

    Type:

        svn add xxxx.rgb
        svn ci

    to check in the new color table.

 2. How to create a color table from scratch:

    At the the beginning of this file, *optionally* add the two lines:

ncolors=n
# r g b

    where n is the number of RGB triplets you have in the file (do
    not count the background/foreground colors).

    Immediately following these two lines, put the RGB triplets in this
    file with one RGB triplet per line.  Do not include the
    background/foreground colors!

    For example:

160 32 240
0   0  180

   Here's a sample color table with 8 colors (the "# r g b" is ignored):

ncolors=8
# r   g   b
160 32  240
0   0   180
60  100 230
120 155 242
176 224 230
46  139 87
100 225 0
210 255 47

   Once you add the above color table to NCL, it will actually have 10
   colors, as the foreground and background colors will be added.

 3. To quickly test this new color table, you can copy it
    to $NCARG_ROOT/lib/ncarg/colormaps, and then run the
    short script, replacing "xxxx" with the name you used above:

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
    
begin
  wks = gsn_open_wks("x11","test")
  gsn_define_colormap(wks,"xxxx")
  gsn_draw_colormap(wks)
end

     If you don't have permission to copy the file to
     $NCARG_ROOT/lib/ncarg/colormaps, then put it wherever
     you like, and setenv NCARG_COLORMAP_PATH to this directory.
     Then, run the above script.

 4. The colormaps directory at:

     http://www.ncl.ucar.edu/Document/Graphics/color_table_gallery.shtml

    will need to be updated. See the README file in
    webdoc/ncl/Document/Graphics for more information.

