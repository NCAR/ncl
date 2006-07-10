"""This script helps you wrap a Fortran subroutine so you
can call it from NCL."""

#
# This script currently doesn't handle missing values. Also, it
# hasn't been heavily tested for cases where there are no
# leftmost dimensions.
#
# It also doesn't yet generate code for the wrapper.c file.
#
# Initialize some stuff.
#
args                = []
farg_names          = []
farg_types          = []
global_dsizes_names = []
global_var_names    = []
index_names         = []
debug               = True
have_leftmost       = False

ntypes = ['numeric','double','float','long','integer','string','logical']
ctypes = [   'void','double','float','long',   'int','string','logical']

#
# Set up class that will hold information on NCL input arguments.
#
class Argument:
  def __init__(self,name,itype,ndims,dsizes=None,min_ndims=0, 
               dsizes_names=None,dsizes_names_str=""):
    self.name      = name
    self.ntype     = ntypes[itype]
    self.ctype     = ctypes[itype]
    self.ndims     = ndims
    self.min_ndims = min_ndims
    if dsizes != None:
      self.dsizes = dsizes
    if dsizes_names != None:
      self.dsizes_names     = dsizes_names
      self.dsizes_names_str = dsizes_names_str
#
# Set up instructions on how to print an instance of this class.
#
  def __str__(self):
    str1 = "Name is '" + self.name + "'\n"
    str1 = str1 + "  NCL type is " + self.ntype + "\n"
    str1 = str1 + "    C type is " + self.ctype + "\n"
    if self.ndims > 0:
      if self.ndims == 1 and self.dsizes[0] == 1:
        str1 = str1 + "  This variable is a scalar\n"
      else:
        str1 = str1 + "  Number of dimensions is " + str(self.ndims) + "\n"
        for i in range(self.ndims):
          if self.dsizes[i] > 1:
            str1 = str1 + "  Dimension # " + str(i) + " is length " +\
                          str(self.dsizes[i]) + "\n"
          else:
            str1 = str1 + "  Dimension # " + str(i) + " is variable\n"
    else:
      str1 = str1 + "  Number of dimensions is variable\n"

    if self.min_ndims > 0:
      str1 = str1 + "  Number of minimum dimensions is " + str(self.min_ndims) + "\n"
      for j in range(self.min_ndims):
        str1 = str1 + "  Dimension ndims_" + self.name + "-" + str(j+1) + \
                      " is " + self.dsizes_names[j] + "\n"
    return str1

#
# Import some stuff.
#
import sys
from string import *

#
# Begin asking for input on function/procedure.
#
print "\nIn order to wrap your Fortran subroutine, I need to ask you"
print "a few questions.\n"

#---------------------------------------------------------------------
# Is this a function or procedure?
#---------------------------------------------------------------------

forp = raw_input("Is this going to be a function (f) or procedure (p)? [f] ")
if (lower(forp) == "p"):
  isfunc       = False
  wrapper_type = "procedure"
else:
  isfunc       = True
  wrapper_type = "function"

#---------------------------------------------------------------------
# Get Fortran, NCL, and wrapper names of function or procedure.
#---------------------------------------------------------------------

valid = False
while not valid:
  ncl_name     = raw_input("\nEnter NCL name of your " + wrapper_type + ": ")
  if ncl_name == "":
    print "Invalid name for NCL function or procedure, reenter"
  else:
    valid = True

valid = False
while not valid:
  fortran_name = raw_input("\nEnter Fortran name of your " + wrapper_type + \
                           ": ")
  if fortran_name == "":
    print "Invalid name for Fortran subroutine, reenter"
  else:
    valid = True

valid = False
while not valid:
  wrapper_name = raw_input("\nEnter name of wrapper C file (without '.c') " +\
                           ": ")
  if wrapper_name == "":
    print "Invalid name for wrapper name, reenter"
  else:
    valid = True

fatal_str        = '    NhlPError(NhlFATAL,NhlEUNKNOWN,"' + ncl_name + ': '
warn_str         = '    NhlPError(NhlWARNING,NhlEUNKNOWN,"' + ncl_name + ': '
return_fatal_str = '    return(NhlFATAL);\n'

#---------------------------------------------------------------------
# How many input arguments are there?
#---------------------------------------------------------------------

valid = False
while not valid:
  num_args = int(raw_input('\nHow many input arguments are there for ' + \
                            ncl_name + '? '))
  if num_args < 0:
    print "Invalid number of arguments: ",num_args
    print "Reenter."
  elif num_args == 0:
    print "This script is for routines that contain arguments Reenter"
  else:
    valid = True

print "\nI need to ask you some questions about each input argument."

#
# Loop across the number of input arguments and get information
# about each one.
#

for i in range(num_args):

#---------------------------------------------------------------------
# Get name of argument.
#---------------------------------------------------------------------

  name = raw_input("\nWhat is the name of argument # " + str(i) + "? ")

#---------------------------------------------------------------------
# Get type of argument.
#---------------------------------------------------------------------

  print "What type is '" + name + "'? [0]"
  for j in range(len(ntypes)):
    print "   ",j,":",ntypes[j]
   
  valid = False
  while not valid:
    rinput = raw_input()
    if rinput == "":
      itype = 0
      valid = True
    else:
      itype = int(rinput)
      if (itype < 0) or (itype >= len(ntypes)):
        print "Invalid type, reenter"
      else:
        valid = True
      
  if itype == 0:
    global_var_names.append("tmp_" + name)
  else:
    global_var_names.append(name)

#---------------------------------------------------------------------
# Get dimension sizes.
#---------------------------------------------------------------------

  print "How many dimensions does '" + name + "' have?"

  valid = False
  while not valid:
    rinput = raw_input("Enter 0 for variable dimensions: [0] ")
    if rinput == "":
      ndims = 0
      valid = True
    else:
      ndims = int(rinput)
      if ndims >= 0:
        valid = True
      else:
        print "Invalid number of dimensions, reenter"

  dsizes       = None
  dsizes_names = None
  min_ndims    = 0

  if ndims > 0:
    print "Enter dimension size of each dimension"
    print "(Hit <return> for variable dimension size)"
    dsizes = []
    for j in range(ndims):
      valid = False
      while not valid:
        rinput = raw_input("Size for dimension # " + str(j) + \
                           " of argument '" + name + "': ")
        if rinput == "":
          valid = True
          dsizes.append(0)
        else:
          if int(rinput) >= 0:
            valid = True
            dsizes.append(int(rinput))
          else:
            print "Invalid size for dimension, reenter"
  else:
    have_leftmost = True
    index_names.append("index_"+name)
#---------------------------------------------------------------------
# Get the minimum dimension size required.
#---------------------------------------------------------------------

    valid = False
    while not valid:
      min_ndims = int(raw_input("How many dimensions does the Fortran routine expect for this variable? "))
      if min_ndims > 0:
        valid = True
      else:
        print "Invalid number of dimensions, reenter"

#---------------------------------------------------------------------
# Get the names of each minimum dimension size.
#---------------------------------------------------------------------

    print "What are the names of each of these dimensions?"      
    if global_dsizes_names != []:
      print "You can use these existing names if they apply: " + \
             str(global_dsizes_names)
    dsizes_names     = []
    dsizes_names_str = ""
    for j in range(min_ndims):
      dsizes_names.insert(0,raw_input("Name of dimension ndims_" + name + \
                                        "-" + str(j+1) + " : "))
      if not dsizes_names[0] in global_dsizes_names: 
        global_dsizes_names.insert(0,dsizes_names[0])
      if not dsizes_names[0] in global_var_names: 
        global_var_names.append(dsizes_names[0])
#
# Create string for variable that will hold the size of these 
# minimum dimensions.
#
      dsizes_names_str = dsizes_names_str + dsizes_names[0]

    if not dsizes_names_str in global_dsizes_names: 
      global_dsizes_names.insert(0,dsizes_names_str)
    if not dsizes_names_str in global_var_names: 
      global_var_names.append(dsizes_names_str)
#---------------------------------------------------------------------
# With all this information, create an instance of the Argument class.
#---------------------------------------------------------------------

  if min_ndims > 0:
    args.append(Argument(name,itype,ndims,dsizes,min_ndims,dsizes_names,\
                         dsizes_names_str))

  else:
    args.append(Argument(name,itype,ndims,dsizes))

#---------------------------------------------------------------------
# Get information on the return value, if a function.
#---------------------------------------------------------------------
if isfunc:
  ret_name = raw_input("\nWhat is the name of the return value? ")

#---------------------------------------------------------------------
# Get type of argument.
#---------------------------------------------------------------------

  print "What type is '" + ret_name + "'? [0] "
  for j in range(len(ntypes)):
    print "   ",j,":",ntypes[j]
   
  valid = False
  while not valid:
    rinput = raw_input()
    if rinput == "":
      ret_itype = 0
      valid = True
    else:
      ret_itype = int(rinput)
      if (ret_itype < 0) or (ret_itype >= len(ntypes)):
        print "Invalid type, reenter"
      else:
        valid = True

  if ret_itype == 0:
    global_var_names.append("tmp_" + ret_name)
  else:
    global_var_names.append(ret_name)

#---------------------------------------------------------------------
# Get dimension sizes.
#---------------------------------------------------------------------

  print "How many dimensions does " + ret_name + " have?"

  valid = False
  while not valid:
    rinput = raw_input("Hit <return> for variable dimensions: ")
    if rinput == "":
      ret_ndims = 0
      valid = True
    else:
      ret_ndims = int(rinput)
      if ret_ndims < 0:
        print "Invalid number of dimensions, reenter"
      else:
        valid = True

  ret_dsizes       = None
  ret_dsizes_names = None
  ret_min_ndims    = 0
  if ret_ndims > 0:
    print "Enter dimension size of each dimension"
    print "(Hit <return> for variable dimension size)"
    ret_dsizes = []
    for j in range(ret_ndims):
      valid = False
      while not valid:
        rinput = raw_input("Size for dimension # " + str(j) + " of return value: ")
        if rinput == "":
          ret_dsizes.append(0)
          valid = True
        else:   
          if int(rinput) < 0:
            print "Invalid size for dimension, reenter"
          else:
            ret_dsizes.append(int(rinput))
            valid = True          
  else:
    have_leftmost = True
    index_names.append("index_"+ret_name)
#---------------------------------------------------------------------
# Get the minimum dimension size required.
#---------------------------------------------------------------------

    valid = False
    while not valid:
      ret_min_ndims = int(raw_input("How many dimensions does the Fortran routine expect for the return value? "))
      if ret_min_ndims <= 0:
        print "Invalid number of dimensions, reenter"
      else:
        valid = True

#---------------------------------------------------------------------
# Get the names of each minimum dimension size.
#---------------------------------------------------------------------

    print "What are the names of each of these dimensions?"      
    if global_dsizes_names != []:
      print "You can use these existing names if they apply: " + str(global_dsizes_names)
    ret_dsizes_names     = []
    ret_dsizes_names_str = ""
    for j in range(ret_min_ndims):
      ret_dsizes_names.insert(0,raw_input("Name of dimension ndims_" + \
               ret_name + "-" + str(j+1) + " : "))
      if not ret_dsizes_names[0] in global_dsizes_names: 
        global_dsizes_names.insert(0,ret_dsizes_names[0])
      if not ret_dsizes_names[0] in global_var_names: 
        global_var_names.append(ret_dsizes_names[0])
#
# Create string for variable that will hold the size of these 
# minimum dimensions.
#
      ret_dsizes_names_str = ret_dsizes_names_str + ret_dsizes_names[0]

    if not ret_dsizes_names_str in global_dsizes_names: 
      global_dsizes_names.insert(0,ret_dsizes_names_str)
    if not ret_dsizes_names_str in global_var_names: 
      global_var_names.append(ret_dsizes_names_str)

#---------------------------------------------------------------------
# With this information, create an instance of the Argument class for
# the return value.
#---------------------------------------------------------------------
    
  if ret_min_ndims > 0:  
    ret_arg = Argument(ret_name,ret_itype,ret_ndims,ret_dsizes,
                       ret_min_ndims,ret_dsizes_names,ret_dsizes_names_str)
  else:
    ret_arg = Argument(ret_name,ret_itype,ret_ndims,ret_dsizes)

#
# Get information about how Fortran function is to be called and what
# types its input variables are.
#

valid = False
while not valid:
  fnum_args = int(raw_input('\nHow many input arguments are there for the Fortran routine ' + fortran_name + '? '))
  if fnum_args <= 0:
    print "Invalid number of dimensions for Fortran routine."
    print "There should be at least one dimension, reenter"
  else:
    valid = True

for i in range(fnum_args):
  print "What is the name of argument # " + str(i) + "? (Be sure to include '&' if appropriate.) "
  print "You can use these existing names if they apply: " + \
        str(global_var_names)
  farg_names.append(raw_input())

  valid = False
  while not valid:
    print "What type is '" + farg_names[i] + "'? [0]"
    for j in range(len(ctypes)):
      print "   ",j,":",ctypes[j]
   
    rinput = raw_input()
    if rinput == "":
      farg_types.append(0)
      valid = True
    else:
      if (int(rinput) < 0) or (int(rinput) >= len(ctypes)):
        print "Invalid type, reenter"
      else:
        farg_types.append(int(rinput))
        valid = True

#
# Print information about each argument for debugging purposes.
#
if debug:
  for i in range(len(args)):
    print args[i]

  print ret_arg

#---------------------------------------------------------------------
#
# Open wrapper file and start writing to it, but first
# make sure this is acceptable.
#
#---------------------------------------------------------------------

print 'I will be creating the files ' + wrapper_name + '.c and ' + \
      'wrapper_' + ncl_name + '.c.'
print 'The contents of wrapper_' + ncl_name + '.c should be copied over to wrapper.c.'

okay = raw_input("Is this okay? (y/n) [y] ")
if (lower(okay) == "n"):
  print "Bye!"
  sys.exit()

#
# Open the files. The first one (w1file) is for actually wrapping the
# Fortran code, and the second one (w2file) is for registering the
# function or procedure. The second file is for temporary use only: you
# should copy its contents to "wrapper.c" where all the built-in 
# functions and procedures are registered.
#
w1file = open(wrapper_name+'.c','w')
w2file = open('wrapper_' + wrapper_name+'.c','w')

#
# Start writing information to the main wrapper code.
#
w1file.write('#include <stdio.h>\n')
w1file.write('#include "wrapper.h"\n\n')

w1file.write("extern void NGCALLF("+ lower(fortran_name) + "," +
             upper(fortran_name) + ")(")
for i in range(len(farg_types)):
  if i == 0:
    w1file.write(ctypes[farg_types[i]] + " *")
  else:
    w1file.write(", " + ctypes[farg_types[i]] + " *")
w1file.write(");\n\n")

w1file.write("NhlErrorTypes " + ncl_name + "_W( void )\n{\n")

#
# Write the argument information.
#
w1file.write("""
/*
 * Input variables
 */
""")

#
# First write the variable itself along with the type.
# Variables that are "void" will be converted to double
# precision, so they will have a "tmp_xxx" variable
# associated with it.
#
for i in range(len(args)):
  w1file.write("/*\n")
  w1file.write(" * Argument # " + str(i) + "\n")
  w1file.write(" */\n")

  w1file.write("  " + args[i].ctype + " *" + args[i].name + ";\n")
  if args[i].ntype == "numeric":
    w1file.write("  double *tmp_" + args[i].name + ";\n")
#
# Write out dimension information.
#
  if args[i].ndims == 0:
    w1file.write("  int ndims_" + args[i].name + ", dsizes_" + args[i].name + \
                "[NCL_MAX_DIMENSIONS];\n")
  else:
#
# We only need to include the dimension sizes if one of the sizes
# is unknown (represented by being set to '0').
#
    if 0 in args[i].dsizes:
      w1file.write("  int dsizes_" + args[i].name + "[" + str(args[i].ndims) + "];\n")

#
# Include a type variable for each variable that is numeric.
#
  if args[i].ntype == "numeric":
    w1file.write("  NclBasicDataTypes type_" + args[i].name + ";\n\n")

if isfunc:
#---------------------------------------------------------------------
#
# Write out return variable information
#
#---------------------------------------------------------------------
  w1file.write("/*\n")
  w1file.write(" * Return variable\n")
  w1file.write(" */\n")

  w1file.write("  " + ret_arg.ctype + " *" + ret_arg.name + ";\n")
  if ret_arg.ntype == "numeric":
    w1file.write("  double *tmp_" + ret_arg.name + ";\n")
#
# Write out dimension information.
#
w1file.write("  int ndims_" + ret_arg.name + ", *dsizes_" + ret_arg.name + \
            ";\n")

#
# Include a type variable.
#
w1file.write("  NclBasicDataTypes type_" + ret_arg.name + ";\n\n")

#---------------------------------------------------------------------
# Declare other variables
#---------------------------------------------------------------------
w1file.write("""
/*
 * Various
 */
""")
if global_dsizes_names != []:
  w1file.write("  int ")
#
# Write the various dimension size variables we've been collecting.
#
  for i in range(len(global_dsizes_names)):
    if i == (len(global_dsizes_names)-1):
      w1file.write(global_dsizes_names[i] + ";\n")
    else:
      w1file.write(global_dsizes_names[i] + ", ")
#
# For any variable that has leftmost dimensions, we need a corresponding
# "index_xxx" variable.
#
  w1file.write("  int ")
  for i in range(len(index_names)):
    if i == (len(index_names)-1):
      w1file.write(index_names[i] + ";\n")
    else:
      w1file.write(index_names[i] + ", ")

if have_leftmost:
  w1file.write("  int i, ndims_leftmost, size_leftmost, size_output;\n")
else:
  w1file.write("  int i, size_output;\n")
  
w1file.write("""
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
""")

#
# Loop across input arguments and generate code that will
# retrieve them from the NCL script.
#
global_dsizes_names_accum = []

for i in range(len(args)):
  w1file.write("/*\n")
  w1file.write(" * Get argument # " + str(i) + "\n")
  w1file.write(" */\n")
  w1file.write("  " + args[i].name + " = (" + args[i].ctype + \
              "*)NclGetArgValue(\n")

  w1file.write("           " + str(i) + ",\n")
  w1file.write("           " + str(len(args)) + ",\n")
  if args[i].ndims == 0:
    w1file.write("           &ndims_" + args[i].name + ",\n")
  else:
    w1file.write("           NULL,\n")

  if (args[i].ndims == 0) or (0 in args[i].dsizes):
    w1file.write("           dsizes_" + args[i].name + ",\n")
  else:
    w1file.write("           NULL,\n")
#
# These are for the missing values, which we are not handling yet.
#
  w1file.write("           NULL,\n")
  w1file.write("           NULL,\n")

  if args[i].ntype == "numeric":
    w1file.write("           &type_" + args[i].name + ",\n")

  w1file.write("           2);\n")

#
# Code for doing some minimal error checking.
#
  if args[i].min_ndims > 0:
    w1file.write("""
/*
 * Check dimension sizes.
 */
""")
    w1file.write("  if(ndims_" + args[i].name + " < " + \
                str(args[i].min_ndims) + ") {\n")
    w1file.write(fatal_str + 'The ' + args[i].name + \
                ' array must have at least ' + str(args[i].min_ndims) + \
                ' dimensions");\n')
    w1file.write(return_fatal_str)
    w1file.write('  }\n')

    if args[i].min_ndims > 1: 
      dstr = "  " + args[i].dsizes_names_str + " = " + args[i].dsizes_names[0]
    for j in range(len(args[i].dsizes_names)):
#
# Create string that will hold total dimension sizes of rightmost
# dimensions.
#
      if j > 0 and args[i].min_ndims > 1: 
        dstr = dstr + " * " + args[i].dsizes_names[j]

      if i == 0 or not args[i].dsizes_names[j] in global_dsizes_names_accum:
        w1file.write('  ' + args[i].dsizes_names[j] + ' = dsizes_' + \
                    args[i].name + '[ndims_' + args[i].name + '-' + 
                    str(j+1) + '];\n\n')
        global_dsizes_names_accum.append(args[i].dsizes_names[j])
      else:
        w1file.write('  if(dsizes_' + args[i].name + '[ndims_' + 
                    args[i].name + '-' + str(j+1) + '] != ' + \
                    args[i].dsizes_names[j] + ') {\n')
        w1file.write(fatal_str + 'The ndims-' + str(j+1) + ' argument of ' + \
                    args[i].name + ' must be of length ' + \
                    args[i].dsizes_names[j] + '");\n')
        w1file.write(return_fatal_str)
        w1file.write('  }\n')
    if args[i].min_ndims > 1: 
      w1file.write(dstr + ";\n\n")

#
# Code to calculate size of leftmost dimensions, if any.
#
first    = True
name_str = ""
for i in range(len(args)):
  if args[i].min_ndims > 0:
    if first:
      w1file.write("""
/*
 * Calculate size of leftmost dimensions.
 */
""")
      w1file.write("  size_leftmost  = 1;\n")
      w1file.write("  ndims_leftmost = 0;\n")
      w1file.write("  for(i = 0; i < ndims_" + args[i].name + "-" + \
                  str(args[i].min_ndims) + "; i++) {\n")

      first_arg_name = args[i].name       # Keep track of this argument
      prev_arg_name  = first_arg_name
      first          = False
      second         = True
    else:
      if second:
        w1file.write("    if(dsizes_" + args[i].name + "[i] != dsizes_" + \
                    first_arg_name + "[i]")
        name_str      = prev_arg_name
        prev_arg_name = args[i].name
        second        = False
      else:
        name_str = name_str + ", " + prev_arg_name
        w1file.write(" ||\n       dsizes_" + args[i].name + "[i] != dsizes_" + \
                    first_arg_name + "[i]")
        prev_arg_name = args[i].name
#
# Close up leftmost dimensions loop.
#
if name_str != "":
  w1file.write(") {\n")
  w1file.write('  ' + fatal_str + 'The leftmost dimensions of ' + \
              name_str + ' and ' + prev_arg_name + \
              ' must be the same");\n')
  w1file.write('  ' + return_fatal_str)
  w1file.write('    }\n')

if not first:
  w1file.write("    size_leftmost *= dsizes_" + first_arg_name + "[i];\n")
  w1file.write("    ndims_leftmost++;\n")
  w1file.write("  }\n\n")

#---------------------------------------------------------------------
# Code to allocate space for coercing input arrays, if any of them
# are numeric.  In addition, if the number of dimensions is unknown,
# then we need to allocate space for the temporary array, and later
# it will be coerced.
#---------------------------------------------------------------------

first = True
for i in range(len(args)):
  name = args[i].name
  if args[i].ntype == "numeric":
    if first:
      first = False
      w1file.write("""
/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
""")
      if isfunc and ret_arg.ntype == "numeric":
#---------------------------------------------------------------------
# While we're here, we can also set the output array type, based
# on whether any of the input is double or not.
#---------------------------------------------------------------------

        w1file.write(""" *
 * The output type defaults to float, unless any of the input arrays
 * are double.
 */
""")
        w1file.write("  type_" + ret_arg.name + " = NCL_float;\n")
      else:
        w1file.write(" */\n")

#---------------------------------------------------------------------
# If input is not already double, then we'll need to allocate a
# temporary array to coerce it to double.
#---------------------------------------------------------------------
    w1file.write("/*\n")
    w1file.write(" * Allocate space for tmp_" + args[i].name + ".\n")
    w1file.write(" */\n")

    w1file.write("  if(type_" + name + " != NCL_double) {\n")
    if args[i].min_ndims > 0:
      w1file.write("    tmp_" + name + " = (double *)calloc(" + \
                  args[i].dsizes_names_str + ",sizeof(double));\n")
    else:
      w1file.write("    tmp_" + name + " = coerce_input_double(" + \
                  name + ",type_" + name + \
                  ",...need input here,0,NULL,NULL);\n")

    w1file.write("    if(tmp_" + name + " == NULL) {\n")
    w1file.write('  ' + fatal_str + \
                'Unable to allocate memory for coercing input array to double");\n')
    w1file.write("  " + return_fatal_str)
    w1file.write("    }\n")
    w1file.write("  }\n")

    if isfunc and ret_arg.ntype == "numeric":
#---------------------------------------------------------------------
# If input is double, then output type should be set to double.
#---------------------------------------------------------------------
      w1file.write("  else {\n")
      w1file.write("    type_" + ret_arg.name + " = NCL_double;\n")
      w1file.write("  }\n")

#---------------------------------------------------------------------
# Code to handle allocating space for output array and its dimension
# sizes. Also, we may have to allocate a temporary array to hold space
# for a double array, if the return value is not double.
#---------------------------------------------------------------------

if isfunc:
#----------------------------------------------------------------------
# Code to calculate size of output array.
#----------------------------------------------------------------------
  w1file.write("""
/*
 * Calculate size of output array.
 */
""")

#
# Create string that will hold total dimension sizes of rightmost
# dimensions of return variable.
#
  if ret_arg.min_ndims > 0:
    if ret_arg.min_ndims > 1: 
      ret_dstr = "  " + ret_arg.dsizes_names_str + " = " + \
                 ret_arg.dsizes_names[0]
    for j in range(len(ret_arg.dsizes_names)):
      if j > 0 and ret_arg.min_ndims > 1: 
        ret_dstr = ret_dstr + " * " + ret_arg.dsizes_names[j]

    if ret_arg.min_ndims > 1: 
      w1file.write(ret_dstr + ";\n")

    w1file.write("  size_output = size_leftmost * " + \
                ret_arg.dsizes_names_str + ";\n")
  else:
    w1file.write("  size_output = ...need input here...;\n")

  w1file.write("""
/* 
 * Allocate space for output array.
 */
""")
  if ret_arg.ntype == "numeric":
    w1file.write("  if(type_" + ret_arg.name + " != NCL_double) {\n")
    w1file.write("    " + ret_arg.name + " = (void *)calloc(size_output, " + \
                  "sizeof(float));\n")
    if ret_arg.min_ndims > 0:
      w1file.write("    tmp_" + ret_arg.name + " = (double *)calloc(" + \
                  ret_arg.dsizes_names_str + ",sizeof(double));\n")
    else:
      w1file.write("    tmp_" + ret_arg.name + \
        " = (double *)calloc(...need input here...,sizeof(double));\n")

    w1file.write("    if(tmp_" + ret_arg.name + " == NULL) {\n")
    w1file.write('  ' + fatal_str + \
                'Unable to allocate memory for temporary output array");\n')
    w1file.write("  " + return_fatal_str)
    w1file.write("    }\n")

    w1file.write("  }\n")
    w1file.write("  else {\n")
    w1file.write("    " + ret_arg.name + " = (void *)calloc(size_output, " + \
                  "sizeof(double));\n")
    w1file.write("  }\n")
  else :
    w1file.write("    " + ret_arg.name + " = (" + ret_arg.ctype + \
                "*)calloc(size_output, sizeof(" + ret_arg.ctype + "));\n")

  w1file.write("  if(" + ret_arg.name + " == NULL) {\n")
  w1file.write(fatal_str + 'Unable to allocate memory for output array");\n')
  w1file.write(return_fatal_str)
  w1file.write("  }\n")

  w1file.write("""
/* 
 * Allocate space for output dimension sizes and set them.
 */
""")
  if ret_arg.ndims > 0:
    w1file.write("  ndims_" + ret_arg.name + " = " + str(ret_arg.ndims) + ";\n")
  else:
    w1file.write("  ndims_" + ret_arg.name + " = ndims_leftmost + " + \
                str(ret_arg.min_ndims) + ";\n")
  w1file.write("  dsizes_" + ret_arg.name + " = (int*)calloc(ndims_" + \
              ret_arg.name + ",sizeof(int));  \n")
  w1file.write("  if( dsizes_" + ret_arg.name + " == NULL ) {\n")
  w1file.write(fatal_str + \
               'Unable to allocate memory for holding dimension sizes");\n')
  w1file.write(return_fatal_str)
  w1file.write("  }\n")
  if ret_arg.min_ndims > 0:
#
# Loop through input arguments until we find one that has leftmost
# dimensions, and then use its leftmost dimensions to assign 
# dimensions to the output array's dimension sizes array.
#
    for i in range(len(args)):
      if args[i].min_ndims > 0:
        w1file.write("  for(i = 0; i < ndims_" + ret_arg.name + "-" + \
                    str(ret_arg.min_ndims) + "; i++) dsizes_" + \
                    ret_arg.name + "[i] = dsizes_" + args[i].name + "[i];\n")
        break
    else:
      w1file.write("  for(i = 0; i < ndims_" + ret_arg.name + \
                  "; i++) dsizes_" + ret_arg.name + \
                  "[i] = ...need input here;\n")
    for i in range(ret_arg.min_ndims):
      w1file.write("  dsizes_" + ret_arg.name + "[ndims_" + ret_arg.name + 
                  "-" + str(ret_min_ndims-i) + "] = " + \
                  ret_dsizes_names[i] + ";\n")
  else:
    w1file.write("  for(i = 0; i < ndims_" + ret_arg.name + "; i++) dsizes_" + \
                 ret_arg.name + "[i] = ...need input here;\n")

#
# Write out code for the loop across leftmost dimensions (if any).
#
if have_leftmost:
  w1file.write("""
/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * one-dimensional subsection.
 */
""")
  index_str = "  "
  for i in range(len(index_names)):
    if i < (len(index_names)-1):
      index_str = index_str + index_names[i] + " = "
    else:
      index_str = index_str + index_names[i] + " = 0;\n"

  w1file.write(index_str)
  w1file.write("  for(i = 0; i < size_leftmost; i++) {\n")
  for i in range(len(args)):
    name = args[i].name
    if args[i].ntype == "numeric" and args[i].ndims == 0:
      w1file.write("/*\n")
      w1file.write(" * Coerce subsection of " + name + " (tmp_" + name + \
                  ") to double if necessary.\n")
      w1file.write(" */\n")
      w1file.write("    if(type_" + name + " != NCL_double) {\n")
      w1file.write("      coerce_subset_input_double(" + name + ",tmp_" + \
                  name + ",index_" + name + ",type_" + name + "," + \
                  args[i].dsizes_names_str + ",0,NULL,NULL);\n")
      w1file.write("    }\n")
      w1file.write("    else {\n")
      w1file.write("      tmp_" + name + " = &((double*)" + name + \
                  ")[index_" + name + "];\n")
      w1file.write("    }\n\n")
#
# Write out code for pointing temporary output array to appropriate
# location in output array, if necessary.
#
  if ret_arg.ntype == "numeric" and ret_arg.ndims == 0:
    w1file.write("""
/*
 * Point temporary output array to void output array if appropriate.
 */
""")
    w1file.write("    if(type_" + ret_arg.name + " == NCL_double) tmp_" + \
                ret_arg.name + " = &((double*)" + ret_arg.name + \
                ")[index_" + ret_arg.name + "];\n\n")

#
# Write code for calling Fortran routine inside loop.
#
  w1file.write("""
/*
 * Call the Fortran routine.
 */
""")
  w1file.write("    NGCALLF("+ lower(fortran_name) + "," + \
              upper(fortran_name) + ")(")
  for i in range(len(farg_names)):
    if i == 0:
      w1file.write(farg_names[i])
    else:
      w1file.write(", " + farg_names[i])
  w1file.write(");\n")


#
# Code for copying values back to void array, if the output is
# supposed to be float.
#
  if isfunc and ret_arg.ntype == "numeric":
    w1file.write("""
/*
 * Coerce output back to float if necessary.
 */
""")
    w1file.write("    if(type_" + ret_arg.name + " == NCL_float) {\n")
    w1file.write("      coerce_output_float_only(" + ret_arg.name + \
                ",tmp_" + ret_arg.name + "," + ret_arg.dsizes_names_str + \
                ",index_" + ret_arg.name + ");\n")
    w1file.write("    }\n")

#
# Write out code for incrementing index variables, if any.
#
  for i in range(len(args)):
    if args[i].ntype == "numeric" and args[i].ndims == 0:
      w1file.write("    index_"  + args[i].name + " += " + \
                  args[i].dsizes_names_str + ";\n")
  if ret_arg.ntype == "numeric" and ret_arg.ndims == 0:
    w1file.write("    index_"  + ret_arg.name + " += " + \
                ret_arg.dsizes_names_str + ";\n")

  w1file.write("  }\n")    # End "for" loop
else:
#
# We are dealing with a function that doesn't have any arguments
# with leftmost dimensions. This is unusual, but possible.
#
# Write code for calling Fortran routine not in a loop.
#
  w1file.write("""
/*
 * Call the Fortran routine.
 */
""")
  w1file.write("  NGCALLF("+ lower(fortran_name) + "," + \
              upper(fortran_name) + ")(")
  for i in range(len(farg_names)):
    if i == 0:
      w1file.write(farg_names[i])
    else:
      w1file.write(", " + farg_names[i])
  w1file.write(");\n")

#
# Set up code for freeing unneeded memory.  Only those input variables
# that had to be coerced to double precision need to be freed.
#
first = True
for i in range(len(args)):
  if args[i].ntype == "numeric":
#
# Write code for calling Fortran routine not in a loop.
#
    if first:
      w1file.write("""
/*
 * Free unneeded memory.
 */
""")
      first = False

    w1file.write("  if(type_" + args[i].name + \
                " != NCL_double) NclFree(tmp_" + args[i].name + ");\n")

if ret_arg.ntype == "numeric":
  w1file.write("  if(type_" + ret_arg.name + \
              " != NCL_double) NclFree(tmp_" + ret_arg.name + ");\n")

#
# Set up code for returning information back to NCL script.
#
if isfunc:
  w1file.write("""
/*
 * Return value back to NCL script.
 */
""")
  w1file.write("  return(NclReturnValue(" + ret_arg.name + ",ndims_" + \
              ret_arg.name + ",dsizes_" + ret_arg.name + ",NULL,type_" + \
              ret_arg.name + ",0));\n")
else:
  w1file.write("""
/*
 * This is a procedure, so no values are returned.
 */
""")
  w1file.write("  return(NhlNOERROR);\n")

w1file.write("}\n")   # End wrapper routine

#
# Write the code for registering this function or procedure with NCL.
# This will be written to a different file, and needs to be copied
# to "wrapper.c".
#

w2file.write('extern NhlErrorTypes ' + ncl_name + '_W(void);\n')
w2file.write('/*\n * Register "' + ncl_name + '".\n *\n')
w2file.write(' * Create private argument array\n */\n')
w2file.write('        nargs = 0;\n')
w2file.write('        args = NewArgs(' + str(num_args) + ');\n\n')
for i in range(num_args):
  if args[i].ndims == 0:
    w2file.write('        SetArgTemplate(args,nargs,"' + args[i].ntype + \
                 '",0,NclANY);nargs++;\n')
  else:
    for j in range(args[i].ndims):
      w2file.write('        dimsizes[' + str(j) + '] = ' + \
                   str(args[i].dsizes[j]) + ';\n')
    w2file.write('\n        SetArgTemplate(args,nargs,"' + args[i].ntype + \
                 '",' + str(args[i].ndims) + ',dimsizes);nargs++;\n')
if isfunc:
  w2file.write('\n        NclRegisterFunc(' + ncl_name + '_W,args,"' + \
               ncl_name + '",nargs);\n')
else:
  w2file.write('\n        NclRegisterProc(' + ncl_name + '_W,args,"' + \
               ncl_name + '",nargs);\n')
#
# Close files.
#
w1file.close()
w2file.close()
