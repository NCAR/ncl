# 
# Mary used this script to:
# - creating opendap and non-opendap versions of ncl, which requires
#   you have a Site.local.dap and Site.local.nodap file in $NCARG.
# - testing both versions
# - creating tarfiles
# - untarring the files and testing them using hand-chosen graphical scripts
# - scping the tar file to cheyenne

import os, sys, glob, getopt, platform, stat, re
import subprocess
from shutil import copyfile,rmtree

#----------------------------------------------------------------------
def get_machine_type():
  return(platform.uname()[0])

#----------------------------------------------------------------------
def get_ldd_command():
  mtype = get_machine_type()
  if(mtype == 'Darwin'):
    return('otool -L')
  else:
    return('ldd')

#----------------------------------------------------------------------
def run_ldd_on_ncl(ncl_root):
  lddcmd = get_ldd_command()
  run_command_print_output('%s %s/bin/ncl' % (lddcmd,ncl_root),'ldd')

#----------------------------------------------------------------------
# Run a UNIX command
#----------------------------------------------------------------------
def run_command(cmd):
  proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
  proc.communicate()

#----------------------------------------------------------------------
# Run a UNIX command and print its output
#----------------------------------------------------------------------
def run_command_print_output(cmd,title):
  proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
  (out, err) = proc.communicate()
  print('%s: %s' % (title,out))

#----------------------------------------------------------------------
# Set up NCL environment and path
#----------------------------------------------------------------------
def set_ncl_environment(ncl_root,src_root,nclt_root):
  os.environ['NCARG_ROOT'] = ncl_root
  os.environ['NCARG'] = src_root
  os.environ['YMAKE_DEV_FILE'] = os.path.join(src_root,'ymakedevfile')
  os.environ['NCARGTEST'] = nclt_root
  os.environ['PATH'] = '%s:%s' % (os.path.join(ncl_root,'bin'),os.environ['PATH'])
  if(mach_name == "geyser"):
    os.environ['LIB_NCAR'] = ''
    os.environ['INC_NCAR'] = ''
    os.environ['NETCDF'] = ''
    os.environ['PATH'] = '%s:%s' % (comp_bin,os.environ['PATH'])

#----------------------------------------------------------------------
def print_equal():
  print('================================================================================')

#----------------------------------------------------------------------
def print_dash():
  print('----------------------------------------------------------------------')

#----------------------------------------------------------------------
def print_ncl_version():
  run_command_print_output('ncl -V','NCL Version')

#----------------------------------------------------------------------
def print_gnu_version():
  run_command_print_output('gcc --version','gcc version')
  run_command_print_output('gfortran --version','gfortran version')

#----------------------------------------------------------------------
def print_intel_version():
  run_command_print_output('icc --version','icc version')
  run_command_print_output('ifort --version','ifort version')

#----------------------------------------------------------------------
def print_which_gnu():
  run_command_print_output('which gcc','which gcc')
  run_command_print_output('which gfortran','which gfortran')

#----------------------------------------------------------------------
def print_which_intel():
  run_command_print_output('which icc','which icc')
  run_command_print_output('which ifort','which ifort')

#----------------------------------------------------------------------
def print_which_ncl():
  run_command_print_output('which ncl','which ncl')

#----------------------------------------------------------------------
# Print NCL environment
#----------------------------------------------------------------------
def print_ncl_environment(echo_ldd=False):
  print_dash()
  print_which_ncl()
  print_ncl_version()
  if (comp_str[0:3] == 'gnu'):
    print_which_gnu()
    print_gnu_version()
  elif (comp_str[0:5] == 'intel'):
    print_which_intel()
    print_intel_version()
  if(echo_ldd):
    run_ldd_on_ncl(ncl_root)
  print_dash()

#----------------------------------------------------------------------
# Remove a directory if it exists and then create it.
#----------------------------------------------------------------------
def create_dir(dir):
  if(os.path.exists(dir)):
    rmtree(dir)
  os.makedirs(dir)

#----------------------------------------------------------------------
# Copy over necessary NCL configuration files to do an OPeNDAP or
# non-OPeNDAP build of NCL.
#----------------------------------------------------------------------
def update_config_files(src_root,opendap=True):
  config_dir = os.path.join(src_root,'config')
  config_files = ['Site.local']
  for file in config_files:
    dst_file = os.path.join(config_dir,file)
    if(opendap):
      src_file = dst_file + '.dap'
    else:
      src_file = dst_file + '.nodap'
    if not os.path.isfile(src_file):
      print('update_config_files: cannot find %s' % src_file)
    else:
      print('update_config_files: copying %s to %s' % (src_file,dst_file))
      copyfile(src_file,dst_file)

#----------------------------------------------------------------------
# Make the 'ncl' executable, given whether it should be OPeNDAP-enabled.
#----------------------------------------------------------------------
def make_ncl(src_root,opendap=True,quiet=True):
  if(quiet):
    hide_output = ' > /dev/null'
  else:
    hide_output = ''

  update_config_files(src_root,opendap)
  ncl_bld_root = os.path.join(src_root,'ni','src','ncl')
  os.chdir(ncl_bld_root)
  if(os.path.isfile('ncl')):
    os.remove('ncl')
  if(opendap):
    print('make_ncl: Building an OPENDAP version of ncl')
  else:
    print('make_ncl: Building a NON-OPENDAP version of ncl')
  os.system('make me %s' % hide_output)
  os.system('make all %s' % hide_output)
  if(not (os.path.isfile('ncl') and os.access('ncl', os.X_OK))):
    print('make_ncl: ERROR, cannot build ncl executable')
    sys.exit(1)
  else:
    os.system('ls -lh ncl')
  os.system('make install %s' % hide_output)

#----------------------------------------------------------------------
# Run the given NCL script.
#----------------------------------------------------------------------
def run_ncl_script(dir,script,title):
  run_command_print_output('ncl %s' % os.path.join(dir,script),title)

#----------------------------------------------------------------------
# Run an NCL graphics script from the plotting test suite.
# We can set wtype to indicate where you want the output:
#
#    ncl 'wtype="x11"' file.ncl
#    ncl 'wtype="png"' file.ncl
#----------------------------------------------------------------------
def run_ncl_graphics_script(test_root,nclt_root,script,title):
  os.chdir(test_root)
  dir = os.path.join(nclt_root,'nclscripts','plotting')
  cmd = 'ncl \'wtype="x11"\' %s' % (os.path.join(dir,script))
# cmd = 'ncl \'wtype="png"\' %s' % (os.path.join(dir,script))
  run_command_print_output(cmd,title)

#----------------------------------------------------------------------
# Run an OPeNDAP test from the test suite
#----------------------------------------------------------------------
def run_opendap_test(test_root,nclt_root):
# This one doesn't work any more
#  run_ncl_graphics_script(test_root,nclt_root,'opendap_plot.ncl','OPenDAP test')
  run_ncl_graphics_script(test_root,nclt_root,'opendap_grib_plot.ncl','OPenDAP test')

#----------------------------------------------------------------------
# Run a test to make sure Triangle is built into NCL.
#----------------------------------------------------------------------
def run_triangle_test(test_root,nclt_root):
  run_ncl_graphics_script(test_root,nclt_root,'trimesh.ncl','Triangle test')

#----------------------------------------------------------------------
# Run one or more tests specific to new features added in release being
# tested. This section should be updated with every release.
#----------------------------------------------------------------------
def run_new_features_test(test_root,nclt_root):
  run_ncl_graphics_script(test_root,nclt_root,'wrf_interp_level_plot.ncl','WRF-ARW test')
  run_ncl_graphics_script(test_root,nclt_root,'wrf_vert_cross_latlon.ncl','WRF-ARW test')
  run_ncl_graphics_script(test_root,nclt_root,'dkrz_icon_mesh.ncl','gsn_coordinates test')

#----------------------------------------------------------------------
# Run one or more tests for bug fixes specific to release being
# tested. This section should be updated with every release.
#----------------------------------------------------------------------
def run_bug_fixes_test(test_root,nclt_root):
  run_ncl_graphics_script(test_root,nclt_root,'xyrefline_marker_bug.ncl','Fixed bugs test')
  run_ncl_graphics_script(test_root,nclt_root,'xy_panel_lbar_bug.ncl','Fixed bugs test')

#----------------------------------------------------------------------
# Run a shapefile test from the test suite
#----------------------------------------------------------------------
def run_shapefiles_test(test_root,nclt_root):
  run_ncl_graphics_script(test_root,nclt_root,'shapefiles_4.ncl','Shapefiles test')

#----------------------------------------------------------------------
# Run an ESMF test from the test suite
#----------------------------------------------------------------------
def run_esmf_test(test_root,nclt_root):
  esmf_exec = os.path.join(ncl_root,'bin','ESMF_RegridWeightGen')
  if(os.path.isfile(esmf_exec)):
    run_command_print_output('ESMF_RegridWeightGen -V','ESMF_RegridWeightGen version')
    run_ncl_graphics_script(test_root,nclt_root,'ESMF_1.ncl','ESMF regridding test')
  else:
    print('run_esmf_test: Cannot find %s' % esmf_exec)

def move_prof_file(ncl_root):
  home = os.path.expanduser("~")
  prof_file = os.path.join(ncl_root,'bin','ncl.prof')
  tmp_file  = os.path.join(home,'ncl.prof')
  if(os.path.isfile(prof_file)):
    os.system('mv -f %s %s' % (prof_file,tmp_file))

def move_prof_file_back(ncl_root):
  home = os.path.expanduser("~")
  prof_file = os.path.join(ncl_root,'bin','ncl.prof')
  tmp_file  = os.path.join(home,'ncl.prof')
  if(os.path.isfile(tmp_file)):
    print('mv -f %s %s' % (tmp_file,prof_file))
    os.system('mv -f %s %s' % (tmp_file,prof_file))

#----------------------------------------------------------------------
# Run the full data processing and plotting test suite and save to 
# HTML file.
#----------------------------------------------------------------------
def run_full_test_suite(nclt_root,ncl_root,version,mach_name,comp_str,do_large=False):
  py_script_dir = os.path.join(nclt_root,'testncl-python')
  output_dir = get_test_dir_name(version,mach_name,comp_str)
# Make sure directory doesn't exist.
  create_dir(os.path.join(py_script_dir,output_dir))
  html_file = output_dir + '.html'
  if(do_large):   # CAN BE SLOW! USE WITH CAUTION
    cmd = './testncl.py -C 8 -s -N %s -d %s -H > %s' % (ncl_root,output_dir,html_file)
  else:
    cmd = './testncl.py -C 8 -N %s -d %s -H > %s' % (ncl_root,output_dir,html_file)
  os.chdir(py_script_dir)
  run_command_print_output(cmd,cmd)
#
# Move the PNGs to a singular directory so we can more 
# easily manipulate and test them.
#
  png_dir = os.path.join(nclt_root,'nclscripts','plotting',output_dir)
  create_dir(png_dir)
  move_pngs(os.path.join(py_script_dir,output_dir),png_dir)

#----------------------------------------------------------------------
# Move PNGS out of testncl-python directory to plotting directory
# so they are in one location that we can easily copy to other
# machines, and to run compare_pngs on.
#----------------------------------------------------------------------
def move_pngs(dir1,dir2):
  cmd = "find %s -name '*.png' -exec mv {} %s/. \;" % (dir1,dir2)
  run_command_print_output(cmd,'Move pngs')

#----------------------------------------------------------------------
# Create a meaningful string like '660_geyser_gnu472' to use for
# creating test directories and/or output file names for testing.
#----------------------------------------------------------------------
def get_test_dir_name(version,mach_name,comp_str):
  tmp_v = re.sub('[.]','',version)
  return(tmp_v + '_' + mach_name + '_' + comp_str)

#----------------------------------------------------------------------
# Create a meaningful string like '660_geyser_gnu472_pngs' to use for
# putting generated PNGs.
#----------------------------------------------------------------------
def create_pngs_dir_name(version,mach_name,comp_str):
  return(get_test_dir_name(version,mach_name,comp_str) + '_pngs')

#----------------------------------------------------------------------
# Run the graphics test suite. PNGs should be put in a directory 
# called something like:
#     $NCARGTEST/nclscripts/plotting/660_denton_pngs
# We need to keep the pngs under plotting so we can compare them
# to other pngs.
#----------------------------------------------------------------------
def run_plotting_test_suite(nclt_root,version,mach_name):
  pngs_dir = create_pngs_dir_name(version,mach_name)
  os.chdir(os.path.join(nclt_root,'nclscripts'))
  os.system('./test_nclscripts -Q -p -o %s-plotting-output -gdir %s' % (pngs_dir,pngs_dir))

#----------------------------------------------------------------------
# This doesn't seem to be working at the moment. It spews out
#  "error in line -1" for every script and hangs at polytest2_wtrans.ncl
#----------------------------------------------------------------------
def run_plotting_test_suite_par(nclt_root,ncl_root,version,mach_name):
  pngs_dir = create_pngs_dir_name(version,mach_name)
  os.chdir(os.path.join(nclt_root,'nclscripts','plotting'))
  os.system('mpirun -n 4 python generate_pngs_parallel.py --test_dir=%s --test_root=%s' % (pngs_dir,ncl_root))

#----------------------------------------------------------------------
# Compare the graphics test suite 
#----------------------------------------------------------------------
def compare_test_pngs(nclt_root,test_dir,version,mach_name):
  pngs_dir = create_pngs_dir_name(version,mach_name)
  os.chdir(os.path.join(nclt_root,'nclscripts','plotting'))
  os.system('mpirun -n 4 python compare_pngs_parallel.py --test_dir=%s --stable_dir=660_stable'%pngs_dir)

#----------------------------------------------------------------------
# Check that our root directory contains all the expected files for
# this release. The expected files should be in an ASCII file as such:
#   bin/ctrans
#   ...
#   lib/libncarg.a
#   ...
#   lib/ncarg/fontcaps/font2
#   ...
#   include/ncarg/hlu/AnnoManager.h
#----------------------------------------------------------------------
def check_ncl_files(ncl_root):
  expected_file_list = os.path.join(cur_dir,'check_files_660')

  lines = open(expected_file_list).readlines()

# Strip off newlines. Isn't there a better way to do this?
  expected = []
  for i in range(len(lines)):
    expected.append(lines[i].rstrip())

# Get the list of files in ncl_root
  found = []
  rootlen = len(ncl_root.split('/'))
  for (path, dirs, files) in os.walk(ncl_root):
    spath = '/'.join(path.strip('/').split('/')[rootlen-1:])
    for file in files:
      if '.tar.gz' not in file:
        found.append(os.path.join(spath,file))

  diff_list1 = list(set(expected) - set(found))
  diff_list2 = list(set(found) - set(expected))

  print_dash()
  if not diff_list1 and not diff_list2:
    print('Comparing files from file %s and directory %s' % (expected_file_list,ncl_root))
    print('Files are the same!')
  else:
    if diff_list1:
      print('Files found only in expected list of files are:')
      print('   ' + ' '.join(diff_list1))
    if diff_list2:
      print('Files found only in new list of files are:')
      print('   ' + ' '.join(diff_list2))
  print_dash()

# Create name for binary tarfile.
def get_tarfile_name(version,mach_type,bit_str,comp_str,opendap):
  if(opendap):
    tar_string = '%s_%s_%s' % (mach_type,bit_str,comp_str)
  else:
    tar_string = '%s_%s_nodap_%s' % (mach_type,bit_str,comp_str)
  tarfile_name = 'ncl_ncarg-%s-%s.tar' % (version,tar_string) + '.gz'
  return(tarfile_name)

# Remove all *~ files from installation directory just in case
def clean_up_install_dir(ncl_root):
  for file in os.listdir(ncl_root):
    if file.endswith('~'):
      os.remove(file)
  idt_bak_file = os.path.join(ncl_root,'lib','ncarg','xapp','Idt.bak')
  if(os.path.isfile(idt_bak_file)):
    os.remove(idt_bak_file)

# Fix the permissions in the installation directory
def fix_permissions(ncl_root):
  fix_dirs = ['bin','include','lib']
  for fdir in fix_dirs:
    for (path, dirs, files) in os.walk(os.path.join(ncl_root,fdir)):
# directories
      for dir in dirs:
        os.chmod(os.path.join(path,dir), stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH | stat.S_IWUSR | 
                                         stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
# Executables
      if(os.path.basename(path) == 'bin'):
        for efile in files:
          os.chmod(os.path.join(path,efile), stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH | stat.S_IWUSR | 
                                             stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
      else:
# All other files
        for ofile in files:
          os.chmod(os.path.join(path,ofile), stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH | stat.S_IWUSR)

#----------------------------------------------------------------------
# Create binary tarfile for NCL
#----------------------------------------------------------------------
def create_tarfile(ncl_root,tmp_test_dir,version,mach_type,bit_str,
                   comp_str,opendap):
  clean_up_install_dir(ncl_root)
#  move_prof_file(ncl_root)
  fix_permissions(ncl_root)
  tar_name = get_tarfile_name(version,mach_type,bit_str,comp_str,opendap)
  tar_path = os.path.join(tmp_test_dir,tar_name)

  tar_dirs = ['bin','include','lib']
  tar_cmd = 'tar -czf ' + tar_path + ' -C ' + ncl_root + ' ' + ' '.join(tar_dirs)
  run_command_print_output(tar_cmd,'')
#  move_prof_file_back(ncl_root)
  return(tar_path)

#----------------------------------------------------------------------
# Create a full path to a directory for running tests in
#----------------------------------------------------------------------
def create_test_directory(version,mach_name,comp_str):
  test_name = get_test_dir_name(version,mach_name,comp_str)
  tmp_dir = os.path.join(cur_dir,test_name)
  create_dir(tmp_dir)
  return(tmp_dir)

#----------------------------------------------------------------------
# Create a directory name for extracting the test NCL tar file to.
# This is so we can keep the opendap and non-opendap directories 
# separate.
#----------------------------------------------------------------------
def get_test_ncl_root_dir_name(nclt_root_dir,opendap):
  if(opendap):
    return(os.path.join(nclt_root_dir,"opendap"))
  else:
    return(os.path.join(nclt_root_dir,"non_opendap"))

#----------------------------------------------------------------------
# Extract the NCL tar file so we can test it.
#----------------------------------------------------------------------
def extract_tarfile(extract_dir,tarfile,version,mach_name,opendap):
  if(not os.path.isfile(tarfile)):
    print('extract_tarfile: Error: Cannot find %s' % tarfile)
    sys.exit(1)
  else:
    print('extract_tarfile: Extracting file %s' % tarfile)
  cmd = 'tar zxf %s -C %s' % (tarfile,extract_dir)
  run_command_print_output(cmd,cmd)
  
#----------------------------------------------------------------------
# Test an NCL binary tarfile that you just created.
#
# Set minimal to True if you only want to run a couple of plotting
# tests. Otherwise, the full suite of tests will be executed which
# can take 30 minutes to 2 hours.
#----------------------------------------------------------------------
def test_ncl_tarfile(src_root,nclt_root,tmp_test_dir,version,mach_name,
                     mach_type,comp_str,tarfile,opendap=True,minimal=False,
                     do_large=False):
  ncl_root_test_dir = get_test_ncl_root_dir_name(tmp_test_dir,opendap)
  create_dir(ncl_root_test_dir)
  extract_tarfile(ncl_root_test_dir,tarfile,version,mach_name,opendap)
  set_ncl_environment(ncl_root_test_dir,src_root,nclt_root)
  print_ncl_environment(echo_ldd=True)
  check_ncl_files(ncl_root_test_dir)
  run_opendap_test(ncl_root_test_dir,nclt_root)
  run_new_features_test(ncl_root_test_dir,nclt_root)
  run_bug_fixes_test(ncl_root_test_dir,nclt_root)
  run_triangle_test(ncl_root_test_dir,nclt_root)
  run_esmf_test(ncl_root_test_dir,nclt_root)
  run_shapefiles_test(ncl_root_test_dir,nclt_root)
  if(not minimal):
#    run_plotting_test_suite(nclt_root,version,mach_name)
#    compare_test_pngs(nclt_root,ncl_root_test_dir,version,mach_name)
    run_full_test_suite(nclt_root,ncl_root_test_dir,version,mach_name,comp_str,do_large)

#----------------------------------------------------------------------
# Copy over non-DAP version of ESMF_RegridWeightGen
#----------------------------------------------------------------------
def copy_esmf_exec(external_root,opendap):
  esmf_exec = 'ESMF_RegridWeightGen'
  src_file = os.path.join(external_root,'bin',esmf_exec)
  if(opendap):
    src_file = src_file + '.dap'
  else:
    src_file = src_file + '.nodap'
  if(not os.path.exists(src_file)):
    print('copy_esmf_exec: Error: Cannot find %s' % src_file)
    return
  dst_file = os.path.join(ncl_root,'bin',esmf_exec)
  copyfile(src_file,dst_file)

def scp_ncl_tarfiles_to_super(tar_list,version):
  remote_dir = "/glade/p/cisl/vast/ncldev/src/tarfiles/dist/ncl/" + version
  if(mach_name == "geyser"):
    for file in tar_list:
      cmd = "cp %s %s" % (file,remote_dir)
      run_command_print_output(cmd,cmd)
  else:
    remote_system = "cheyenne.ucar.edu"
    for file in tar_list:
      cmd = "scp %s %s:%s" % (file,remote_system,remote_dir)
      run_command_print_output(cmd,cmd)

def check_python_version():
  if sys.version_info[0:2] < (2,7):
    raise Exception("Must use python 2.7 or greater!\n\tTry:\n\tmodule load python all-python-libs")

#----------------------------------------------------------------------
# Main driver
#----------------------------------------------------------------------
check_python_version()

version   = '6.6.0'
mach_name = 'cisl-molalla'
mach_type = 'MacOS_10.13'
bit_str   = '64bit'
comp_str  = 'gnu710'
src_root  = '/Users/haley/src/ncl-trunk-clean'
nclt_root = '/Users/haley/ncargtest'
ncl_root  = '/Users/haley/dev/clean'
ext_root  = '/Users/haley/dev/external'
cur_dir   = os.getcwd()

tmp_test_dir = create_test_directory(version,mach_name,comp_str)
set_ncl_environment(ncl_root,src_root,nclt_root)

# First create a non-opendap enabled NCL and test it.
opendap = False
make_ncl(src_root,opendap,quiet=False)
copy_esmf_exec(ext_root,opendap)
nondap_tarfile = create_tarfile(ncl_root,tmp_test_dir,version,mach_type,
                                bit_str,comp_str,opendap)
test_ncl_tarfile(src_root,nclt_root,tmp_test_dir,version,mach_name,mach_type,
                 comp_str,nondap_tarfile,opendap,minimal=True,do_large=False)

# Second create an opendap enabled NCL and test it.
opendap = True
make_ncl(src_root,opendap,quiet=False)
copy_esmf_exec(ext_root,opendap)
dap_tarfile = create_tarfile(ncl_root,tmp_test_dir,version,mach_type,
                             bit_str,comp_str,opendap)

test_ncl_tarfile(src_root,nclt_root,tmp_test_dir,version,mach_name,mach_type,
                 comp_str,dap_tarfile,opendap,minimal=True,do_large=False)

# Copy both opendap and non-opendap tarfiles to the NCAR super for backup
scp_ncl_tarfiles_to_super([dap_tarfile,nondap_tarfile],version)
