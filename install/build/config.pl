#!/usr/local/bin/perl
#
#      $Id: config.pl,v 1.1 1993-02-20 00:10:17 clyne Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1992				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		config.pl
#
#	Author:		John Clyne
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Thu Dec 3 12:21:11 MST 1992
#
#	Description:	
#

#
#       Return a list of values for a given macro from the config file for this
#       system
#

package	config;

$configFile;

sub     get_config_values {
	local($file, $symbol) = @_;

	local($line, @value);

	$line = `grep ^$symbol $file`;
	@value = split(/\s+/,$line);
	shift value;
	shift value;

	return(@value);
}


sub	check_config {
	local ($dir, $file) = @_;	# the config file

	local (@libpaths, @incpaths, $tmppath, $copt, $fopt, $cfile);

	$cfile = "$dir/$file";

	@libpaths = &get_config_values($cfile, "LOCAL_INC_SEARCH");
	@incpaths = &get_config_values($cfile, "LOCAL_LIB_SEARCH");
	$tmppath = &get_config_values($cfile, "TMPROOT");
	$copt = &get_config_values($cfile, "COPT");
	$fopt = &get_config_values($cfile, "FOPT");

print STDOUT <<"EOF";

	This script will set the BINROOT, LIBROOT, INCROOT, MANROOT and VERSION
	macros in $cfile in order to 
	correctly generate tar files for a $sys_type architecture. 
	However, the remaining macros are left unmolested. Some of the 
	more interesting ones are set as follows:

	LOCAL_INC_SEARCH	: @incpaths
	LOCAL_LIB_SEARCH	: @libpaths
	TMPROOT			: $tmppath
	COPT			: $copt
	FOPT			: $fopt

	It may be necessary to edit $cfile
	manually or run $dir/config.perl if 
	the above values are not correct.


EOF

	print STDOUT "Press <RETURN> to continue, q(quit) ";

	$_ = <STDIN>;
	chop;
	if (/^q/ || /^Q/) {
		return(-1);
	}

	return(0);
}

sub	main'DoConfig {
	local ($config_dir, $config_file , $path, $version) = @_;

	local ($binpath, $incpath, $libpath, $manpath, $file);

	$file = "$config_dir/$config_file";


	if (&check_config($config_dir, $config_file) < 0) {
		return(-1);
	}


	$cmd = "/bin/cp $file $file.bak";
	system($cmd);
	if ($? != 0) {
		print "\"$cmd\" exited with error\n";
		return(-1);
	}

	$configFile = $file;

	$binpath = "$path/bin";
	$incpath = "$path/include";
	$libpath = "$path/lib";
	$manpath = "$path/man";

	


	open(ED, "| ed - $file > /dev/null");

	print ED <<"EOF";
/^VERSION/
c
VERSION		= $version
.
/^BINROOT/
c
BINROOT		= $binpath
.
/^LIBROOT/
c
LIBROOT		= $libpath
.
/^INCROOT/
c
INCROOT		= $incpath
.
/^MANROOT/
c
MANROOT		= $manpath
.
w
q
EOF

	close ED;

	return(0);
}


sub	main'CloseConfig {

	local($cmd);

	if (defined ($configFile)) {
		$cmd = "/bin/mv $configFile.bak $configFile";
		system($cmd);
	}
}
1;

