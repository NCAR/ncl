#!/usr/bin/env perl

use strict;
use warnings;
use FileHandle;
use File::stat;
use Getopt::Long;

my ($fname, @fnames, $ufunc_level, $func_stats, @lines);
my (%file_mdata);
my (%proc_map);
my (%file_map, @mfile_line_info);
my ($FILE_LN_CACHE_SZ);
my (@ncl_src_dirs, $verbose);
$verbose = 0;

# File line cache size
$FILE_LN_CACHE_SZ = 4096;

# Subroutines ...

# Initialize color bins
sub init_colors
{
	my ($colors_ref, $median_val) = @_;
	my ($MAX_TIMEVAL) = 99999;
	
	$colors_ref->[0] = { VAL => $median_val/100, COLOR => "white" };
	$colors_ref->[1] = { VAL => $median_val/10, COLOR => "blue" };
	$colors_ref->[2] = { VAL => $median_val, COLOR => "green" };
	$colors_ref->[3] = { VAL => $median_val * 10, COLOR => "yellow" };
	$colors_ref->[4] = { VAL => $median_val * 100, COLOR => "orange" };
	$colors_ref->[5] = { VAL => $median_val * 1000, COLOR => "red" };
	$colors_ref->[6] = { VAL => $MAX_TIMEVAL, COLOR => "red" };
}

# Get color corresponding to a value (time)
sub get_color
{
	my ($colors_ref, $utime) = @_;
	my ($MAX_DELTA) = 0.000001;
	my ($i);

	$i = 0;

	# FIXME : Does binsrch help here ?
	while(defined($colors_ref->[$i]->{VAL}) && ($utime > $colors_ref->[$i]->{VAL})) {
		# Correct for stupid perl floating pt math
		if(($utime - $colors_ref->[$i]->{VAL}) < $MAX_DELTA){
			last;
		}
		$i++;
	}

	"\"$colors_ref->[$i]->{COLOR}\"";
}

# Assign color to a line
sub assign_line_color
{
	my ($file_map_ref) = @_;
	my ($fname, $file_info);
	while(($fname, $file_info) = each %{$file_map_ref}){
		my ($i, $num_lines, $med_time, $upper_outlier_limit); 
		my (@valid_lines, $num_valid_lines, @colors);
		$num_lines = @{$file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}]};

		# Find median value of times taken for each line
		@valid_lines = grep { defined $_ } @{$file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}]};
		$num_valid_lines = @valid_lines;
		@valid_lines = sort line_info_comparator_by_time (@valid_lines);
		$med_time = $valid_lines[$num_valid_lines/2]->{TOTAL_TIME};
		if($med_time == 0){
			$med_time = 0.000001;
		}
		if($verbose) { print "Median val is $med_time\n"; }

		# Initialize color bin for this file
		&init_colors(\@colors, $med_time);

		# For each line calc percentage time taken and line color
		for($i = 1; $i < $num_lines; $i++){
			if(defined($file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}][$i])){
				my $tmp_val;
#				if($verbose) { &printv_line_info($file_info->{LINE_INFO_REF}[$i]); }
				if($file_info->{TOTAL_TIME} > 0){
					$tmp_val = $file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}][$i]->{TOTAL_TIME} / $file_info->{TOTAL_TIME} * 100;
				}
				else{
					$tmp_val = 0.0;
				}
				$tmp_val = sprintf("%.3f", $tmp_val);
				$file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}][$i]->{PERC_TIME} = $tmp_val;
				$file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}][$i]->{LINE_COLOR} = &get_color(\@colors, $file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}][$i]->{TOTAL_TIME});
			}
		}
		undef(@valid_lines);
	}
}

# Print (Verbose) proc info
sub printv_proc_info
{
	my $proc_info = $_[0];
	print "PROC_INFO: START_TIME = $proc_info->{START_TIME}, END_TIME = $proc_info->{END_TIME}\n";
	print "\t TOTAL_TIME = $proc_info->{TOTAL_TIME}\n";
	print "\t NEST_LEVEL = $proc_info->{NEST_LEVEL}, FUNC_LEVEL = $proc_info->{FUNC_LEVEL}, NUM_CALLS = $proc_info->{NUM_CALLS}\n";
}

# Comparator for sorting proc_infos in proc_map by total time
sub proc_info_comparator_by_time
{
	$proc_map{$b}->{TOTAL_TIME} <=> $proc_map{$a}->{TOTAL_TIME};
}

# Print (verbose) line info
sub printv_line_info
{
	my $line_info = $_[0];
	print "LINE_INFO: LINE_NUM = $line_info->{LINE_NUM}\n";
	print "\t START_TIME = $line_info->{START_TIME}, END_TIME = $line_info->{END_TIME}\n";
	print "\t TOTAL_TIME = $line_info->{TOTAL_TIME}\n";
	print "\t NUM_CALLS = $line_info->{NUM_CALLS}\n";
}

# Comparator for sorting line_infos by total time
sub line_info_comparator_by_time
{
	$b->{TOTAL_TIME} <=> $a->{TOTAL_TIME};
}

# Print (verbose) file info
sub printv_file_info
{
	my $file_info = $_[0];
	print "FILE_INFO: NAME = $file_info->{NAME} \n";
	print "\t TOTAL_TIME = $file_info->{TOTAL_TIME}, LINE_INFO_REF = $file_info->{LINE_INFO_REF} \n";
}

# Analyze the events from the prof log file
sub analyze_prof_log
{
	my ($entering, $leaving, $funcname, $timestamp, $proc_info);
	my ($file_info, $eline_no, $line_info);
	my ($analysis_complete);
	my ($root_ncl_fname);
	my $i;
	my ($lines_ref, $sline_no, $func_level, $proc_map_ref, $file_map_ref, $line_map_ref, $nxt_avail_line_info_row_ref) = @_;
	my $num_lines = @$lines_ref;
	$eline_no = 0;
	$analysis_complete = 0;
	for($i=$sline_no; $i<$num_lines; $i++){
		$_ = $lines_ref->[$i];
		$eline_no = $i;
		if($verbose) { print "PARSING LINE : ", $_; }
		if(/^\r*\\(F_EN)\s+(.*)/ || /^\r*\/(F_EX)\s+(.*)/){
			# Gather func stats...
			if($1 eq 'F_EN'){
				$entering = 1;
			}
			else{
				$entering = 0;
			}
			$_ = $2;
			# Get funcname, timestamp
			if(/(\w+)\s+\(([0-9]+\.?[0-9]*)\)/){
				$funcname = $1;
				$timestamp = $2;
				if($verbose){print "FUNCNAME :", $funcname, ", TIME :", $timestamp, "\n";}
			}
			if(exists $proc_map_ref->{$funcname}){
				# Update procedure info entry
				$proc_info = $proc_map_ref->{$funcname};
				if($entering){
					# Update nest and call cnts
					$proc_info->{NEST_LEVEL} = $proc_info->{NEST_LEVEL} + 1;
					$proc_info->{NUM_CALLS} = $proc_info->{NUM_CALLS} + 1;
					if($proc_info->{NEST_LEVEL} == 0){
						$proc_info->{START_TIME} = $timestamp;
						$proc_info->{END_TIME} = $timestamp;
					}
					$func_level = $func_level + 1;
				}
				else{
					if($proc_info->{NEST_LEVEL} == 0){
						my ($time_in_call);
						$proc_info->{END_TIME} = $timestamp;
						$time_in_call = $proc_info->{END_TIME} - $proc_info->{START_TIME};
						$proc_info->{TOTAL_TIME} = $proc_info->{TOTAL_TIME} + $time_in_call;
					}
					$proc_info->{NEST_LEVEL} = $proc_info->{NEST_LEVEL} - 1;
					$func_level = $func_level - 1;
				}
				$proc_map_ref->{$funcname} = $proc_info;
			}
			else{
				# Create and add a procedure info entry
				$proc_info = {START_TIME => $timestamp, END_TIME => 0.0, TOTAL_TIME => 0.0, NEST_LEVEL => 0, FUNC_LEVEL => $func_level, NUM_CALLS => 1};
				$proc_map_ref->{$funcname} = $proc_info;
				$func_level = $func_level + 1;
			}
		}
		elsif(/^\r*\\(L_EN)\s+(.*)/ || /^\r*\/(L_EX)\s+(.*)/){
			# Gather line stats...
			if($1 eq 'L_EN'){
				$entering = 1;
			}
			else{
				$entering = 0;
			}
			$_ = $2;
			#if(/([0-9]+)\s+\w+\s+([\w\/\$.]+)\s+\(([0-9]+\.?[0-9]*)\)/){
			if(/([0-9]+)\s+([0-9]+)\s+\(([0-9]+\.?[0-9]*)\)/){
				my ($line_no, $fname, $timestamp);
				$line_no = $1;
				$fname = $2;
				$timestamp = $3;
				if($verbose) { print "LINE NO: $1, FILENAME: $2, TIME: $3\n"; }
				if(exists $file_map_ref->{$fname}){
					# Create/Update file line info 
					my ($file_line_info, $file_line_info_row);
					$file_info = $file_map_ref->{$fname};
					$file_line_info = $file_info->{LINE_INFO_REF};
					$file_line_info_row = $file_info->{LINE_INFO_ROW};
					if(defined($file_line_info->[$file_line_info_row][$line_no])){
						$line_info = $file_line_info->[$file_line_info_row][$line_no];
					}
					else{
						$line_info = { LINE_NUM => $line_no, START_TIME => 0.0, END_TIME => 0.0, TOTAL_TIME => 0.0, PERC_TIME => 0.0, NUM_CALLS => 0, LINE_COLOR => "\"white\""};
					}
					if($entering){
						$line_info->{START_TIME} = $timestamp;
						$line_info->{END_TIME} = $timestamp;
						$line_info->{NUM_CALLS}++;
					}
					else{
						my $time_in_line;
						
						$line_info->{END_TIME} = $timestamp;
						$time_in_line = $line_info->{END_TIME} - $line_info->{START_TIME};
						$line_info->{TOTAL_TIME} += $time_in_line;
						$file_info->{TOTAL_TIME} += $time_in_line;
					}
					$file_line_info->[$file_line_info_row][$line_no] = $line_info;
				}
				else{
					# Add a file info
					# FIXME: Do we need to store line num & file name in recs too ?
					$line_info = {LINE_NUM => $line_no, START_TIME => $timestamp, END_TIME => 0.0, TOTAL_TIME => 0.0, PERC_TIME => 0.0, NUM_CALLS => 1, LINE_COLOR => "\"white\""};
					$line_map_ref->[${$nxt_avail_line_info_row_ref}][$line_no] = $line_info;
					$file_info = {NAME => $fname, TOTAL_TIME => 0.0, LINE_INFO_REF => $line_map_ref, LINE_INFO_ROW => ${$nxt_avail_line_info_row_ref}};
					${$nxt_avail_line_info_row_ref} += 1;
					$file_map_ref->{$fname} = $file_info;
				}
			}
		}
		elsif(/^\r*\\(FL_EN)\s+(.*)/ || /^\r*\/(FL_EX).*/){
			if($1 eq 'FL_EN'){
				$root_ncl_fname = $2;
				if($verbose) { print "Starting analyzing file, $root_ncl_fname\n"; }
			}
			else{
				$analysis_complete = 1;
				if($verbose) { print "Finished analyzing file, $root_ncl_fname\n"; }
				last;
			}
		}
	}
	if($verbose){
		my ($key, $value);
		print "\n-------------------\n";
		print " FUNCTION STAT MAP \n";
		print "-------------------\n";
		while(($key, $value) = each %{$proc_map_ref}){
			&printv_proc_info($value);
		}
	}

	if($verbose) { print "Analyzed $root_ncl_fname($eline_no)\n"; }
	{ ROOT_NCL_FILENAME => $root_ncl_fname, FUNC_LEVEL => $func_level, LAST_LINE_PROCESSED => $eline_no, ANALYSIS_COMPLETE => $analysis_complete };
}

# Print (stdout) a summary of NCL function usage
sub print_func_stats
{
	my ($proc_info, $key, %proc_map);
	my ($fname, $func_level, $proc_map_ref) = @_;
	%proc_map = %{$proc_map_ref};
	print "\n-----------------------------------------------\n";
	print "\t FUNCTION STATISTICS ($fname) \n"; 
	if($func_level != 0){
		print "\t\t\(for levels <= $func_level \)\n";
	}
	print "-----------------------------------------------\n";
	if(!defined($fname)){ exit; }
	foreach $key (sort proc_info_comparator_by_time keys %proc_map){
		$proc_info = $proc_map{$key};
		if($proc_info->{FUNC_LEVEL} <= $func_level){
			print "FUNCNAME = ", $key, "\n";
			print "\t TOTAL_TIME = ", $proc_info->{TOTAL_TIME}," s,\tNUM_CALLS = ", $proc_info->{NUM_CALLS}, "\n";
		}
	}
}

# Get file meta data
sub get_file_mdata
{
	my ($lines_ref, $sline_no, $file_mdata_ref) = @_;
	my ($eline_no, $mdata_read_complete, $i, $qname, $fname);
	my $num_lines = @$lines_ref;
	$eline_no = $sline_no;
	$mdata_read_complete = 0;
	for($i=$sline_no; $i<$num_lines; $i++){
		$_ = $lines_ref->[$i];
		$eline_no = $i;
		if($verbose) { print "PARSING LINE : ", $_; }
		if(/^\r*\\MDATA/){
			if($verbose) { print "Started reading mdata\n"; }
		} 
		elsif (/^\r*FL_NM\s+([0-9]+)\s+([\w\/\$.]+)/){
			$qname = $1;
			$fname = $2;
			$file_mdata_ref->{$qname} = $fname;
		}
		elsif (/^\r*\/MDATA/){
			if($verbose) { print "Finished reading mdata\n"; }
			$mdata_read_complete = 1;	
		}
		
	}
	{ LAST_LINE_PROCESSED => $eline_no, MDATA_READ_COMPLETE => $mdata_read_complete };
}

# FIXME: Move XML stuff to a diff module
# Convert txt/code to html
sub htmlify
{
	my ($str) = @_;

	$str =~ s/&/&amp;/g;
	$str =~ s/</&lt;/g;
	$str =~ s/>/&gt;/g;
	$str =~ s/<</&laquo;/g;
	$str =~ s/"/&quot;/g;
	$str;
}

# Write the XML header
sub write_XML_header
{
	my ($OSTR) = @_;
	print $OSTR "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
	print $OSTR "<?xml-stylesheet href=\"ncl_pr_analyze.xsl\" type=\"text/xsl\" ?>\n";
}

# Write (only) the XML start tag
sub write_XML_start_tag
{
	my ($OSTR, $str, $att) = @_;
	if(defined($att)){
		print $OSTR "<".$str." ".$att.">\n";
	}
	else{
		print $OSTR "<".$str.">\n";
	}
}

# Write (only) the XML end tag
sub write_XML_end_tag
{
	my ($OSTR, $str) = @_;
	print $OSTR "</".$str.">\n";
}

# Write data with XML tags
sub write_XML_data
{
	my ($OSTR, $tag_name, $att, $str) = @_;
	&write_XML_start_tag($OSTR, $tag_name, $att);
	$str = &htmlify($str);
	print $OSTR $str."\n";
	&write_XML_end_tag($OSTR, $tag_name);
}

# Write line statistics to an XML file
sub write_line_stats
{
	my ($root_ncl_fnameq, $ncl_src_dirs_ref, $file_map_ref, $file_mdata_ref) = @_;
	my ($root_ncl_fname, $ncl_fnameq, $ncl_fname, $file_info);

	$root_ncl_fname = $file_mdata_ref->{$root_ncl_fnameq};
	# For each ncl source, ncl_fname, referred by root ncl source, root_ncl_fname
	while(($ncl_fnameq, $file_info) = each %{$file_map_ref}){
		my ($i, $num_lines, $xml_fname, @lines); 
		my $XML_LSTAT_FILE = new FileHandle;
		my $NCL_FILE = new FileHandle;

		$ncl_fname = $file_mdata_ref->{$ncl_fnameq};
		if($verbose) { print "Writing line stats for $ncl_fname\n"; }
		# Expand env vars in ncl filenames, if possible
		if($ncl_fname =~ m/\$(\w+)/){
			if(defined $ENV{$1}){
				$ncl_fname =~ s/\$(\w+)/$ENV{$1}/g;
			}
			else{
				$ncl_fname =~ s/\$(\w+)\///g;
			}
			if($verbose) { print "Writing line stats for transformed $ncl_fname\n"; }
		}

		# Use user-specified ncl src directories to find an ncl source if needed
		if(!stat($ncl_fname)){
			my $ncl_src_dirs = @$ncl_src_dirs_ref;	
			my $i;
			for ($i=0; $i<$ncl_src_dirs; $i++){
				# FIXME: Non-portable - use catfile
				$ncl_fname = $ncl_src_dirs_ref->[$i]. "/" . $ncl_fname;
				if(stat($ncl_fname)){
					if($verbose) { print "Writing line stats for transformed $ncl_fname\n"; }
					last;
				}
			}
		}
		unless($NCL_FILE->open("$ncl_fname")) {
			print "WARNING: Cannot open ncl source file, '$ncl_fname', ignoring...\n";
			next;
		}
		@lines = <$NCL_FILE>;
		$NCL_FILE->close();

		# Create a unique name for the XML prof log from the ncl and root ncl names
		$xml_fname = $ncl_fname;
		#$xml_fname =~ s/[.][^.]*$/\.xml/;
		$xml_fname =~ s/[.]/_/g;
		$xml_fname =~ s/[_][^_]*$/\.xml/;
		$root_ncl_fname =~ s/[.]/_/g;
		$xml_fname = $root_ncl_fname . "__" . $xml_fname;
		# FIXME: Non-portable
		$xml_fname =~ s/\//_/g;

		# Write the XML prof log info
		$XML_LSTAT_FILE->open(">$xml_fname")
			or die "Error opening output XML file '$xml_fname'\n";
		&write_XML_header($XML_LSTAT_FILE);
		&write_XML_start_tag($XML_LSTAT_FILE, "ncl_stats", "file=\"$ncl_fname\"");

		$num_lines = @{$file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}]};
		for($i = 1; ($i < $num_lines) and (defined($lines[$i-1])) ; $i++){
			&write_XML_start_tag($XML_LSTAT_FILE, "line_stat");
			if(defined($file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}][$i])){
				my $line_info;
#				if($verbose) { &printv_line_info($file_info->{LINE_INFO_REF}[$i]); }
				$line_info = $file_info->{LINE_INFO_REF}[$file_info->{LINE_INFO_ROW}][$i];
				&write_XML_data($XML_LSTAT_FILE, "line", "line_no=\"$i\" color=$line_info->{LINE_COLOR} time=\"$line_info->{TOTAL_TIME}s\" perc_time=\"$line_info->{PERC_TIME}\%\"", $lines[$i-1]);
			}
			else{
				&write_XML_data($XML_LSTAT_FILE, "line_no", "", $i);
				&write_XML_data($XML_LSTAT_FILE, "line", "", $lines[$i-1]);
			}
			&write_XML_end_tag($XML_LSTAT_FILE, "line_stat");
		}

		&write_XML_end_tag($XML_LSTAT_FILE, "ncl_stats");
		$XML_LSTAT_FILE->close();
	}
}

# FIXME: Support func_level and file_level...
sub usage()
{
	print "Usage: ./ncl_prof_analyzer.pl --log=NCL_PROF_LOGFILES --level=NCL_PROF_FUNC_LOG_LEVEL --ncl-srcdir=PATH_TO_NCL_SRC --verbose\n\n";
	print "eg: ./ncl_prof_analyzer.pl --log=ncl_file1_prof.log ncl_file2_prof.log\n\n";
	print " (The command above displays the function stats in stdout and creates ";
	print "ncl_file1.xml and ncl_file2.xml. The output xml file names correspond ";
	print "to the ncl files, ncl_file1.ncl and ncl_file2.ncl, used to create log ";
	print "files) \n\n";
}

# Main program ...
$ufunc_level = 0;

GetOptions(
			"log=s{,}"			=> \@fnames,			# logfile
			"level=i"			=> \$ufunc_level,		# tracing func level
			"ncl-srcdir=s{,}"	=> \@ncl_src_dirs,		# ncl source dir list
			"verbose"			=> \$verbose
			);

if(!@fnames){
	&usage();
	exit;
}
else{
	if($verbose){
		print "LOGFILES = '@fnames', LEVEL = $ufunc_level, VERBOSE = $verbose\n";
	}
}

if(@ncl_src_dirs){
	@ncl_src_dirs = split(/;/, join(';', @ncl_src_dirs));
}

# Add the current directory in the search path by default
push @ncl_src_dirs, ".";

# foreach (prof log file)
foreach $fname (@fnames){
	my ($NCL_PROF_LOGFILE, $sline_no, $num_lines, $file_processing_stats);
	my ($i, $func_level, $root_ncl_fname, $nxt_avail_line_info_row);
	my ($is_analyze_phase, $file_post_processing_stats);

	$NCL_PROF_LOGFILE = new FileHandle;
	$NCL_PROF_LOGFILE->open("$fname")
		or die "Error opening log file '$fname'\n";

	$func_level = 0;
	$sline_no = 0;
	$nxt_avail_line_info_row = 0;
	# Start analysing the file after reading
	$is_analyze_phase = 1;
	do{
		undef(@lines);
		# Read and process prof log in blocks
		for($i=0; $i<$FILE_LN_CACHE_SZ; $i++){
			my ($tmp_line);
			if(defined($tmp_line = <$NCL_PROF_LOGFILE>)){
				push @lines, $tmp_line;
			}
			else{
				last;
			}
		}
		$num_lines = @lines;
		while($num_lines > 0){
			if($is_analyze_phase){
				#FIXME: We might want to analyze only the level requested
				$file_processing_stats = &analyze_prof_log(\@lines, $sline_no, $func_level, \%proc_map, \%file_map, \@mfile_line_info, \$nxt_avail_line_info_row);
				if($verbose) { print "FILE PROC STATS : $file_processing_stats->{ROOT_NCL_FILENAME}, $file_processing_stats->{LAST_LINE_PROCESSED}\n"; }

				if(!defined($root_ncl_fname)){
					$root_ncl_fname = $file_processing_stats->{ROOT_NCL_FILENAME};
				}
				if($file_processing_stats->{ANALYSIS_COMPLETE}){
					if($verbose) { print "FILE ANALYSIS COMPLETE... moving to post proc\n"; }
					$sline_no = $file_processing_stats->{LAST_LINE_PROCESSED} + 1;
					$is_analyze_phase = 0;
				}
				else{
					if($verbose) { print "FILE ANALYSIS NOT COMPLETE... reading more data\n"; }
					$sline_no = 0;
					$func_level = $file_processing_stats->{FUNC_LEVEL};
					# FIXME: We could loop indefinitely with a corrupt file !
					$num_lines -= ($file_processing_stats->{LAST_LINE_PROCESSED} + 1);
				}
			}
			else{
				# post processing phase
				$file_post_processing_stats = &get_file_mdata(\@lines, $sline_no, \%file_mdata);
				$sline_no = $file_post_processing_stats->{LAST_LINE_PROCESSED} + 1;
				$num_lines -= ($file_post_processing_stats->{LAST_LINE_PROCESSED} + 1);
				if($file_post_processing_stats->{MDATA_READ_COMPLETE}){
					if($verbose) { print "POST PROCESSING PHASE...\n"; }
					&assign_line_color(\%file_map);
					&print_func_stats($root_ncl_fname, $ufunc_level, \%proc_map);
					&write_line_stats($root_ncl_fname, \@ncl_src_dirs, \%file_map, \%file_mdata);
					undef(%proc_map);
					undef(%file_map);
					undef(@mfile_line_info);
					$nxt_avail_line_info_row = 0;
					undef($root_ncl_fname);
					$func_level = 0;
					undef(%file_mdata);
					# Get back to analysis phase
					$is_analyze_phase = 1;
				}
				else{
					# Read more lines to get complete meta data
					if($verbose) { print "Reading more data to get the mdata...\n"; }
					$sline_no = 0;
				}
			}
		}
	}while(@lines);
	$NCL_PROF_LOGFILE->close();
}

# END

