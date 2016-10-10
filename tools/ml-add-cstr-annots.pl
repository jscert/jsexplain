#!/usr/bin/perl
# An attempt at automatic annotation generation for OCaml files.
# Takes multiple filenames as input.

foreach $fname (@ARGV) {

	open(FILE, "< $fname") or die("Could not open $fname\n");
	open(NEW, "> $fname.temp");
	while($line = <FILE>) {
		if ($line =~ m/^(?!.*\[.*?\])(?!.*->)(\s*\|.*?)(of (.*))?$/) {
			$no = split(/\*/, $3);
			$annot = " [\@f";
			for (my $i=0; $i < $no; $i++) {
				$annot = $annot . " label" . $i;
				if ($i < $no -1) {
					$annot = $annot . ",";
				}
			}
			$annot = $annot . "] ";
			print NEW $1 . $annot . $2 . " (* Auto Generated Attributes *)\n";
		} else {
			print NEW $line;
		}
	}
	close(FILE);
	close(NEW);
	rename("$fname.temp", $fname) or die "can't rename $fname.temp to $fname: $!";
}
