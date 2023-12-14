use v5.20;
use feature qw(signatures);
no warnings qw(experimental::signatures);
use Data::Dump qw(dump);

sub foo($fn) {
    open(my $fh, "<$fn");
    print "(\n";
    while(<$fh>) {
	chomp;
	my ($p,$n)=split(/ /);
	print "((";
	foreach(split(//, $p)) {
	    print "0 " if /\./;
	    print "1 " if /#/;
	    print "nil " if /\?/;
	}
	print ") ";
	$n=~s/,/ /g;
	print "$n";
	print ")\n";
    }
    print ")\n";
}

print "(defparameter input '(\n";
foo("12-ex.txt");
foo("12-input.txt");
print "))\n";
