use v5.20;
use feature qw(signatures);
no warnings qw(experimental::signatures);
use Data::Dump qw(dump);

my $sum;

while(<DATA>) {
    chomp;
    if (/START/) {
	$sum=0;
    }
    elsif (/END/) {
	print "\n  sum = $sum\n\n";
    }
    else {
    }
}

__DATA__
START
END
START
END
