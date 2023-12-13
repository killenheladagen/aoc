use v5.20;
use feature qw(signatures);
no warnings qw(experimental::signatures);
use Data::Dump qw(dump);

my $sum;
my @uni;
while(<DATA>) {
    chomp;
    if (/END/) {
	print "$sum\n";
    }
    if ($_ ne "") {
	push(@uni, $_);
    }
    else {
	dump(\@uni);
	my $last="";
	my $h=@uni;
	dump($h);
	for my $i (1..$h-1) {
	    if ($uni[$i] eq $uni[$i-1])

		
	    }
	    $last=$_;
	}
	@uni=();
    }
}

__DATA__
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#

END
