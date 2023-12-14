use v5.20;
use feature qw(signatures);
no warnings qw(experimental::signatures);
use Data::Dump qw(dump);

my $verbose=0;

sub is_symmetric_around($m,$array) {
    my $max_idx=scalar(@{$array})-1;
    return 0 if($m<0 or $m>=$max_idx);
    my $a = $m;
    my $b = $m + 1;
    print "\tAround $m?\n" if $verbose>1;
    while(1) {
	print "\t\t$a=$b?\n" if $verbose>1;
	return 1 if ($a<0 or $b>$max_idx);
	return 0 if ($array->[$a] ne $array->[$b]);
	$a--;
	$b++;
    }
}

sub transpose($source_matrix) {
    my $w=length($source_matrix->[0]);
    my @result=();
    for my $i (0..$w-1) {
	push(@result,"");
    }
    foreach my $row (@{$source_matrix}) {
	my $i=0;
	foreach my $c (split(//,$row)) {
	    $result[$i].=$c;
	    $i++;
	}
    }
    return \@result;
}

sub symmetry_line($array, $old_sym) {
     #print "Testing symmetry_line:\n";
     #dump($array);
    
    my $n=scalar(@{$array});
    for my $i (0..$n-1) {
	next unless is_symmetric_around($i,$array);
	return $i+1 if ($i+1) != $old_sym;
    }
    return 0;
}

sub symmetry_value($matrix) {
    print "Unmodified:\n" if $verbose;
    dump($matrix) if $verbose;
    my $old_h=symmetry_line($matrix, -1);
    my $old_v=symmetry_line(transpose($matrix), -1);
    print "=> $old_h,$old_v\n";
    for(my $y=0;$y<scalar(@{$matrix});$y++) {
	for(my $x=0;$x<length($matrix->[0]);$x++) {
	    my $old = substr($matrix->[$y], $x, 1);
	    my $new = ($old eq '.') ? 'X' : '.';
	    substr($matrix->[$y], $x, 1, $new);
	    dump($matrix) if $verbose;
	    my $val=100*symmetry_line($matrix, $old_h)+symmetry_line(transpose($matrix), $old_v);
	    print "($x,$y): sym=$val\n" if $val and $verbose;
	    substr($matrix->[$y], $x, 1, $old);
	    return $val if $val;
	}
    }
    return 0;
}

my $sum=0;
my $sym;
my @uni;
$verbose=1;
while(<DATA>) {
    chomp;
    if (/END/) {
	$verbose=0;
	print "\nEND\n";
	print "\n$sum\n\n";
	$sum=0;
    }
    elsif ($_ ne "") {
	push(@uni, $_);
    }
    else {
	#dump(\@uni);
	my $sym=symmetry_value(\@uni);
	if($sym==0 and 0) {
	    dump(\@uni);
	    dump(transpose(\@uni));
	    $verbose=1;
	    print is_symmetric_around(7,transpose(\@uni))."\n";
	    $verbose=0;
	}
	print "sym_val=$sym\n";
	$sum+=$sym;
	@uni=();
    }
}

# 35028 too low => 35538
# b: 37148 too high
# b: 36147 too high
# b: 30442

__DATA__
X.XX..XX.
..X.XX.X.
XX......X
XX......X
..X.XX.X.
..XX..XX.
X.X.XX.X.

X...XX..X
X....X..X
..XX..XXX
XXXXX.XX.
XXXXX.XX.
..XX..XXX
X....X..X

END
....X....X.
..XX.XXXX.X
XXXX.XXXX.X
XXXX..XX..X
XXXXXX..XXX
....XX..XX.
.....XXXX..
XXXX......X
..XX.XXXX.X
..XX..XX..X
..X.X.XX.X.
XXX.XX..XX.
X.X.X.XX.X.
XXXX.XXXX.X
XXXXX.XX.XX

.X......XX.X.X...
.XX.X..XXXXXX.X..
..X.XX..X..XX.XXX
XX....XXXXXXX...X
...XX..X.X...XXX.
...XX..X.X...XXX.
XX....XXXXXXX...X
..X.XX..X..XX.XXX
.XX.X..XXXXXX.X..
.X......XX.X.X...
.X.XXXXXXXX....X.
..XX.XX.X..XX.XX.
..XX.XX....XX.XX.
.X.XXXXXXXX....X.
.X......XX.X.X...
.XX.X..XXXXXX.X..
..X.XX..X..XX.XXX

XX..XXXX..XXX
.X..XXXX..X..
X...X..X...XX
XX.X....X.XXX
...X.XX.X.X..
.XXX....XXX..
X.XXXXXXXX.XX
X..XX..XX..XX
.X.X....X.X..

...XX.X..X.XX..
..XX.X.XX.X.XX.
XXX.X..XX..X.XX
XXX.X......X.XX
..XX..X..X..XX.
..X....XX....X.
....XX.XX.XX...
XXXX.XXXXXX.XXX
....X.XXXX.....
..XXXX....XXXX.
..XXX.X..X.XXX.
XXX..XXXXXX..XX
...X........X..

...XX..XXX.....
..X....X..XX...
...X.XXX.X.XX..
...X..XX.X.XX..
..X....X..XX...
...XX..XXX.....
X.XX.XXXXXXXX.X
X....X.X.X..X..
XXX....XXX..XX.
...XXX..XXX.XXX
X..X...X..X..X.
XXXXX.X........
X..XX.X...X..X.
X..XX.X...X..X.
XXXXX.X........
X..X...X..X..X.
...XXX..XXX.XXX

..XX....X
....XXXX.
XXXX....X
XX.......
....X..X.
....XXXX.
..X......
..X......
XXX......
XXX.XXX..
..XX.XX.X
XXX......
...X.XX.X

.X...X.X.X.
.X...X...X.
..XXXXXX.X.
.X.XXX.X.X.
.XX...X.X..
.X..X......
..X.....X..
X.XX.X.X.X.
.....XX.XXX
.....XX.XXX
X.XX.X.X.X.
..X.....X..
.X..X......
.XX...X.X..
.X.XXX.X.X.
..XXXXXX.X.
.X...X...X.

X.X...XX..X
XXXX..XX.X.
X..X.......
X..X.......
XXXX..XX.X.
X.X.X.XX..X
...X.XXXXX.
XX..X.XX...
X.X...X....
XX.X.X.X.XX
.XXX...X.XX
.X.X.XXXXXX
.X.XX.X..XX
.XX.XX.XX.X
.XX.XX.XX.X

X.....X
.XX..X.
.XXXX.X
.XXXX.X
.XX..X.
X.....X
XXXXX..
XXXXX..
X....XX
.XX..X.
.XXXX.X

...X.XX.X..
X...XX...XX
X..XX....XX
.XXXX.X.XXX
.XX.XXX.X..
...X.XXXXXX
X.X..XXX.XX
X.X..XXX.XX
...X.XXXXXX
.XX.XXX.X..
.XX.X.X.XXX
X..XX....XX
X...XX...XX

.XXXXX..X
...XX.XX.
X.XXX....
X.XXX....
...XX.XX.
.XXXXX..X
XXXXXX..X
..XX...X.
...X..XX.
XX..X.XX.
.XX......
....X.XX.
..XXXX..X

.X....X.XXX...XXX
.XXXXXX..XX.X..X.
.XXXXXX..XX.X..X.
.X....X.XXX...XXX
X.XXXX.X....X.X..
...........X.....
.X.XX.X.X.XX....X
X......XX.XX...X.
X..XX...XX.....X.
.XX..XX..X.XXX..X
.X.XX.X.X...X.X..
X......X.X..X.X.X
XXX..XXX.X..XX...
.XXXXXX.X.X.X.XX.
X..XX..X.X....X.X
..X..X.....X.XX..
X.XXXX.X.X.XX.X..

...XX.XX.XX..
..X.....X..X.
..X.X....X.X.
XXX.XX..XX.XX
...X.XXXX.X..
..XX..XX..XX.
..X.X.XX.X.X.
XX..........X
.....X..X....
...X.XXXX.X..
..XX..XX..XX.

XX.X.X.X.
.XX..XXXX
...X.X...
..X.X..XX
.XXXX....
.XXX.....
.X....XXX
...XX..XX
X.X.X.X..
X.XX.X.XX
XX...X.XX
XXXX..X..
.X..XX...
.X...XXXX
XX...X...
...XXXX..
...XXXX..

......XX.XX.X
........X..X.
XX..XX.X.XX.X
X....XXX....X
.XXXX.X......
XX..XXXXX..XX
..XX..X.X..X.
XX..XXXXXXXXX
X....X.X..X.X

.XX.XX.X.XX..XX.X
XXXXXXX..X....X..
XXXX.X.X.X....X.X
X..XX...X..XX..X.
XXXX.X...X.XX.X..
X..X...........X.
.XX..X...........
XXXX.X.XXXXXXXXXX
XXXX.XX.XXXXXXXX.
XXXX..XX.XX..XX.X
....X.XXXXXXXXXXX
X..X.......XX....
....X.XXX.X..X.XX

X..XX..XXXXXXXX..
XX.XX.XXX.XX.X..X
..X..X...X.X..X..
...XX....X...X..X
XX....XXXX.X.XXX.
XX....XXXXX.XX...
XX....XXX.X....X.
..XXXX.X..XX....X
X..XX..X.X.X..XXX
.........X.X....X
XX.XX.XXXXX.XX..X
XX.XX.XXXXX.XX..X
.........X.X....X

...XXXX.XXXXX..
.XXX..XXXX.X...
.XXX..XXXX.X...
....XXX.XXXXX..
XXXX...X.X..XXX
XX.XXX.XX..XXXX
.XXXXXXXX.XXXXX
XXX.XXX.X......
XXXX..XXXX.XX.X
XXXX..XXXX.XX.X
XXX.XXX.X......
.XXXXXXXX.XXXXX
XX.XXX.XX..XXXX
XXXX...X.X..XXX
....XXX.XXXXX..

XX.XX..XX
XX.XX..XX
XX.XXXX.X
.....X..X
XXX.X.X..
.XX..X...
XXX......
XXX......
.XX..X...
.XX.X.X..
.....X..X
XX.XXXX.X
XX.XX..XX

X.X.X..XX...XX.
X.X..X.X.XX....
XX......X.X.XX.
.XX.XX..XXX.XX.
...X.XXX.X.X..X
...X.XXX.X.X..X
.X..XX..XXX.XX.

XX.X......X.XXX
X...X..X.X...XX
XX.X......X.XXX
XXXXX.XX.XXXXXX
XXX.XX..XX.XXXX
X.XX.XXXX.XX.XX
..X.X....X.X...
.XX........XX..
X..XX.XX.XX..XX
....X....X.....
..XXXX..XXXX...
.XX.XX..XX.XX..
XXX.X....X.XXXX
.X.X.XXXX.X.X..
XX...XXXX...XXX
..X..XXXX..X...
XXX...XX...XXXX

..XXXXX
XX.....
.X..X..
.X..X..
XX.....
..XXXXX
..X...X
XX....X
XXXXX..
..XXX..
X.XXX..
XXXXX..
XX....X
..X...X
..XXXXX
XX.....
.X..X..

X.XX.X.XXXXXX
XX.X..XXXXXXX
X...X.X......
X...X.X......
.X.X..XXXXXXX
X.XX.X.XXXXXX
X..XXX...XX..
X.XXXX.X....X
..XXXX.X.XX.X

X.XXXXXXX..XX...X
..XX..XXX.X.XX..X
..X...X.XXXXXX...
..X...X.XXXXXX...
..XX..XXX.X.XX..X
X.XXXXXXX..XX...X
...XXXX...XXXXX.X
X..X.X..XX..X.XXX
.X.X.X..XXX..X.X.
XXXX..X.XXX.X....
.....XX.X.XXXX..X
.....XX.X.XXXX..X
XXXX..X.XXX.X....
.X.X.X..XXX..X.X.
X....X..XX..X.XXX

XX..XXX.XX.XX
XXX..X.....XX
.XX..X.XXX.XX
...XX.....X..
XXX..X.XXX.XX
..X...X.XXXXX
..XX..X.X....
X.XX.XXXX.X..
.X...X.X.X.XX
.X...X.X.X.XX
X.XX.XXXX.X..
..XX..X.X....
..X...X.XXXXX
XXX..X.XXX.XX
...XX...X.X..
.XX..X.XXX.XX
XXX..X.....XX

..X..X.X.
.XX.X.XXX
.XX.X.XXX
..X..X.X.
.XXX.XX..
.XX...X..
.X......X
X.X.XX.X.
X.X.XX.X.
.X......X
.XX...X..
.X.X.XX..
..X..X.X.

.XX..X.
.X.XX..
X.X....
X.X....
.X.XX..
.XX..X.
X...X..
XXX...X
.XXX...
XX.X.XX
XX.X.XX
.X.X...
XXX...X
X...X..
.XX..X.

X.XXX..X.XX.X
X..XXX.XXXXXX
.X.X..XX....X
.X.X..XX....X
...XXX.XXXXXX
X.XXX..X.XX.X
X.X...X..XX..
X.....X......
XXXX.X..X..X.

.X.XXXX
..X..X.
X..XX..
..X....
XX...XX
.X.XX.X
.XX..XX
.XX..XX
.XXXX.X
XX...XX
..X....
X..XX..
..X..X.
.X.XXXX
.X.XXXX
..X..X.
X..XX..

.X...XXX.....
..X.X.XXXXXX.
XX.....X.X..X
.X.....X.X..X
..X.X.XXXXXX.
.X...XXX.....
..X..X...X..X
X..X..X....XX
X..X..X....XX

XX.XX.XX.X....X
..XXXXXX.X..XXX
....XX..X.X..XX
X..X.XXX.XX....
X..X.XXX.XX....
....XX..X.X..XX
..XXXXXX.X.XXXX
XX.XX.XX.X....X
.X....XXXX.....
XX.XXX.XX.XX.X.
......XXXX..X..
.XXX.X.X.....XX
..XX.X..X....X.
XXX.....XX...XX
XX.XXXXX....XX.
...XXXX..X.X..X
...XXXX..X.X..X

XX.XX.X...XX.X.X.
XX.XX.X...X.X.XX.
XX..XX.X.XXX.X.X.
...X.X.X.X....XXX
XX.XX....X....XX.
XX.XX....X....XX.
...X.X.X.X....XXX
XX..XX.X.XXX.X.X.
XX.XX.X...X.X.XX.
XXXXX.X...XX.X.X.
..X.X...XXX.X.XXX
..X...X....XX.XX.
..X..X..X.XXX.X..
XXXX..X.X.XXXXX.X
...XXXXXX.XXXX..X

..XXX.XXXX.XXX.
XXXX.X.XX.X.XXX
...X.X....X.X..
..XX..X..X..XX.
XX.XXXX..XX.X.X
XX.X.X.XX.X.X.X
.....X....X....

...XX..XX.XX.
.XXX...X.XX..
X.X...X.X....
X.X...X.X....
.XXX...X.XX..
X..XX..XX.XX.
XXX.X..X....X
X..XX.X....XX
X..X..XX..X.X
.....XX..X..X
.XXXX..X..XXX
XXXXX..XXX.X.
.X.X.....X.XX
.X.X.....X.XX
XXXXX..XXX.X.
.XXXX..X..XXX
.....XX..X..X

X.XXX.X...XXX.X
....X..X..X.XX.
X..X..X...XXX.X
.XXX.X..X.XXXXX
X...X....XX..X.
XXXX.X.....XXXX
...XXXXX.XX..XX
...XXXXX.XX..XX
XXXX.X...X.XXXX
X...X....XX..X.
.XXX.X..X.XXXXX
X..X..X...XXX.X
....X..X..X.XX.
X.XXX.X...XXX.X
X.XXX.X...XXX.X

XXXXXX.XX.X
X.XX.XXXXXX
..XX..XXXX.
X.XX.X....X
X....X.XX.X
.XXXX......
X...XX....X

X.XXXXX
..X.X..
X...X..
XX.XX..
....XX.
XXX.XXX
...XXXX
.XX....
X.X.XXX
X..X.XX
X....XX
..XX...
..XX...

..X...XX...X.
...X..XX..X..
...XX....XX..
.....X..X.X..
XXXXX.XX.XXXX
XX...X..X...X
...X.XXXX.X..
XXXX.XXXX.XXX
..XX.XXXX.XX.
XXX.XXXXXX.XX
XXX..X..X..XX

..XXXX...X.XX..XX
XXXX.X.......XX..
......XX..X..XX..
X.....XXXXX......
.X.XXX...X.......
..X.X..X.X...XX..
....XX.XX.XXXXXXX
XXX.XX...X.X.XX.X
.XX....XXX.XX..XX

..XX....XX.X..X..
..XX....XX.X..X..
XX..XXX.......X..
X.XX.XXX..X.X.XXX
.X..XXX.XXXXXXX.X
.XXXX.XXX.XX....X
.X..X.X..X..XX.X.
X....XX.X.XX..X..
XXXXXXXXX.X.XXXXX
.......X...X..XXX
XXXXXXX.X..X...XX
XX..XXXX...XXXX..
.X..X..X.X.XX....

....X..
XX.XX.X
X.XXXX.
X.XXXX.
XX.XX.X
XXX..XX
..X..X.
..X..X.
XXX..XX

.XXXX..
XXXXXX.
.XXXX.X
X....X.
..XX...
..XX...
X....X.
.XXXX.X
.XXXX..

XXX.XX.XXXX....
.XX....XX...XX.
..XX..XX.......
..XX..XX...X..X
.X.XXXX.X.XX..X
XX.X..X.XXX.XX.
...X.......X..X

XXXXX.X.XX.
X..XX......
X..X..X....
.XX....X..X
XXXX....XX.
.....XX.XX.
....XXXXXXX
....X...XX.
XXXXXX.....
X..X..XX..X
XXXXX.X.XXX
XXXXX......
.XX.X..X..X
X..X.XXXXXX
....X..X..X

XXX..XX.XX.XX.XX.
XX.X.X....X.X..X.
X.X.....X.XX.X..X
..XX.XXX.X.X.....
...XX..X...X.....
..XX.X..XX...XXXX
.X..X.X..X...X..X
XXX...X...X.X....
X..XX....X.XXX..X
X.X.X.......XXXXX
.X.XX...X.XXX....
.X.XX...X.XXX....
X.X.X.......XXXXX

.X...X.XX.X...X..
X.XX.X.XX.X.XX.XX
...X..X..X..X....
..X.XX.XX.XX.X...
XXX..X....X..XXXX
XXX..X....X..XXXX
X..X.XXXXXX.X..X.

.XX..XXX.
X..X...XX
X..X.XXXX
.......XX
X..X.....
XXXXX....
.XX.XX.XX

XXXX.XXX.X.
XXXX...X.X.
X...X....XX
X..XX..X.X.
X..X.X.X..X
X..X.XX.XX.
X..XX..XX.X
.XX..XX.X.X
.XX.X..XXX.
XXXXXXXX...
XXXXXXXX...
.XX.X..XXX.
.XX..XX.X.X

..X.XX.XXXX
XXX.XXX.XX.
XX..XXX....
XX...XX....
XXX.XXX.XX.
..X.XX.XXXX
.X...XX....
...XX......
.X.X.XX....
X.XXX.X....
..X..XX.XX.

XX...X...X...
XX...X..XX...
X...X...XXXXX
X.X..X.......
X.XXXXX.X....
XXXXX..XXXX..
..XX.XX.X..XX

.XXXXX.
.X.X..X
X.XXX.X
.X.XXXX
X.X..X.
.X.XX..
.X.XX..
X.X..X.
.X.XXXX
X.XXX.X
.X.X..X
.X.XXX.
.X..XXX
.X..X..
.X..X..

X.X..X.X.XXXXXX.X
X.X..X.X.XXXXXX.X
.........X.XX.X..
...X..X..XX..XX..
.X.X.XXXX......XX
.XXX.XX..XXXXXX..
XXX..XXXX.XX.X.XX
..XXXXXXX.X..X.XX
X..XX...X.XXXX.X.
XX.XX.X..XX..XX..
XX.X.X.X..X..X..X
X..XX.X..........
XX.XX...XX.XX.XX.
.X..XXXX.XXXXXX.X
..XXX..XX.XXXX.XX
....XXXX..XXXX..X
X..XX....X....X..

..XXXX..X..XX..
.X....X.X.X..X.
.XX..XX..XXXXXX
X..XX..XXX.XX.X
.XX..XX....XX..
....X...X..XX..
..XXXX..X......

XX.XXX.XXXXX...
XX.XX...X..X.XX
XX.XX...X..X.XX
.X.XXX.XXXXX...
XXXX.XXXX.X.XXX
XX...XXXXX..XXX
X.XXX.X.XXXXXXX
..XX.X..XXXX...
.XXX..XXXXX.X..
....X.X.XX..XXX
X....X.X.XX.X..
X..X..X.XXXXX..
....X..XXXX.XXX
.X...XXXX.XXXXX
XXX.X.X.XX.XXXX

X..X..X..XX.X
X..X.X.XX.X.X
.XX...X..X...
XXXX.X....X.X
X..XX.XXXX.XX
X..XX......XX
XXXX...XX...X

X...XX.XX.X
XX.XX..X.X.
.XXX..XX..X
.XXX..XX..X
XX.XX..XXX.
X...XX.XX.X
X........X.
X........X.
X...XX.XX.X
XX.XX..XXX.
.XXX..XX..X

.X..XXXXXXXX.
X...X....X.XX
...X.X.X.X..X
XXXXXXXX.X..X
XXXXXXXX.X..X
...X.X.X.X..X
X...X....X.XX
.X..XXXXXXXX.
XX.XXX.X...X.
X...XX..X.XX.
XXX.....X...X
X.X.X...X....
X....XXXX..X.
X.....XXX..XX
..XXXXX..X.X.
X.XX.X.X...XX
X.XX.X.X...X.

X.XXX...XXXX...XX
...XX..X....X..XX
.XX..X.XX..XX.X..
X.X.X....XX....X.
XX....X......X...
XX.....X....X....
....X.X......X.X.
X.XX..X.XXXX.X..X
X...XX.X.XX.X.XX.
X...XX.X.XX.X.XX.
XXXX..X.XXXX.X..X
....X.X......X.X.
XX.....X....X....

X.XXX.X...X..
XX...X...X.XX
.......XXX...
.XXXX.XX.XXXX
XX.XXX..XX...
....XXXXX..XX
XX.XXX.X.XXXX
.XXX..XX.X.XX
.XX.X....XXXX
..X.....X.XXX
XXXX..X....XX
XXX.....X..XX
X.X.X.X.X.X..
X.X.X..XX.XXX
X.X.X..XX.XXX
X.X.XXX.X.X..
XXX.....X..XX

XXX..XXX.X.X.
X.X..X.XXX..X
X.X..X.XXX.X.
XX....XXX.X..
.X....X..XXX.
...XX...X..X.
X..XX..X..X.X
X..XX..X..X.X
...XX...X.XX.
.X....X..XXX.
XX....XXX.X..
X.X..X.XXX.X.
X.X..X.XXX..X
XXX..XXX.X.X.
.XXXXXX.X..X.

X..XX.X...X.X..
.X...XXX.XXXX.X
.XXX.XX.X......
X.X.X..XXXX....
X.X.X..XXXX....
.XXX.XX.X......
.X...XXX.XXXX.X
X..XX.X...X.X..
.XX.XXX.X.X.XX.
.XX.XXX.X.X.XX.
X..XX.X...X.X..
.X...XXXXXXXX.X
.XXX.XX.X......

....X.XXXX....XXX
...XX.X.XXXXXXXX.
.....X...........
XXXX..X..........
.XX.X.X.X.X..X.X.
XXXX.X..X.XXXX.X.
X..X.XXXX.X..X.XX
....X....X.XX.X..
XXXXX..X...XX...X
.XX.XXX...XXXX...
.XX..XX...X..X...

X..X..XXX...X.X
XXXX..X.X....X.
........XX...XX
XXX.X.X.X....XX
..X...X.X.XXX..
.....XX...X..XX
...X..X.X..XXXX
XX.X...XX..X...
XX.X...XX..X...
...X..X.X..XXXX
.....XX...X..XX
..X...X.X.XXX..
XXX.X.X.X....XX
........XX...XX
XXXX..X.X....X.

.....X.XX.X..
XX....XXXX...
..X...X..X...
XXX.XX....XX.
XX...XX..XX..
XXXX.X....X.X
XX..XXXXXXXX.
.....XX..XX..
XX.XX..X...XX
..XXX......XX
XX...X.XX.X..
XX..XXXXXXXX.
...XXXXXXXXXX
..XX...XX...X
...XXX.XX.XXX

XX......XXXX..X..
XX.X..X.XXX....XX
XX..XX..XX.X.X..X
XX..XX..XX.X.X..X
XX.X..X.XXX....XX
XX......XXXX..X..
.XX.XX.XX...X..XX
.X..XX..X.XX....X
XXXXXXXXXX...XXXX
XXX.XX.XXX...X.X.
X.X....X.X..XXX..
.X..XX..X.X.X.X.X
.XXXXXXXX..X..XX.
XXXXXXXXXX.XXX...
..XXXX.X..X.XX.X.
...X..X...XX.XX..
XX.X..X.XXXX..X.X

..XX.....X..X..
.X..X.X.X.XX.X.
X.XX.X.X.XXXX.X
XXXXXX.XXXXXXX.
XX..XX.X..XX..X
XX..XX..X.XX.X.
......X..X..X..
X....XX.XX..XX.
.X..X.XXXX..XXX
XXXXXX..XX..XX.
XXXXXX.XXX..XXX
.XXXX...X.XX.X.
X.XX.X.X......X

..XX.X.
XXXXXX.
X.XX.X.
.XXXX.X
XX..XX.
..XX..X
.XXXX.X
.XXXX.X
..XX..X

XXX..X.
XX..X.X
...XX.X
..XX..X
...XX..
XX..X.X
..XX.X.
..X....
......X
......X
..X....
..XX.X.
XX..X.X
...XX..
..XX..X
...XX.X
XX..XXX

.XXX...XX....
XX..XX.X..XX.
XXXX..XX..XX.
X...XX...X...
.XXXXXXXX.XX.
..XXX..XXXXXX
....XX.XX....
X...XX.X.XXXX
..XX.XX.X....
X...XX.X..XX.
..X.XXX...XX.
..X.XXX...XX.
X...XX.X..XX.
..XX.XX.X....
X...XX.X.XXXX

....X.XXXX.
......XX..X
.....X..XX.
X..XXX.XXXX
X..XX..XXXX
X..XX.XX...
......XXX.X
....X..X.XX
.....X..X..
XXXX.X..XX.
XXXX.X..X..
XXXX.X..X..
XXXX.X...X.
.....X..X..
....X..X.XX
......XXX.X
X..XX.XX...

XXXXXXXX......X
...XX...XXXXXX.
...XX...XX..XX.
.XX..XX..X..X..
..XXXX..X.XX.X.
X......X......X
......X........

XXX...X.XXXXX
XXX...X.XX.XX
XXXXXXXXX.X..
..X.X.X.XX..X
....X..X.....
..XXX.XX..XX.
XX.XXXX..XXXX

...XX.X..
..XX.X.XX
X.XX..XXX
.X..XXXXX
..X...X..
.X.XXXXXX
.XXXXXXXX

...XXX.
...XXX.
..X.X..
.X.XX.X
.X...X.
X.XXX.X
X.XXX.X
.X...X.
.X.XX.X
..X.X..
X..XXX.

.XX..XX
....X..
XX..X..
X.X....
X.X....
XX..X..
....X..
.XX.XXX
X..X.XX

.XX.XXX.X.X
X....XX..X.
X.X.X.XXXX.
X.X.X.XXXX.
X....XX..X.
.XX.XXX.X.X
X.X.XXXXXX.
XX.X..XX..X
X...X...X.X
XX...X..X.X
.X.X.X.X...
.X.X.X.X...
XX...X....X
X...X...X.X
XX.X..XX..X

XX.X..XXX...X
XXX.XX......X
XXXX.X..XXX.X
....XX....X.X
...X.XX.X...X
XXX.X..XXXXXX
.....XXXX....
.....XXXX....
XXX.X..XXXXXX
...X.XX.X...X
....XX....X.X
XXXX.X..XXX.X
XXX.X.......X
XX.X..XXX...X
XXXX.XXXXXXXX

....X....XX
XXXX..X.X..
XXXXX....XX
X..XX...X.X
X..X.....XX
X..X..X..XX
....X.X.X..
....XXX.XXX
......X.XXX
XXXXXXX..XX
XXXXXX.X.XX

.XXXX..XXXXXX
X....XX..XX..
X.XX.X..X..X.
......X.XXXX.
XXXXXXX..XX..
X.XX.XX..XX..
.XXXX.X.XXXX.
XXXXXXX.X..XX
XX..XX.X.XX.X
X....X.X....X
.XXXX..X.XX.X
X....XX......
X....XXX....X
......X.XXXX.
.XXXX..XXXXXX

...XX....
XX...X..X
X..XXXXX.
.X.XX.X..
..XXXX..X
XXX...XXX
XXX...XXX
...XXX..X
.X.XX.X..
X..XXXXX.
XX...X..X
...XX....
.X.X..X..
.X.X..X..
...XX....
XX...X..X
X..XXXXX.

..XX..X..X.X.X.XX
..XX..X..X.X...XX
....XX.XXXX..X.XX
X.X...X.XXX.XXX..
XX.X.XXX.X...X...
XX.X.XXX.X...X...
X.X...X.XXX.XXX..
....XX.XXXX..X.XX
..XX..X..X.X...XX
..XX..X..X.X.X.XX
XX....XXX..X..X.X
......X.X.....XX.
X.X..XXXXX......X
.XXX..XX.X.X.X..X
..X.XXX..X.XX.XXX

XXX.XX....XX.XX
X..XX.XXXX.XX..
X..XX.XXXX.XX..
XXX.XX....XX.XX
.X..XX.XX.XX..X
XX.....XX.....X
..X.XX....XX.X.
.X....X..X....X
XX.XX.X..X.XX.X
..X..........X.
X..XX......XX..
X......XX....X.
XX...XX..XX...X

.....XX..XX..XX
..X..X.XX.XXX.X
..XX..X..XX..X.
..X.XX.X.XX.X.X
X.X..XX.XXXX.XX
..X.....XXXX...
.X...X.XXXXXX.X
X.X............
X.X............
.X...X.XXXXXX.X
..X.....XXXX...
X.X..XX.XXXX.XX
..X.XX.X.XX.X.X

.......XX.X
.XX..X.X..X
.XX..X.X..X
.......XX.X
X..XXX..XXX
XXXX.XXXXX.
X..XXX.XXXX
......XX.XX
.X..XXXX.XX
X..XXX...XX
.......XX..

XX.X.X.X..XX...
X.X..XXXX.XX..X
...XX..X.X.XXXX
X.XX...XXXXXX..
X..XXX..X.X.X..
XX..XXXX.XX..XX
X..X....X.X.XXX
X..X....X.X.XXX
XX..XXXX.XX..XX

.X.......
.X....X..
.X....X..
.X.......
.XXXX..XX
..XX.XX.X
.X.......
XXXX....X
XXX..XX..
.XX.X..X.
.XXXXXXXX

..X....X...
..XXXXXX...
..X....X...
X..X.XX..XX
X...XX...XX
.XX....XX.X
X...XX...X.
X...XX...X.
.XX....XX.X

.XXX..X
.XX....
..XXXX.
.XX.XXX
X..XXX.
.XXX.X.
.XXX.X.
X..XXX.
..X.XXX
..XXXX.
.XX....
.XXX..X
.XXX..X
.XX....
..XXXX.

.XX..XX
X......
..XXXX.
X..XX..
.XX..XX
X..XX..
XXX..XX

XX....XX.
....X.X.X
X...X.X.X
XX....XX.
..X..XX..
...XX..X.
.X......X
.X.X.X.XX
.X.X.X.XX

XXX...X.X
......X.X
XXX.XXX..
XXX.XXX..
......X.X
XXX...X.X
..XXXXX.X
XX.....X.
.XX..XX.X
...XXXXX.
XXXX..X.X

...X....X.....X
..XXXXXXXX..X..
..X......X..XX.
..XXXXXXXX...X.
XX..XXXX..XX..X
XX.XXXXXX.XX...
....X..X.....XX
XXX..XX..XXXXX.
.X..X..X..X...X
..X.XXXX.X...X.
...XXXXXX....XX
XXX......XXXXX.
XX..XXXX..XX..X
XXX......XXXX.X
..X.X..X.X..XX.
XXXX.XX.XXXXX.X
..X......X..XXX

...X..XXXX.XX...X
.XX.X.X..X.X.XX.X
.XX...XXXX...XX.X
..XXX......XXX..X
X.X.X......X.X.XX
...X.XX..XX.X....
.X...X....X...X.X
XX..X......X..XX.
X..X.XXXXXX.X..XX
XXX...XXXX...XXXX
XXX...XXXX...XXXX
X..X.XXXXXX.X..XX
XX..X......X..XX.
.X...X....X...X.X
...X.XX..XX.X....

.X.X.XX.X.XXX
X.X.X..X.X.XX
XXX..XX..XXXX
.XX......XX..
.XX.X..X.XX.X
XXXXXXXXXXXX.
XXXXXXXXXXXX.
.XX.X..X.XX.X
.XX......XX..
XXX..XX..XXXX
X.X.X..X.X.XX

XX...X...X...XXXX
X...XX.XX.....XXX
X...XX.XX.....XXX
XX...X...XX..XXXX
X.X.XXXX.........
XXXXX..X...X.XX..
X.....X.X....X...
.X.....XX..XX.X..
XX.X.X.XXX..XXXXX

XX....X.X...XXXX.
XX....X.X...XXXX.
XXX.XX.X.XX.XXXXX
X..X.XX.XX..XX...
.X..X..........XX
XXX.XXXX.X.XXXXX.
.X....X.X.X.X.X..
.X...XX.XXX.X..XX
.X....X.XXX.X..XX
.X....X.X.X.X.X..
XXX.XXXX.X.XXXXX.
.X..X..........XX
X..X.XX.XX..XX...
XXX.XX.X.XX.XXXXX
XX....X.X...XXXX.

X.XX.XX
XX..XXX
..XX...
.X.X...
X......
..XXX..
.XXXX..
X.XXX..
..XXX..

XX.......XX....
XXXX.X..X..X..X
X...XXXX....XXX
XXX.X.X..XX..X.
XX..X.X..XX..X.
X...XXXX....XXX
XXXX.X..X..X..X
XX.......XX....
..XX....XXXX...

X.X.XXXXXX.X.XXXX
XX.X.X..X.X.XXXXX
XXX.X....X.XXXXXX
X..X..XX..X..X..X
.X...X..X...X.XX.
X..XXX..XXX..XXXX
.XX..X..X..XX.XX.
.X...XXXX...X.XX.
....XX..XX.......
..XX.X..X.XX..XX.
.....X..X..X..XX.
XX.XXX..XXX.XXXXX
.X.XX....XX.X....

.XXXX..XXXX..X..X
X.X..XX..X.X.XX..
..XX....XX....XX.
.X..X..X..X.X..XX
X....XX....X.X.X.
.X..X..X..X.XXXXX
X....XX....XX..XX

END
