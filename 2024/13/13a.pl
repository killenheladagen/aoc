#!/usr/bin/perl
use v5.20;
use feature qw(signatures);
no warnings qw(experimental::signatures);
use Scalar::Util::Numeric qw(isint);

sub fewest_tokens($ax,$ay,$bx,$by,$px,$py) {
    #print "fewest_tokens($ax,$ay,$bx,$by,$px,$py)\n";
    my $cheapest = 999999;
    for(my $na = 0;$na <= 100; $na++) {
        my $x = $na * $ax;
        my $y = $na * $ay;
        next if ($x > $px || $y > $py);
        my $nbx = ($px - $x) / $bx;
        my $nby = ($py - $y) / $by;
        next if ($nbx != $nby);
        next unless isint $nbx;
        my $cost = 3*$na+$nbx;
        $cheapest = $cost if $cost < $cheapest;
    }
    return ($cheapest == 999999) ? 0 : $cheapest;
}

sub fewest_tokens_for_file($file) {
    my $sum = 0;
    my ($ax,$ay,$bx,$by);
    open(FH, "$file") or die;
    while(<FH>) {
        if (/^Button A: X\+(\d+), Y\+(\d+)/) {
            $ax=int($1);
            $ay=int($2);
        }
        if (/^Button B: X\+(\d+), Y\+(\d+)/) {
            $bx=int($1);
            $by=int($2);
        }
        if (/^Prize: X=(\d+), Y=(\d+)/) {
            my $px=int($1);
            my $py=int($2);
            $sum += fewest_tokens($ax,$ay,$bx,$by,$px,$py);
        }
    }
    return $sum;
}

my $x = fewest_tokens_for_file("test.txt");
printf("%d\n", $x);
die if ($x != 480);
printf("%d\n", fewest_tokens_for_file("input.txt"));
