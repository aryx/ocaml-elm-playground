(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pascal_disk.mli *)

let hello = {|program Hello;
begin
  writeln('Hello, world!')
end.
|}

let fact =
  {|program Factorials;
{ n! up to 7!: 8! is 40320, more than a 16-bit integer holds }
var i: integer;

function fact(n: integer): integer;
begin
  if n = 0 then fact := 1
  else fact := n * fact(n - 1)
end;

begin
  for i := 0 to 7 do
    writeln(i:2, '! = ', fact(i):5)
end.
|}

let sieve =
  {|program Sieve;
{ Eratosthenes: cross out the multiples of each prime }
const n = 200;
var prime: array[2..n] of boolean;
    i, j, count: integer;
begin
  for i := 2 to n do prime[i] := true;
  count := 0;
  for i := 2 to n do
    if prime[i] then begin
      write(i:4);
      count := count + 1;
      if count mod 10 = 0 then writeln;
      j := i + i;
      while j <= n do begin
        prime[j] := false;
        j := j + i
      end
    end;
  writeln;
  writeln(count, ' primes below ', n)
end.
|}

let hanoi =
  {|program Hanoi;
{ n disks from one peg to another: n - 1 out of the way, the
  largest across, the n - 1 back on top of it }
var moves: integer;

procedure move(n: integer; from, onto, via: char);
begin
  if n > 0 then begin
    move(n - 1, from, via, onto);
    moves := moves + 1;
    writeln('disk ', n, ' from ', from, ' to ', onto);
    move(n - 1, via, onto, from)
  end
end;

begin
  moves := 0;
  move(3, 'A', 'C', 'B');
  writeln(moves, ' moves')
end.
|}

let queens =
  {|program Queens;
{ Wirth's eight queens (Algorithms + Data Structures = Programs,
  1976, 3.5): a queen per column, each row tried, backtracking }
var a: array[1..8] of boolean;   { no queen on row i }
    b: array[2..16] of boolean;  { nor on the diagonal i + j }
    c: array[-7..7] of boolean;  { nor on the diagonal i - j }
    x: array[1..8] of integer;   { the row of column j's queen }
    i, solutions: integer;

procedure print;
var k: integer;
begin
  for k := 1 to 8 do write(x[k]:3);
  writeln
end;

procedure try(j: integer);
var i: integer;
begin
  for i := 1 to 8 do
    if a[i] and b[i + j] and c[i - j] then begin
      x[j] := i;
      a[i] := false; b[i + j] := false; c[i - j] := false;
      if j < 8 then try(j + 1)
      else begin
        solutions := solutions + 1;
        if solutions <= 3 then print
      end;
      a[i] := true; b[i + j] := true; c[i - j] := true
    end
end;

begin
  for i := 1 to 8 do a[i] := true;
  for i := 2 to 16 do b[i] := true;
  for i := -7 to 7 do c[i] := true;
  solutions := 0;
  try(1);
  writeln(solutions, ' solutions')
end.
|}

let scopes =
  {|program Scopes;
{ add, inside sum, changes sum's total: lod 1,5 and str 1,5, the
  static link followed once, however deep add's recursion goes }

procedure sum(n: integer);
var total: integer;

  procedure add(k: integer);
  begin
    total := total + k;
    if k > 1 then add(k - 1)
  end;

begin
  total := 0;
  add(n);
  writeln('1 + ... + ', n, ' = ', total)
end;

begin
  sum(10);
  sum(100)
end.
|}

let parity =
  {|program Parity;
{ each function calls the other: one must be declared before its
  body, forward, since the compiler reads the text once }

function isodd(n: integer): boolean; forward;

function iseven(n: integer): boolean;
begin
  if n = 0 then iseven := true else iseven := isodd(n - 1)
end;

function isodd;
begin
  if n = 0 then isodd := false else isodd := iseven(n - 1)
end;

begin
  writeln('7 is odd: ', isodd(7));
  writeln('10 is odd: ', isodd(10))
end.
|}

let points =
  {|program Points;
type point = record x, y: integer end;
     polygon = array[1..4] of point;
var square: polygon;
    corner: point;
    i: integer;

procedure shift(var p: point; dx, dy: integer);
begin
  p.x := p.x + dx;
  p.y := p.y + dy
end;

{ the shoelace formula: twice the area is the sum of the cross
  products of the corners taken in turn }
function area(var s: polygon): integer;
var k, next, twice: integer;
begin
  twice := 0;
  for k := 1 to 4 do begin
    next := k mod 4 + 1;
    twice := twice + s[k].x * s[next].y - s[next].x * s[k].y
  end;
  area := abs(twice) div 2
end;

begin
  square[1].x := 0; square[1].y := 0;
  square[2].x := 4; square[2].y := 0;
  square[3].x := 4; square[3].y := 3;
  square[4].x := 0; square[4].y := 3;
  writeln('area: ', area(square));
  for i := 1 to 4 do shift(square[i], 10, 20);
  corner := square[3];
  writeln('corner 3 is now at (', corner.x, ', ', corner.y, ')');
  writeln('area: ', area(square))
end.
|}

let guess =
  {|program Guess;
var secret, guess, tries: integer;
begin
  secret := random(100) + 1;
  tries := 0;
  writeln('I am thinking of a number from 1 to 100.');
  repeat
    write('Your guess? ');
    readln(guess);
    tries := tries + 1;
    if guess < secret then writeln('Too small.')
    else if guess > secret then writeln('Too big.')
  until guess = secret;
  writeln('Right, in ', tries, ' tries!')
end.
|}

let files =
  [ ("HELLO.PAS", hello); ("FACT.PAS", fact); ("SIEVE.PAS", sieve); ("HANOI.PAS", hanoi); ("QUEENS.PAS", queens);
    ("SCOPES.PAS", scopes); ("PARITY.PAS", parity); ("POINTS.PAS", points); ("GUESS.PAS", guess) ]
