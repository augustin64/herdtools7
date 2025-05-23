type Color of enumeration {RED, GREEN, BLUE};
type SubColor subtypes Color;
type Status of enumeration {OK, ERROR};

func main() => integer
begin
  var b1: boolean = TRUE as boolean && FALSE as boolean;
  // Binary operations on bitvectors remove all bitfields from the result type.
  var a : bits(8) = Ones{8} as bits(8) {[0] flag} XOR Zeros{8} as bits(8) {[7] flag};
  // The next statement is illegal: both bitvectors must have the same length for XOR.
  // var - : bits(8) = Ones{8} XOR Zeros{7};
  var b : bits(8) = Ones{8} as bits(8) {[0] flag} + (1024 as integer);
  var c : bits(16) =  (Ones{8} as bits(8) {[0] flag}) ::
                      (Zeros{8} as bits(8) {[0] flag});

  var d : boolean = (5 as integer{1..10}) < (6 as integer{1..10});
  var e : boolean = (Ones{8} as bits(8) {[0] flag}) ==
                    (Ones{8} as bits(8) {[7] flag});
  // The next statement is illegal: both bitvectors must have the same length for ==.
  // var - : boolean = Ones{8} == Zeros{9};
  var f : boolean = (RED as Color) != (GREEN as SubColor);
  // The next statement is illegal: comparing labels declared in
  // different enumerations is not allowed.
  // var - : boolean = RED != OK;

  var g : integer{25} = (5 as integer{5}) * (5 as integer{5});
  var h : integer = (5 as integer{5}) * (5 as integer);
  var i : integer{20, 24..25, 28, 30, 35..36, 42} =
          (5 as integer{5..7}) * (5 as integer{4..6});
  // The next statement is illegal, since 5 does not divide by 2.
  // var -  = (5 as integer{5..10}) DIV (2 as integer{2});

  var j : real = 5.5 ^ 7;
  var real_pow_int : real = 5.0 ^ (5 as integer{0..10});

  // String concatenation first converts literals to their string representation.
  var k : string = 0 :: '1' :: 2.0 :: TRUE :: "foo" :: RED;

  return 0;
end;
