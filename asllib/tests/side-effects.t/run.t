  $ aslref binop-read-read.asl
  $ aslref binop-read-write.asl
  $ aslref --use-conflicting-side-effects-extension binop-read-write.asl
  File binop-read-write.asl, line 11, characters 10 to 31:
    let y = set_and_return () + X;
            ^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects WritesGlobal "X" and ReadsGlobal "X"
  [1]
  $ aslref binop-write-write.asl
  $ aslref --use-conflicting-side-effects-extension binop-write-write.asl
  File binop-write-write.asl, line 11, characters 10 to 47:
    let y = set_and_return () + set_and_return ();
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects WritesGlobal "X" and WritesGlobal "X"
  [1]
  $ aslref binop-read-write-diff.asl
  $ aslref binop-write-write-diff.asl

  $ aslref bool-binop-write-write.asl
  Should print.
  Should print.

  $ aslref binop-throw-read.asl
  E caught
  $ aslref binop-throw-write.asl
  E caught
  $ aslref --use-conflicting-side-effects-extension binop-throw-write.asl
  File binop-throw-write.asl, line 18, characters 12 to 43:
      let y = throwing () + set_and_return ();
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects ThrowsException "E" and WritesGlobal "X"
  [1]
  $ aslref binop-throw-throw.asl
  E caught
  $ aslref --use-conflicting-side-effects-extension binop-throw-throw.asl
  File binop-throw-throw.asl, line 11, characters 12 to 37:
      let y = throwing () + throwing ();
              ^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects ThrowsException "E" and ThrowsException "E"
  [1]
  $ aslref binop-throw-caught.asl
  E caught
  $ aslref binop-throw-not-caught.asl
  E caught
  $ aslref --use-conflicting-side-effects-extension binop-throw-not-caught.asl
  File binop-throw-not-caught.asl, line 21, characters 12 to 37:
      let x = throws_E () + caught_F ();
              ^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects ThrowsException "E" and ThrowsException "E"
  [1]
  $ aslref binop-throw-otherwised.asl
  E caught

  $ aslref binop-throw-unknown.asl
  Caught E.
  $ aslref binop-write-unknown.asl
  $ aslref binop-unknown-unknown.asl

  $ aslref binop-throw-atc.asl
  E caught
  $ aslref --use-conflicting-side-effects-extension binop-throw-atc.asl
  File binop-throw-atc.asl, line 16, characters 12 to 41:
      let y = throwing () + performs_atc ();
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects ThrowsException "E" and PerformsAssertions
  [1]
  $ aslref binop-write-atc.asl
  File binop-write-atc.asl, line 5, characters 10 to 11:
    return (1 as integer {2});
            ^
  ASL Execution error: Mismatch type:
    value 1 does not belong to type integer {2}.
  [1]
// We don't need to decide about the following:
// $ aslref binop-atc-atc.asl

  $ aslref constant-func.asl
  $ aslref constant-func-read.asl
  File constant-func-read.asl, line 5, character 2 to line 6, character 19:
    let y = X;
    return x * x + y;
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref constant-func-write.asl
  File constant-func-write.asl, line 5, character 2 to line 6, character 19:
    X = 3;
    return x * x + 3;
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref constant-func-unknown.asl
  File constant-func-unknown.asl, line 3, character 2 to line 4, character 23:
    let y = ARBITRARY: integer {0..3};
    return x * x + 3 + y;
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref constant-func-throw.asl
  File constant-func-throw.asl, line 5, characters 2 to 14:
    throw E {-};
    ^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref constant-func-throw-caught.asl
  File constant-func-throw-caught.asl, line 5, character 2 to line 9,
    character 6:
    try
      throw E {-};
    catch
      when E => return 19;
    end;
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref constant-func-local-var.asl
  $ aslref constant-func-local-type-global-let.asl
  File constant-func-local-type-global-let.asl, line 5, character 2 to line 9,
    character 15:
    let k = x as integer {K};
  
    assert k == x;
  
    return 2 * k;
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref constant-func-local-type-local-let.asl
  $ aslref constant-func-sig-let.asl
  File constant-func-sig-let.asl, line 3, character 0 to line 6, character 4:
  pure func foo(x: integer {0..K}) => integer
  begin
    return x;
  end;
  ASL Type error: expected a pure expression/subprogram.
  [1]

  $ aslref for-var-no-edit.asl
  $ aslref for-var-edits.asl
  $ aslref --use-conflicting-side-effects-extension for-var-edits.asl
  File for-var-edits.asl, line 6, character 2 to line 8, character 6:
    for i = 0 to x do
      x = y * y + x;
    end;
  ASL Type error: conflicting side effects ReadsLocal "x" and WritesLocal "x"
  [1]
  $ aslref for-read-write-global.asl
  $ aslref --use-conflicting-side-effects-extension for-read-write-global.asl
  File for-read-write-global.asl, line 14, character 2 to line 16, character 6:
    for i = 0 to read_X () do
      X = y * y + x ;
    end;
  ASL Type error: conflicting side effects ReadsGlobal "X" and WritesGlobal "X"
  [1]
  $ aslref while-var-edits.asl
  $ aslref repeat-var-edits.asl

  $ aslref for-read.asl
  $ aslref for-write.asl
  File for-write.asl, line 5, character 2 to line 7, character 11:
    let x = X;
    X = x + 1;
    return x;
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref for-write-throw.asl
  File for-write-throw.asl, line 5, characters 2 to 14:
    throw E {-};
    ^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref for-throw-throw.asl
  File for-throw-throw.asl, line 5, characters 2 to 14:
    throw E {-};
    ^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref for-throw.asl
  File for-throw.asl, line 5, characters 2 to 14:
    throw E {-};
    ^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref for-unknown.asl

  $ aslref config-uses-var.asl
  File config-uses-var.asl, line 2, characters 0 to 26:
  config Y: integer = X + 3;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref config-uses-config.asl
  File config-uses-config.asl, line 2, characters 0 to 22:
  config Y: integer = X;
  ^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref config-uses-let.asl
  File config-uses-let.asl, line 2, characters 0 to 22:
  config Y: integer = X;
  ^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref config-uses-constant.asl
  $ aslref config-uses-local-var.asl
  $ aslref config-uses-local-let.asl
  $ aslref config-uses-local-constant.asl
  $ aslref config-uses-var-through-func.asl
  File config-uses-var-through-func.asl, line 5, characters 2 to 11:
    return X;
    ^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref config-uses-config-through-func.asl
  File config-uses-config-through-func.asl, line 5, characters 2 to 11:
    return X;
    ^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref config-uses-let-through-func.asl
  File config-uses-let-through-func.asl, line 5, characters 2 to 11:
    return X;
    ^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref config-uses-constant-through-func.asl
  $ aslref config-uses-atc.asl
  File config-uses-atc.asl, line 3, characters 9 to 10:
    return 0 as integer {10};
           ^
  ASL Execution error: Mismatch type:
    value 0 does not belong to type integer {10}.
  [1]
  $ aslref config-uses-unknown.asl
  File config-uses-unknown.asl, line 3, characters 2 to 36:
    return ARBITRARY: integer {0..10};
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]

  $ aslref assert-read.asl
  $ aslref assert-write.asl
  File assert-write.asl, line 5, character 2 to line 7, character 11:
    let x = X;
    X = x + 1;
    return x;
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref assert-throw.asl
  File assert-throw.asl, line 5, characters 2 to 14:
    throw E {-};
    ^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref assert-atc.asl
  File assert-atc.asl, line 3, characters 9 to 10:
    assert 0 as integer {3} == 2;
           ^
  ASL Execution error: Mismatch type:
    value 0 does not belong to type integer {3}.
  [1]

  $ aslref type-read-config.asl
  $ aslref type-read-constant.asl
  $ aslref type-read-let.asl
  $ aslref type-read-local.asl
  File type-read-local.asl, line 5, characters 18 to 19:
    let y: integer {x} = x;
                    ^
  ASL Type error: expected a symbolically evaluable expression/subprogram.
  [1]
  $ aslref type-read-local-let.asl
  $ aslref type-read.asl
  File type-read.asl, line 3, characters 19 to 20:
  type T of integer {X};
                     ^
  ASL Type error: expected a symbolically evaluable expression/subprogram.
  [1]
  $ aslref type-write.asl
  File type-write.asl, line 5, character 2 to line 7, character 11:
    let x = X;
    X = x + 1;
    return x;
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref type-unknown.asl
  File type-unknown.asl, line 3, characters 2 to 35:
    return ARBITRARY: integer {0, 1};
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref type-func-atc.asl
  File type-func-atc.asl, line 3, characters 9 to 10:
    assert 0 as integer {3} == 2;
           ^
  ASL Execution error: Mismatch type:
    value 0 does not belong to type integer {3}.
  [1]
  $ aslref type-func-local-var.asl
  $ aslref type-local-var.asl
  File type-local-var.asl, line 5, characters 15 to 16:
    var y: bits (x);
                 ^
  ASL Type error: expected a symbolically evaluable expression/subprogram.
  [1]
  $ aslref type-throw.asl
  File type-throw.asl, line 5, characters 2 to 14:
    throw E {-};
    ^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]

  $ aslref assert-atc.asl
  File assert-atc.asl, line 3, characters 9 to 10:
    assert 0 as integer {3} == 2;
           ^
  ASL Execution error: Mismatch type:
    value 0 does not belong to type integer {3}.
  [1]
  $ aslref assert-read.asl
  $ aslref assert-throw.asl
  File assert-throw.asl, line 5, characters 2 to 14:
    throw E {-};
    ^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref assert-write.asl
  File assert-write.asl, line 5, character 2 to line 7, character 11:
    let x = X;
    X = x + 1;
    return x;
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref assert-unknown.asl

  $ aslref rec-assert-throw.asl
  File rec-assert-throw.asl, line 15, characters 9 to 37:
    assert throwing (n - 1, FALSE) == 3;
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref rec-binop-atc-throw.asl
  File rec-binop-atc-throw.asl, line 3, character 0 to line 10, character 4:
  func throwing (n: integer, b: boolean) => integer
  begin
    if b then
      throw E {-};
    else
      return foo (n);
    end;
  end;
  ASL Warning: the mutually-recursive functions throwing, foo have no recursive
  limit annotation.
  File rec-binop-atc-throw.asl, line 15, characters 37 to 38:
    let x = throwing (n - 1, FALSE) * (2 as integer {3});
                                       ^
  ASL Execution error: Mismatch type:
    value 2 does not belong to type integer {3}.
  [1]
  $ aslref --use-conflicting-side-effects-extension rec-binop-atc-throw.asl
  File rec-binop-atc-throw.asl, line 15, characters 10 to 54:
    let x = throwing (n - 1, FALSE) * (2 as integer {3});
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "throwing" and PerformsAssertions
  [1]
  $ aslref rec-binop-read-throw.asl
  File rec-binop-read-throw.asl, line 4, character 0 to line 11, character 4:
  func throwing (n: integer, b: boolean) => integer
  begin
    if b then
      throw E {-};
    else
      return foo (n);
    end;
  end;
  ASL Warning: the mutually-recursive functions throwing, foo have no recursive
  limit annotation.
  $ aslref --use-conflicting-side-effects-extension rec-binop-read-throw.asl
  File rec-binop-read-throw.asl, line 22, characters 10 to 45:
    let x = throwing (n - 1, FALSE) * read_X ();
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "throwing" and ReadsGlobal "X"
  [1]
  $ aslref rec-binop-unknown.asl
  $ aslref rec-binop-read.asl
  File rec-binop-read.asl, line 3, character 0 to line 6, character 4:
  func not_throwing (n: integer) => integer
  begin
    return foo (n);
  end;
  ASL Warning: the mutually-recursive functions not_throwing, foo have no
  recursive limit annotation.
  $ aslref --use-conflicting-side-effects-extension rec-binop-read.asl
  File rec-binop-read.asl, line 17, characters 10 to 42:
    let x = not_throwing (n - 1) * read_X ();
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "not_throwing" and ReadsGlobal "X"
  [1]
  $ aslref rec-binop-read-local.asl
  $ aslref rec-binop-write.asl
  File rec-binop-write.asl, line 3, character 0 to line 6, character 4:
  func not_throwing (n: integer) => integer
  begin
    return foo (n);
  end;
  ASL Warning: the mutually-recursive functions not_throwing, foo have no
  recursive limit annotation.
  $ aslref --use-conflicting-side-effects-extension rec-binop-write.asl
  File rec-binop-write.asl, line 18, characters 10 to 43:
    let x = not_throwing (n - 1) * write_X ();
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "not_throwing" and WritesGlobal "X"
  [1]
  $ aslref rec-assert.asl
  File rec-assert.asl, line 1, character 0 to line 4, character 4:
  func not_throwing (n: integer) => integer
  begin
    return foo (n);
  end;
  ASL Warning: the mutually-recursive functions not_throwing, foo have no
  recursive limit annotation.
  File rec-assert.asl, line 9, characters 34 to 35:
    let x = not_throwing (n - 1) * (2 as integer {3});
                                    ^
  ASL Execution error: Mismatch type:
    value 2 does not belong to type integer {3}.
  [1]
  $ aslref --use-conflicting-side-effects-extension rec-assert.asl
  File rec-assert.asl, line 9, characters 10 to 51:
    let x = not_throwing (n - 1) * (2 as integer {3});
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "not_throwing" and PerformsAssertions
  [1]
  $ aslref rec-binop-atc.asl
  File rec-binop-atc.asl, line 1, character 0 to line 4, character 4:
  func not_throwing (n: integer) => integer
  begin
    return foo (n);
  end;
  ASL Warning: the mutually-recursive functions not_throwing, foo have no
  recursive limit annotation.
  File rec-binop-atc.asl, line 9, characters 34 to 35:
    let x = not_throwing (n - 1) * (2 as integer {3});
                                    ^
  ASL Execution error: Mismatch type:
    value 2 does not belong to type integer {3}.
  [1]
  $ aslref --use-conflicting-side-effects-extension rec-binop-atc.asl
  File rec-binop-atc.asl, line 9, characters 10 to 51:
    let x = not_throwing (n - 1) * (2 as integer {3});
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "not_throwing" and PerformsAssertions
  [1]
  $ aslref rec-binop-read-write.asl
  File rec-binop-read-write.asl, line 3, character 0 to line 6, character 4:
  func not_throwing (n: integer) => integer
  begin
    return foo (n);
  end;
  ASL Warning: the mutually-recursive functions not_throwing, foo have no
  recursive limit annotation.
  $ aslref --use-conflicting-side-effects-extension rec-binop-read-write.asl
  File rec-binop-read-write.asl, line 17, characters 10 to 42:
    let x = not_throwing (n - 1) * read_X ();
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "not_throwing" and ReadsGlobal "X"
  [1]
  $ aslref rec-binop-write-throw.asl
  File rec-binop-write-throw.asl, line 4, character 0 to line 11, character 4:
  func throwing (n: integer, b: boolean) => integer
  begin
    if b then
      throw E {-};
    else
      return foo (n);
    end;
  end;
  ASL Warning: the mutually-recursive functions throwing, foo have no recursive
  limit annotation.
  $ aslref --use-conflicting-side-effects-extension rec-binop-write-throw.asl
  File rec-binop-write-throw.asl, line 23, characters 10 to 46:
    let x = throwing (n - 1, FALSE) * write_X ();
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "throwing" and WritesGlobal "X"
  [1]
  $ aslref rec-constant.asl
  $ aslref constant-rec.asl
  File constant-rec.asl, line 12, characters 10 to 17:
    let r = foo (1);
            ^^^^^^^
  ASL Dynamic error: recursion limit reached.
  [1]
  $ aslref rec-local-type.asl
  File rec-local-type.asl, line 12, characters 10 to 24:
    let r = Zeros{foo (0)};
            ^^^^^^^^^^^^^^
  ASL Type error: constrained integer expected, provided integer.
  [1]
  $ aslref rec-binop-rec.asl
  File rec-binop-rec.asl, line 6, character 0 to line 11, character 4:
  func foo (n: integer) => integer
  begin
    if n <= 0 then return 1; end;
    let x = bar (n - 1) * bar (n - 2);
    return 2 * x;
  end;
  ASL Warning: the mutually-recursive functions foo, bar have no recursive
  limit annotation.
  $ aslref --use-conflicting-side-effects-extension rec-binop-rec.asl
  File rec-binop-rec.asl, line 9, characters 10 to 35:
    let x = bar (n - 1) * bar (n - 2);
            ^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: conflicting side effects CallsRecursive "bar" and CallsRecursive "bar"
  [1]

  $ aslref print-var.asl
  0
  1
  2

  $ aslref global-var-initialisation.asl
  X = 5
  Y0 = 0
  Y1 = 1
  Y2 = 2
  Y3 = 3
  Y4 = 4
  $ aslref global-throw-initialisation.asl
  File global-throw-initialisation.asl, line 8, characters 0 to 29:
  let X: integer = throwing ();
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Execution error: unexpected exception E thrown during the evaluation of
    the initialisation of the global storage element "X".
  [1]

  $ aslref config-type-uses-let.asl
  File config-type-uses-let.asl, line 2, characters 0 to 36:
  config Y : integer {0 .. 2 * X} = 0;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
