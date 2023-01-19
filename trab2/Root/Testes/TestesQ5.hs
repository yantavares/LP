module Root.Testes.TestesQ5 where

import Root.Questoes.Q5.AbsLI
import Root.Questoes.Q5.Interpreter
import Test.Hspec

-------------------------------------- testes da questao 1

prog1 =
  Prog
    ( SBlock
        [ SAss (Ident "x") (EInt 1),
          SAss (Ident "soma") (EInt 0),
          SAss (Ident "c") (EInt 10),
          SdoWhile
            ( SBlock
                [ SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                  SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                ]
            )
            (EVar (Ident "c"))
        ]
    )

{- prog1

{
  x = 1;
  soma = 0;
  c = 10;
  do {
    soma = soma + c;
    c = c - 1;
    }
  while (c)
}

-}

testCase1 = executeP [] prog1 == Right [("x", ValorInt 1), ("soma", ValorInt 55), ("c", ValorInt 0)]

prog2 =
  Prog
    ( SBlock
        [ SAss (Ident "x") (EInt 1),
          SAss (Ident "soma") (EInt 0),
          SAss (Ident "c") (EInt 1),
          SdoWhile
            ( SBlock
                [ SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                  SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                ]
            )
            (EVar (Ident "c"))
        ]
    )

{-

{
  x = 1;
  soma = 0;
  c = 1;
  do {
    soma = soma + c;
    c = c - 1;
    }
  while (c)
}

-}

testCase2 = executeP [] prog2 == Right [("x", ValorInt 1), ("soma", ValorInt 1), ("c", ValorInt 0)]

------------------------------------ fim testes da questao 1

-------------------------------------- testes da questao 2
prog3 =
  Prog
    ( SBlock
        [ SAss (Ident "w") (EStr "hello"),
          SAss (Ident "v") (EStr "world"),
          SAss (Ident "a") (ECon (EVar (Ident "w")) (EVar (Ident "v")))
        ]
    )

{-

 {
  w = "hello";
  v = "world";
  a = w ++ v;
 }

-}

-- executeP :: RContext -> Program  -> RContext
testCase3 = executeP [] prog3 == Right [("w", ValorStr "hello"), ("v", ValorStr "world"), ("a", ValorStr "helloworld")]

-----------------------------------------
-----------------------------------------

{--
 {
  a = true;
  b = false;
  e = true;
  d = ! a || b && e;
  f = a && b || ! e;
 }

--}

prog4 =
  Prog
    ( SBlock
        [ SAss (Ident "a") ETrue,
          SAss (Ident "b") EFalse,
          SAss (Ident "e") ETrue,
          SAss
            (Ident "d")
            ( EOr
                (ENot (EVar (Ident "a")))
                (EAnd (EVar (Ident "b")) (EVar (Ident "e")))
            ),
          SAss
            (Ident "f")
            ( EOr
                (EAnd (EVar (Ident "a")) (EVar (Ident "b")))
                (ENot (EVar (Ident "e")))
            )
        ]
    )

testCase4 =
  executeP [] prog4
    == Right
      [ ("a", ValorBool True),
        ("b", ValorBool False),
        ("e", ValorBool True),
        ("d", ValorBool False),
        ("f", ValorBool False)
      ]

------------------------------------ fim testes da questao 2

-------------------------------------- testes da questao 3
prog5 = Prog (SAss (Ident "x") (EInt 1))

{-

 x = 1;

-}

-- executeP :: RContext -> Program  -> RContext
testCase5 = executeP [] prog5 == Right [("x", ValorInt 1)]

-----------------------------------------
-----------------------------------------

{--

{
  x = 1;
  soma = 0;
  c = 10;
  while (c) {
    soma = soma + c;
    c = c - 1;
    }
  }

--}

prog6 =
  Prog
    ( SBlock
        [ SAss (Ident "x") (EInt 1),
          SAss (Ident "soma") (EInt 0),
          SAss (Ident "c") (EInt 10),
          SWhile
            (EVar (Ident "c"))
            ( SBlock
                [ SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                  SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                ]
            )
        ]
    )

testCase6 = executeP [] prog6 == Right [("x", ValorInt 1), ("soma", ValorInt 55), ("c", ValorInt 0)]

-----------------------------------------
-----------------------------------------

{--

{
  x = 1;
  y = 0;
  z = x / y;
  w = z + 1;
}

--}

prog7 =
  Prog
    ( SBlock
        [ SAss (Ident "x") (EInt 1),
          SAss (Ident "y") (EInt 0),
          SAss (Ident "z") (EDiv (EVar (Ident "x")) (EVar (Ident "y"))),
          SAss (Ident "w") (EAdd (EVar (Ident "z")) (EInt 1))
        ]
    )

testCase7 = executeP [] prog7 == Left "divisao por 0"

------------------------------------ fim testes da questao 3

-------------------------------------- testes da questao 4

{--

{
  x = 1;
  soma = 0;
  c = 10;
  while (c) {
    soma = soma + c;
    c = c - 1;
    }
  }

--}

prog8 =
  Prog
    ( SBlock
        [ SAss (Ident "x") (EInt 1),
          SAss (Ident "soma") (EInt 0),
          SAss (Ident "c") (EInt 10),
          SWhile
            (EVar (Ident "c"))
            ( SBlock
                [ SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                  SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                ]
            )
        ]
    )

testCase8 = executeP [] prog8 == Right [("x", ValorInt 1), ("soma", ValorInt 55), ("c", ValorInt 0)]

-----------------------------------------
-----------------------------------------

prog9 =
  Prog
    ( SBlock
        [ SAss (Ident "x") (EInt 0),
          STry
            [SAss (Ident "soma") (EInt 0), SAss (Ident "c") (EDiv (EVar (Ident "x")) (EVar (Ident "soma")))]
            [SAss (Ident "x") (EInt 1)]
            [SAss (Ident "y") (EInt 2)]
        ]
    )

{-

 {
  x = 0;
  try {
    soma = 0;
    c = x / soma;
    }
  catch {
    x = 1;
    }
  finally {
    y = 2;
    }
 }

-}

-- executeP :: RContext -> Program  -> RContext
testCase9 = executeP [] prog9 == Right [("x", ValorInt 1), ("soma", ValorInt 0), ("y", ValorInt 2)]

-----------------------------------------
-----------------------------------------

{-

{
  x = 0;
  try {
    soma = 10;
    c = x / soma;
    }
  catch {
    x = 1;
    }
  finally {
    y = 2;
    }
  }

-}

prog10 =
  Prog
    ( SBlock
        [ SAss (Ident "x") (EInt 0),
          STry
            [SAss (Ident "soma") (EInt 10), SAss (Ident "c") (EDiv (EVar (Ident "x")) (EVar (Ident "soma")))]
            [SAss (Ident "x") (EInt 1)]
            [SAss (Ident "y") (EInt 2)]
        ]
    )

testCase10 = executeP [] prog10 == Right [("x", ValorInt 0), ("soma", ValorInt 10), ("c", ValorInt 0), ("y", ValorInt 2)]

-- uma condicao necessaria (mas nao suficiente) da implementacao eh que o valor de testSuite seja "True"
testSuite =
  foldl
    (&&)
    True
    [ testCase1,
      testCase2,
      testCase3,
      testCase4,
      testCase5,
      testCase6,
      testCase7,
      testCase8,
      testCase9,
      testCase10
    ]

main = hspec $ do
  describe "Suite de Testes do Trabalho 02, Exercício 5" $ do
    it "O teste do primeiro programa deve retornar True" $ do
      testCase1 `shouldBe` True
    it "O teste do segundo programa deve retornar True" $ do
      testCase2 `shouldBe` True
    it "O teste do terceiro programa deve retornar True" $ do
      testCase3 `shouldBe` True
    it "O teste do quarto programa deve retornar True" $ do
      testCase4 `shouldBe` True
    it "O teste do quinto programa deve retornar True" $ do
      testCase5 `shouldBe` True
    it "O teste do sexto programa deve retornar True" $ do
      testCase6 `shouldBe` True
    it "O teste do setimo programa deve retornar True" $ do
      testCase7 `shouldBe` True
    it "O teste do oitavo programa deve retornar True" $ do
      testCase8 `shouldBe` True
    it "O teste do nono programa deve retornar True" $ do
      testCase9 `shouldBe` True
    it "O teste do décimo programa deve retornar True" $ do
      testCase10 `shouldBe` True