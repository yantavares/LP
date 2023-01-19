module Root.Testes.TestesQ1 where

import Root.Questoes.Q1.AbsLI
import Root.Questoes.Q1.Interpreter
import Test.Hspec

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

testCase1 = executeP [] prog1 == [("x", 1), ("soma", 55), ("c", 0)]

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

testCase2 = executeP [] prog2 == [("x", 1), ("soma", 1), ("c", 0)]

-- uma condicao necessaria (mas nao suficiente) da implementacao eh que o valor de testSuite seja "True"
testSuite = foldl (&&) True [testCase1, testCase2]

-- note que o programa abaixo entra em loop infinito, conforme esperado
-- "executeP [] prog3" entra em loop.
prog3 =
  Prog
    ( SBlock
        [ SAss (Ident "x") (EInt 1),
          SAss (Ident "soma") (EInt 0),
          SAss (Ident "c") (EInt 0),
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
  c = 0;
  do {
    soma = soma + c;
    c = c - 1;
    }
  while (c)
}

-}

--------------------- VERIFICAÇÃO DOS TESTES ---------------------
main = hspec $ do
  describe "Suite de Testes do Trabalho 02, Exercício 1" $ do
    it "O teste do primeiro programa deve retornar True" $ do
      testCase1 `shouldBe` True
    it "O teste do segundo programa deve retornar True" $ do
      testCase2 `shouldBe` True