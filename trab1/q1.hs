module Root.Modelo.ModeloDados where

type Medicamento = String

type Quantidade = Int

type Horario = Int

type EstoqueMedicamentos = [(Medicamento, Quantidade)]

type Prescricao = (Medicamento, [Horario])

type Receituario = [Prescricao]

type PlanoMedicamento = [(Horario, [Medicamento])]

type Plantao = [(Horario, [Cuidado])]

data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento

instance Show Cuidado where
  show (Comprar m q) =
    "Comprar "
      ++ Prelude.show q
      ++ " comprimido(s) do medicamento: "
      ++ m
  show (Medicar m) = "Ministrar medicamento: " ++ m




-- | find m [x | (x, m) <- e] == False = (m, q) : e

med1 :: Medicamento
med1 = "Adera"

med2 :: Medicamento
med2 = "Alprazolam"

med3 :: Medicamento
med3 = "Donepezila"

med4 :: Medicamento
med4 = "Lactulona"

med5 :: Medicamento
med5 = "Mirtazapina"

med6 :: Medicamento
med6 = "Pantoprazol"

med7 :: Medicamento
med7 = "Patz"

med8 :: Medicamento
med8 = "Quetiapina"

med9 :: Medicamento
med9 = "Xarelto"

estoque1 :: EstoqueMedicamentos
estoque1 = [(med4, 10), (med6, 5), (med7, 0)]

estoque2 :: EstoqueMedicamentos
estoque2 = [(med4, 10), (med6, 5), (med7, 10)]

estoque3 :: EstoqueMedicamentos
estoque3 = [(med4, 10), (med6, 50), (med7, 10), (med8, 20)]

receituario1 :: Receituario
receituario1 = [(med4, [8, 17]), (med6, [6]), (med7, [22])]

receituario2 :: Receituario
receituario2 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 22, 23])]

receituarioInvalido1 :: Receituario
-- Invalido: horário não está ordenado de forma crescente
receituarioInvalido1 = [(med4, [22, 10])]

receituarioInvalido2 :: Receituario
-- Invalido: medicamentos não estão ordenbados de forma crescente
receituarioInvalido2 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med2, [8, 22, 23])]

receituarioInvalido3 :: Receituario
-- Invalido: medicamentos não estão ordenbados de forma crescente
receituarioInvalido3 = [(med4, [8, 17]), (med3, [6]), (med7, [22])]

receituarioInvalido4 :: Receituario
-- Invalido: horário não estão ordenbados de forma crescente
receituarioInvalido4 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 23, 22])]

plano1 :: PlanoMedicamento
plano1 = [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]

plano2 :: PlanoMedicamento
plano2 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med7, med8]), (23, [med8])] :: [(Int, [String])]

planoInvalido1 :: PlanoMedicamento
-- Invalido: horário não está ordenado de forma crescente
planoInvalido1 = [(8, [med4]), (6, [med6]), (17, [med4]), (22, [med7])]

planoInvalido2 :: PlanoMedicamento
-- Invalido: medicamentos não estão ordenbados de forma crescente
planoInvalido2 = [(6, [med6]), (8, [med8, med4]), (17, [med4]), (22, [med7, med8]), (23, [med8])] :: [(Int, [String])]

planoInvalido3 :: PlanoMedicamento
-- Invalido: medicamentos não estão ordenbados de forma crescente
planoInvalido3 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med8, med7]), (23, [med8])] :: [(Int, [String])]

planoInvalido4 :: PlanoMedicamento
-- Invalido: horário não está ordenado de forma crescente
planoInvalido4 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (23, [med8]), (22, [med7, med8])] :: [(Int, [String])]

plantao1 :: Plantao
plantao1 =
  [ (6, [Medicar med6]),
    (8, [Medicar med4]),
    (17, [Medicar med4]),
    (22, [Medicar med7])
  ]

plantao2 :: Plantao
plantao2 =
  [ (6, [Medicar med6]),
    (8, [Medicar med4]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantao3 :: Plantao
plantao3 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido1 :: Plantao
plantaoInvalido1 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    (22, [Medicar med7]),
    -- Invalido: não está em horario crescente
    (17, [Medicar med4, Comprar med7 30])
  ]

plantaoInvalido2 :: Plantao
plantaoInvalido2 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    -- Invalido: comprar o mesmo medicamento no horário
    (17, [Medicar med4, Comprar med4 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido3 :: Plantao
plantaoInvalido3 =
  [ (6, [Medicar med6, Medicar med9]),
    -- Invalido: ordem dos medicamentos não está ordenada
    (8, [Medicar med4, Medicar med2]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido4 :: Plantao
plantaoInvalido4 =
  [ (6, [Medicar med6]),
    -- Invalido: comprar o mesmo medicamento no horário
    (8, [Comprar med4 20, Medicar med4, Medicar med8]),
    (17, [Medicar med4]),
    (22, [Medicar med7, Medicar med8]),
    (23, [Medicar med8])
  ]

-- ! CODIGO AQUI --------------------------------------------------

find :: Eq t => t -> [t] -> Bool
find _ [] = False
find n (x:xs)
    | x == n = True
    | otherwise = find n xs

replace :: (Eq a, Num b) => (a, b) -> [(a, b)] -> [(a, b)]
replace _ [] = []
replace tup (x:xs)
    | fst tup == fst x = (fst tup, snd tup + snd x) : xs
    | otherwise = x : replace tup xs

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento m q e
    | find m [x | (x, m) <- e] == True = replace (m,q) e
    | otherwise = (m, q) : e

-- ! --------------------------------------------------------------
            
caso1_1 = comprarMedicamento med7 30 estoque1 == [(med4, 10), (med6, 5), (med7, 30)]

caso1_2 = comprarMedicamento med1 20 estoque1 == [(med1, 20), (med4, 10), (med6, 5), (med7, 0)]

caso1_3 = comprarMedicamento med6 2 estoque1 == [(med4, 10), (med6, 7), (med7, 0)]

caso1_4 = comprarMedicamento med9 20 [] == [(med9, 20)]

testeExercicio1 = and [caso1_1, caso1_2, caso1_3, caso1_4]