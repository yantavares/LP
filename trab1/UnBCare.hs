import Data.List
import Data.Char
import Data.String

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

{-
 *** Aluno: Yan Tavares
 *** Matricula: 202014323
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}


findMed :: Eq t => t -> [t] -> Bool
findMed _ [] = False
findMed n (x:xs)
    | x == n = True
    | otherwise = findMed n xs

replace :: (Eq a, Num b) => (a, b) -> [(a, b)] -> [(a, b)]
replace _ [] = []
replace tup (x:xs)
    | fst tup == fst x = (fst tup, snd tup + snd x) : xs
    | otherwise = x : replace tup xs

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento m q e
    | findMed m [x | (x, m) <- e] == True = replace (m,q) e
    | otherwise = (m, q) : e
    
{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento m e
   | findMed m [x | (x, m) <- e] == True = Just (replace (m, -1) e)
   | otherwise = Nothing

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento m (x:xs)
   | m == fst x = snd x
   | otherwise = consultarMedicamento m xs

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos r = sort [(a, length b) | (a, b) <- r]
{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}
isUniqueSorted :: Eq a => [a] -> Bool
isUniqueSorted [] = True
isUniqueSorted [a] = True
isUniqueSorted (x:xb:xs)
   | x /= xb = True && isUniqueSorted (xb:xs)
   | otherwise = False

checkTime :: Ord a => [[a]] -> Bool
checkTime [] = True
checkTime (x:xs)
   | sort x == x && isUniqueSorted (sort x) = True && checkTime xs
   | otherwise = False

receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido r =  sort [a | (a, b) <- r] == [a | (a, b) <- r] && checkTime ([b | (a, b) <- r])

planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido p = sort [a | (a, b) <- p] == [a | (a, b) <- p] && checkTime ([b | (a, b) <- p])

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:



 -}


hora :: [(b1, b2)] -> [b1]
hora plantao = map fst plantao
item :: [(a, b)] -> [b]
item plantao = map snd plantao
medicarLista :: [(a, [Cuidado])] -> [[Cuidado]]
medicarLista plantao = map (filter medicarConst ) (item plantao)
comprarLista :: [(a, [Cuidado])] -> [[Cuidado]]
comprarLista plantao = map (filter comprarConst) (item plantao)
medicarConst :: Cuidado -> Bool
medicarConst (Medicar _) = True
medicarConst _ = False
comprarConst :: Cuidado -> Bool
comprarConst (Comprar _ _) = True
comprarConst _ = False
medicarLimpo :: [(a, [Cuidado])] -> [[Medicamento]]
medicarLimpo plantao = map (map (\(Medicar m) -> m)) (medicarLista plantao)
comprarLimpo :: [(a, [Cuidado])] -> [[Medicamento]]
comprarLimpo plantao = map (map (\(Comprar m _) -> m)) (comprarLista plantao)
tudoLimpo :: [a] -> [b] -> [(a, b)]
tudoLimpo [] _ = []
tudoLimpo (a:as) (b:bs) = (a,b) : tudoLimpo (as) (bs)

checkRemedios :: Ord a => [[a]] -> Bool
checkRemedios [] = True
checkRemedios (a:as)
    | a == sort a && isUniqueSorted (sort a) && checkRemedios as= True
    | otherwise = False

makeList :: [(a, [Cuidado])] -> [([Medicamento], [Medicamento])]
makeList plantao = tudoLimpo (medicarLimpo plantao) (comprarLimpo plantao)

checkUnique :: Eq t => ([t], [t]) -> Bool
checkUnique (a,[]) = True
checkUnique ([a], b) = not (findMed a b) 
checkUnique ((a:as), b)
    | findMed a b || checkUnique (as, b) = False
    | otherwise = True
  
checkUniqueAll :: [(a, [Cuidado])] -> [Bool]
checkUniqueAll plantao = map checkUnique (makeList plantao)

isAllTrue :: [Bool] -> Bool
isAllTrue [] = True
isAllTrue (a:as) = a == True && isAllTrue as


plantaoValido :: Plantao -> Bool
plantaoValido plantao = sort (hora plantao) == hora plantao && isUniqueSorted (hora plantao) && checkRemedios (comprarLimpo (plantao)) && checkRemedios (medicarLimpo (plantao)) && isAllTrue (checkUniqueAll (plantao))

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

  receituarioInvalido4 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 23, 22])]

  plano1 :: PlanoMedicamento
  plano1 = [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]
-}

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   
    | x `elem` xs = rmdups xs
    | otherwise = x : rmdups xs

soChaves :: Ord a1 => [(a2, [a1])] -> [a1]
soChaves receituario = rmdups (sort (concat [y | y <- map snd receituario]))

grupo :: Eq t => t -> [(a, [t])] -> [a]
grupo _ [] = []
grupo a (b:bs)
  | findMed a (snd b) = fst b : grupo a bs
  | otherwise = grupo a bs

grupoItens :: Eq t => [t] -> [(a, [t])] -> [[a]]
grupoItens [] _ = []
grupoItens (a:as) receituario = grupo a receituario : grupoItens as receituario

receitaPlano :: [a] -> [b] -> [(a, b)]
receitaPlano [] _ = []
receitaPlano _ [] = []
receitaPlano (a:as) (b:bs) = (a, b) : receitaPlano (as) (bs)

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receituario = receitaPlano (soChaves receituario) ( grupoItens (soChaves receituario) receituario)

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano plano = receitaPlano (soChaves plano) ( grupoItens (soChaves plano) plano)

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = undefined

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz = undefined

{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto = undefined








































-- ! TESTES

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

--------------------- EXERCÍCIO 1 ---------------------
caso1_1 = comprarMedicamento med7 30 estoque1 == [(med4, 10), (med6, 5), (med7, 30)]

caso1_2 = comprarMedicamento med1 20 estoque1 == [(med1, 20), (med4, 10), (med6, 5), (med7, 0)]

caso1_3 = comprarMedicamento med6 2 estoque1 == [(med4, 10), (med6, 7), (med7, 0)]

caso1_4 = comprarMedicamento med9 20 [] == [(med9, 20)]

testeExercicio1 = and [caso1_1, caso1_2, caso1_3, caso1_4]

--------------------- EXERCÍCIO 2 ---------------------
caso2_1 = tomarMedicamento med4 estoque1 == Just [(med4, 9), (med6, 5), (med7, 0)]

caso2_2 = Nothing == (tomarMedicamento med8 estoque1)

testeExercicio2 = and [caso2_1, caso2_2]

--------------------- EXERCÍCIO 3 ---------------------
caso3_1 = consultarMedicamento med6 estoque1 == 5

caso3_2 = consultarMedicamento "Aas" estoque1 == 0

testeExercicio3 = and [caso3_1, caso3_2]

--------------------- EXERCÍCIO 4 ---------------------
caso4_1 = demandaMedicamentos receituario1 == [(med4, 2), (med6, 1), (med7, 1)]

caso4_2 = demandaMedicamentos receituario2 == [(med4, 2), (med6, 1), (med7, 1), (med8, 3)]

testeExercicio4 = and [caso4_1, caso4_2]

--------------------- EXERCÍCIO 5a ---------------------
caso5_1a = receituarioValido receituario1

caso5_2a = receituarioValido receituario2

caso5_3a = not $ receituarioValido receituarioInvalido1

caso5_4a = not $ receituarioValido receituarioInvalido2

caso5_5a = not $ receituarioValido receituarioInvalido3

caso5_6a = not $ receituarioValido receituarioInvalido4

--------------------- EXERCÍCIO 5b ---------------------
testeExercicio5a = and [caso5_1a, caso5_2a, caso5_3a, caso5_4a, caso5_5a, caso5_6a]

caso5_1b = planoValido plano1

caso5_2b = planoValido plano1

caso5_3b = not $ planoValido planoInvalido1

caso5_4b = not $ planoValido planoInvalido2

caso5_5b = not $ planoValido planoInvalido3

caso5_6b = not $ planoValido planoInvalido4

testeExercicio5b = and [caso5_1b, caso5_2b, caso5_3b, caso5_4b, caso5_5b, caso5_6b]

--------------------- EXERCÍCIO 6 ---------------------
caso6_1 = plantaoValido plantao1

caso6_2 = plantaoValido plantao2

caso6_3 = plantaoValido plantao3

caso6_4 = not (plantaoValido plantaoInvalido1)

caso6_5 = not (plantaoValido plantaoInvalido2)

caso6_6 = not (plantaoValido plantaoInvalido3)

caso6_7 = not (plantaoValido plantaoInvalido4)

testeExercicio6 = and [caso6_1, caso6_2, caso6_3, caso6_4, caso6_5, caso6_6, caso6_7]

--------------------- EXERCÍCIO 7 ---------------------
caso7_1 = geraPlanoReceituario receituario1 == [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]

caso7_2 = geraPlanoReceituario receituario2 == [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med7, med8]), (23, [med8])]

testeExercicio7 = and [caso7_1, caso7_2]

--------------------- EXERCÍCIO 8 ---------------------
caso8_1 = geraReceituarioPlano (geraPlanoReceituario receituario1) == receituario1

caso8_2 = geraReceituarioPlano (geraPlanoReceituario receituario2) == receituario2

testeExercicio8 = and [caso8_1, caso8_2]

--------------------- EXERCÍCIO 9 ---------------------
-- caso9_1 = isNothing (executaPlantao plantao1 estoque1)

caso9_2 = executaPlantao plantao1 estoque2 == Just [(med4, 8), (med6, 4), (med7, 9)]

caso9_3 = executaPlantao plantao2 estoque1 == Just [(med4, 8), (med6, 4), (med7, 29)]

-- testeExercicio9 = and [caso9_1, caso9_2, caso9_3]

--------------------- EXERCÍCIO 10 ---------------------
caso10_1 = not (satisfaz plantao1 plano1 estoque1)

caso10_2 = satisfaz plantao1 plano1 estoque2

caso10_3 = satisfaz plantao2 plano1 estoque1

testeExercicio10 = and [caso10_1, caso10_2, caso10_3]

--------------------- EXERCÍCIO 11 ---------------------
caso11_1 = satisfaz plantao plano1 estoque1
  where
    plantao = plantaoCorreto plano1 estoque1

caso11_2 = satisfaz plantao plano2 estoque2
  where
    plantao = plantaoCorreto plano2 estoque2

testeExercicio11 = and [caso11_1, caso11_2]