# Manipulação de Linguagens Regulares e Linguagens Livres de Contexto

## Universidade Federal de Santa Catarina - CTC - Departamento de Informática e Estatística

Novembro de 2020

Professora: Jerusa Marchi

Aluno: Henrique da Cunha Buss

## Visão geral

A aplicação foi desenvolvida usando a linguagem [Elm](https://elm-lang.org/),
uma linguagem puramente funcional focada em desenvolvimento Web, inspirada em
[Haskell](https://www.haskell.org/). Para rodar a aplicação, basta abrir o
arquivo `Aplicacao/index.html` com um browser, ou visitar
[a página GitHub do projeto](https://neovier.github.io/LFC01/).

## Modelagem de dados

As estruturas de dados e sua modelagem está contida na pasta `src/Models`, onde
cada parte importante tem o seu arquivo com suas definições. Em geral, foram
usados [custom types](https://guide.elm-lang.org/types/custom_types.html),
fornecidos pela linguagem, e
[type aliases](https://guide.elm-lang.org/types/type_aliases.html), que são
semelhantes a objetos em linguagens como JavaScript.

### Símbolos

Símbolos são a unidade atômica de uma palavra: podem ser um único caractere,
ou um grupo, como em `[a-z]`, `[a-zA-Z]`, etc.

### Alfabetos

Assim como muitos dos dados relacionados a autômatos, alfabetos podem ser
determinísticos ou não determinísticos, sendo que um alfabeto determinístico é
uma simples lista de [símbolos](###Símbolos), enquanto um
alfabeto não determinístico contém o tipo especial Epsilon junto da sua lista de
símbolos.

### Estados

Um estado de um autômato pode ser o estado Morto, ou um estado Válido com um
rótulo (`String`).

### Transições

Existem duas variações de transições, novamente uma determinística e uma não
determinística, onde a determinística possui um [estado](###Estados) anterior
(pensando em um diagrama, o estado de onde a flecha sai), um próximo estado
(pensando em um diagrama, o estado onde a flecha chega), e uma lista de
Condições ([Símbolos](###Símbolos)), que no caso determinístico é uma simples
lista de símbolos. Transições não determinísticas diferem no fato que podem ter
vários próximos estados (de chegada), além de que suas condições podem ter
Epsilon ou não.

### Autômatos Finitos

Automâtos finitos podem ser determinísticos ou não. Um autômato finito é
definido como em aula: uma quintupla que possui

- Uma lista de [estados](###Estados)
- Um estado inicial
- Uma lista de estados finais
- Um [alfabeto](###Alfabetos)
- Uma lista de [transições](###Transições)

Sendo que autômatos determinísticos usam as variações determinísticas destes
itens, e os não determinísticos usam as variações não determinísticas. Por
exemplo, um AFD possui um alfabeto determinístico, enquanto um AFND possui um
alfabeto não determinístico.

### Símbolos Terminais

Um símbolo terminal é simplesmente um [símbolo](###Símbolos).

### Símbolos Não Terminais

Um símbolo não terminal é modelado como uma `String`.

### Produções

Uma produção possui um [símbolo não terminal](###Símbolos-Não-Terminais) de
origem (como o `S` em `S -> aA`), e uma lista de corpos de produção. Um corpo de
produção, por sua vez, possui um [símbolo terminal](###Símbolos-Terminais)
consumido (como o `a` em `S -> aA`), e possivelmente um símbolo não terminal
destino (como o `A` em `S -> aA`). Assim, podemos dizer que a produção
`S -> aA | aB | bB | b` possui `S` como símbolo de origem, e os corpos
`aA`, `aB`, `bB` e `b` como corpos de produção.

### Gramáticas

Gramáticas regulares são, também, definidas como em aula: uma quadrupla com

- Uma lista de [símbolos não terminais](###Símbolos-Não-Terminais)
- Uma lista de [símbolos terminais](###Símbolos-Terminais)
- Uma lista de [produções](###Produções)
- Um símbolo não terminal inicial

Além disso, possuem o campo `acceptsEmpty` para dizer se a gramática aceita a
palavra vazia ou não, já que usa um alfabeto determinístico e não poderia
conter Epsilon.

### Expressões Regulares

Expressões regulares são definidas através de um custom type recursivo, de modo
que uma expressão regular pode ser:

- Epsilon (palavra vazia)
- Um [Símbolo](###Símbolos) (que abrange grupos como `[0-9]`, assim como
  símbolos como `a`)
- A união de duas expressões regulares
- A concatenação de duas expressões regulares
- O operador `*` de uma expressão regular
- O operador `+` de uma expressão regular
- O operador `?` de uma expressão regular

Deste modo, expressões regulares tomam uma forma de árvore, onde cada folha é
Epsilon ou um Símbolo.

## Entrada de dados

Para a entrada de dados, foi utilizada a biblioteca
[Parser](https://github.com/elm/parser), além de funções auxiliares adicionais.
Toda a parte de entrada de dados está na pasta `Aplicacao/src/Parsing`, e cada
modelo de dados possui seu próprio arquivo.

Para inserir um autômato finito determinístico, basta clicar no botão
`Carregar autômato finito determinístico`, e selecionar um arquivo como definido
nas especificações do trabalho. O mesmo vale para autômatos finitos não
determinísticos, gramáticas regulares e expressões regulares. Vale salientar que
só será possível ler corretamente se o botão selecionado e o conteúdo do arquivo
forem compatíveis, ou seja, ao clicar em
`Carregar autômato finito determinístico`, o arquivo selecionado deve
obrigatoriamente ser de um autômato finito determinístico.

O usuário também tem a possibilidade de editar autômatos já carregados, podendo
remover estados e alterar os estados destino de cada transição.

## Saída de dados

A apresentação de dados na tela ocorre através de um browser, abrindo o arquivo
`Aplicacao/src/index.html`, executando `elm reactor` dentro da pasta
`Aplicacao` e navegando até `src/Main.elm`, ou acessando a
[página GitHub do projeto](https://neovier.github.io/LFC01/). O arquivo
`index.html` pode ser gerado novamente através do comando
`elm make src/Main.elm` dentro da pasta `Aplicacao`. Para a visualização do
programa, foi seguida a
[The Elm Architecture](https://guide.elm-lang.org/architecture/), que define
um `modelo` de dados, um modelo de `mensagens` e uma função de `update`. Na
interface do programa, temos três colunas:

- À esquerda, o histórico, que mostra a lista de itens usados. Cada entrada do
  histórico possui um botão de remover e um botão indicando o tipo do item (AFD,
  AFND, GR ou ER). Ao clicar no botão que indica o tipo do item, o mesmo vai ser
  mostrado na coluna central.
- Ao centro, o item atualmente selecionado, ou mensagens de erro. Por exemplo,
  a tabela de transições no caso de autômatos.
- À direita, a parte de controles. Os botões disponíveis dependem do item atual
  e do histórico, mas sempre estarão disponíveis os botões para carregar novos
  itens.

## Operações

Todas as operações são feitas a partir da coluna da direita na interface, e
algumas dependem do item atual e do histórico para ficarem disponíveis. As ações
de carregar um novo item (AFD, AFND, GR ou ER) estão sempre disponíveis e no
topo da lista de controles.

Se o item atual é um autômato (determinístico ou não), aparece uma caixa de
texto onde o usuário pode inserir uma sentença, e abaixo da caixa de texto uma
mensagem informando se a sentença é reconhecida pelo autômato ou não.

Se o item atual é um AFD, surgem as operações `Fazer complemento`,
`Minimizar AFD` e `Converter para GR`.

Se o item atual é um AFND, surge a operação `Converter AFND para AFD`.

Se o item atual é uma GR, surge a operação `Converter para AFND`.

Se o item atual é uma ER, surge a operação `Converter para AFD`, que irá
converter todas as expressões em autômatos separados (um autômato para cada
expressão).

Se os dois itens mais recentes (mais acima) do histórico forem AFDs, surgem
as operações `Fazer união nos últimos dois autômatos` e
`Fazer interseção nos últimos dois autômatos`.

## Testes

Os arquivos de teste estão separados na pasta Teste nas pastas `automatos`,
`gramaticas` e `regexes`, e podem ser carregados pela aplicação. Os arquivos de
teste fornecidos têm a extensão `.txt`, embora isso não vá importar, desde
que tenham o formato válido.
