# Projeto Final da Disciplina Programação Funcional

Repositório referente ao projeto final da disciplina **Programação Funcional** 2019.2.

O projeto se trata da implementação da [especificação](https://docs.google.com/document/d/13Jqq8MKZykaF2XrFsTUXjf350UE7eJJoS0J5Ki16zTE/edit) nas linguagens Haskell e JavaScript

## Configuração
![](https://img.icons8.com/dusk/64/000000/settings.png)

Para a execução do projeto em Haskell garanta que possui instalado em sua máquina o GHCI(compilador para Haskell), e o Cabal, para utilização de pacotes.

O projeto utiliza Aeson como _parser_ para JSON. Sua instalação pode ser feita através dos comandos:

```shell
$ cabal update
$ cabal install aeson
```

Além disso, para os testes é utilizado o HUnit, caso não o tenha instalado, execute o comando abaixo:

```shell
$ cabal install HUnit
```

## Execução do Projeto

### Haskell
![](https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/64px-Haskell-Logo.svg.png)

Para ter acesso às funções implementadas, execute na raiz do projeto Haskell (_src/core-haskell_) o comando:

```shell
$ ghci index.hs
```

Feito isso, as funções estão disponíveis para uso. Exemplo:

```shell
$ db <- getTransactions       /** Carrega as transações */
$ getMaxBalance db 2019 2
```

A execução dos testes pode ser feita através da função `runTests`.

### JavaScript
![](https://img.icons8.com/color/64/000000/javascript.png)

Para ter acesso a API de JavaScript, garanta que você executou na pasta raiz do projeto JavaScript (_src/core-js_) os comandos:

```shell
$ yarn install
$ yarn start
```

Feito isso, importe as funções desejadas do arquivo _/src/core-js/index.ts_.

#### Execução da interface web

Para a executar a UI certifique-se de estar no pacote _/src/ui_, em seguida, execute os comandos:

```shell
$ yarn install
$ yarn start
```

Feito isso, a aplicação estará disponível no endereço http://localhost:8080.

<p align="center">
  <img width="500" src="https://i.ibb.co/tK5Q7S8/Captura-de-tela-de-2019-11-27-12-55-16.png" alt="Captura-de-tela-de-2019-11-27-12-55-16" border="0">
</p>

## Organização de Pacotes
![](https://img.icons8.com/dusk/64/000000/folder-tree.png)

```js
data/                      /** Módulo contendo os dados usados por toda aplicação */

src/  
  +- core-haskell/         /** Módulo contendo a estrutura e implementação do projeto haskell */
    +- packages/           /** Módulo contendo a API para módulos externos */
    |  +- json-parser/  
    |    +- JsonParser.hs  /** Código responsável pelo parser dos arquivos json para um tipo haskell*/
    +- resolvers/          /** Módulo contendo as funções responsáveis por resolver um pedido do cliente e devolver os dados */
    |  +- Queries.hs       /** Código responsável pelas funções de consulta */
    |  +- Filters.hs       /** Código responsável pelas funções de filtragem */
    |  +- Helpers.hs       /** Código responsável pela definição de funções auxiliares, usadas nos métodos de consulta */
    +- tests/              /** Módulo contendo os testes das funcionalidades do projeto */
    |  +- TestAverages.hs
    |  +- TestBalances.hs
    |  +- TestCashFlows.hs
    |  +- TestFilters.hs
    |  +- Tests.hs
    |  +- TestValues.hs
    +- typings/            /** Módulo contendo todos os tipos da aplicação */
    |  +- GregorianCalendar.hs  
    |  +- Transaction.hs  
    |  +- TransactionType.hs  
    index.hs               /** Ponto de entrada do projeto Haskell, que possibilita o acesso aos metódos de sua API */

  +- core-js               /** Módulo contendo a estrutura e implementação do projeto javascript */
    +- resolvers/          /** Módulo contendo as funções responsáveis por resolver um pedido do cliente e devolver os dados */
    |  +- queries.ts       /** Código responsável pelas funções de consulta */
    |  +- filters.ts       /** Código responsável pelas funções de filtragem */
    |  +- helpers.ts       /** Código responsável pela definição de funções auxiliares, usadas nos métodos de consulta */
    +- typings/            /** Módulo contendo todos os tipos da aplicação */
    |  +- global.ts  
    package.json  
    tsconfig.json  
    index.ts               /** Ponto de entrada do projeto JS, que possibilita o acesso aos metódos de sua API */
    util.ts                /** Código contendo a implementação de funções utilitárias para listas e objetos*/

  +- ui  
    +- *Coming Soon*  
```
## Funcionalidades
![](https://img.icons8.com/dusk/64/000000/api-settings.png)

As funcionalidades da aplicação são referentes ao conjunto de dados disponível no diretório _/data_.

**Consultas, funções e operações disponíveis:**

* [x] Filtrar transações por ano.
* [x] Filtrar transações por ano e mês.
* [x] Calcular o valor das receitas (créditos) em um determinado mês e ano.
* [x] Calcular o valor das despesas (débitos) em um determinado mês e ano.
* [x] Calcular a sobra (receitas - despesas) de determinado mês e ano
* [x] Calcular o saldo final em um determinado ano e mês
* [x] Calcular o saldo máximo atingido em determinado ano e mês
* [x] Calcular o saldo mínimo atingido em determinado ano e mês
* [X] Calcular a média das receitas em determinado ano
* [X] Calcular a média das despesas em determinado ano
* [x] Calcular a média das sobras em determinado ano
* [x] Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista contendo pares (dia,saldoFinalDoDia).