## Projeto Final da Disciplina Programação Funcional

Repositório referente ao projeto final da disciplina **Programação Funcional** 2019.2.

O projeto se trata da implementação da [especificação](https://docs.google.com/document/d/13Jqq8MKZykaF2XrFsTUXjf350UE7eJJoS0J5Ki16zTE/edit) nas linguagens Haskell e JavaScript

### Configuração

Para a execução do projeto em Haskell garanta que possui instalado em sua máquina o GHCI, compilador para Haskell, e o Cabal, para utilização de pacotes.

Além disso, é utilizado o Aeson como _parser_ para JSON. Sua instalação pode ser feita através dos comandos:

```
$ cabal update
$ cabal install aeson
```

### Execução do Projeto

#### Haskell

Execute na raiz do projeto haskell (_/src/core-haskell_) o seguinte comando:

```
$ ghci index.hs
```

#### JavaScript

Para ter acesso a API de JavaScript garanta que você executou os seguintes comandos:

```js
  yarn install
  yarn start
```
**Obs:** O mesmo deve ser feito, caso você deseje ter acesso a UI do projeto, nesse caso certifique de estar no pacote _/src/ui.

Em seguida, importe as funções desejadas do arquivo _/src/core-js/index.ts_.

### Funcionalidades

As funcionalidades da aplicação são referentes ao conjunto de dados disponível no diretório _/data_.

Consultas, funções e operações disponíveis:

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
