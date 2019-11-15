type Transacao = {
  
  /** Data which transaction ocurres*/
  data: GregorianCalendar,

  /** Text identifier of the transaction(sets by the bank)*/
  textoIdentificador: string,

  /** Transaction value*/
  valor: number,

  /** Description(extra informations) about the transaction*/
  descricao: string,

  /** Number of the DOC*/
  numeroDOC: string

  /** The types of the transaction have */
  tipos: TipoTransacao[]
}


type GregorianCalendar = {
  year: number,
  month: number,
  dayOfMonth: number
}

enum TipoTransacao {
  SALDO_CORRENTE = "Saldo Corrente", 
  VALOR_APLICACAO = "Valor aplicado",
  RECEITA_OPERACIONAL= "Receita Operacional", 
  TAXA_CONDOMINIO= "Taxas de Condominio",
  TAXA_EXTRA= "Taxas Extras",
  TAXA_SALAO_FESTA= "Taxas Salao de Festas",
  MULTAS_JUROS= "Multas e Juros",
  TAXA_AGUA= "Taxa de Agua",
  RECUPERACAO_ATIVOS = "Recuperacao de Ativos",
  MULTA_JURO_CORRECAO_COBRANCA= "Multa, Juros e Correcao de Cobranca",
  OUTRAS_RECEITAS= "Outras receitas",
    
  DESPESAS_PESSOAL= "Despesas com pessoal",
  TERCEIRIZACAO_FUNCIONARIOS= "Terceirizacao de Funcionarios",
  VIGILANCIA= "Vigilancia",
  SALARIO_FUNCIONARIOS_ORGANICOS= "Salario dos funcionarios organicos",
  ADIANTAMENTO_SALARIAL_FUNCIONARIOS_ORGANICOS= "Adiantamento salarial dos funcionarios organicos",
    
  FERIAS= "Ferias",
  INSS= "INSS funcionarios e vigilancia",
  FGTS= "FGTS",
  PIS= "PIS",
  ISS= "ISS",
  BENEFICIO_SOCIAL= "Beneficio social dos funcionarios organicos",
  OUTRAS_DESPESAS_PESSOAL= "Outras despesas com pessoal",
    
  DESPESAS_ADMINISTRATIVAS= "Despesas Administrativas",
  ENERGISA= "Energisa",
  CAGEPA= "CAGEPA",
  COMPRA= "Compra",
  ADMINISTRACAO_CONDOMINIO= "Administracao do condominio",
  MANUTENCAO= "Manutencao realizada",
  ABASTECIMENTO= "Abastecimentos",
  SERVICOS_TERCEIROS= "Servicos realizados por terceiros",
  IRPF= "Imposto de renda recolhido na fonte",
  TARIFAS_BANCARIAS= "Tarifas e taxas bancarias",
  OUTRAS_DESPESAS_ADMINISTRATIVAS= "Outras despesas administrativas",
  APLICACAO= "Aplicacao",
    
  OUTROS= "Outros"
}
