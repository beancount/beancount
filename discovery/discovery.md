# Discovery - Beancount

## Descrição do Sistema
O Beancount é uma linguagem de computador para contabilidade de dupla entrada. Ele permite que o usuário defina registros de transações financeiras em um arquivo de texto simples, leia-os na memória e gere relatórios detalhados.

## Jobs to Be Done (JTBD)
"Quando estou gerenciando minhas finanças de longo prazo, quero registrar minhas transações usando apenas texto puro de forma manual ou automatizada, para que eu tenha controle absoluto, privacidade e independência de plataformas proprietárias."

## Oportunidade e Limitações
Como o Beancount funciona estritamente via arquivos de texto, novos usuários ou pessoas que gerenciam muitas contas sofrem para validar se a sintaxe do arquivo está correta antes de rodar o programa, ou acham difícil categorizar e converter extratos bancários de forma rápida. O preenchimento manual é altamente propenso a erros de digitação e espaçamento.

## Personas e Mapas de Empatia

### Persona 1: O Desenvolvedor Pragmático (Lucas, 26 anos)
*   **Perfil:** Engenheiro de software, fã de open-source e automação. Usa Linux e passa o dia no terminal.
*   **Objetivo:** Manter o controle de seus gastos e investimentos de forma vitalícia, sem que bancos ou apps fiquem minerando seus dados.
*   **Frustração:** Perde muito tempo caçando erros de digitação e sintaxe no arquivo `.beancount` quando tenta rodar o relatório no final do mês.

#### Mapa de Empatia - Lucas
*   **O que pensa e sente?** "Preciso de privacidade absoluta nos meus dados financeiros. Detesto interfaces gráficas lentas; texto puro versionado no Git é o futuro."
*   **O que vê?** Planilhas do Excel que quebram com o tempo, aplicativos de banco cheios de anúncios e comunidades open-source elogiando a contabilidade de dupla entrada.
*   **O que ouve?** Colegas devs dizendo que gerenciar finanças por arquivos de texto dá muito trabalho e que "não vale a pena o esforço manual".
*   **O que fala e faz?** Defende o uso de ferramentas locais e seguras. No fim do mês, tenta organizar tudo pelo terminal, mas se estressa corrigindo erros de sintaxe bobos.
*   **Dores:** Perder minutos preciosos caçando uma linha desbalanceada ou uma data mal formatada que travou o parser do Beancount.
*   **Necessidades:** Um validador instantâneo pré-execução para garantir que seu arquivo `.beancount` está perfeito antes de rodar os relatórios.

---

### Persona 2: A Analista de Dados Organizada (Mariana, 31 anos)
*   **Perfil:** Trabalha com BI, gosta de gráficos de finanças e planilhas, mas cansou das limitações do Excel para contabilidade de dupla entrada.
*   **Objetivo:** Migrar suas finanças para texto puro para versionar no Git, mas quer praticidade.
*   **Frustração:** Acha a curva de aprendizado inicial do Beancount árdua e sente falta de um assistente simples que ajude a estruturar as transações sem errar as contas de débito/crédito.

#### Mapa de Empatia - Mariana
*   **O que pensa e sente?** "Quero a precisão da contabilidade de dupla entrada, mas a sintaxe do Beancount me assusta um pouco. Tenho medo de lançar valores errados."
*   **O que vê?** Tutoriais complexos em inglês, arquivos de exemplo gigantescos e cheios de códigos que parecem difíceis de replicar manualmente.
*   **O que ouve?** Que o Beancount é excelente para gerar gráficos de evolução patrimonial se os dados forem inseridos perfeitamente.
*   **O que fala e faz?** Tenta padronizar suas contas de despesas (`Expenses:Food`, `Expenses:Transport`), mas frequentemente erra o sinal (positivo/negativo) dos lançamentos.
*   **Dores:** A falta de um feedback visual imediato ou de um assistente que diga "sua transação está certa e balanceada".
*   **Necessidades:** Um assistente inteligente que valide se a estrutura de débito/crédito fecha em zero e sugira correções simples.