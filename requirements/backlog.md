# Backlog de Requisitos - Assistente Beancount

## Histórias de Usuário (User Stories)

### US01 - Validação de Sintaxe Básica
* **Como** usuário do Beancount,
* **quero** que o assistente valide a sintaxe do meu arquivo de texto,
* **para que** eu saiba se esqueci de fechar aspas ou errar o formato da data antes de rodar o sistema principal.

### US02 - Geração de Transações Simples
* **Como** Lucas (Persona Desenvolvedor),
* **quero** digitar uma frase natural (ex: "gastei 50 reais no mercado ontem") e receber a transação formatada no padrão Beancount,
* **para que** eu não precise digitar manualmente toda a estrutura de contas.

### US03 - Sugestão de Categorização de Contas
* **Como** Mariana (Persona Analista de Dados),
* **quero** que o sistema sugira automaticamente as contas de despesa (ex: `Expenses:Food` para mercado),
* **para que** eu mantenha meu plano de contas organizado sem esforço.

### US04 - Identificação de Saldos Incorretos
* **Como** usuário do Beancount,
* **quero** que o assistente verifique se os lançamentos de débito e crédito de uma transação somam zero,
* **para que** o arquivo não quebre por falta de balanceamento financeiro.

### US05 - Exportação de Logs de Erro
* **Como** desenvolvedor,
* **quero** ver mensagens de erro claras e explicativas com a linha exata onde a sintaxe falhou,
* **para que** a correção seja rápida diretamente no VS Code.