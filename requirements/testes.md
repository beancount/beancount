# Cenários de Teste - Assistente Beancount

## Cenário 1: Validação de Transação Correta (Fluxo Principal)
* **Dado** que o usuário fornece uma string de transação bem formatada: `"2026-03-01 * 'Supermercado' 'Compras do mês' Expenses:Food 50.00 BRL Assets:Checking -50.00 BRL"`
* **Quando** o assistente procesa a validação,
* **Então** o sistema deve retornar sucesso indicando que a transação está balanceada e a sintaxe está 100% correta.

## Cenário 2: Transação Desbalanceada (Fluxo de Exceção)
* **Dado** que o usuário fornece uma transação onde os valores não somam zero: `"2026-03-01 * 'Cinema' Expenses:Entertainment 40.00 BRL Assets:Checking -30.00 BRL"`
* **Quando** o assistente roda a verificação de saldo,
* **Então** o sistema deve acusar um erro de balanceamento informando a diferença de 10.00 BRL.

## Cenário 3: Erro de Sintaxe de Data (Fluxo de Exceção)
* **Dado** que o usuário insere uma data no formato incorreto brasileiro: `"01/03/2026 * 'Almoço' Expenses:Food 25.00 BRL"`
* **Quando** o parser do assistente analisa a linha,
* **Então** ele deve rejeitar o formato e alertar que o padrão correto do Beancount obrigatoriamente segue o formato ISO `AAAA-MM-DD`.