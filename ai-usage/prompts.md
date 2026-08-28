# Registro de Uso de Inteligência Artificial

## Contexto de Utilização
A Inteligência Artificial foi integrada ao ciclo de desenvolvimento do BeanWizard atuando como ferramenta de suporte arquitetural, validação de consistência de requisitos e aceleração de escrita de especificações UML. O objetivo foi mapear o comportamento do ecossistema Beancount (contabilidade em texto puro) e garantir conformidade com os padrões formais de engenharia de software antes da implementação do assistente auxiliar.

## Detalhamento das Interações e Engenharia de Prompts

### 1. Modelagem de Necessidades e Validação de Domínio (Discovery)
A IA foi utilizada para gerar matrizes de comportamento do usuário final com base nas regras rígidas da contabilidade de dupla entrada, estabelecendo as fronteiras do problema de usabilidade.
> "Atue como um Engenheiro de Software sênior especialista em finanças e contabilidade de dupla entrada. Preciso estruturar a fase de Discovery para um assistente que ajuda usuários do Beancount a não errarem a sintaxe de seus arquivos. Crie o JTBD e duas personas detalhadas (uma técnica e uma não-técnica) focadas nas dores de escrita manual."

### 2. Decomposição de Requisitos e Critérios de Aceite
Utilizou-se a ferramenta para traduzir as dores mapeadas em artefatos formais de gerenciamento de produto (Histórias de Usuário), além de prever os primeiros cenários extremos de testes financeiros.
> "Com base nas personas criadas para o assistente Beancount, monte um backlog com 5 Histórias de Usuário completas seguindo o padrão 'Como... Quero... Para que...'. Adicione também 3 cenários de teste práticos detalhando o comportamento esperado de sucesso e falha de balanceamento de moedas."

### 3. Prototipagem Rápida de Algoritmo de Validação
Geração do núcleo lógico em script isolado para comprovar a viabilidade técnica das expressões regulares e regras de cálculo necessárias para o motor do assistente.
> "Gere um script em Python simples e robusto chamado `bean_wizard.py` que simule esse assistente. Ele deve receber uma string de transação, validar via Regex se a data está no formato YYYY-MM-DD e checar aritmeticamente se os valores de débito e crédito passados se anulam (somam zero)."

### 4. Geração Automática de Artefatos UML (Mermaid)
Uso de modelos de linguagem para converter os diagramas arquiteturais concebidos para o projeto em código renderizável pelo interpretador Markdown.
> "Gere a documentação de arquitetura do BeanWizard em formato Markdown contendo o Diagrama de Componentes, Diagrama de Classes e Diagrama de Sequência utilizando a sintaxe do Mermaid."

### 5. Engenharia de Contexto para Modelos LLM Integrados (`prompts.md`)
Definição sistemática do arquivo de configuração de contexto e restrições operacionais da IA do assistente, prevendo mecanismos obrigatórios de logs e rastreabilidade para auditoria.
> "Estruture o arquivo `prompts.md` definindo a persona do BeanWizard, suas regras de validação de data e balanço financeiro, e inclua uma diretriz obrigatória de Registro de Uso para salvar logs de auditoria de cada validação realizada."