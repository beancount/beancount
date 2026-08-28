import re
import sys


def validar_transacao_beancount(linha_transacao):
    print(f"--- Analisando transação: {linha_transacao} ---\n")

    # 1. Validação de Data (Formato ISO: AAAA-MM-DD)
    padrao_data = r"^\d{4}-\d{2}-\d{2}"
    if not re.match(padrao_data, linha_transacao):
        print(
            "[ERRO SINTAXE]: A data inicial precisa estar no formato ISO (AAAA-MM-DD).")
        print("[DICA]: Formatos como DD/MM/AAAA não são aceitos pelo Beancount.\n")
        return False

    print("[OK]: Formato de data válido.")

    # 2. Simulação de verificação de balanço (Débito e Crédito)
    # Extrai todos os números (valores financeiros) da string
    valores = [float(x) for x in re.findall(
        r'[-+]?\d*\.\d+|\d+', linha_transacao) if '.' in x]

    if valores:
        balanco = sum(valores)
        # Permite uma pequena margem de erro para pontos flutuantes
        if abs(balanco) > 0.001:
            print(
                f"[ERRO FINANCEIRO]: A transação está desbalanceada por {balanco:.2f} BRL.")
            print(
                "[DICA]: Em contabilidade de dupla entrada, a soma dos lançamentos deve ser zero.\n")
            return False
        print("[OK]: Lançamentos balanceados (Soma residual é zero).")
    else:
        print(
            "[AVISO]: Nenhum valor numérico decimal foi encontrado para checagem de balanço.")

    print("\n[SUCESSO]: Transação validada perfeitamente pelo BeanWizard!\n")
    return True


if __name__ == "__main__":
    print("==================================================")
    print("      BEANWIZARD - ASSISTENTE DE BEANCOUNT        ")
    print("==================================================\n")

    # Caso de teste 1: Sucesso total
    teste_sucesso = "2026-03-01 * 'Supermercado' Expenses:Food 50.00 BRL Assets:Checking -50.00 BRL"
    validar_transacao_beancount(teste_sucesso)

    # Caso de teste 2: Erro de balanço
    teste_erro_balanco = "2026-03-01 * 'Cinema' Expenses:Entertainment 40.00 BRL Assets:Checking -30.00 BRL"
    validar_transacao_beancount(teste_erro_balanco)

    # Caso de teste 3: Erro de data brasileira
    teste_erro_data = "01/03/2026 * 'Almoço' Expenses:Food 25.00 BRL"
    validar_transacao_beancount(teste_erro_data)
