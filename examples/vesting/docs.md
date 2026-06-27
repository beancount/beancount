# RSU Vesting Example

This directory contains an example of how to track the vesting of Restricted Stock Units (RSUs) using Beancount.

## Key Concepts

The core idea demonstrated here is the separation of **unvested** shares from **vested** shares using different commodities. This allows you to track your unvested portfolio value separately from your actual liquid assets.

### Commodities
*   **`HOOL`**: The actual common stock (liquid asset).
*   **`HOOL.UNVEST`**: A tracking commodity representing unvested shares.

### Account Structure

*   **`Assets:US:Hooli:Unvested:...`**: Holds the `HOOL.UNVEST` commodity. Represents future potential value.
*   **`Income:US:Hooli:Awards`**: The source of the unvested grants.
*   **`Income:US:Hooli:RSU`**: The realized income (in fiat currency, e.g., USD) at the moment of vesting.
*   **`Assets:US:Hooli:RSURefund`**: A clearing account used to balance the vesting transaction, taxes, and the final share conversion. It handles the cash flow differences and rounding errors.
*   **`Expenses:Hooli:Vested`**: specific expense account to balance the removal of `HOOL.UNVEST` upon vesting.

## Transaction Flow

1.  **Grant**: You receive an award.
    *   `Income:Awards` -> `Assets:Unvested` (in `HOOL.UNVEST`).

2.  **Vesting Event (Income & Taxes)**: Shares vest.
    *   Recognize `Income:RSU` (in USD).
    *   Record tax expenses (Fed, State, Medicare, etc.).
    *   The net result is credited to `Assets:RSURefund`.

3.  **Conversion**: Unvested tracking shares become real shares.
    *   Reduce `Assets:Unvested` (remove `HOOL.UNVEST`).
    *   Increase `Assets:Brokerage` (add `HOOL` at the vesting price/cost basis).
    *   Balance against `Assets:RSURefund` and `Expenses:Vested`.

4.  **Cash Payout**:
    *   Any remaining cash in `Assets:RSURefund` (from fractional shares or tax over-withholding) is transferred to your checking account.

## Files

*   `vesting.beancount`: The complete example file demonstrating the commodities, accounts, and transaction lifecycle.
