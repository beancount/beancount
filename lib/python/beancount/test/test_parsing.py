

def __test_posting_regexp():

    test_posting_lines = """\
  Expenses:Financial:Commissions                                           24.88 USD
  Assets:Investments:RBC-Broker:Account-RSP                      86.132 "NBC860"
  Assets:Investments:RBC-Broker:Account-US                              -130.00 IWM  @ 71.2701 USD
  Assets:Investments:RBC-Broker:Account-US                              9255.05 USD
  Expenses:Financial:Commissions                                            9.95 USD
  ! Expenses:Financial:Fees
    """

    for line in test_posting_lines.splitlines():
        print
        print line
        mo = posting_re.match(line)
        print (mo.groups() if mo else None)

