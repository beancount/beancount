from beancount.__beancount import Amount

print(Amount("1", "2").number)
print(Amount("1", "2233a").currency)
