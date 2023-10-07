type Account object:
    name
    balance

    func new(name):  
        return Account{ name: name, balance: 0.0 }

    meth deposit(amt):
        balance += amt

    meth withdraw(amt):
        if amt > balance:
            throw error.InsufficientFunds
        else:
            balance -= amt

    meth show(title):
        print '{title or ''}, {name}, {balance}'

var a = Account.new('Savings')
a.show('Initial')
a.deposit(1000.00)
a.show('After deposit')
a.withdraw(100.00)
--try a.withdraw(2000.00)
a.show('After withdraw')