type Account:
    name    String
    balance float

    func deposit(amt float):
        balance += amt

    func withdraw(amt float):
        if amt > balance:
            throw error.InsufficientFunds
        else:
            balance -= amt

    func show(title String):
        print "$(title), $(name), $(balance)"

func Account.new(name String) Account:
    return Account{name=name, balance=0.0}

var a = Account.new('Savings')
a.show('Initial')
a.deposit(1000.00)
a.show('After deposit')
a.withdraw(100.00)
--a.withdraw(2000.00)
a.show('After withdraw')