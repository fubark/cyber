type Account object:
    name
    balance

    func new(name):  
        return Account{ name: name, balance: 0 }

    func deposit(self, amt):
        self.balance += amt

    func withdraw(self, amt):
        if amt > self.balance:
            return error(#InsufficientFunds)
        else:
            self.balance -= amt

    func show(self, title):
        print '{title or ''}, {self.name}, {self.balance}'

a = Account.new('Savings')
a.show('Initial')
a.deposit(1000.00)
a.show('After deposit')
try a.withdraw(100.00)
--try a.withdraw(2000.00)
a.show('After withdraw')