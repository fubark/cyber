type Account:
    name    string
    balance float

    fn deposit(self, amt float):
        self.balance += amt

    fn withdraw(self, amt float):
        if amt > self.balance:
            throw error.InsufficientFunds
        else:
            self.balance -= amt

    fn show(self, title string):
        print "$(title), $(self.name), $(self.balance)"

fn Account.new(name string) Account:
    return Account{name=name, balance=0.0}

var a = Account.new('Savings')
a.show('Initial')
a.deposit(1000.00)
a.show('After deposit')
a.withdraw(100.00)
--a.withdraw(2000.00)
a.show('After withdraw')