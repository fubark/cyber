type Account:
    name    str
    balance float

fn Account :: @init(name str) -> Account:
    return {name=name, balance=0}

fn (&Account) deposit(amt float):
    self.balance += amt

fn (&Account) withdraw(amt float) -> !void:
    if amt > self.balance:
        return error.InsufficientFunds
    else:
        self.balance -= amt

fn (&Account) show(title str):
    print('%{title}, %{self.name}, %{self.balance}')

a := Account('Savings')
a.show('Initial')
a.deposit(1000.00)
a.show('After deposit')
a.withdraw(100.00)!
--a.withdraw(2000.00)!
a.show('After withdraw')