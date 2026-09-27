module Bank {
  const accounts: Set[str]
  type Status = Idle | Busy(int)
  var balances: (str -> int)
  var status: Status
  pure def canWithdraw(b: int, n: int): bool = ((b - n) >= 0)
  val total = balances.keys().fold(0, ((acc, k) => (acc + balances.get(k))))
  action init = all {
    balances' = accounts.mapBy((k => 0)),
    status' = Idle,
  }
  action deposit(who: str, n: int): bool = all {
    who.in(accounts),
    (n > 0),
    balances' = balances.set(who, (balances.get(who) + n)),
    status' = Busy(n),
  }
  action withdraw(who: str, n: int): bool = all {
    who.in(accounts),
    canWithdraw(balances.get(who), n),
    balances' = balances.set(who, (balances.get(who) - n)),
    status' = match status {
      | Busy(k) => Busy((k + n))
      | _ => Idle
    },
  }
  val noNegatives = accounts.forall((a => (balances.get(a) >= 0)))
  val totalNonNegative = (total >= 0)
  action step = {
    nondet who = accounts.oneOf()
    nondet n = 1.to(5).oneOf()
    any {
      deposit(who, n),
      withdraw(who, n),
    }
  }
  temporal eventuallyBusy = eventually((status != Idle))
}

module BankTest {
  import Bank(accounts = Set("a", "b")).*
  run happyTest = init.then(deposit("a", 3)).then(withdraw("a", 1)).expect((balances.get("a") == 2))
}
