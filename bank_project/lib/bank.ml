class bank_account initial_balance =
  object (self)
    val mutable balance = initial_balance

    method deposit amount =
      balance <- balance + amount

    method withdraw amount =
      if balance >= amount then begin
        balance <- balance - amount;
        true
      end else
        false

    method get_balance = balance
  end
