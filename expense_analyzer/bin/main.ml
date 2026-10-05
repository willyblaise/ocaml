type expense = {
  date : string;
  description : string;
  amount : float;
  category : string;
}

let expenses = [
  {
    date = "2026-09-24";
    description = "Coffee";
    amount = 5.75;
    category = "Food";
  };
  {
    date = "2026-09-24";
    description = "Gas";
    amount = 54.20;
    category = "Transportation";
  };
  {
    date = "2026-09-23";
    description = "Groceries";
    amount = 82.31;
    category = "Food";
  };
]

let total_expenses expenses =
  List.fold_left
    (fun total expense -> total +. expense.amount)
    0.0
    expenses

let () =
  let total = total_expenses expenses in
  Printf.printf "Total expenses: $%.2f\n" total
