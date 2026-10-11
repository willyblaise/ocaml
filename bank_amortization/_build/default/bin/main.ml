let () = print_endline "Hello, World!"
type mortgage_params = {
  principal : float;
  annual_rate : float; (* Expressed as a decimal, e.g., 0.065 for 6.5% *)
  years : int;
}

type payment_entry = {
  month : int;
  payment : float;
  principal_paid : float;
  interest_paid : float;
  remaining_balance : float;
}

(** Calculate the fixed monthly payment *)
let monthly_payment params =
  let monthly_rate = params.annual_rate /. 12.0 in
  let total_months = float_of_int (params.years * 12) in
  if monthly_rate = 0.0 then
    params.principal /. total_months
  else
    let factor = (1.0 +. monthly_rate) ** total_months in
    params.principal *. (monthly_rate *. factor) /. (factor -. 1.0)

(** Generate the complete schedule using tail recursion *)
let amortization_schedule params =
  let pmt = monthly_payment params in
  let monthly_rate = params.annual_rate /. 12.0 in
  let total_months = params.years * 12 in

  let rec build_schedule month balance acc =
    if month > total_months || balance <= 0.001 then
      List.rev acc
    else
      let interest = balance *. monthly_rate in
      (* Handle final payment rounding/overpayment *)
      let principal_part = min balance (pmt -. interest) in
      let actual_pmt = principal_part +. interest in
      let new_balance = balance -. principal_part in
      let entry = {
        month;
        payment = actual_pmt;
        principal_paid = principal_part;
        interest_paid = interest;
        remaining_balance = Float.max 0.0 new_balance;
      } in
      build_schedule (month + 1) new_balance (entry :: acc)
  in
  build_schedule 1 params.principal []

(** Pretty print the first n rows and loan summary *)
let print_schedule ~preview_rows params =
  let pmt = monthly_payment params in
  let schedule = amortization_schedule params in
  let total_paid = List.fold_left (fun acc e -> acc +. e.payment) 0.0 schedule in
  let total_interest = total_paid -. params.principal in

  Printf.printf "=== Mortgage Summary ===\n";
  Printf.printf "Loan Amount   : $%.2f\n" params.principal;
  Printf.printf "Annual Rate   : %.2f%%\n" (params.annual_rate *. 100.0);
  Printf.printf "Term          : %d years\n" params.years;
  Printf.printf "Monthly PMT   : $%.2f\n" pmt;
  Printf.printf "Total Paid    : $%.2f\n" total_paid;
  Printf.printf "Total Interest: $%.2f\n\n" total_interest;

  Printf.printf "%-6s | %-10s | %-10s | %-10s | %-12s\n"
    "Month" "Payment" "Principal" "Interest" "Balance";
  Printf.printf "---------------------------------------------------------\n";

  let rec show_preview n = function
    | [] -> ()
    | _ when n = 0 -> Printf.printf "  ... remaining rows omitted ...\n"
    | e :: rest ->
      Printf.printf "%-6d | $%-9.2f | $%-9.2f | $%-9.2f | $%-11.2f\n"
        e.month e.payment e.principal_paid e.interest_paid e.remaining_balance;
      show_preview (n - 1) rest
  in
  show_preview preview_rows schedule

let () =
  let loan = {
    principal = 300_000.0;
    annual_rate = 0.065;
    years = 30;
  } in
  print_schedule ~preview_rows:360 loan

