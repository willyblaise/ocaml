let () = print_endline "Hello, World!"

type loan = {
  principal : float;
  annual_rate : float;
  years : int;
}

let mortgage = {
  principal = 300000.0;
  annual_rate = 6.5;
  years = 30;
}

let monthly_payment loan =
  let monthly_rate = loan.annual_rate /. 100.0 /. 12.0 in
  let payments = loan.years * 12 in
  let factor = (1.0 +. monthly_rate) ** float_of_int payments in
  loan.principal *. monthly_rate *. factor /. (factor -. 1.0)

let () =
  let payment = monthly_payment mortgage in
  Printf.printf "Monthly payment: $%.2f\n" payment
