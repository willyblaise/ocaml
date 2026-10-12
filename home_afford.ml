
let rec read_float prompt =
  print_string prompt;
  flush stdout;
  try float_of_string (read_line ())
  with Failure _ ->
    print_endline "Please enter a valid number.";
    read_float prompt;;

let income = read_float "Annual income: ";;
let down_payment = read_float "Down payment: ";;
let home_price = read_float "Home price: ";;

let loan = home_price -. down_payment;;
let annual_rate = 0.07;;
let monthly_rate = annual_rate /. 12.0;;
let months = 360.0;;

(* Mortgage principal and interest *)
let denominator =
  1.0 -. ((1.0 +. monthly_rate) ** (-. months));;

let mortgage_payment =
  loan *. monthly_rate /. denominator;;

(* Property tax: 2% of home value per year *)
let annual_property_tax = home_price *. 0.02;;
let monthly_property_tax = annual_property_tax /. 12.0;;

(* Property Insurance: 2500 per year *)
let annual_insurance = 2500.0;;
let monthly_insurance = annual_insurance /. 12.0;;

(* HOA fees: $1,200 per year *)
let annual_hoa = 1200.0;;
let monthly_hoa = annual_hoa /. 12.0;;

(* Total monthly housing cost *)
let total_monthly_payment =
  mortgage_payment +. monthly_property_tax +. monthly_hoa +. annual_insurance;;

(* Affordability *)
let monthly_income = income /. 12.0;;
let payment_ratio =
  total_monthly_payment /. monthly_income *. 100.0;;

Printf.printf "\n--- Mortgage Estimate ---\n";;
Printf.printf "Loan amount:             $%.2f\n" loan;;
Printf.printf "Mortgage principal/interest: $%.2f\n" mortgage_payment;;
Printf.printf "Monthly property tax:    $%.2f\n" monthly_property_tax;;
Printf.printf "Monthly Insurance:       $%.2f\n" monthly_insurance;;
Printf.printf "Monthly HOA:             $%.2f\n" monthly_hoa;;
Printf.printf "Total monthly payment:   $%.2f\n" total_monthly_payment;;
Printf.printf "Monthly gross income:    $%.2f\n" monthly_income;;
Printf.printf "Income spent on housing: %.1f%%\n" payment_ratio;;
