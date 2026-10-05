open Bank_project

let () =
  let acc = new Bank.bank_account 100 in
  acc#deposit 500;
  (* let _  = (acc#withdraw 27) in *) 
  ignore (acc#withdraw 27);
  Printf.printf "Balance: $%d\n" acc#get_balance

let () = print_endline "Hello, World!"


