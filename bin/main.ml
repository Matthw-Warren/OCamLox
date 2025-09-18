
let scanner = OCamLox.Utils.runPromt ()

let () = OCamLox.Token.print_tokens scanner.token_list
