let () =
  Core.Command.group
    ~summary:"Connect Four"
    [ "interactive", Interactive.command; "solve", Solve.command ]
  |> Command_unix.run
;;
