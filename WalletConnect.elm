port module WalletConnect exposing (connect, disconnect)

port connect : () -> Cmd msg
port disconnect : () -> Cmd msg
