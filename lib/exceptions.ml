open Env

exception FnReturn of value
exception Break of value

exception UnexpectedBlockReturn
exception TypeError
exception TypeErrorWithInfo of string
exception UnexpectedToken of Token.tokentype
exception UnexpectedOperator
