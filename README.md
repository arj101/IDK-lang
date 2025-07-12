# IDK lang
A language when you don't know what you are doing.
Has most of what you expect from an average programming language except modules and import statements (code was always meant to be written in a single file).
Supports object oriented programming through a very unsound implementation(and inefficient) of classes and inheritance.
Also can make functional programming nerds happy with first class functions, closures and array iterator functions.
The lexer might be broken idk, I didnt test it.

* Very efficient syntax, no need of semicolons or newlines, just write everything on a single line if you feel like it ;)
  * Commas are optional in arrays and function parameters.

* Identifiers can be non unicode, so feel free to use emojis as variable names, but idk if it will work, i didnt test it (works with the dot product symbol though ⨀)


Sample code:
```
println([1 5 25 ].filter(fun (x): x > 10)) 

fun close(x) {
    let y = 5
    return fun (): x + y
}

println(close(5)())
```

**How to run**
* Ensure that you have OCaml installed, along with dune
```
dune exec --profile release idk path/to/code/file.idk
```
