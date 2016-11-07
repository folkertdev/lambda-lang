
##Building

This projects uses [stack](https://docs.haskellstack.org/en/stable/README/). 

Installalling and building is as simple as 

```sh
stack setup
stack build
```

##REPL

The repl can be started using 

```sh
stack exec repl
```

##Compilation

```sh 
# the '--' makes the argument go to the executed program instead of 'stack'
stack exec interpreter -- examples/fibonacci.txt
```
