# R bindings for nlpipe

Simple R bindings to connect to a NLPipe server (see http:://github.com/vanatteveldt/nlpipe).

```{r}
> devtools::install_github("vanatteveldt/nlpiper")
> nlpiper::process("test_upper", "test")
[1] "TEST"
```

You can specify the server url (default: localhost:5001) as a `server=` option to each call, or globally set

```{r}
options(nlpiper.server="http://server.example.com:5000")
```
