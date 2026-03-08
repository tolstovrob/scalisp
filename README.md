# Scalisp

Когда-то тут будет исчерпывающий README, а пока что тут инструкция по запуску.

Для начала работы удобно использовать just:

```bash
just repl                   # для REPL
just run path/to/file.lsp   # для запуска из файла
```

Но можно и без него, через sbt:

```bash
sbt run                                             # для REPL
sbt "runMain entrypoint.runFile path/to/file.lsp"   # для запуска из файла
```
