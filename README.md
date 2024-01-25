# Ocaml grep

Search patterns in files

## How to use

Install dependencies

```bash
opam install core ppx_jane ANSITerminal
```

Add shortcut

```
ln -s $PROJECT_PATH/_build/default/bin/main.exe $PROJECT_PATH/grep
```

And run the program

```
./grep "test_pattern" test_file
```

The interface is simmilar to the standard grep

## License

[MIT](LICENSE)
