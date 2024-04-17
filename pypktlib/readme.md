Minimal python implementation of combinator library. 

Run the example: 
```
    ./mario.py ./progs/prog1_py.c
    make prog1
    ./prog1
```

Usage: 
    1. define atoms and the pipelines inline in a C program. Look at `./progs/prog1_py.c` as an example. All the python code is wrapped in the PYTHON({ }) macro, which is just a noop.
    2. pass an annotated C program to `mario.py`, which will run all the python blocks and replace them with the printed output. `mario.py` ultimately runs a compile function that turns the defined pipe into a handler. 
    3. the output of `mario.py` should be a compileable c program.