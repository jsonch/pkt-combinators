X 0. Change `AtomState` to a `Constructor`

0. Add a project operation for metadata fields
1. Add a builtin for `ingress_port` or `ingress_port_id`
    - should be in metadata
    - has to be set on packet rx?
2. Change `at` to `move` and delete the un-necessary passes that implement the weird `at` semantics
3. New combinators:
    - `batch(n, pipe)`
    - `lock(bank)`
    - `unlock(bank)`
    - `atomic(pipe)`
4. Figure out semantics for memory banks
    - Do we still want `New`?
5. Consider supporting Pipe arguments?
    - Is a pipe with arguments just a python function? 