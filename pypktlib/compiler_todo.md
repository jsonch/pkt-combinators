X 0. Change `AtomState` to a `Constructor`
X 1. Add a project operation for metadata fields
1. Change `at` to `move` and delete the un-necessary passes that implement the weird `at` semantics
2. Add a builtin for `ingress_port` or `ingress_port_id`
    - should be in metadata
    - has to be set on packet rx?
3. New combinators:
    - `batch(n, pipe)`
    - `lock(bank)`
    - `unlock(bank)`
    - `atomic(pipe)`
5. Figure out semantics for memory banks
    - Do we still want `New`?
6. Consider supporting Pipe arguments?
    - Is a pipe with arguments just a python function? 