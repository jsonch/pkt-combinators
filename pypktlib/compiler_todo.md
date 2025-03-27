X 0. Change `AtomState` to a `Constructor`
X 1. Add a project operation for metadata fields
X 2. Change `at` to `move` and delete the un-necessary passes that implement the weird `at` semantics
    - Done with temporary hack that converts `move(loc)` to `at(loc, Noop)`, which ends up having 
      the same semantics.
3. Review "memory bank" semantics.
4. Add a builtin for `ingress_port` or `ingress_port_id`
    - should be in metadata
    - has to be set on packet rx?
5. New combinators:
    - `batch(n, pipe)`
    - `lock(bank)`
    - `unlock(bank)`
    - `atomic(pipe)`
6. Figure out semantics for memory banks
    - Do we still want `New`?
7. Consider supporting Pipe arguments?
    - Is a pipe with arguments just a python function?