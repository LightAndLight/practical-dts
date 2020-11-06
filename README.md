# `practical-dts`

## Why don't you use `singletons`?

For many singleton types in `singletons`, there exist isomorphic alternatives that have memory
and time asymptotics (see `pdts-nat`, `pdts-fin` for some examples). For practical usages of 
dependent types, we the more efficient representations.

## Highlights

* `pdts-record` - an efficient record type
* `pdts-hvector` - an efficient heterogeneous vector (anonymous record)
* `sql` - a well-typed PostgreSQL library 
