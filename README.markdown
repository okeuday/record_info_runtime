`record_info_runtime` Parse Transform
=====================================

Provides the functions:
    record_info_size/1
    record_info_fields/1

Which are the runtime equivalent of:
    record_info(size, _)
    record_info(fields, _)

Using
-----

Add this line to your module:
    -compile([{parse_transform, record_info_runtime}]).

Make sure your `lib_dirs` (in rebar) or
`ERL_LIBS` (in the environmental variables) has the path to
the `record_info_runtime.beam` file when compiling your module.  If no records
are defined within your module, the parse transform is idempotent.

Build
-----

    rebar compile

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

License
-------

BSD

