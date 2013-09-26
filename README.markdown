`record_info_runtime` `Parse Transform`
=======================================

Provides the functions:

    record_info_size/1
    record_info_fields/1

Which are the runtime equivalent of:

    record_info(size, _)
    record_info(fields, _)

The complete list of functions provided by the parse transform for
runtime usage with a variable storing the atom record name is below:

* `records/0` - List of all record names defined
* `record_new/1` - Create a record with the defaults assigned
* `record_info_size/1` - Provide the record size (same as `erlang:tuple_size/1`)
* `record_info_fields/1` - Provide the record fields list (atom names for each record field)
* `record_info_fieldtypes/1` - Provide the record fields paired with their types in a list of tuples

Using
-----

Add this line to your module to use the `record_info_runtime` parse transform:

    -compile([{parse_transform, record_info_runtime}]).

Build
-----

    rebar compile

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

License
-------

BSD

