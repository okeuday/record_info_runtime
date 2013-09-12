`record_info_runtime` `Parse Transform`
=======================================

Provides the functions:

    record_info_size/1
    record_info_fields/1

Which are the runtime equivalent of:

    record_info(size, _)
    record_info(fields, _)

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

