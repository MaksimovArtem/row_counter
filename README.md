row_counter
=====

Creates simple process structure 
                 
                 
    row_counter_sup ---- counter_server
                     |
                     |-- file_count_sup ---- file_FileName1_count_server
                                         |-- file_FileName2_count_server
                                         ...

where file_FileNameX_count_server is the gen_server that counts rows in a single .erl file

.erl files are taken from UNIX catalog path that sent as input

Build
-----

    $ rebar3 compile
