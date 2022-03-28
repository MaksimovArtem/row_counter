row_counter
=====

Creates simple process structure 
                 
                 
    row_counter_sup ---- counter_server
                     |
                     |-- file_count_sup ---- file_FileName1_count_server
                                         |-- file_FileName2_count_server
                                         ...

where file_FileNameX_count_server is the gen_server that counts rows in a single .erl file

.erl files are taken from UNIX catalog path that sent as input. All .erl files from subdirectories also will be checked

Code was checked on the next folder structure:

     
     InputFolder ---- SubDir1 ---- ErlFile1
                  |            |-- ErlFile2
                  |            ...
                  |
                  |-- SubDir2 ---- ErlFile1
                  |            |-- ErlFile2
                  |            ...
                  ...


Build
-----

    $ rebar3 compile
