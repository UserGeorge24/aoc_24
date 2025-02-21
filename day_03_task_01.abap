  v_src = REDUCE #( INIT v_init TYPE string
                    FOR <fs_row> IN t_data_tab
                    NEXT v_init = |{ v_init }{ to_lower( <fs_row> ) }| ).

  FIND ALL OCCURRENCES OF REGEX 'mul\([0-9]{1,3},[0-9]{1,3}\)' IN v_src RESULTS DATA(results).

  LOOP AT results ASSIGNING FIELD-SYMBOL(<fs_pos>).

    DATA(v_substring) = v_src+<fs_pos>-offset(<fs_pos>-length).

    FIND ALL OCCURRENCES OF REGEX '[0-9]{1,3}' IN v_substring RESULTS DATA(matches).

    DATA(v_offset) = VALUE #( matches[ 1 ]-offset OPTIONAL ).
    DATA(v_length) = VALUE #( matches[ 1 ]-length OPTIONAL ).

    v_num_01 = v_substring+v_offset(v_length).

    v_offset = VALUE #( matches[ 2 ]-offset OPTIONAL ).
    v_length = VALUE #( matches[ 2 ]-length OPTIONAL ).

    v_num_02 = v_substring+v_offset(v_length).

    v_sum += v_num_01 * v_num_02.

    CLEAR: v_length, v_num_01, v_num_02 ,v_offset, v_substring.

  ENDLOOP.
