  v_src = REDUCE #( INIT v_init TYPE string
                    FOR <fs_row> IN t_data_tab
                    NEXT v_init = |{ v_init }{ to_lower( <fs_row> ) }| ).

  FIND ALL OCCURRENCES OF REGEX: |don't\\(\\)|                  IN v_src RESULTS DATA(results_dont),
                                 'do\(\)'                       IN v_src RESULTS DATA(results_do),
                                 'mul\([0-9]{1,3},[0-9]{1,3}\)' IN v_src RESULTS DATA(results_number).

  LOOP AT results_dont ASSIGNING FIELD-SYMBOL(<fs_results_dont>).
    <fs_results_dont>-line = 0.
  ENDLOOP.

  LOOP AT results_do ASSIGNING FIELD-SYMBOL(<fs_results_do>).
    <fs_results_do>-line = 1.
  ENDLOOP.

  LOOP AT results_number ASSIGNING FIELD-SYMBOL(<fs_results_number>).
    <fs_results_number>-line = 2.
  ENDLOOP.

  APPEND LINES OF: results_do     TO t_matches,
                   results_dont   TO t_matches,
                   results_number TO t_matches.

  SORT t_matches BY offset ASCENDING.

  DATA: v_dont TYPE boolean.

* 0 - don't()
* 1 - do()
* 2 - mul()

  LOOP AT t_matches ASSIGNING FIELD-SYMBOL(<fs_pos>).

    v_dont = COND #( WHEN <fs_pos>-line = 0 THEN abap_true  ELSE v_dont ).
    v_dont = COND #( WHEN <fs_pos>-line = 1 THEN abap_false ELSE v_dont ).

    CHECK v_dont EQ abap_false AND <fs_pos>-line EQ 2.

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
