  LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_src>).

    SPLIT <fs_src> AT '' INTO TABLE t_levels.

    v_num_01 = VALUE i( t_levels[ 1 ] OPTIONAL ).
    v_num_02 = VALUE i( t_levels[ 2 ] OPTIONAL ).

    v_minus = COND #( WHEN v_num_01 GT v_num_02 THEN abap_true ELSE abap_false ).
    v_plus  = COND #( WHEN v_num_01 LT v_num_02 THEN abap_true ELSE abap_false ).

    IF v_minus IS INITIAL AND v_plus IS INITIAL.
      CONTINUE.
    ENDIF.

    DATA(v_safe) = REDUCE #( INIT v_count TYPE i
                             FOR <fs_row> IN t_levels TO lines( t_levels ) - 1
                             INDEX INTO v_index
                             LET v_next = VALUE #( t_levels[ v_index + 1 ] OPTIONAL )
                             IN
                             NEXT v_count = COND #( WHEN v_plus  EQ abap_true AND CONV i( <fs_row> ) GE CONV i( v_next )
                                                      OR v_minus EQ abap_true AND CONV i( <fs_row> ) LE CONV i( v_next )
                                                      OR abs( CONV i( <fs_row> ) - CONV i( v_next ) ) NOT BETWEEN 1 AND 3
                                                      THEN v_count + 1
                                                      ELSE v_count ) ).

    v_sum = COND #( WHEN v_safe EQ 0 THEN v_sum + 1 ELSE v_sum ).

    CLEAR: v_minus, v_plus.

  ENDLOOP.
