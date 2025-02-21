  LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_src>).

    SPLIT <fs_src> AT '' INTO v_num_01 v_num_02.

    APPEND: v_num_01 TO t_numbers_01,
            v_num_02 TO t_numbers_02.

  ENDLOOP.

  SORT: t_numbers_01 DESCENDING,
        t_numbers_02 DESCENDING.

  v_sum = REDUCE #( INIT v_init TYPE i
                    FOR <fs_num_01> IN t_numbers_01
                    INDEX INTO v_index
                    NEXT v_init += abs( <fs_num_01> - VALUE #( t_numbers_02[ v_index ] OPTIONAL ) ) ).
