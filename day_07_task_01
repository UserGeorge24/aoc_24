CLASS lcl DEFINITION.

 PUBLIC SECTION.
  DATA: data_tab TYPE stringtab.
  METHODS: get_sum RETURNING VALUE(r_val) TYPE int8.
 PROTECTED SECTION.
    DATA v_sum TYPE int8.
    METHODS: generate_combinations IMPORTING iv_slots TYPE i
                                   RETURNING VALUE(r_tab) TYPE stringtab.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD generate_combinations.

    DATA: lv_index TYPE i,
          lv_comb TYPE string.

    " Calculate the total number of combinations: 2^slots
    DATA(lv_total_combinations) = CONV i( 2 ** iv_slots ).

    " Loop through all possible combinations
    DO lv_total_combinations TIMES.
      lv_comb = ''.
      DATA(lv_temp_index) = sy-index - 1. " Start from 0

      DO iv_slots TIMES.
        " Add '+' for 0 and '*' for 1 based on binary representation
        IF lv_temp_index MOD 2 = 0.
          lv_comb = '+' && lv_comb.
        ELSE.
          lv_comb = '*' && lv_comb.
        ENDIF.
        lv_temp_index = lv_temp_index DIV 2.
      ENDDO.

      APPEND lv_comb TO r_tab.
    ENDDO.
  ENDMETHOD.

  METHOD get_sum.

    DATA: v_sum_temp TYPE int8,
          v_test_values TYPE int8,
          t_combinations TYPE stringtab.

    LOOP AT data_tab ASSIGNING FIELD-SYMBOL(<fs_data_tab>).

      CLEAR v_sum_temp.
      REFRESH t_combinations.

      SPLIT <fs_data_tab> AT ':' INTO TABLE DATA(temp).

      v_test_values = VALUE #( temp[ 1 ] OPTIONAL ).

      SPLIT VALUE #( temp[ 2 ] OPTIONAL ) AT space INTO TABLE temp.

      DELETE temp INDEX 1.

      t_combinations = me->generate_combinations( lines( temp ) ).

      LOOP AT t_combinations ASSIGNING FIELD-SYMBOL(<fs_t_combinations>).

        LOOP AT temp ASSIGNING FIELD-SYMBOL(<fs_temp>).

          DATA(offset) = sy-tabix - 1.

          CASE <fs_t_combinations>+offset(1).
          WHEN '+'.
              v_sum_temp += <fs_temp>.
          WHEN '*'.
              v_sum_temp *= <fs_temp>.
          ENDCASE.

        ENDLOOP.

        IF v_sum_temp EQ v_test_values.
          v_sum += v_sum_temp.
          EXIT.
        ENDIF.
        CLEAR v_sum_temp.
      ENDLOOP.

    ENDLOOP.

    r_val = v_sum.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: object TYPE REF TO lcl,
        data_tab TYPE stringtab.

  object = NEW #( ).
  object->data_tab = data_tab.
  DATA(sum) = object->get_sum( ).

  BREAK-POINT.
