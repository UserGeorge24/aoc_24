CLASS lcl DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_states,
            number    TYPE int8,
            instances TYPE int8,
           END OF ty_states,
           tty_states TYPE STANDARD TABLE OF ty_states.

    DATA: data_tab TYPE stringtab.

    METHODS: blink RETURNING VALUE(r_sum) TYPE int8,
             put_to_table.

  PROTECTED SECTION.
    DATA t_before_blink TYPE tty_states.
    METHODS: add_new_number IMPORTING i_num       TYPE int8
                                      i_instances TYPE int8
                            CHANGING ct_tab TYPE tty_states.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD add_new_number.

    ASSIGN ct_tab[ number = i_num ] TO FIELD-SYMBOL(<fs_row>).

    IF sy-subrc EQ 0.
      <fs_row>-instances += i_instances.
    ELSE.
      INSERT VALUE #( number    = i_num
                      instances = i_instances ) INTO TABLE ct_tab.
    ENDIF.
  ENDMETHOD.

  METHOD put_to_table.

    SPLIT VALUE #( me->data_tab[ 1 ] OPTIONAL ) AT space INTO TABLE DATA(tab).

    LOOP AT tab ASSIGNING FIELD-SYMBOL(<fs_tab>).
      add_new_number( EXPORTING i_num       = CONV int8( <fs_tab> )
                                i_instances = 1
                      CHANGING  ct_tab = t_before_blink ).
    ENDLOOP.
  ENDMETHOD.

  METHOD blink .

    DATA: t_after_blink TYPE tty_states.

    DO 75 TIMES.

      LOOP AT t_before_blink ASSIGNING FIELD-SYMBOL(<fs_before_blink>).

        IF <fs_before_blink>-number EQ 0.

          add_new_number( EXPORTING i_num  = 1
                                    i_instances = <fs_before_blink>-instances
                          CHANGING  ct_tab = t_after_blink ).

        ELSEIF strlen( condense( conv string( <fs_before_blink>-number ) ) ) MOD 2 = 0.

          DATA(v_offset) = strlen( condense( conv string( <fs_before_blink>-number ) ) ) / 2.

          DATA(v_num_string) = condense( CONV string( <fs_before_blink>-number ) ).
          DATA(v_left_half)  = v_num_string+0(v_offset).
          DATA(v_right_half) = |{ v_num_string+v_offset(*) ALPHA = OUT }|.

          add_new_number( EXPORTING i_num  = CONV int8( v_left_half )
                                    i_instances = <fs_before_blink>-instances
                          CHANGING  ct_tab = t_after_blink ).

          add_new_number( EXPORTING i_num  = CONV int8( v_right_half )
                                    i_instances = <fs_before_blink>-instances
                          CHANGING  ct_tab = t_after_blink ).
        ELSE.

          add_new_number( EXPORTING i_num  = <fs_before_blink>-number * 2024
                                    i_instances = <fs_before_blink>-instances
                          CHANGING  ct_tab = t_after_blink ).
        ENDIF.
      ENDLOOP.

      t_before_blink = t_after_blink.
      REFRESH t_after_blink.
    ENDDO.

    r_sum = REDUCE #(	INIT v_count TYPE int8
                      FOR <fs_row> IN t_before_blink
                      NEXT v_count += <fs_row>-instances ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: object   TYPE REF TO lcl,
        data_tab TYPE stringtab.

  object = NEW #( ).
  object->data_tab = data_tab.
  object->put_to_table( ).
  DATA(v_sum) = object->blink( ).

  BREAK-POINT.
