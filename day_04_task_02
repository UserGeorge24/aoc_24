CLASS lcl DEFINITION.

 PUBLIC SECTION.
  DATA: t_data_tab TYPE stringtab.
  METHODS: sum_diagonals,
           get_sum RETURNING VALUE(r_val) TYPE i.

  PROTECTED SECTION.
    DATA: v_sum TYPE i.
      
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD get_sum.
    r_val = v_sum.
  ENDMETHOD.

  METHOD sum_diagonals.

    LOOP AT me->t_data_tab ASSIGNING FIELD-SYMBOL(<fs_data>).

      DO strlen( <fs_data> ) - 1 TIMES.

        CHECK <fs_data>+sy-index(1) EQ 'A'.

        DATA(v_prev_row) = VALUE #( t_data_tab[ sy-tabix - 1 ] OPTIONAL ).
        DATA(v_next_row) = VALUE #( t_data_tab[ sy-tabix + 1 ] OPTIONAL ).

        DATA(v_prev) = sy-index - 1.
        DATA(v_next) = sy-index + 1.

        TRY.
          DATA(top_left)     = v_prev_row+v_prev(1).
          DATA(bottom_left)  = v_next_row+v_prev(1).
          DATA(top_right)    = v_prev_row+v_next(1).
          DATA(bottom_right) = v_next_row+v_next(1).
        CATCH cx_root.
          CONTINUE.
        ENDTRY.

        IF top_left NE 'A' AND top_right NE 'A' AND bottom_left NE 'A' AND bottom_right NE 'A' AND
           top_left NE 'X' AND top_right NE 'X' AND bottom_left NE 'X' AND bottom_right NE 'X' .

          v_sum = COND #( WHEN top_left NE bottom_right AND top_right NE bottom_left THEN v_sum + 1 ELSE v_sum ).
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: o_lcl TYPE REF TO lcl,
        t_data_tab TYPE stringtab.
          
  o_lcl = NEW #( ).
  o_lcl->t_data_tab = t_data_tab.
  o_lcl->sum_diagonals( ).
  DATA(sum) = o_lcl->get_sum( ).
