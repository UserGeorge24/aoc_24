
CLASS lcl DEFINITION.

 PUBLIC SECTION.

  DATA: t_data_tab TYPE stringtab,
        v_src      TYPE string.

  METHODS: sum_vertical,
           sum_horizontal,
           sum_diagonals,
           get_sum RETURNING VALUE(r_val) TYPE i.

  PROTECTED SECTION.

    DATA: v_vertical              TYPE i,
          v_vertical_reverse      TYPE i,
          v_horizontal            TYPE i,
          v_horizontal_reverse    TYPE i,
          v_digonal_left          TYPE i,
          v_digonal_left_reverse  TYPE i,
          v_digonal_right         TYPE i,
          v_digonal_right_reverse TYPE i.

    METHODS: sum_diagonal IMPORTING iv_right TYPE boolean,
             convert_rows.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD get_sum.
    r_val = v_vertical     + v_vertical_reverse      + v_horizontal    + v_horizontal_reverse    +
            v_digonal_left + v_digonal_left_reverse  + v_digonal_right + v_digonal_right_reverse .
  ENDMETHOD.

  METHOD convert_rows.

    LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_t_data_tab>).
      <fs_t_data_tab> = reverse( <fs_t_data_tab> ).
    ENDLOOP.

  ENDMETHOD.

  METHOD sum_diagonal.

    DATA: v_from_loop TYPE i,
          v_from      TYPE i.

    v_from_loop = 1.
    DATA(v_matrix_diagonal_max_elem_num) = strlen( VALUE #( t_data_tab[ 1 ] OPTIONAL ) ).
    CLEAR v_src.

    DO v_matrix_diagonal_max_elem_num * 2 TIMES.

      v_from = COND #( WHEN sy-index GE v_matrix_diagonal_max_elem_num THEN v_matrix_diagonal_max_elem_num - 1 ELSE sy-index - 1 ).

      LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_data>) FROM v_from_loop.

        v_src = |{ v_src }{ <fs_data>+v_from(1) }|.
        v_from -= 1.

        CHECK v_from LT 0.
        EXIT.
      ENDLOOP.

      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

      FIND ALL OCCURRENCES OF REGEX: |xmas| IN to_lower( v_src ) MATCH COUNT DATA(diagonal_temp),
                                     |samx| IN to_lower( v_src ) MATCH COUNT DATA(diagonal_rev_temp).
      IF iv_right EQ abap_true.
        v_digonal_right         += diagonal_temp.
        v_digonal_right_reverse += diagonal_rev_temp.
      ELSE.
        v_digonal_left         += diagonal_temp.
        v_digonal_left_reverse += diagonal_rev_temp.
      ENDIF.

      IF sy-index GE v_matrix_diagonal_max_elem_num.
        v_from_loop += 1.
      ENDIF.

      CLEAR v_src.
    ENDDO.
  ENDMETHOD.

  METHOD sum_vertical.

    CLEAR v_src.

*  Find vertical and reverse
   DO.
     DATA(v_from) = sy-index - 1.
     TRY.
       v_src = REDUCE #( INIT src TYPE string
                         FOR <fs_row> IN t_data_tab
                         NEXT src = |{ src }{ <fs_row>+v_from(1) }| ).
     CATCH cx_root.
       EXIT.
     ENDTRY.

     FIND ALL OCCURRENCES OF REGEX: |xmas| IN to_lower( v_src ) MATCH COUNT DATA(vert_temp),
                                    |samx| IN to_lower( v_src ) MATCH COUNT DATA(vert_ver_temp).
     v_vertical         += vert_temp.
     v_vertical_reverse += vert_ver_temp.
   ENDDO.

  ENDMETHOD.

  METHOD sum_horizontal.
    LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_t_data_tab>).
       FIND ALL OCCURRENCES OF REGEX: |xmas| IN to_lower( <fs_t_data_tab> ) MATCH COUNT DATA(horizontal),
                                      |samx| IN to_lower( <fs_t_data_tab> ) MATCH COUNT DATA(horizontal_reverse).
      v_horizontal += horizontal.
      v_horizontal_reverse += horizontal_reverse.
    ENDLOOP.
  ENDMETHOD.

  METHOD sum_diagonals.
    me->sum_diagonal( iv_right = abap_true ).
    me->convert_rows( ).
    me->sum_diagonal( iv_right = abap_false ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: o_lcl TYPE REF TO lcl,
        t_data_tab TYPE stringtab.

  o_lcl = NEW #( ).
  o_lcl->t_data_tab = t_data_tab.
  o_lcl->sum_horizontal( ).
  o_lcl->sum_vertical( ).
  o_lcl->sum_diagonals( ).
  DATA(sum) = o_lcl->get_sum( ).
