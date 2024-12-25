CLASS lcl DEFINITION.

  PUBLIC SECTION.
    DATA: data_tab TYPE stringtab.
    METHODS: get_all_distinct_chars,
             calc_total_price RETURNING VALUE(r_val) TYPE i.

  PROTECTED SECTION.

    DATA: v_sum TYPE i,
          t_distinct_char TYPE TABLE OF char01.

    METHODS: left_search IMPORTING it_coords     TYPE match_result_tab
                                   i_curr_line   TYPE int4
                                   i_curr_offset TYPE int4
                          CHANGING c_line_count  TYPE i
                                   ct_visited    TYPE stringtab,
             right_search IMPORTING it_coords     TYPE match_result_tab
                                    i_curr_line   TYPE int4
                                    i_curr_offset TYPE int4
                          CHANGING c_line_count   TYPE i
                                   ct_visited     TYPE stringtab,
             down_search IMPORTING it_coords     TYPE match_result_tab
                                   i_curr_line   TYPE int4
                                   i_curr_offset TYPE int4
                          CHANGING c_offset_count  TYPE i
                                   ct_visited    TYPE stringtab,
             up_search   IMPORTING it_coords     TYPE match_result_tab
                                   i_curr_line   TYPE int4
                                   i_curr_offset TYPE int4
                          CHANGING c_offset_count  TYPE i
                                   ct_visited    TYPE stringtab,
             get_all_adjacent IMPORTING t_coord    TYPE match_result_tab
                              CHANGING  t_adjacent TYPE match_result_tab.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD up_search.

    DATA s_prev_coords TYPE match_result.

    LOOP AT it_coords ASSIGNING FIELD-SYMBOL(<fs_row>) STEP -1 WHERE line   LT i_curr_line
                                                                 AND offset EQ i_curr_offset.
      IF s_prev_coords IS INITIAL.
        IF abs( <fs_row>-line - i_curr_line ) NE 1.
          EXIT.
        ENDIF.
        DATA(v_string_coords)         = |{ i_curr_line }{ i_curr_offset }{ <fs_row>-line }{ <fs_row>-offset }|.
        DATA(v_string_coords_reverse) = |{ <fs_row>-line }{ <fs_row>-offset }{ i_curr_line }{ i_curr_offset }|.
        IF NOT line_exists( ct_visited[ table_line = v_string_coords ] ).
          s_prev_coords = <fs_row>.
          APPEND: v_string_coords         TO ct_visited,
                  v_string_coords_reverse TO ct_visited.
          c_offset_count += 2.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF abs( <fs_row>-line - s_prev_coords-line ) EQ 1.
        v_string_coords = |{ s_prev_coords-line }{ s_prev_coords-offset }{ <fs_row>-line }{ <fs_row>-offset }|.
        v_string_coords_reverse = |{ <fs_row>-line }{ <fs_row>-offset }{ s_prev_coords-line }{ s_prev_coords-offset }|.
        IF NOT line_exists( ct_visited[ table_line = v_string_coords ] ).
          s_prev_coords = <fs_row>.
          APPEND: v_string_coords         TO ct_visited,
                  v_string_coords_reverse TO ct_visited.
          c_offset_count += 2.
       ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD down_search.

    DATA s_prev_coords TYPE match_result.

    LOOP AT it_coords ASSIGNING FIELD-SYMBOL(<fs_row>) WHERE line   GT i_curr_line
                                                         AND offset EQ i_curr_offset.
      IF s_prev_coords IS INITIAL.
        IF abs( <fs_row>-line - i_curr_line ) NE 1.
          EXIT.
        ENDIF.
        DATA(v_string_coords)         = |{ i_curr_line }{ i_curr_offset }{ <fs_row>-line }{ <fs_row>-offset }|.
        DATA(v_string_coords_reverse) = |{ <fs_row>-line }{ <fs_row>-offset }{ i_curr_line }{ i_curr_offset }|.
        IF NOT line_exists( ct_visited[ table_line = v_string_coords ] ).
          s_prev_coords = <fs_row>.
          APPEND: v_string_coords         TO ct_visited,
                  v_string_coords_reverse TO ct_visited.
          c_offset_count = 2.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF abs( <fs_row>-line - s_prev_coords-line ) EQ 1.
        v_string_coords = |{ s_prev_coords-line }{ s_prev_coords-offset }{ <fs_row>-line }{ <fs_row>-offset }|.
        v_string_coords_reverse = |{ <fs_row>-line }{ <fs_row>-offset }{ s_prev_coords-line }{ s_prev_coords-offset }|.
        IF NOT line_exists( ct_visited[ table_line = v_string_coords ] ).
          s_prev_coords = <fs_row>.
          APPEND: v_string_coords         TO ct_visited,
                  v_string_coords_reverse TO ct_visited.
          c_offset_count += 2.
       ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD left_search.

    DATA s_prev_coords TYPE match_result.

    LOOP AT it_coords ASSIGNING FIELD-SYMBOL(<fs_row>) STEP - 1 WHERE line   EQ i_curr_line
                                                                  AND offset LT i_curr_offset.
      IF s_prev_coords IS INITIAL.
        IF abs( <fs_row>-offset - i_curr_offset ) NE 1.
          EXIT.
        ENDIF.

        DATA(v_string_coords)         = |{ i_curr_line }{ i_curr_offset }{ <fs_row>-line }{ <fs_row>-offset }|.
        DATA(v_string_coords_reverse) = |{ <fs_row>-line }{ <fs_row>-offset }{ i_curr_line }{ i_curr_offset }|.
        IF NOT line_exists( ct_visited[ table_line = v_string_coords ] ).
          s_prev_coords = <fs_row>.
          APPEND: v_string_coords         TO ct_visited,
                  v_string_coords_reverse TO ct_visited.
          c_line_count += 2.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF abs( <fs_row>-offset - s_prev_coords-offset ) EQ 1.
        v_string_coords = |{ s_prev_coords-line }{ s_prev_coords-offset }{ <fs_row>-line }{ <fs_row>-offset }|.
        v_string_coords_reverse = |{ <fs_row>-line }{ <fs_row>-offset }{ s_prev_coords-line }{ s_prev_coords-offset }|.
        IF NOT line_exists( ct_visited[ table_line = v_string_coords ] ).
          s_prev_coords = <fs_row>.
          APPEND: v_string_coords         TO ct_visited,
                  v_string_coords_reverse TO ct_visited.
          c_line_count += 2.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD right_search.

    DATA s_prev_coords TYPE match_result.

    LOOP AT it_coords ASSIGNING FIELD-SYMBOL(<fs_row>) WHERE line   EQ i_curr_line
                                                         AND offset GT i_curr_offset.
      IF s_prev_coords IS INITIAL.
        IF abs( <fs_row>-offset - i_curr_offset ) NE 1.
          EXIT.
        ENDIF.

        DATA(v_string_coords)         = |{ i_curr_line }{ i_curr_offset }{ <fs_row>-line }{ <fs_row>-offset }|.
        DATA(v_string_coords_reverse) = |{ <fs_row>-line }{ <fs_row>-offset }{ i_curr_line }{ i_curr_offset }|.

        IF NOT line_exists( ct_visited[ table_line = v_string_coords ] ).
          s_prev_coords = <fs_row>.
          c_line_count = 2.
          APPEND: v_string_coords         TO ct_visited,
                  v_string_coords_reverse TO ct_visited.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF abs( <fs_row>-offset - s_prev_coords-offset ) EQ 1.

        v_string_coords         = |{ s_prev_coords-line }{ s_prev_coords-offset }{ <fs_row>-line }{ <fs_row>-offset }|.
        v_string_coords_reverse = |{ <fs_row>-line }{ <fs_row>-offset }{ s_prev_coords-line }{ s_prev_coords-offset }|.

        IF NOT line_exists( ct_visited[ table_line = v_string_coords ] ).
          s_prev_coords = <fs_row>.
          APPEND: v_string_coords         TO ct_visited,
                  v_string_coords_reverse TO ct_visited.
          c_line_count += 2.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_all_adjacent.

    DATA: s_curr_coords TYPE match_result.

    LOOP AT t_coord ASSIGNING FIELD-SYMBOL(<fs_coord>).

      CLEAR s_curr_coords.

      IF LINES( t_adjacent ) EQ 0.
        APPEND <fs_coord> TO t_adjacent.
        CONTINUE.
      ENDIF.

      LOOP AT t_adjacent ASSIGNING FIELD-SYMBOL(<fs_adjacent>) WHERE line EQ <fs_coord>-line.

        IF abs( <fs_adjacent>-offset - <fs_coord>-offset ) EQ 1 AND
           NOT line_exists( t_adjacent[ line   = <fs_coord>-line
                                        offset = <fs_coord>-offset ] ).
          APPEND <fs_coord> TO t_adjacent.
          EXIT.
        ENDIF.
      ENDLOOP.

      LOOP AT t_adjacent ASSIGNING <fs_adjacent> WHERE offset EQ <fs_coord>-offset.

        IF abs( <fs_adjacent>-line - <fs_coord>-line ) EQ 1 AND
           NOT line_exists( t_adjacent[ line   = <fs_coord>-line
                                        offset = <fs_coord>-offset ] ).
          APPEND <fs_coord> TO t_adjacent.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD calc_total_price.

    DATA: v_line   TYPE i,
          v_offset TYPE i,
          v_line_count TYPE i,
          v_offset_cnt TYPE i,
          v_internal_borders TYPE i,
          v_counter          TYPE i,
          s_prev_coords TYPE match_result,
          t_visited TYPE stringtab,
          t_adjacent_coords TYPE match_result_tab.

    LOOP AT t_distinct_char ASSIGNING FIELD-SYMBOL(<fs_distinct_char>).

      FIND ALL OCCURRENCES OF <fs_distinct_char> IN TABLE data_tab RESULTS DATA(t_coords).

      DATA(v_lines_num) = lines( t_coords ).

      v_line   = -1.
      v_offset = -1.

      WHILE LINES( t_coords ) GT 0.

        REFRESH t_adjacent_coords.

        DO.
          DATA(v_count_of_adjacent) = LINES( t_adjacent_coords ).
          me->get_all_adjacent( EXPORTING t_coord   = t_coords
                                CHANGING t_adjacent = t_adjacent_coords ).
          CHECK v_count_of_adjacent EQ LINES( t_adjacent_coords ).
          EXIT.
        ENDDO.

        LOOP AT t_adjacent_coords ASSIGNING FIELD-SYMBOL(<fs_coord>).

          v_counter += 1.
*  ****
*  ****
*  *X->
*  ****
*  ****
          me->right_search( EXPORTING it_coords     = t_coords
                                      i_curr_line   = <fs_coord>-line
                                      i_curr_offset = <fs_coord>-offset
                            CHANGING c_line_count   = v_line_count
                                     ct_visited     = t_visited ).
*  ****
*  ****
*  <-X*
*  ****
*  ****
          me->left_search( EXPORTING it_coords     = t_coords
                                     i_curr_line   = <fs_coord>-line
                                     i_curr_offset = <fs_coord>-offset
                            CHANGING c_line_count  = v_line_count
                                     ct_visited    = t_visited ).
*  ****
*  ****
*  *X**
*  *|**
*  *v**
          me->down_search( EXPORTING it_coords     = t_coords
                                     i_curr_line   = <fs_coord>-line
                                     i_curr_offset = <fs_coord>-offset
                            CHANGING c_offset_count = v_offset_cnt
                                     ct_visited    = t_visited ).
*  *^**
*  *|**
*  *X**
*  ****
*  ****
          me->up_search( EXPORTING it_coords     = t_coords
                                   i_curr_line   = <fs_coord>-line
                                   i_curr_offset = <fs_coord>-offset
                          CHANGING c_offset_count = v_offset_cnt
                                   ct_visited    = t_visited ).

          v_internal_borders += v_line_count + v_offset_cnt.

          v_line   = <fs_coord>-line.
          v_offset = <fs_coord>-offset.

          CLEAR: v_line_count, v_offset_cnt.

        ENDLOOP.

        DATA(v_external_borders) = ( v_counter * 4 ) - v_internal_borders.

        IF v_counter * v_counter EQ v_external_borders.
          v_sum += v_external_borders.
        ELSE.
          v_sum += v_external_borders * v_counter.
        ENDIF.

        CLEAR: v_internal_borders, v_counter.
        REFRESH t_visited.

        LOOP AT t_adjacent_coords ASSIGNING <fs_coord>.
          DELETE t_coords WHERE line   = <fs_coord>-line
                            AND offset = <fs_coord>-offset.
        ENDLOOP.
      ENDWHILE.

    ENDLOOP.

    r_val = v_sum.

  ENDMETHOD.

  METHOD get_all_distinct_chars.
    LOOP AT data_tab ASSIGNING FIELD-SYMBOL(<fs_data>).
      DO strlen( <fs_data> ) TIMES.
        DATA(v_syindex) = sy-index - 1.
        CHECK NOT line_exists( t_distinct_char[ table_line = <fs_data>+v_syindex(1) ] ).
        APPEND <fs_data>+v_syindex(1) TO t_distinct_char.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: object   TYPE REF TO lcl,
        data_tab TYPE stringtab.

  object = NEW #( ).
  object->data_tab = data_tab.
  object->get_all_distinct_chars( ).
  DATA(v_sum) = object->calc_total_price( ).

  BREAK-POINT.
