CLASS lcl DEFINITION.

  PUBLIC SECTION.
    DATA: data_tab TYPE stringtab.

    METHODS: get_all_indexes,
             loop_path RETURNING VALUE(r_sum) TYPE i.

  PROTECTED SECTION.

    CONSTANTS: up    TYPE string VALUE 'up',
               down  TYPE string VALUE 'down',
               left  TYPE string VALUE 'left',
               right TYPE string VALUE 'right'.

    DATA:
      t_0_indexes TYPE match_result_tab,
      t_1_indexes TYPE match_result_tab,
      t_2_indexes TYPE match_result_tab,
      t_3_indexes TYPE match_result_tab,
      t_4_indexes TYPE match_result_tab,
      t_5_indexes TYPE match_result_tab,
      t_6_indexes TYPE match_result_tab,
      t_7_indexes TYPE match_result_tab,
      t_8_indexes TYPE match_result_tab,
      t_9_indexes TYPE match_result_tab,
      t_visited_indexes TYPE match_result_tab,
      v_sum TYPE i.

    METHODS:
             get_next_direction IMPORTING i_current_direction TYPE string
                                RETURNING VALUE(r_val) TYPE string,
             get_next_index_tab IMPORTING i_index TYPE i
                                RETURNING VALUE(r_tab) TYPE match_result_tab,
             get_next_level     IMPORTING it_curr_indexes TYPE match_result_tab
                                          i_curr_level TYPE i
                                RETURNING VALUE(rt_next_level) TYPE match_result_tab.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD loop_path.

    DATA: t_curr_indexes TYPE match_result_tab,
          v_syindex TYPE syindex.

    LOOP AT t_0_indexes ASSIGNING FIELD-SYMBOL(<fs_0_index>).

      APPEND <fs_0_index> TO t_curr_indexes.

      DO 9 TIMES.

        v_syindex = sy-index.

        t_curr_indexes = me->get_next_level( it_curr_indexes = t_curr_indexes
                                             i_curr_level    = sy-index ).
        CHECK lines( t_curr_indexes ) EQ 0.
        EXIT.
      ENDDO.

      IF v_syindex EQ 9.
        v_sum += lines( t_curr_indexes ).
      ENDIF.

      REFRESH: t_curr_indexes, t_visited_indexes.
      CLEAR v_syindex.

    ENDLOOP.

    r_sum = v_sum.

  ENDMETHOD.

  METHOD get_next_level.

    DATA: o_cx_root TYPE REF TO cx_root,
          t_next_indexes TYPE match_result_tab,
          s_temp_index TYPE match_result,
          v_direction  TYPE string.

    LOOP AT it_curr_indexes ASSIGNING FIELD-SYMBOL(<fs_curr_index>).

      CLEAR s_temp_index.
      v_direction = left.
      t_next_indexes = get_next_index_tab( i_curr_level ).

      DO 4 TIMES.

        TRY.
          CASE v_direction.
            WHEN left.
              s_temp_index = t_next_indexes[ line   = <fs_curr_index>-line
                                             offset = <fs_curr_index>-offset - 1 ].
            WHEN down.
              s_temp_index = t_next_indexes[ line   = <fs_curr_index>-line + 1
                                             offset = <fs_curr_index>-offset ].
            WHEN right.
              s_temp_index = t_next_indexes[ line   = <fs_curr_index>-line
                                             offset = <fs_curr_index>-offset + 1 ].
            WHEN up.
              s_temp_index = t_next_indexes[ line   = <fs_curr_index>-line - 1
                                             offset = <fs_curr_index>-offset ].
          ENDCASE.
        CATCH cx_root INTO o_cx_root.
          v_direction = get_next_direction( v_direction ).
          FREE o_cx_root.
          CONTINUE.
        ENDTRY.

        v_direction = get_next_direction( v_direction ).

        IF NOT line_exists( t_visited_indexes[ line   = s_temp_index-line
                                               offset = s_temp_index-offset ] ).
          APPEND: s_temp_index TO t_visited_indexes,
                  s_temp_index TO rt_next_level.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_next_index_tab.
    r_tab = SWITCH #( i_index
                     WHEN 1 THEN t_1_indexes
                     WHEN 2 THEN t_2_indexes
                     WHEN 3 THEN t_3_indexes
                     WHEN 4 THEN t_4_indexes
                     WHEN 5 THEN t_5_indexes
                     WHEN 6 THEN t_6_indexes
                     WHEN 7 THEN t_7_indexes
                     WHEN 8 THEN t_8_indexes
                     WHEN 9 THEN t_9_indexes ).
  ENDMETHOD.

  METHOD get_next_direction.
    r_val = SWITCH #( i_current_direction
                      WHEN left  THEN up
                      WHEN up    THEN right
                      WHEN right THEN down
                      WHEN down  THEN left ).
  ENDMETHOD.

  METHOD get_all_indexes.

    FIND ALL OCCURRENCES OF:
                             '0' IN TABLE data_tab RESULTS t_0_indexes,
                             '1' IN TABLE data_tab RESULTS t_1_indexes,
                             '2' IN TABLE data_tab RESULTS t_2_indexes,
                             '3' IN TABLE data_tab RESULTS t_3_indexes,
                             '4' IN TABLE data_tab RESULTS t_4_indexes,
                             '5' IN TABLE data_tab RESULTS t_5_indexes,
                             '6' IN TABLE data_tab RESULTS t_6_indexes,
                             '7' IN TABLE data_tab RESULTS t_7_indexes,
                             '8' IN TABLE data_tab RESULTS t_8_indexes,
                             '9' IN TABLE data_tab RESULTS t_9_indexes.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: object   TYPE REF TO lcl,
        data_tab TYPE stringtab.

  object = NEW #( ).
  object->data_tab = data_tab.
  object->get_all_indexes( ).
  DATA(v_sum) = object->loop_path( ).

  BREAK-POINT.
