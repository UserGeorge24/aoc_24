
CLASS lcl DEFINITION.
  PUBLIC SECTION.

    DATA: data_tab TYPE stringtab,
          t_locations_of_antennas TYPE match_result_tab,
          t_locations_of_antinode TYPE match_result_tab.

    METHODS: set_locations_of_antennas,
             count_antinodes,
             get_all_locations_of_actual IMPORTING i_char TYPE char01
                                         RETURNING VALUE(r_tab) TYPE match_result_tab,
             get_all_locations RETURNING VALUE(r_tab) TYPE match_result_tab.

  PROTECTED SECTION.

    METHODS: get_diff_of_offset IMPORTING i_match_row_01 TYPE match_result
                                          i_match_row_02 TYPE match_result
                                RETURNING VALUE(r_offset) TYPE i.

    METHODS: get_diff_of_line IMPORTING i_match_row_01 TYPE match_result
                                        i_match_row_02 TYPE match_result
                              RETURNING VALUE(r_line) TYPE i.

    METHODS: find_horizontal IMPORTING i_match_row_01 TYPE match_result
                                       i_match_row_02 TYPE match_result
                                       i_offset_diff  TYPE i.

    METHODS: find_vertical IMPORTING i_match_row_01 TYPE match_result
                                     i_match_row_02 TYPE match_result
                                     i_lines_diff   TYPE i.

    METHODS: find_diagonal IMPORTING i_match_row_01 TYPE match_result
                                     i_match_row_02 TYPE match_result
                                     i_lines_diff   TYPE i
                                     i_offset_diff  TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD get_all_locations.
    r_tab = me->t_locations_of_antinode.
  ENDMETHOD.

  METHOD count_antinodes.

    DATA:
          s_current_antenna_loc TYPE match_result,
          s_two_antenna_diff    TYPE match_result.

    LOOP AT data_tab INTO DATA(s_actual_row).

      DATA(v_sytabix) = sy-tabix.

      DO strlen( s_actual_row ) TIMES.
        DATA(v_syindex) = sy-index - 1.
        DATA(v_antenna) = s_actual_row+v_syindex(1).

        CHECK v_antenna NE '.'.

        s_current_antenna_loc = VALUE #( line   = sy-tabix
                                         offset = v_syindex
                                         length = 1 ).

        DATA(t_locations_of_similar_antenna) = me->get_all_locations_of_actual( CONV char01( v_antenna ) ).

        LOOP AT t_locations_of_similar_antenna ASSIGNING FIELD-SYMBOL(<fs_locations_similar_antenna>).

*   I got the current one
          IF <fs_locations_similar_antenna>-line   EQ s_current_antenna_loc-line  AND
             <fs_locations_similar_antenna>-offset EQ s_current_antenna_loc-offset.
            CONTINUE.
          ENDIF.

*  Count the differencies between two points
          DATA(v_offset_diff) = get_diff_of_offset( i_match_row_01 = s_current_antenna_loc i_match_row_02 = <fs_locations_similar_antenna> ).
          DATA(v_line_diff)   = get_diff_of_line(   i_match_row_01 = s_current_antenna_loc i_match_row_02 = <fs_locations_similar_antenna> ).

          IF <fs_locations_similar_antenna>-line EQ s_current_antenna_loc-line.

*       Horizontal
            me->find_horizontal( i_match_row_01 = s_current_antenna_loc
                                 i_match_row_02 = <fs_locations_similar_antenna>
                                 i_offset_diff  = v_offset_diff ).

          ELSEIF <fs_locations_similar_antenna>-offset EQ s_current_antenna_loc-offset.

*       Vertical
            me->find_vertical( i_match_row_01 = s_current_antenna_loc
                               i_match_row_02 = <fs_locations_similar_antenna>
                               i_lines_diff   = v_line_diff ).

          ELSE.

*       Diagonal
            me->find_diagonal( i_match_row_01 = s_current_antenna_loc
                               i_match_row_02 = <fs_locations_similar_antenna>
                               i_lines_diff   = v_line_diff
                               i_offset_diff  = v_offset_diff ).
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_vertical.

    DATA v_found TYPE boolean.

* Looking up
    DATA(v_antinode_line) = COND i( WHEN i_match_row_01-line GT i_match_row_02-line THEN i_match_row_02-line - i_lines_diff ELSE i_match_row_01-line - i_lines_diff ).

    DO.
      IF v_antinode_line GT 0 AND NOT line_exists( me->t_locations_of_antinode[ line   = v_antinode_line
                                                                                 offset = i_match_row_01-offset
                                                                                 length = 1 ] ).
        APPEND VALUE #( line   = v_antinode_line
                        offset = i_match_row_01-offset
                        length = 1 ) TO me->t_locations_of_antinode.
        v_found = abap_true.
      ELSEIF v_antinode_line LE 0.
        EXIT.
      ENDIF.
      v_antinode_line -= i_lines_diff.
    ENDDO.
* Looking down
    v_antinode_line = COND #( WHEN i_match_row_01-line GT i_match_row_02-line THEN i_match_row_01-line + i_lines_diff ELSE i_match_row_02-line + i_lines_diff ).

    DO.
      IF v_antinode_line LE LINES( me->data_tab ) AND NOT line_exists( me->t_locations_of_antinode[ line   = v_antinode_line
                                                                                offset = i_match_row_01-offset
                                                                                length = 1 ] ).
        APPEND VALUE #( line   = v_antinode_line
                        offset = i_match_row_01-offset
                        length = 1 ) TO me->t_locations_of_antinode.
        v_found = abap_true.
      ELSEIF v_antinode_line GT LINES( me->data_tab ) .
        EXIT.
      ENDIF.
      v_antinode_line += i_lines_diff.
    ENDDO.

    IF NOT line_exists( me->t_locations_of_antinode[ line   = i_match_row_02-line
                                                     offset = i_match_row_02-offset
                                                     length = 1 ] ).
      APPEND VALUE #( line   = i_match_row_02-line
                      offset = i_match_row_02-offset
                      length = 1 ) TO me->t_locations_of_antinode.
    ENDIF.

  ENDMETHOD.

  METHOD find_diagonal.

    DATA: v_antinode_line   TYPE i,
          v_antinode_offset TYPE i,
          v_found TYPE boolean.

* Down left and up right
    IF i_match_row_01-offset GT i_match_row_02-offset AND
       i_match_row_01-line   LT i_match_row_02-line   OR
       i_match_row_02-offset GT i_match_row_01-offset AND
       i_match_row_02-line   LT i_match_row_01-line.

* Looking down left
*******
*****X*
*******
***X***
*******
*#*****
      TRY.
        v_antinode_line   = COND #( WHEN i_match_row_01-line   GT i_match_row_02-line   THEN i_match_row_01-line + i_lines_diff    ELSE i_match_row_02-line + i_lines_diff ).
        v_antinode_offset = COND #( WHEN i_match_row_01-offset LT i_match_row_02-offset THEN i_match_row_01-offset - i_offset_diff ELSE i_match_row_02-offset - i_offset_diff ).

        DO.
          DATA(v_antinode_row) = VALUE #( data_tab[ v_antinode_line ] ).

          IF v_antinode_offset GE 0 AND NOT line_exists( me->t_locations_of_antinode[ line   = v_antinode_line
                                                                                      offset = v_antinode_offset
                                                                                      length = 1 ] ).
            APPEND VALUE #( line   = v_antinode_line
                            offset = v_antinode_offset
                            length = 1 ) TO me->t_locations_of_antinode.
            v_found = abap_true.
          ELSEIF v_antinode_offset LT 0.
            EXIT.
          ENDIF.

          v_antinode_line   += i_lines_diff.
          v_antinode_offset -= i_offset_diff.

        ENDDO.
      CATCH cx_root.
      ENDTRY.

* Looking up right
*******
*****#*
*******
***X***
*******
*X*****
      TRY.
        v_antinode_line   = COND #( WHEN i_match_row_01-line   LT i_match_row_02-line   THEN i_match_row_01-line - i_lines_diff    ELSE i_match_row_02-line - i_lines_diff ).
        v_antinode_offset = COND #( WHEN i_match_row_01-offset GT i_match_row_02-offset THEN i_match_row_01-offset + i_offset_diff ELSE i_match_row_02-offset + i_offset_diff ).

        DO.
          v_antinode_row = VALUE #( data_tab[ v_antinode_line ] ).

          IF v_antinode_offset LT strlen( v_antinode_row ) AND  NOT line_exists( me->t_locations_of_antinode[ line   = v_antinode_line
                                                                                                              offset = v_antinode_offset
                                                                                                              length = 1 ] ).
            APPEND VALUE #( line   = v_antinode_line
                            offset = v_antinode_offset
                            length = 1 ) TO me->t_locations_of_antinode.
            v_found = abap_true.
          ELSEIF v_antinode_offset GE strlen( v_antinode_row ).
            EXIT.
          ENDIF.

          v_antinode_line -= i_lines_diff.
          v_antinode_offset += i_offset_diff.

        ENDDO.
      CATCH cx_root.
      ENDTRY.

* Down right and up left
    ELSE.

* Looking down right
*******
*X*****
*******
***X***
*******
*****#*
      TRY.
        v_antinode_offset = COND #( WHEN i_match_row_01-offset GT i_match_row_02-offset THEN i_match_row_01-offset + i_offset_diff ELSE i_match_row_02-offset + i_offset_diff ).
        v_antinode_line   = COND #( WHEN i_match_row_01-line   GT i_match_row_02-line   THEN i_match_row_01-line + i_lines_diff    ELSE i_match_row_02-line + i_lines_diff ).

        DO.
          v_antinode_row = VALUE #( data_tab[ v_antinode_line ] ).
          IF v_antinode_offset LT strlen( v_antinode_row ) AND NOT line_exists( me->t_locations_of_antinode[ line   = v_antinode_line
                                                                                                             offset = v_antinode_offset
                                                                                                             length = 1 ] ).
            APPEND VALUE #( line   = v_antinode_line
                            offset = v_antinode_offset
                            length = 1 ) TO me->t_locations_of_antinode.
            v_found = abap_true.
          ELSEIF v_antinode_offset GE strlen( v_antinode_row ).
            EXIT.
          ENDIF.
          v_antinode_line += i_lines_diff.
          v_antinode_offset += i_offset_diff.
        ENDDO.
      CATCH cx_root.
      ENDTRY.


* Looking up left
*#*****
*******
***X***
*******
*****X*
*******
      TRY.
        v_antinode_line   = COND #( WHEN i_match_row_01-line   LT i_match_row_02-line   THEN i_match_row_01-line - i_lines_diff    ELSE i_match_row_02-line - i_lines_diff ).
        v_antinode_offset = COND #( WHEN i_match_row_01-offset LT i_match_row_02-offset THEN i_match_row_01-offset - i_offset_diff ELSE i_match_row_02-offset - i_offset_diff ).

        DO.
          v_antinode_row = VALUE #( data_tab[ v_antinode_line ] ).

          IF v_antinode_offset GE 0 AND NOT line_exists( me->t_locations_of_antinode[ line   = v_antinode_line
                                                                                      offset = v_antinode_offset
                                                                                      length = 1 ] ).
            APPEND VALUE #( line   = v_antinode_line
                            offset = v_antinode_offset
                            length = 1 ) TO me->t_locations_of_antinode.
            v_found = abap_true.
          ELSEIF v_antinode_offset LT 0.
            EXIT.
          ENDIF.
          v_antinode_line -= i_lines_diff.
          v_antinode_offset -= i_offset_diff.
        ENDDO.
      CATCH cx_root.
      ENDTRY.
    ENDIF.

    IF NOT line_exists( me->t_locations_of_antinode[ line   = i_match_row_02-line
                                                     offset = i_match_row_02-offset
                                                     length = 1 ] ).
      APPEND VALUE #( line   = i_match_row_02-line
                      offset = i_match_row_02-offset
                      length = 1 ) TO me->t_locations_of_antinode.
    ENDIF.

  ENDMETHOD.

  METHOD find_horizontal.

    DATA: v_antinode_line   TYPE string,
          v_antinode_offset TYPE i,
          v_found TYPE boolean.

* Looking forward

    v_antinode_offset = COND i( WHEN i_match_row_01-offset GT i_match_row_02-offset THEN i_match_row_01-offset + i_offset_diff ELSE i_match_row_02-offset + i_offset_diff ).

    v_antinode_line = VALUE #( data_tab[ i_match_row_01-line ] OPTIONAL ).

*  .0...0...#
* If the length o f line not bigger than the antinode offset, its found one

    DO.
      IF v_antinode_offset LT strlen( v_antinode_line ) - 1 AND NOT line_exists( me->t_locations_of_antinode[ line   = i_match_row_01-line
                                                                                                          offset = v_antinode_offset
                                                                                                          length = 1 ] ).
        APPEND VALUE #( line   = i_match_row_01-line
                        offset = v_antinode_offset
                        length = 1 ) TO me->t_locations_of_antinode.
        v_found = abap_true.
      ELSEIF v_antinode_offset GT strlen( v_antinode_line ).
        EXIT.
      ENDIF.
      v_antinode_offset += i_offset_diff.
    ENDDO.

* Looking backward
    v_antinode_offset = COND #( WHEN i_match_row_01-offset GT i_match_row_02-offset THEN i_match_row_02-offset - i_offset_diff  ELSE i_match_row_01-offset - i_offset_diff ).

    DO.
*   If the location of antinode not out of map, its found
      IF v_antinode_offset GE 0 AND NOT line_exists( me->t_locations_of_antinode[ line   = i_match_row_01-line
                                                                                  offset = v_antinode_offset
                                                                                  length = 1 ] ).
          APPEND VALUE #( line   = i_match_row_01-line
                          offset = v_antinode_offset
                          length = 1 ) TO me->t_locations_of_antinode.
          v_found = abap_true.
      ELSEIF v_antinode_offset LT 0.
        EXIT.
      ENDIF.
      v_antinode_offset -= i_offset_diff.
    ENDDO.

    IF NOT line_exists( me->t_locations_of_antinode[ line   = i_match_row_02-line
                                                     offset = i_match_row_02-offset
                                                     length = 1 ] ).
      APPEND VALUE #( line   = i_match_row_02-line
                      offset = i_match_row_02-offset
                      length = 1 ) TO me->t_locations_of_antinode.
    ENDIF.

  ENDMETHOD.

  METHOD get_diff_of_line.

    IF i_match_row_01-line GT i_match_row_02-line.
      r_line = i_match_row_01-line - i_match_row_02-line.
    ELSE.
      r_line = i_match_row_02-line - i_match_row_01-line.
    ENDIF.

  ENDMETHOD.

  METHOD get_diff_of_offset.
    IF i_match_row_01-offset GT i_match_row_02-offset.
      r_offset = ( i_match_row_01-offset + i_match_row_01-length ) - ( i_match_row_02-offset + i_match_row_02-length ).
    ELSE.
      r_offset = ( i_match_row_02-offset + i_match_row_02-length ) - ( i_match_row_01-offset + i_match_row_01-length ).
    ENDIF.
  ENDMETHOD.

  METHOD get_all_locations_of_actual.
    FIND ALL OCCURRENCES OF i_char IN TABLE me->data_tab RESULTS r_tab.
  ENDMETHOD.

  METHOD set_locations_of_antennas.
    FIND ALL OCCURRENCES OF REGEX '[^.]' IN TABLE me->data_tab RESULTS me->t_locations_of_antennas.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: object TYPE REF TO lcl,
        data_tab TYPE stringtab.

  object = NEW #( ).
  object->data_tab = data_tab.
  object->set_locations_of_antennas( ).
  object->count_antinodes( ).
  DATA(t_loc) = object->get_all_locations( ).

  BREAK-POINT.
