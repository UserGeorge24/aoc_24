CLASS lcl DEFINITION.
  PUBLIC SECTION.

    CONSTANTS: up    TYPE c VALUE '^',
               down  TYPE c VALUE 'v',
               right TYPE c VALUE '>',
               left  TYPE c VALUE '<'.

    DATA: t_data_tab    TYPE stringtab.

    METHODS: set_coordinates,
             count_route,
             get_sum RETURNING VALUE(r_val) TYPE i.
  PROTECTED SECTION.

    DATA: t_coordinates TYPE match_result_tab,
          v_rest_x_sum   TYPE i.

    METHODS: left_direction  IMPORTING i_coord TYPE match_result,
             right_direction IMPORTING i_coord TYPE match_result,
             up_direction    IMPORTING i_coord TYPE match_result,
             down_direction  IMPORTING i_coord TYPE match_result,

             up_get_next_hshtg_coords IMPORTING i_coord TYPE match_result
                                      RETURNING VALUE(r_hashtag_coords) TYPE match_result,
             down_get_next_hshtg_coords IMPORTING i_coord TYPE match_result
                                        RETURNING VALUE(r_hashtag_coords) TYPE match_result,
             left_get_next_hshtg_coords IMPORTING i_coord TYPE match_result
                                        RETURNING VALUE(r_hashtag_coords) TYPE match_result,
             right_get_next_hshtg_coords IMPORTING i_coord TYPE match_result
                                          RETURNING VALUE(r_hashtag_coords) TYPE match_result,
             are_there_more_to_be_process IMPORTING i_direction TYPE c
                                                    i_coord TYPE match_result
                                          RETURNING VALUE(r_val) TYPE boolean,
             set_rest_of_the_x IMPORTING i_direction TYPE c
                                         i_coord TYPE match_result.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD set_rest_of_the_x.

    CASE i_direction.
      WHEN up.
        LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_data>) STEP - 1 FROM i_coord-line.
          CHECK <fs_data>+i_coord-offset(1) EQ '.'.
          v_rest_x_sum += 1.
        ENDLOOP.
      WHEN down.
        LOOP AT t_data_tab ASSIGNING <fs_data> FROM i_coord-line.
          CHECK <fs_data>+i_coord-offset(1) EQ '.'.
          v_rest_x_sum += 1.
        ENDLOOP.
      WHEN right.

        DATA(s_line) = VALUE #( t_data_tab[ i_coord-line ] OPTIONAL ).

        FIND ALL OCCURRENCES OF '.' IN s_line+i_coord-offset(*) MATCH COUNT v_rest_x_sum.

      WHEN left.
        s_line = VALUE #( t_data_tab[ i_coord-line ] OPTIONAL ).

        FIND ALL OCCURRENCES OF '.' IN s_line+0(i_coord-offset) MATCH COUNT v_rest_x_sum.
    ENDCASE.

    v_rest_x_sum += 1.

  ENDMETHOD.

  METHOD are_there_more_to_be_process.

    DATA s_coords TYPE match_result.

    CASE i_direction.
      WHEN up.
        s_coords = me->up_get_next_hshtg_coords( i_coord ).
      WHEN down.
        s_coords = me->down_get_next_hshtg_coords( i_coord ).
      WHEN right.
        s_coords = me->right_get_next_hshtg_coords( i_coord ).
      WHEN left.
        s_coords = me->left_get_next_hshtg_coords( i_coord ).
    ENDCASE.

    r_val = xsdbool( s_coords IS NOT INITIAL ).

  ENDMETHOD.

  METHOD get_sum.
    FIND ALL OCCURRENCES OF abap_true IN TABLE t_data_tab MATCH COUNT r_val.
    r_val += v_rest_x_sum.
  ENDMETHOD.

  METHOD right_get_next_hshtg_coords.
    LOOP AT t_coordinates ASSIGNING FIELD-SYMBOL(<fs_coords>) WHERE line   EQ i_coord-line
                                                                AND offset GT i_coord-offset.
      r_hashtag_coords = <fs_coords>.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD left_get_next_hshtg_coords.
    LOOP AT t_coordinates ASSIGNING FIELD-SYMBOL(<fs_coords>) STEP -1 WHERE line   EQ i_coord-line
                                                                        AND offset LT i_coord-offset.
      r_hashtag_coords = <fs_coords>.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD down_get_next_hshtg_coords.
    LOOP AT t_coordinates ASSIGNING FIELD-SYMBOL(<fs_coords>) WHERE offset EQ i_coord-offset
                                                                AND line   GT i_coord-line.
      r_hashtag_coords = <fs_coords>.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD up_get_next_hshtg_coords.
    LOOP AT t_coordinates ASSIGNING FIELD-SYMBOL(<fs_coords>) STEP - 1 WHERE offset EQ i_coord-offset
                                                                         AND line   LT i_coord-line.
      r_hashtag_coords = <fs_coords>.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD right_direction.

    DATA(s_hstg_coords) = me->right_get_next_hshtg_coords( i_coord ).

    ASSIGN t_data_tab[ i_coord-line ] TO FIELD-SYMBOL(<fs_line>).

    DATA(v_length) = ( s_hstg_coords-offset - i_coord-offset ) - 1.

    REPLACE SECTION OFFSET: i_coord-offset           LENGTH v_length OF <fs_line> WITH repeat( occ = v_length val = abap_true ),
                            s_hstg_coords-offset - 1 LENGTH 1        OF <fs_line> WITH down.
  ENDMETHOD.

  METHOD left_direction.

    DATA(s_hstg_coords) = me->left_get_next_hshtg_coords( i_coord ).

    ASSIGN t_data_tab[ i_coord-line ] TO FIELD-SYMBOL(<fs_line>).

    DATA(v_length) = i_coord-offset - ( s_hstg_coords-offset + 1 ).

    REPLACE SECTION OFFSET: s_hstg_coords-offset + 2 LENGTH v_length OF <fs_line> WITH repeat( occ = v_length val = abap_true ),
                            s_hstg_coords-offset + 1 LENGTH 1        OF <fs_line> WITH up.
  ENDMETHOD.

  METHOD up_direction.

    DATA(s_hstg_coords) = me->up_get_next_hshtg_coords( i_coord ).

    ASSIGN t_data_tab[ s_hstg_coords-line + 1 ] TO FIELD-SYMBOL(<fs_row>).

    REPLACE SECTION OFFSET i_coord-offset LENGTH 1 OF <fs_row> WITH right.

    LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_data>) STEP -1 FROM i_coord-line TO s_hstg_coords-line + 2.
      REPLACE SECTION OFFSET i_coord-offset LENGTH 1 OF <fs_data> WITH abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD down_direction.

    DATA(s_hstg_coords) = me->down_get_next_hshtg_coords( i_coord ).

    LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_data>) FROM i_coord-line TO s_hstg_coords-line - 1.
      REPLACE SECTION OFFSET i_coord-offset LENGTH 1 OF <fs_data> WITH abap_true.
    ENDLOOP.

    REPLACE SECTION OFFSET i_coord-offset LENGTH 1 OF <fs_data> WITH left.

  ENDMETHOD.

  METHOD count_route.

    DATA v_direction TYPE c.

    v_direction = '^'.

    DO.
      FIND FIRST OCCURRENCE OF v_direction IN TABLE t_data_tab RESULTS DATA(s_position).

      IF are_there_more_to_be_process( i_direction = v_direction
                                       i_coord     = s_position ) EQ abap_false.
        me->set_rest_of_the_x( EXPORTING i_coord     = s_position
                                         i_direction = v_direction ).
        EXIT.
      ENDIF.

      CASE v_direction.
        WHEN up.
          me->up_direction( s_position ).
          v_direction = right.
        WHEN down.
          me->down_direction( s_position ).
          v_direction = left.
        WHEN right.
          me->right_direction( s_position ).
          v_direction = down.
        WHEN left.
          me->left_direction( s_position ).
          v_direction = up.
      ENDCASE.
    ENDDO.
  ENDMETHOD.

  METHOD set_coordinates.
    FIND ALL OCCURRENCES OF '#' IN TABLE t_data_tab RESULTS t_coordinates.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: o_lcl TYPE REF TO lcl,
        t_data_tab TYPE stringtab.

  o_lcl = NEW #( ).
  o_lcl->t_data_tab = t_data_tab.
  o_lcl->set_coordinates( ).
  o_lcl->count_route( ).
  DATA(sum) = o_lcl->get_sum( ).

  BREAK-POINT.
