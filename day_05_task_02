
CLASS lcl DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_page_ord_rules,
            first  TYPE n LENGTH 2,
            second TYPE n LENGTH 2,
           END OF ty_page_ord_rules,
           tty_page_ord_rules TYPE TABLE OF ty_page_ord_rules.

    DATA: t_data_tab       TYPE stringtab,
          t_page_ord_rules TYPE TABLE OF ty_page_ord_rules,
          t_updates        TYPE stringtab,
          t_proper_updates TYPE stringtab.

    METHODS: split_two_section,
             collect_proper_updates IMPORTING i_fix TYPE boolean,
             delete_proper_updates,
             get_sum RETURNING VALUE(r_val) TYPE i.

  PROTECTED SECTION.
    METHODS: fix_current_rec IMPORTING i_fix TYPE boolean
                                       i_updates TYPE string
                             CHANGING ct_update TYPE stringtab,
             is_there_any_err IMPORTING it_update TYPE stringtab
                              RETURNING VALUE(r_val) TYPE boolean,
             is_there_err_in_pairs IMPORTING iv_update TYPE string
                                             it_update TYPE stringtab
                                             it_find  TYPE tty_page_ord_rules
                                             i_sytabix TYPE sytabix
                                   RETURNING VALUE(r_val) TYPE boolean,
             is_there_any_err_in_elements IMPORTING iv_update TYPE string
                                                    it_update TYPE stringtab
                                                    it_find  TYPE tty_page_ord_rules
                                                    i_sytabix TYPE sytabix
                                          RETURNING VALUE(r_val) TYPE boolean.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD fix_current_rec.

    DATA: t_find TYPE TABLE OF ty_page_ord_rules,
          v_exit   TYPE boolean,
          v_string TYPE string.

    LOOP AT ct_update ASSIGNING FIELD-SYMBOL(<fs_update>).

      DATA(v_sytabix) = sy-tabix.

      t_find = VALUE #( FOR <fs_row> IN t_page_ord_rules WHERE ( first  = <fs_update>
                                                              OR second = <fs_update> )
                                                               ( CORRESPONDING #( <fs_row> ) ) ).

      v_exit = me->is_there_err_in_pairs( EXPORTING iv_update = <fs_update>
                                                    it_update = ct_update
                                                    it_find   = t_find
                                                    i_sytabix = v_sytabix       ).

      IF v_exit EQ abap_true AND i_fix EQ abap_true.
        DATA(v_01)                = <fs_update>.
        <fs_update>               = VALUE #( ct_update[ v_sytabix + 1 ] OPTIONAL ).
        ct_update[ v_sytabix + 1 ] = v_01.
        CLEAR v_exit.
        CONTINUE.
      ELSEIF v_exit EQ abap_true.
        EXIT.
      ENDIF.

      v_exit = me->is_there_any_err_in_elements( EXPORTING iv_update = <fs_update>
                                                           it_update = ct_update
                                                           it_find   = t_find
                                                           i_sytabix = v_sytabix   ).
      IF v_exit EQ abap_true.
        EXIT.
      ENDIF.

      AT LAST.
        IF i_fix EQ abap_true.
          IF me->is_there_any_err( ct_update ) EQ abap_false.
           LOOP AT ct_update ASSIGNING FIELD-SYMBOL(<fs_row1>).
             v_string = COND #( WHEN v_string IS INITIAL THEN <fs_row1> ELSE |{ v_string },{ <fs_row1> }| ).
           ENDLOOP.
           APPEND v_string TO t_proper_updates.
         ENDIF.
        ELSE.
          APPEND i_updates TO t_proper_updates.
        ENDIF.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.

  METHOD is_there_any_err_in_elements.

    DATA(v_count_elements) = REDUCE #( INIT v_count TYPE i
                                       FOR <fs_element> IN it_update FROM sy-tabix + 1
                                       FOR <fs_find>    IN it_find WHERE ( first  = iv_update
                                                                       AND second = <fs_element> )
                                       NEXT v_count += 1 ).

    CHECK v_count_elements NE lines( it_update ) - i_sytabix.
      r_val = abap_true.
  ENDMETHOD.

  METHOD is_there_err_in_pairs.

    DATA(temp) = VALUE stringtab( FOR <fs_row2> IN it_update FROM i_sytabix ( <fs_row2> ) ).

    LOOP AT it_find ASSIGNING FIELD-SYMBOL(<fs_t_find>) WHERE second EQ iv_update.
      FIND <fs_t_find>-first IN TABLE temp.
      CHECK sy-subrc EQ 0.
      r_val = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_sum.
    LOOP AT t_proper_updates ASSIGNING FIELD-SYMBOL(<fs_proper_update>).
      SPLIT <fs_proper_update> AT ',' INTO TABLE DATA(tab).
      r_val += VALUE #( tab[ lines( tab ) / 2 ] OPTIONAL ).
    ENDLOOP.
  ENDMETHOD.

  METHOD delete_proper_updates.

    LOOP AT t_updates ASSIGNING FIELD-SYMBOL(<fs_update>).
      CHECK line_exists( t_proper_updates[ table_line = <fs_update> ] ).
      DELETE t_updates INDEX sy-tabix.
    ENDLOOP.

    REFRESH t_proper_updates.

  ENDMETHOD.

  METHOD is_there_any_err.

    DATA t_find TYPE TABLE OF ty_page_ord_rules.

    r_val = abap_false.

    LOOP AT it_update ASSIGNING FIELD-SYMBOL(<fs_update>).

      DATA(v_sytabix) = sy-tabix.

      t_find = VALUE #( FOR <fs_row> IN t_page_ord_rules WHERE ( first  = <fs_update>
                                                              OR second = <fs_update> )
                                                               ( CORRESPONDING #( <fs_row> ) ) ).

      r_val = me->is_there_err_in_pairs( EXPORTING iv_update = <fs_update>
                                                   it_update = it_update
                                                   it_find   = t_find
                                                   i_sytabix = v_sytabix ).
      IF r_val EQ abap_true.
        EXIT.
      ENDIF.

      r_val = me->is_there_any_err_in_elements( EXPORTING iv_update = <fs_update>
                                                          it_update = it_update
                                                          it_find   = t_find
                                                          i_sytabix = v_sytabix   ).
      IF r_val EQ abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD split_two_section.

    DATA v_flag TYPE boolean.

    LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_data>).

      IF <fs_data> EQ space.
        v_flag = abap_true.
      ENDIF.

      IF v_flag EQ abap_false.
        SPLIT <fs_data> AT '|' INTO TABLE DATA(temp).
        APPEND VALUE #( first  = VALUE #( temp[ 1 ] OPTIONAL )
                        second = VALUE #( temp[ 2 ] OPTIONAL )  ) TO t_page_ord_rules.
      ELSE.
        APPEND <fs_data> TO t_updates.
      ENDIF.

    ENDLOOP.

    DELETE t_updates INDEX 1.

  ENDMETHOD.

  METHOD collect_proper_updates.

    DATA: t_update TYPE stringtab,
          v_err TYPE boolean.

    v_err = abap_true.

    LOOP AT t_updates ASSIGNING FIELD-SYMBOL(<fs_updates>).

      SPLIT <fs_updates> AT ',' INTO TABLE t_update.

      IF i_fix EQ abap_false.
        me->fix_current_rec( EXPORTING i_fix     = i_fix
                                       i_updates = <fs_updates>
                             CHANGING  ct_update = t_update ).
      ELSE.

        WHILE v_err EQ abap_true.
          me->fix_current_rec( EXPORTING i_fix     = i_fix
                                         i_updates = <fs_updates>
                               CHANGING  ct_update = t_update ).

          IF me->is_there_any_err( t_update ) EQ abap_false.
            v_err = abap_false.
          ENDIF.
        ENDWHILE.
        v_err = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: o_lcl TYPE REF TO lcl,
        t_data_tab TYPE stringtab.

  o_lcl = NEW #( ).
  o_lcl->t_data_tab = t_data_tab.
  o_lcl->split_two_section( ).
  o_lcl->collect_proper_updates( abap_false ).
  o_lcl->delete_proper_updates( ).
  o_lcl->collect_proper_updates( abap_true ).
  DATA(sum) = o_lcl->get_sum( ).
