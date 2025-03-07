CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_page_ord_rules,
            first  TYPE n LENGTH 2,
            second TYPE n LENGTH 2,
           END OF ty_page_ord_rules.

    DATA: t_data_tab       TYPE stringtab,
          t_page_ord_rules TYPE TABLE OF ty_page_ord_rules,
          t_updates        TYPE stringtab,
          t_proper_updates TYPE stringtab.

    METHODS: split_two_section,
             collect_proper_updates,
             get_sum RETURNING VALUE(r_val) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD get_sum.
    LOOP AT t_proper_updates ASSIGNING FIELD-SYMBOL(<fs_proper_update>).
      SPLIT <fs_proper_update> AT ',' INTO TABLE DATA(tab).
      r_val += VALUE #( tab[ lines( tab ) / 2 ] OPTIONAL ).
    ENDLOOP.
  ENDMETHOD.

  METHOD collect_proper_updates.

    DATA: t_update TYPE stringtab,
          t_find   TYPE TABLE OF ty_page_ord_rules,
          v_exit   TYPE boolean.

    LOOP AT t_updates ASSIGNING FIELD-SYMBOL(<fs_updates>).

      SPLIT <fs_updates> AT ',' INTO TABLE t_update.

      LOOP AT t_update ASSIGNING FIELD-SYMBOL(<fs_update>).

        DATA(v_sytabix) = sy-tabix.

        t_find = VALUE #( FOR <fs_row> IN t_page_ord_rules WHERE ( first  = <fs_update>
                                                                OR second = <fs_update> )
                                                                 ( CORRESPONDING #( <fs_row> ) ) ).

        DATA(temp) = VALUE stringtab( FOR <fs_row2> IN t_update FROM v_sytabix ( <fs_row2> ) ).

        LOOP AT t_find ASSIGNING FIELD-SYMBOL(<fs_t_find>) WHERE second EQ <fs_update>.
          FIND <fs_t_find>-first IN TABLE temp.
          CHECK sy-subrc EQ 0.
          v_exit = abap_true.
          EXIT.
        ENDLOOP.

        IF v_exit EQ abap_true.
          EXIT.
        ENDIF.

        DATA(v_count_elements) = REDUCE #( INIT v_count TYPE i
                                           FOR <fs_element> IN t_update FROM sy-tabix + 1
                                           FOR <fs_find>    IN t_find WHERE ( first  = <fs_update>
                                                                          AND second = <fs_element> )
                                           NEXT v_count += 1 ).

        IF v_count_elements NE lines( t_update ) - v_sytabix.
          EXIT.
        ENDIF.

        AT LAST.
          APPEND <fs_updates> TO t_proper_updates.
        ENDAT.
      ENDLOOP.

      CLEAR: v_exit.
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
ENDCLASS.

START-OF-SELECTION.

  DATA: o_lcl TYPE REF TO lcl,
        t_data_tab TYPE stringtab.

  o_lcl = NEW #( ).
  o_lcl->t_data_tab = t_data_tab.
  o_lcl->split_two_section( ).
  o_lcl->collect_proper_updates( ).
  DATA(sum) = o_lcl->get_sum( ).
