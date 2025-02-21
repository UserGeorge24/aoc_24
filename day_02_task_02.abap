CLASS lcl DEFINITION.

 PUBLIC SECTION.

  CLASS-DATA t_levels_ori TYPE TABLE OF string.
  CLASS-DATA t_levels TYPE TABLE OF string.

  CLASS-METHODS: is_safe RETURNING VALUE(r_val) TYPE boolean,
                 remove_element IMPORTING i_index TYPE i,
                 is_diff_morethan_3 IMPORTING num_01 TYPE i
                                              num_02 TYPE i
                                    RETURNING VALUE(r_val) TYPE boolean,
                 is_against_sort_rule IMPORTING num_01 TYPE i
                                                num_02 TYPE i
                                                v_inc  TYPE boolean
                                      RETURNING VALUE(r_val) TYPE boolean.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD remove_element.
    t_levels = t_levels_ori.
    DELETE t_levels INDEX i_index.
  ENDMETHOD.

  METHOD is_safe.

    DATA: v_num_01 TYPE i,
          v_num_02 TYPE i,
          v_inc    TYPE boolean.

    IF VALUE #( t_levels[ 1 ] OPTIONAL ) Eq VALUE #( t_levels[ 2 ] OPTIONAL ).
      r_val = abap_false.
      RETURN.
    ENDIF.

    v_inc = COND #( WHEN VALUE #( t_levels[ 1 ] OPTIONAL ) LT VALUE #( t_levels[ 2 ] OPTIONAL )
                      THEN abap_true
                      ELSE abap_false ).

    LOOP AT t_levels ASSIGNING FIELD-SYMBOL(<fs_t_levels>).

      AT LAST.
        r_val = abap_true.
        RETURN.
      ENDAT.

      v_num_01 = <fs_t_levels>.
      v_num_02 = VALUE #( t_levels[ sy-tabix + 1 ] OPTIONAL )..

      IF is_diff_morethan_3( EXPORTING num_01 = v_num_01
                                       num_02 = v_num_02 ).
        r_val = abap_false.
        RETURN.
      ENDIF.

      IF is_against_sort_rule( num_01 = v_num_01
                               num_02 = v_num_02
                               v_inc  = v_inc ).
        r_val = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

    r_val = abap_true.

  ENDMETHOD.

  METHOD is_diff_morethan_3.
    r_val = COND #( WHEN abs( num_01 - num_02 ) BETWEEN 1 AND 3 THEN abap_false ELSE abap_true ).
  ENDMETHOD.

  METHOD is_against_sort_rule.
    r_val = COND #( WHEN v_inc EQ abap_true  AND num_01 GE num_02 THEN abap_true
                    WHEN v_inc EQ abap_false AND num_01 LE num_02 THEN abap_true  ).
  ENDMETHOD.

ENDCLASS.

 START-OF-SELECTION.

  LOOP AT t_data_tab ASSIGNING FIELD-SYMBOL(<fs_src>).

    SPLIT <fs_src> AT '' INTO TABLE lcl=>t_levels_ori.

    lcl=>t_levels = lcl=>t_levels_ori.

    DO LINES( lcl=>t_levels_ori ) + 1 TIMES.
      IF lcl=>is_safe(  ).
        v_sum += 1.
        EXIT.
      ELSE.
        lcl=>remove_element( sy-index ).
      ENDIF.
    ENDDO.
  ENDLOOP.
