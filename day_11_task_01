CLASS lcl DEFINITION.

  PUBLIC SECTION.
    DATA: data_tab TYPE stringtab.

    METHODS: blink RETURNING VALUE(r_sum) TYPE i,
             put_to_table.

  PROTECTED SECTION.
    DATA t_before_blink TYPE stringtab.

ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD put_to_table.
    DATA(v_string) = VALUE #( me->data_tab[ 1 ] OPTIONAL ).
    SPLIT v_string AT space INTO TABLE t_before_blink.
  ENDMETHOD.

  METHOD blink .

    DATA: t_after_blink  TYPE stringtab.
    DATA v_one TYPE string VALUE '1'.

    DO 25 TIMES.

      LOOP AT t_before_blink ASSIGNING FIELD-SYMBOL(<fs_before_blink>).

        IF <fs_before_blink> EQ 0.
          APPEND 1 TO t_after_blink.
        ELSEIF strlen( condense( <fs_before_blink> ) ) MOD 2 = 0.
          DATA(v_offset) = strlen( condense( <fs_before_blink> ) ) / 2.
          APPEND: <fs_before_blink>+0(v_offset)                   TO t_after_blink,
                  |{ <fs_before_blink>+v_offset(*) ALPHA = OUT }| TO t_after_blink.
        ELSE.
          APPEND CONV int8( <fs_before_blink> * 2024 ) TO t_after_blink.
        ENDIF.

      ENDLOOP.

      t_before_blink = t_after_blink.
      REFRESH t_after_blink.
    ENDDO.

    r_sum = lines( t_before_blink ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: object   TYPE REF TO lcl,
        data_tab TYPE stringtab.

  object = NEW #( ).
  object->data_tab = data_tab.
  object->put_to_table( ).
  DATA(v_sum) = object->blink( ).
