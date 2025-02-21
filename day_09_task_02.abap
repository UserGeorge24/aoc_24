CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_index,
            index TYPE i,
           END OF ty_index,
           tty_index TYPE STANDARD TABLE OF ty_index WITH DEFAULT KEY.

    DATA: data_tab TYPE stringtab,
          t_disk_map TYPE TABLE OF string.

    METHODS: convert_to_itab_format,
             move_file_blocks,
             get_filesystem_checksum RETURNING VALUE(r_val) TYPE int8.

  PROTECTED SECTION.

    DATA: v_id TYPE i VALUE 0,
          t_checked_num TYPE TABLE OF sytabix.

    METHODS: get_next_id RETURNING VALUE(r_val) TYPE i,
             get_index_of_most_down_nums IMPORTING i_from TYPE i
                                         CHANGING ct_indexes TYPE tty_index,
             get_index_of_most_up_dots IMPORTING i_needed_spaces TYPE i
                                                 i_index_limit   TYPE i
                                       CHANGING ct_indexes TYPE tty_index.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD get_filesystem_checksum.

    DATA v_sytabix TYPE sytabix.

    LOOP AT t_disk_map ASSIGNING FIELD-SYMBOL(<fs_disk_map>) .

      CHECK <fs_disk_map> NE '.'.

      v_sytabix = sy-tabix - 1.

      r_val += v_sytabix * <fs_disk_map>.

    ENDLOOP.
  ENDMETHOD.

  METHOD move_file_blocks.

    DATA: t_most_down_nums_indexes TYPE tty_index,
          t_most_up_dots_indexes TYPE tty_index,
          v_from TYPE i.

    v_from = LINES( t_disk_map ).

    TRY.
      DO.

        REFRESH: t_most_down_nums_indexes, t_most_up_dots_indexes.

        get_index_of_most_down_nums( EXPORTING i_from    = v_from
                                     CHANGING ct_indexes = t_most_down_nums_indexes ).

        v_from = VALUE #( t_most_down_nums_indexes[ LINES( t_most_down_nums_indexes ) ]-index OPTIONAL ).

        get_index_of_most_up_dots( EXPORTING i_needed_spaces = lines( t_most_down_nums_indexes )
                                             i_index_limit   = VALUE #( t_most_down_nums_indexes[ 1 ]-index OPTIONAL )
                                   CHANGING  ct_indexes      = t_most_up_dots_indexes ).

        IF VALUE #( t_most_down_nums_indexes[ 1 ] OPTIONAL ) LE
           VALUE #( t_most_up_dots_indexes[ 1 ] OPTIONAL ).
          EXIT.
        ENDIF.

        IF LINES( t_most_up_dots_indexes ) GE LINES( t_most_down_nums_indexes ).
          LOOP AT t_most_up_dots_indexes ASSIGNING FIELD-SYMBOL(<fs_index>).
            APPEND <fs_index>-index TO t_checked_num..
          ENDLOOP.
        ELSE.
          LOOP AT t_most_down_nums_indexes ASSIGNING <fs_index>.
            APPEND <fs_index>-index TO t_checked_num..
          ENDLOOP.
         CONTINUE.
        ENDIF.

        DATA(v_id) = VALUE #( t_disk_map[ VALUE #( t_most_down_nums_indexes[ 1 ]-index OPTIONAL ) ] OPTIONAL ).

        LOOP AT t_most_up_dots_indexes ASSIGNING FIELD-SYMBOL(<fs_dots_indexes>).
          ASSIGN t_disk_map[ <fs_dots_indexes>-index ] TO FIELD-SYMBOL(<fs_curr_val>).
          CHECK sy-subrc EQ 0.
          <fs_curr_val> = v_id.
        ENDLOOP.

        LOOP AT t_most_down_nums_indexes ASSIGNING FIELD-SYMBOL(<fs_nums_indexes>).
          ASSIGN t_disk_map[ <fs_nums_indexes>-index ] TO <fs_curr_val>.
          CHECK sy-subrc EQ 0.
          <fs_curr_val> = '.'.
        ENDLOOP.

      ENDDO.
    CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD get_index_of_most_up_dots.

    DATA: v_counter TYPE i,
          v_sytabix TYPE sy-tabix.

    LOOP AT t_disk_map ASSIGNING FIELD-SYMBOL(<fs_disk_map>) WHERE table_line EQ '.'.

      CHECK sy-tabix LT i_index_limit.

      IF v_sytabix IS INITIAL.
        v_sytabix = sy-tabix.
        v_counter = 1.
        APPEND VALUE #( index = sy-tabix ) TO ct_indexes.
        CONTINUE.
      ENDIF.

      IF i_needed_spaces EQ v_counter.
        EXIT.
      ELSEIF sy-tabix - v_sytabix GT 1.
        v_sytabix = sy-tabix.
        v_counter = 1.
        REFRESH ct_indexes.
        APPEND VALUE #( index = sy-tabix ) TO ct_indexes.
      ELSE.
        v_sytabix = sy-tabix.
        v_counter += 1.
        APPEND VALUE #( index = sy-tabix ) TO ct_indexes.
      ENDIF.

      AT LAST.
        REFRESH ct_indexes.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_index_of_most_down_nums.

    DATA v_val TYPE string.

    LOOP AT t_disk_map ASSIGNING FIELD-SYMBOL(<fs_disk_map>) STEP - 1 FROM i_from WHERE table_line NE '.'.

      CHECK NOT line_exists( t_checked_num[ table_line = sy-tabix ] ).

      IF v_val IS INITIAL .
        v_val = <fs_disk_map>.
        APPEND VALUE #( index = sy-tabix ) TO ct_indexes.
        CONTINUE.
      ENDIF.

      IF <fs_disk_map> NE v_val.
        EXIT.
      ELSE.
        APPEND VALUE #( index = sy-tabix ) TO ct_indexes.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD convert_to_itab_format.

    DATA: v_disk_map_flat TYPE string,
          v_repeated_val  TYPE string,
          v_inner_syindex TYPE i.

* Flat the input
    v_disk_map_flat = VALUE #( data_tab[ 1 ] OPTIONAL ).

    DO strlen( v_disk_map_flat ) TIMES.

      DATA(v_index) = sy-index - 1.

* Repeat dots where MOD 2 = 0
* Repeat every ID where MOD 2 != 0 char

      IF sy-index MOD 2 = 0.
        v_repeated_val = condense( repeat( val = '.' occ = v_disk_map_flat+v_index(1) ) ).

        DO strlen( v_repeated_val ) TIMES.
          APPEND v_repeated_val+v_inner_syindex(1) TO t_disk_map.
          v_inner_syindex += 1.
        ENDDO.

      ELSE.
        DATA(v_id)           =  get_next_id( ).
        DATA(v_length_of_id) = strlen( condense( CONV string( v_id ) ) ).
        v_repeated_val       = condense( repeat( val = condense( CONV string( v_id ) ) occ = v_disk_map_flat+v_index(1) ) ).

        DO strlen( v_repeated_val ) / v_length_of_id TIMES.
          APPEND v_repeated_val+v_inner_syindex(v_length_of_id) TO t_disk_map.
          v_inner_syindex += v_length_of_id.
        ENDDO.

      ENDIF.

      v_inner_syindex = 0.

    ENDDO.
  ENDMETHOD.

  METHOD get_next_id.
    r_val = v_id.
    v_id += 1.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: object TYPE REF TO lcl,
        data_tab TYPE stringtab.

  object = NEW #( ).
  object->data_tab = data_tab.
  object->convert_to_itab_format( ).
  object->move_file_blocks( ).
  DATA(sum) = object->get_filesystem_checksum( ).
  BREAK-POINT.
