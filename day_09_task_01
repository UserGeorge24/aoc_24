
CLASS lcl DEFINITION.

  PUBLIC SECTION.
    DATA: data_tab TYPE stringtab,
          t_disk_map TYPE TABLE OF string.

    METHODS: convert_to_itab_format,
             move_file_blocks,
             get_filesystem_checksum RETURNING VALUE(r_val) TYPE int8.

  PROTECTED SECTION.

    DATA v_id TYPE i VALUE 0.

    METHODS: get_next_id RETURNING VALUE(r_val) TYPE i,
             get_most_down_num_index RETURNING VALUE(r_val) TYPE i,
             get_most_up_dot_index RETURNING VALUE(r_val) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD get_filesystem_checksum.
    DELETE t_disk_map WHERE table_line EQ '.'.
    r_val = REDUCE #( INIT v_sum TYPE int8
                      FOR <fs_row> IN t_disk_map
                      INDEX INTO v_index
                      NEXT v_sum += ( v_index - 1 ) * <fs_row> ).
  ENDMETHOD.

  METHOD move_file_blocks.

    TRY.
      DO.
        DATA(v_most_up_dot_index)   = get_most_up_dot_index( ).
        DATA(v_most_down_num_index) = get_most_down_num_index( ).

        IF v_most_down_num_index LT v_most_up_dot_index.
          EXIT.
        ENDIF.

        ASSIGN: t_disk_map[ v_most_down_num_index ] TO FIELD-SYMBOL(<fs_most_down_num_value>),
                t_disk_map[ v_most_up_dot_index ]   TO FIELD-SYMBOL(<fs_most_up_dot_value>).

        <fs_most_up_dot_value>   = <fs_most_down_num_value>.
        <fs_most_down_num_value> = '.'.

      ENDDO.
    CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD get_most_up_dot_index.
    r_val = line_index( t_disk_map[ table_line = '.' ] ).
  ENDMETHOD.

  METHOD get_most_down_num_index.
    LOOP AT t_disk_map TRANSPORTING NO FIELDS STEP - 1 WHERE table_line NE '.'.
      r_val = sy-tabix.
      EXIT.
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
