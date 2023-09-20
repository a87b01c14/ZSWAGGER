*&---------------------------------------------------------------------*
*& Report ZSWAG_LOG_MONITOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zswag_log_monitor.


TYPES: BEGIN OF ty_data,
         icon         TYPE icon_d,
         zmsg_id      TYPE ztswag_log-zmsg_id,
         zext_sys_no  TYPE ztswag_log-zext_sys_no,
         zext_sys_txt TYPE ztswag_ext_sys-zext_sys_txt,
         zif_no       TYPE ztswag_log-zif_no,
         zif_txt      TYPE ztswag_if-zif_txt,
         zext_key     TYPE ztswag_log-zext_key,
         zsend_time   TYPE ztswag_log-zsend_time,
         zmsgty       TYPE ztswag_log-zmsgty,
         zrequest     TYPE ztswag_log-zrequest,
         zresponse    TYPE ztswag_log-zresponse,
         ernam        TYPE ztswag_log-ernam,
         erdat        TYPE ztswag_log-erdat,
         erzet        TYPE ztswag_log-erzet,
         aenam        TYPE ztswag_log-aenam,
         aedat        TYPE ztswag_log-aedat,
         aezet        TYPE ztswag_log-aezet,
       END OF ty_data.

DATA: gt_data TYPE STANDARD TABLE OF ty_data,
      gs_data TYPE ty_data.
DATA: go_salv TYPE REF TO zcl_salv.
DATA: gv_text TYPE string.
DATA: go_container    TYPE REF TO cl_gui_custom_container.
DATA: go_textedit   TYPE REF TO   cl_gui_textedit.

*&---------------------------------------------------------------------*
*& 选择屏幕 / SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_msg_id FOR gs_data-zmsg_id.
  SELECT-OPTIONS: s_sys_no FOR gs_data-zext_sys_no.
  SELECT-OPTIONS: s_if_no FOR gs_data-zif_no.
  SELECT-OPTIONS: s_key FOR gs_data-zext_key.
  SELECT-OPTIONS: s_erdat FOR gs_data-erdat.
  SELECT-OPTIONS: s_msgty FOR gs_data-zmsgty.
SELECTION-SCREEN END OF BLOCK blk1.

START-OF-SELECTION.
  SELECT CASE zmsgty WHEN 'S' THEN @icon_green_light
                     WHEN 'E' THEN @icon_red_light
                     ELSE @icon_yellow_light END AS icon,
         a~*,b~zext_sys_txt,c~zif_txt
    INTO CORRESPONDING FIELDS OF TABLE @gt_data
    FROM ztswag_log AS a
    JOIN ztswag_ext_sys AS b ON b~zext_sys_no = a~zext_sys_no
    JOIN ztswag_if AS c ON c~zif_no = a~zif_no
    WHERE a~zmsg_id IN @s_msg_id AND
          a~zext_sys_no IN @s_sys_no AND
          a~zif_no IN @s_if_no AND
          a~zext_key IN @s_key AND
          a~erdat IN @s_erdat AND
          a~zmsgty IN @s_msgty.


END-OF-SELECTION .
  "SALV creation with only table passed
  go_salv = NEW zcl_salv(
    im_table    = REF #( gt_data )
*   im_pfstatus = 'ZSALV_STATUS'
    im_t_events = VALUE #( ( name = zcl_salv=>events-link_click form = 'FRM_LINK_CLICK' )
*                           ( name = zcl_salv=>events-added_function form = 'FRM_ADDED_FUNCTION')
                         ) ).


  go_salv->set_column_key( im_colname = 'ZMSG_ID' ).
  go_salv->set_column_hotspot( im_colname = 'ZREQUEST' ).
  go_salv->set_column_hotspot( im_colname = 'ZRESPONSE' ).

  "Display full screen grid
  go_salv->display( ).

FORM frm_link_click USING sender TYPE REF TO cl_salv_events_table
                             row    TYPE salv_de_row
                             column TYPE salv_de_column.

  FIELD-SYMBOLS:<fs_table> TYPE STANDARD TABLE,
                <fs_row>   TYPE any,
                <fs_col>   TYPE any,
                <fs_cell>  TYPE string.

  DATA: ref_table TYPE REF TO data.

  ref_table = go_salv->get_data( ).
  ASSIGN ref_table->* TO <fs_table>.
  CHECK sy-subrc = 0.
  READ TABLE <fs_table> ASSIGNING <fs_row> INDEX row.
  CHECK sy-subrc = 0.
  CASE column .
    WHEN 'ZREQUEST' OR  'ZRESPONSE'.
      ASSIGN COMPONENT column OF STRUCTURE <fs_row> TO <fs_cell>.
      CHECK sy-subrc = 0.
      gv_text = <fs_cell>.
      cl_demo_output=>display_json( gv_text ).
*      CALL SCREEN 100.

  ENDCASE.
ENDFORM.

FORM show_text.
  DATA: lv_text(512) TYPE c,
        lt_text      LIKE STANDARD TABLE OF lv_text.
  DATA: lv_len TYPE i VALUE 512.
  WHILE strlen( gv_text ) > lv_len.
    lv_text = gv_text(lv_len) && cl_abap_char_utilities=>newline.
    APPEND lv_text TO lt_text.
    gv_text = gv_text+lv_len(*).
  ENDWHILE.
  IF gv_text IS NOT INITIAL.
    APPEND gv_text TO lt_text.
  ENDIF.

  IF go_container IS INITIAL.
    CREATE OBJECT go_container
      EXPORTING
        container_name              = 'CONT100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CREATE OBJECT go_textedit
      EXPORTING
*       style                  = 0
        parent                 = go_container
*       lifetime               =
*       name                   =
*       wordwrap_mode          = cl_gui_textedit=>wordwrap_at_fixed_position
*       wordwrap_position      = lv_len
*       wordwrap_to_linebreak_mode = cl_gui_textedit=>true
*       filedrop_mode          = dropfile_event_off
      EXCEPTIONS
        error_cntl_create      = 1
        error_dp_create        = 2
        gui_type_not_supported = 3
        error_cntl_init        = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    go_textedit->set_readonly_mode( ).
  ENDIF.
  CALL METHOD go_textedit->set_text_as_stream
    EXPORTING
      text   = lt_text
    EXCEPTIONS
      OTHERS = 0.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module SHOW_TEXT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE show_text OUTPUT.
  SET PF-STATUS 'STATUS_100'.
* SET TITLEBAR 'xxx'.
  PERFORM show_text.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
