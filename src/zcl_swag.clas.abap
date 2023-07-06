CLASS zcl_swag DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_parameters_tt TYPE STANDARD TABLE OF seosubcodf WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_url,
        regex       TYPE string,
        group_names TYPE STANDARD TABLE OF seosconame WITH DEFAULT KEY,
      END OF ty_url .
    TYPES:
      BEGIN OF ty_meta,
        summary TYPE string,
        url     TYPE ty_url,
        method  TYPE string,
        handler TYPE string,
        tags    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      END OF ty_meta .
    TYPES:
      BEGIN OF ty_meta_internal,
        meta       TYPE ty_meta,
        obj        TYPE REF TO object,
        parameters TYPE ty_parameters_tt,
        classname  TYPE seoclsname,
      END OF ty_meta_internal .
    TYPES:
      ty_meta_internal_tt TYPE STANDARD TABLE OF ty_meta_internal WITH DEFAULT KEY .
    TYPES:
      ty_meta_tt TYPE STANDARD TABLE OF ty_meta WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_externaldoc,
        description TYPE string,
        url         TYPE string,
      END OF ty_externaldoc .
    TYPES:
      BEGIN OF ty_tagdescription,
        tag         TYPE string,
        description TYPE string,
        externaldoc TYPE ty_externaldoc,
      END OF ty_tagdescription .
    TYPES:
      ty_tagdescription_tt TYPE STANDARD TABLE OF ty_tagdescription WITH DEFAULT KEY .

    TYPES:
      BEGIN OF ty_base,
        ext_sys_no TYPE ztswag_log-zext_sys_no,
        if_no      TYPE ztswag_log-zif_no,
        ext_key    TYPE ztswag_log-zext_key,
        send_time  TYPE ztswag_log-zsend_time,
      END OF ty_base ,
      BEGIN OF ty_request,
        base_info TYPE ty_base,
        request   TYPE string,
      END OF ty_request ,
      BEGIN OF ty_response,
        msgty    TYPE bapiret2-type,
        msg      TYPE bapiret2-message,
        response TYPE string,
      END OF ty_response .

    CONSTANTS:
      BEGIN OF c_parm_kind,
        importing TYPE seopardecl VALUE '0',
        exporting TYPE seopardecl VALUE '1',
        changing  TYPE seopardecl VALUE '2',
        returning TYPE seopardecl VALUE '3',
      END OF c_parm_kind .
    CONSTANTS:
      BEGIN OF c_method,
        get    TYPE string VALUE 'GET',
        post   TYPE string VALUE 'POST',
        put    TYPE string VALUE 'PUT',
        delete TYPE string VALUE 'DELETE',
      END OF c_method .

    METHODS constructor
      IMPORTING
        !ii_server       TYPE REF TO if_http_server
        !iv_base         TYPE string
        !iv_swagger_json TYPE string DEFAULT '/swagger.json'
        !iv_swagger_html TYPE string DEFAULT '/swagger.html'
        !iv_title        TYPE string .
    METHODS register
      IMPORTING
        !iv_clsname TYPE seoclass-clsname
      RAISING
        zcx_swag.
    METHODS run
      RAISING
        cx_static_check .
    METHODS set_tagdescription
      IMPORTING
        !is_description TYPE ty_tagdescription .
  PROTECTED SECTION.

    DATA mv_base TYPE string .
    DATA mi_server TYPE REF TO if_http_server .
    DATA mt_meta TYPE ty_meta_internal_tt .
    DATA mt_tagdescription TYPE ty_tagdescription_tt .
    DATA mv_swagger_json TYPE string .
    DATA mv_swagger_html TYPE string .
    DATA mv_title TYPE string .
    DATA ms_log TYPE ztswag_log .

    METHODS create_instance
      IMPORTING
                !is_meta       TYPE ty_meta_internal
      RETURNING
                VALUE(rs_meta) TYPE ty_meta_internal
      RAISING   zcx_swag.
    METHODS build_parameters
      IMPORTING
        !is_meta             TYPE ty_meta_internal
      RETURNING
        VALUE(rt_parameters) TYPE abap_parmbind_tab .
    METHODS create_data
      IMPORTING
        !is_meta       TYPE ty_meta_internal
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
    METHODS from_body
      IMPORTING
        !is_meta TYPE ty_meta_internal
        !ir_ref  TYPE REF TO data .
    METHODS from_query
      IMPORTING
        !is_meta TYPE ty_meta_internal
        !ir_ref  TYPE REF TO data .
    METHODS from_path
      IMPORTING
        !is_meta TYPE ty_meta_internal
        !ir_ref  TYPE REF TO data .
    METHODS generate_spec
      IMPORTING
        !iv_title       TYPE clike
        !iv_description TYPE clike .
    METHODS generate_ui
      IMPORTING
        !iv_json_url TYPE string
        !iv_dist     TYPE string DEFAULT ''
        !iv_title    TYPE clike DEFAULT ''
      RETURNING
        VALUE(rv_ui) TYPE string .
    METHODS json_reply
      IMPORTING
        !is_meta       TYPE ty_meta_internal
        !it_parameters TYPE abap_parmbind_tab .
    METHODS text_reply
      IMPORTING
        !is_meta       TYPE ty_meta_internal
        !it_parameters TYPE abap_parmbind_tab .
    METHODS validate_parameters
      IMPORTING
        !it_parameters TYPE ty_parameters_tt .
    METHODS validate_interface
      RETURNING
        VALUE(rv_valid) TYPE abap_bool .
    METHODS set_log_base
      IMPORTING
        !is_input TYPE any .
    METHODS set_log_error
      IMPORTING
        VALUE(iv_error) TYPE string
        !it_parameters  TYPE abap_parmbind_tab .
    METHODS set_log_response
      IMPORTING
        VALUE(iv_response) TYPE clike .
    METHODS wrap_error_response
      IMPORTING
        VALUE(iv_error) TYPE string
      RETURNING
        VALUE(rv_error) TYPE string .
    METHODS save_log.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_swag IMPLEMENTATION.


  METHOD build_parameters.

    DATA: ls_parameter LIKE LINE OF rt_parameters,
          lr_dref      TYPE REF TO data.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters,
                   <lg_comp>      TYPE any,
                   <lg_struc>     TYPE any.

    lr_dref = create_data( is_meta ).
    ASSIGN lr_dref->* TO <lg_struc>.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>.
      ASSIGN COMPONENT <ls_parameter>-sconame OF STRUCTURE <lg_struc> TO <lg_comp>.
      ASSERT sy-subrc = 0.
      ls_parameter-name  = <ls_parameter>-sconame.
      GET REFERENCE OF <lg_comp> INTO ls_parameter-value.
      INSERT ls_parameter INTO TABLE rt_parameters.
    ENDLOOP.

    from_path( is_meta = is_meta
               ir_ref  = lr_dref ).

    IF is_meta-meta-method = c_method-get.
      from_query( is_meta = is_meta
                  ir_ref  = lr_dref ).
    ELSE.
      from_body( is_meta = is_meta
                 ir_ref  = lr_dref ).
    ENDIF.
    set_log_base( <lg_struc> ).
  ENDMETHOD.


  METHOD constructor.

    mi_server       = ii_server.
    mv_base         = iv_base.
    mv_swagger_json = iv_swagger_json.
    mv_swagger_html = iv_swagger_html.
    mv_title        = iv_title.
    CALL FUNCTION 'ZFUN_GET_DATE_REC'
      EXPORTING
        iv_mode = 'I'
      CHANGING
        cs_data = ms_log.
    TRY.
        ms_log-zmsg_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.

  ENDMETHOD.


  METHOD create_data.

    DATA: lo_struct     TYPE REF TO cl_abap_structdescr,
          lt_components TYPE cl_abap_structdescr=>component_table,
          lo_typedescr  TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components,
                   <ls_parameter> LIKE LINE OF is_meta-parameters.


    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>.
      APPEND INITIAL LINE TO lt_components ASSIGNING <ls_component>.
      <ls_component>-name = <ls_parameter>-sconame.

      lo_typedescr = zcl_swag_map_type=>get_typedescr( <ls_parameter> ).

      <ls_component>-type ?= lo_typedescr.
    ENDLOOP.

    lo_struct = cl_abap_structdescr=>get( lt_components ).

    CREATE DATA rr_data TYPE HANDLE lo_struct.

  ENDMETHOD.


  METHOD from_body.

    DATA: lv_cdata  TYPE string,
          lo_writer TYPE REF TO cl_sxml_string_writer,
          lv_json   TYPE xstring.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters,
                   <lg_comp>      TYPE any,
                   <lg_struc>     TYPE any.


    ASSIGN ir_ref->* TO <lg_struc>.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>
        WHERE pardecltyp = c_parm_kind-importing.
      READ TABLE is_meta-meta-url-group_names FROM <ls_parameter>-sconame
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* ignore parameters that are part of url
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <ls_parameter>-sconame OF STRUCTURE <lg_struc> TO <lg_comp>.
      ASSERT sy-subrc = 0.

      IF <ls_parameter>-type = 'STRING'.
        <lg_comp> = mi_server->request->get_cdata( ).
        ms_log-zrequest = <lg_comp>.
      ELSEIF <ls_parameter>-type = 'XSTRING'.
        <lg_comp> = mi_server->request->get_data( ).
        ms_log-zrequest = <lg_comp>.
      ELSE.
        lv_cdata = mi_server->request->get_cdata( ).
        ms_log-zrequest = lv_cdata.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_cdata CHANGING data = <lg_comp> ).
      ENDIF.

* multiple body input parameters not allowed
* todo, this should be validated earlier
      RETURN.

    ENDLOOP.

  ENDMETHOD.


  METHOD from_path.

    DEFINE _store.
      READ TABLE is_meta-meta-url-group_names INDEX &1 INTO lv_component.
      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_component OF STRUCTURE <lg_struc> TO <lg_comp>.
        ASSERT sy-subrc = 0.
        <lg_comp> = lv_match&1.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lv_path      TYPE string,
          lv_component TYPE string,
          lv_match1    TYPE string,
          lv_match2    TYPE string,
          lv_match3    TYPE string,
          lv_match4    TYPE string,
          lv_match5    TYPE string.

    FIELD-SYMBOLS: <lg_comp>  TYPE any,
                   <lg_struc> TYPE any.


    ASSIGN ir_ref->* TO <lg_struc>.

    lv_path = mi_server->request->get_header_field( '~path' ).
    lv_path = cl_http_utility=>unescape_url( lv_path ).

    FIND REGEX is_meta-meta-url-regex IN lv_path
      SUBMATCHES lv_match1 lv_match2 lv_match3 lv_match4 lv_match5.

    _store 1.
    _store 2.
    _store 3.
    _store 4.
    _store 5.

  ENDMETHOD.


  METHOD from_query.

    DATA: lv_field  TYPE string.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters,
                   <lg_comp>      TYPE any,
                   <lg_struc>     TYPE any.


    ASSIGN ir_ref->* TO <lg_struc>.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>
        WHERE pardecltyp = c_parm_kind-importing.
      READ TABLE is_meta-meta-url-group_names FROM <ls_parameter>-sconame
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* ignore parameters that are part of url
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <ls_parameter>-sconame OF STRUCTURE <lg_struc> TO <lg_comp>.
      ASSERT sy-subrc = 0.

      lv_field = to_lower( <ls_parameter>-sconame ).
      <lg_comp> = mi_server->request->get_form_field( lv_field ).

    ENDLOOP.

  ENDMETHOD.


  METHOD generate_spec.

    DATA: lv_spec TYPE string,
          lo_spec TYPE REF TO zcl_swag_spec.

    LOOP AT mt_meta ASSIGNING FIELD-SYMBOL(<ls_meta>).
      <ls_meta> = create_instance( <ls_meta> ).
    ENDLOOP.
    CREATE OBJECT lo_spec
      EXPORTING
        iv_title          = iv_title
        iv_description    = iv_description
        it_meta           = mt_meta
        iv_base           = mv_base
        it_tagdescription = mt_tagdescription.

    lv_spec = lo_spec->generate( ).

    mi_server->response->set_cdata( lv_spec ).
    mi_server->response->set_content_type( 'application/json' ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  ENDMETHOD.


  METHOD generate_ui.
* todo, IV_DIST not supplyed from anywhere?

    DEFINE _add.
      CONCATENATE rv_ui &1 cl_abap_char_utilities=>newline
        INTO rv_ui ##NO_TEXT.
    END-OF-DEFINITION.

    _add '<!DOCTYPE html>'.
    _add '<html>'.
    _add '<head>'.
    _add '<meta charset="UTF-8">'.

    _add '<title>'.
    IF iv_title IS INITIAL.
      _add 'Swagger UI'.
    ELSE.
      _add iv_title.
    ENDIF.
    _add '</title>'.

    _add '<link href="iv_dist/swagger-ui.css" media="screen" rel="stylesheet" type="text/css"/>'.
    _add '<style>'.
    _add '  body {'.
    _add '    margin:0;'.
    _add '    background: #fafafa;'.
    _add '  }'.
    _add '</style>'.
    _add '</head>'.
    _add '<body>'.
    _add '<div id="swagger-ui"></div>'.
    _add '<script src="iv_dist/swagger-ui-bundle.js" type="text/javascript"></script>'.
    _add '<script src="iv_dist/swagger-ui-standalone-preset.js" type="text/javascript"></script>'.
    _add '<script type="text/javascript">'.
    _add 'window.onload = function() {'.
    _add 'const ui = SwaggerUIBundle({'.
    _add '  url: "swagger.json",'.
    _add '  validatorUrl: "",'.
    _add '  dom_id: "#swagger-ui",'.
    _add '  presets: ['.
    _add '    SwaggerUIBundle.presets.apis,'.
    _add '    Array.isArray(SwaggerUIStandalonePreset) ? SwaggerUIStandalonePreset : SwaggerUIStandalonePreset.default'.
    _add '  ],'.
    _add '  plugins: ['.
    _add '    SwaggerUIBundle.plugins.DownloadUrl'.
    _add '  ],'.
    _add '  layout: "StandaloneLayout"'.
    _add '})'.
    _add 'window.ui = ui'.
    _add '}'.
    _add '</script>'.
    _add '</body>'.
    _add '</html>'.

    IF iv_dist IS INITIAL.
      REPLACE ALL OCCURRENCES OF 'iv_dist'
        IN rv_ui WITH 'https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/3.22.2'.
    ELSE.
      REPLACE ALL OCCURRENCES OF 'iv_dist'
        IN rv_ui WITH iv_dist ##NO_TEXT.
    ENDIF.

    REPLACE FIRST OCCURRENCE OF 'swagger.json'
      IN rv_ui WITH iv_json_url ##NO_TEXT.

    mi_server->response->set_cdata( rv_ui ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  ENDMETHOD.


  METHOD json_reply.

    DATA: lv_json   TYPE string.

    FIELD-SYMBOLS: <ls_meta>      LIKE LINE OF is_meta-parameters,
                   <ls_parameter> LIKE LINE OF it_parameters,
                   <lg_struc>     TYPE any.


    READ TABLE is_meta-parameters ASSIGNING <ls_meta>
      WITH KEY pardecltyp = c_parm_kind-returning.
    IF sy-subrc  = 0.
      READ TABLE it_parameters ASSIGNING <ls_parameter>
        WITH KEY name = <ls_meta>-sconame.
      ASSERT sy-subrc = 0.
      ASSIGN <ls_parameter>-value->* TO <lg_struc>.
      IF <ls_meta>-type = 'ZCL_SWAG=>TY_RESPONSE'.
        ASSIGN COMPONENT 'MSGTY' OF STRUCTURE <lg_struc> TO FIELD-SYMBOL(<fs_msgty>).
        IF sy-subrc = 0.
          ms_log-zmsgty = <fs_msgty>.
        ENDIF.
      ENDIF.
      lv_json = /ui2/cl_json=>serialize( data = <lg_struc> compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    ENDIF.

    set_log_response( lv_json ).
    mi_server->response->set_cdata( lv_json ).

  ENDMETHOD.


  METHOD register.

    DATA: ls_meta LIKE LINE OF mt_meta,
          lt_meta TYPE ty_meta_tt,
          lo_obj  TYPE REF TO cl_abap_objectdescr.

    TRY.
        CALL METHOD (iv_clsname)=>meta
          RECEIVING
            rt_meta = lt_meta.
        LOOP AT lt_meta INTO ls_meta-meta.
          ls_meta-classname = iv_clsname.
          APPEND ls_meta TO mt_meta.
          CLEAR ls_meta.
        ENDLOOP.
      CATCH cx_root INTO DATA(lo_error).
        zcx_swag=>raise_text( lo_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD run.

    DATA: lv_path       TYPE string,
          lv_method     TYPE string,
          lt_parameters TYPE abap_parmbind_tab,
          lx_swag       TYPE REF TO zcx_swag.
    DATA: ls_error TYPE string.
    FIELD-SYMBOLS: <ls_meta> LIKE LINE OF mt_meta.


    lv_path = mi_server->request->get_header_field( '~path' ).
    lv_path = cl_http_utility=>unescape_url( lv_path ).
    lv_method = mi_server->request->get_method( ).

    IF lv_path = mv_base && mv_swagger_html.
      generate_ui(
        iv_json_url = mv_base && mv_swagger_json
        iv_title    = mv_title && ' - Swagger' ).
      RETURN.
    ELSEIF lv_path = mv_base && mv_swagger_json.
      generate_spec(
        iv_title       = mv_title
        iv_description = mv_title && ' REST functions' ).
      RETURN.
    ENDIF.

    LOOP AT mt_meta ASSIGNING <ls_meta> WHERE meta-method = lv_method.

      FIND REGEX <ls_meta>-meta-url-regex IN lv_path.
      IF sy-subrc = 0.
        TRY.
            <ls_meta> = create_instance( <ls_meta> ).
            lt_parameters = build_parameters( <ls_meta> ).
            IF validate_interface(  ).
              CALL METHOD <ls_meta>-obj->(<ls_meta>-meta-handler)
                PARAMETER-TABLE lt_parameters.
            ELSE.
              set_log_error( iv_error = CONV #( '接口未启用'(002) )  it_parameters = lt_parameters ).
            ENDIF.
          CATCH zcx_swag INTO lx_swag.
            ls_error = wrap_error_response( lx_swag->get_text( ) ).
            mi_server->response->set_cdata( ls_error ).
            mi_server->response->set_status( code = lx_swag->status_code reason = lx_swag->get_text( ) ).
            set_log_response( ls_error ).
            RETURN.
        ENDTRY.

        mi_server->response->set_compression( ).

        LOOP AT <ls_meta>-parameters
            TRANSPORTING NO FIELDS
            WHERE pardecltyp = c_parm_kind-returning
            AND ( type = 'STRING' OR type = 'XSTRING' ).
* assumption: RETURNING only, no EXPORTING at the same time
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          text_reply( is_meta       = <ls_meta>
                      it_parameters = lt_parameters ).
        ELSE.
          mi_server->response->set_header_field( name  = 'content-type'
                                                 value = 'application/json' ).
          json_reply( is_meta       = <ls_meta>
                      it_parameters = lt_parameters ).
        ENDIF.

        mi_server->response->set_header_field( name  = 'cache-control'
                                               value = 'no-cache' ).

        mi_server->response->set_status( code = 200 reason = '200' ).

        save_log( ).
        RETURN.

      ENDIF.
    ENDLOOP.

    mi_server->response->set_cdata( '404, swagger' ).
    mi_server->response->set_status( code = 404 reason = '404' ).

  ENDMETHOD.


  METHOD set_tagdescription.

    APPEND is_description TO mt_tagdescription.

  ENDMETHOD.


  METHOD text_reply.

    FIELD-SYMBOLS: <lg_any>       TYPE any,
                   <ls_meta>      LIKE LINE OF is_meta-parameters,
                   <ls_parameter> LIKE LINE OF it_parameters.


    READ TABLE is_meta-parameters ASSIGNING <ls_meta>
      WITH KEY pardecltyp = c_parm_kind-returning.
    IF sy-subrc  = 0.
      READ TABLE it_parameters ASSIGNING <ls_parameter>
        WITH KEY name = <ls_meta>-sconame.
      ASSERT sy-subrc = 0.

      ASSIGN <ls_parameter>-value->* TO <lg_any>.

      CASE <ls_meta>-type.
        WHEN 'XSTRING'.
          mi_server->response->set_data( <lg_any> ).
        WHEN 'STRING'.
          mi_server->response->set_cdata( <lg_any> ).
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDIF.
    set_log_response( <lg_any> ).
  ENDMETHOD.


  METHOD validate_parameters.

* no EXPORTING, no CHANGING
    LOOP AT it_parameters TRANSPORTING NO FIELDS
        WHERE pardecltyp = c_parm_kind-exporting
        OR pardecltyp = c_parm_kind-changing.
      ASSERT 0 = 1.
    ENDLOOP.

* no reference types
* todo

* todo, max one importing parameter? apart from path parameters?

  ENDMETHOD.


  METHOD create_instance.
    DATA: lo_obj      TYPE REF TO cl_abap_objectdescr,
          lo_consumer TYPE REF TO zif_swag_consumer.
    rs_meta = is_meta.
    TRY.
        CREATE OBJECT lo_consumer TYPE (rs_meta-classname).
      CATCH cx_root.
        zcx_swag=>raise_text( |{ rs_meta-classname }{  TEXT-003 }| ).
    ENDTRY.

    rs_meta-obj = lo_consumer.

    lo_obj ?= cl_abap_objectdescr=>describe_by_object_ref( lo_consumer ).

    READ TABLE lo_obj->methods
      WITH KEY name = rs_meta-meta-handler
      visibility = cl_abap_objectdescr=>public
      TRANSPORTING NO FIELDS.
* method must exist and be public
    IF sy-subrc <> 0.
      zcx_swag=>raise_text( |{ rs_meta-meta-handler }{  TEXT-004 }| ).
    ENDIF.

    SELECT * FROM seosubcodf
      INTO TABLE rs_meta-parameters
      WHERE clsname = rs_meta-classname
      AND cmpname = rs_meta-meta-handler
      AND sconame NOT LIKE 'ZCX_%'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC
    " the method does not have any parameters
    IF sy-subrc <> 0.
      zcx_swag=>raise_text( |{ rs_meta-meta-handler }{  TEXT-005 }| ).
    ENDIF.
    validate_parameters( rs_meta-parameters ).


  ENDMETHOD.


  METHOD validate_interface.
    DATA ls_ztswag_if TYPE ztswag_if.
    IF ms_log-zif_no IS NOT INITIAL.
      SELECT SINGLE * INTO @ls_ztswag_if FROM ztswag_if WHERE zif_no = @ms_log-zif_no.
      IF sy-subrc = 0 AND ls_ztswag_if-zactive = abap_true.
        rv_valid = abap_true.
        "don't save log.
        IF ls_ztswag_if-zlog <> abap_true.
          CLEAR ms_log.
        ENDIF.
      ELSE.
        rv_valid = abap_false.
      ENDIF.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD set_log_base.
    FIELD-SYMBOLS: <ls_base> TYPE ty_base.
    ASSIGN ('is_input-iv_request-base_info') TO <ls_base>.
    IF sy-subrc = 0.
      ms_log-zext_key = <ls_base>-ext_key.
      ms_log-zext_sys_no = to_upper( <ls_base>-ext_sys_no ).
      ms_log-zif_no = to_upper( <ls_base>-if_no ).
      ms_log-zsend_time = <ls_base>-send_time.
    ENDIF.
  ENDMETHOD.


  METHOD set_log_error.
    FIELD-SYMBOLS: <ls_response> TYPE ty_response.
    READ TABLE it_parameters ASSIGNING FIELD-SYMBOL(<fs_parameters>) WITH KEY name = 'RV_RESPONSE'.
    IF sy-subrc = 0.
      ASSIGN ('<fs_parameters>-value->*') TO <ls_response>.
      IF sy-subrc = 0.
        <ls_response>-msgty = 'E'.
        <ls_response>-msg = iv_error.
      ENDIF.
    ENDIF.
    ms_log-zmsgty = 'E'.
  ENDMETHOD.


  METHOD set_log_response.
    GET TIME.
    CALL FUNCTION 'ZFUN_GET_DATE_REC'
      EXPORTING
        iv_mode = 'M'
      CHANGING
        cs_data = ms_log.
    ms_log-zresponse = iv_response.
  ENDMETHOD.
  METHOD wrap_error_response.
    DATA ls_response TYPE ty_response.
    ls_response-msgty = 'E'.
    ls_response-msg = iv_error.
    ms_log-zmsgty = 'E'.
    rv_error = /ui2/cl_json=>serialize( data = ls_response compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.

  METHOD save_log.
    IF ms_log-zif_no IS NOT INITIAL.
      MODIFY ztswag_log FROM ms_log.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
