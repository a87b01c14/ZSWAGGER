class ZCL_SWAG_CONSUMER definition
  public
  create public .

public section.

  interfaces ZIF_SWAG_CONSUMER
      data values MV_BASE = `/zapi` .

  aliases META
    for ZIF_SWAG_CONSUMER~META .

  methods GET_PLANT
    importing
      !IV_REQUEST type ZCL_SWAG=>TY_REQUEST
    returning
      value(RV_RESPONSE) type ZCL_SWAG=>TY_RESPONSE .
  methods GET_BUKRS
    importing
      !IV_REQUEST type ZCL_SWAG=>TY_REQUEST
    returning
      value(RV_RESPONSE) type ZCL_SWAG=>TY_RESPONSE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SWAG_CONSUMER IMPLEMENTATION.


  METHOD zif_swag_consumer~meta.
    FIELD-SYMBOLS: <ls_meta> LIKE LINE OF rt_meta.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = '获取工厂信息'.
    <ls_meta>-url-regex = '/IF001/'.
    <ls_meta>-method    = zcl_swag=>c_method-post.
    <ls_meta>-handler   = 'GET_PLANT'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = '获取公司信息'.
    <ls_meta>-url-regex = '/IF002/'.
    <ls_meta>-method    = zcl_swag=>c_method-post.
    <ls_meta>-handler   = 'GET_BUKRS'.
  ENDMETHOD.


  METHOD get_plant.
    SELECT werks,name1 INTO TABLE @DATA(lt_werks) FROM t001w.
    rv_response-response = /ui2/cl_json=>serialize( data = lt_werks compress = abap_false pretty_name = abap_true ).
    rv_response-msgty = 'S'.
    rv_response-msg = 'Success'.
  ENDMETHOD.


  METHOD get_bukrs.
    SELECT bukrs,butxt INTO TABLE @DATA(lt_bukrs) FROM t001.
    rv_response-response = /ui2/cl_json=>serialize( data = lt_bukrs compress = abap_false pretty_name = abap_true ).
    rv_response-msgty = 'S'.
    rv_response-msg = 'Success'.
  ENDMETHOD.
ENDCLASS.
