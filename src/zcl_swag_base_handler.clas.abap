class ZCL_SWAG_BASE_HANDLER definition
  public
  abstract
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF ty_router_class,
        clsname TYPE seoclass-clsname,
      END OF ty_router_class .
  types:
    tt_router_class TYPE STANDARD TABLE OF ty_router_class WITH DEFAULT KEY .

  class-data MV_BASE type STRING .
  class-data MV_TITLE type STRING .
  PROTECTED SECTION.

    METHODS get_router_classes
      RETURNING
        VALUE(rt_classes) TYPE tt_router_class .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SWAG_BASE_HANDLER IMPLEMENTATION.


  METHOD get_router_classes.
    DATA: lv_base LIKE mv_base.
    lv_base = |`{ mv_base }`|.
    SELECT a~clsname INTO TABLE @rt_classes FROM seoclass AS a
        JOIN seometarel AS b ON a~clsname = b~clsname AND b~refclsname = 'ZIF_SWAG_CONSUMER' AND b~reltype = '1'
        JOIN seoredef AS c ON c~clsname = a~clsname AND mtdname = 'MV_BASE' AND attvalue = @lv_base
        WHERE a~clstype = '0'.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.

    DATA: lo_swag   TYPE REF TO zcl_swag.
    DATA: lt_classes TYPE tt_router_class.

    CREATE OBJECT lo_swag
      EXPORTING
        ii_server = server
        iv_title  = mv_title
        iv_base   = mv_base.
    lt_classes = get_router_classes(  ).
    TRY.
        LOOP AT lt_classes INTO DATA(ls_class).
          lo_swag->register( ls_class-clsname ).
        ENDLOOP.
        lo_swag->run( ).
      CATCH zcx_swag.
        "handle exception
      CATCH cx_static_check.
        "handle exception
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
