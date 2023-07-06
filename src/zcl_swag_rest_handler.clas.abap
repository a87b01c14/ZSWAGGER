class ZCL_SWAG_REST_HANDLER definition
  public
  inheriting from ZCL_SWAG_BASE_HANDLER
  final
  create public .

public section.

  class-data GV_BASE type STRING value `/zapi` ##NO_TEXT.
  class-data GV_TITLE type STRING value `REST API` ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SWAG_REST_HANDLER IMPLEMENTATION.


  METHOD class_constructor.
    mv_base = gv_base.
    mv_title = gv_title.
  ENDMETHOD.
ENDCLASS.
