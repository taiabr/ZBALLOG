CLASS zcl_ballog_simple DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !obj TYPE bal_s_log-object
        !sub TYPE bal_s_log-subobject OPTIONAL
        !des TYPE bal_s_log-extnumber OPTIONAL
      RAISING
        zcx_ballog .
    METHODS save
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog_simple
      RAISING
        zcx_ballog .
    METHODS display
      IMPORTING
        !popup TYPE abap_bool OPTIONAL
      RAISING
        zcx_ballog .
    METHODS add
      IMPORTING
        !log             TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog_simple
      RAISING
        zcx_ballog .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA o_logref TYPE REF TO zcl_ballog .
ENDCLASS.



CLASS ZCL_BALLOG_SIMPLE IMPLEMENTATION.


  METHOD add.
    IF log IS SUPPLIED.
      me->o_logref->log_add( i_logdata = log ).
    ELSE.
      me->o_logref->log_add( ).
    ENDIF.
    ro_ballog = me.
  ENDMETHOD.


  METHOD constructor.
    CLEAR me->o_logref.
    me->o_logref = zcl_ballog_factory=>new(
      iv_object = obj
      iv_subobj = sub
      iv_descri = des
    ).
  ENDMETHOD.


  METHOD display.
    me->o_logref->display( iv_popup = popup ).
  ENDMETHOD.


  METHOD save.
    me->o_logref->save( ).
    ro_ballog = me.
  ENDMETHOD.
ENDCLASS.
