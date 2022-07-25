CLASS zcl_ballog_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS new
      IMPORTING
        !iv_object       TYPE bal_s_log-object
        !iv_subobj       TYPE bal_s_log-subobject OPTIONAL
        !iv_descri       TYPE bal_s_log-extnumber OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog
      RAISING
        zcx_ballog .
    CLASS-METHODS show
      IMPORTING
        !iv_object TYPE bal_s_log-object
        !iv_subobj TYPE bal_s_log-subobject OPTIONAL
        !iv_descri TYPE bal_s_log-extnumber OPTIONAL
        !iv_addstr TYPE baltabname OPTIONAL
        !iv_begda  TYPE begda DEFAULT sy-datum
        !iv_endda  TYPE endda DEFAULT sy-datum
        !iv_usact  TYPE abap_bool OPTIONAL
        !iv_popup  TYPE abap_bool OPTIONAL
        !iv_newwd  TYPE abap_bool OPTIONAL
      RAISING
        zcx_ballog .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BALLOG_FACTORY IMPLEMENTATION.


  METHOD new.

    ro_ballog = NEW zcl_ballog(
      iv_object = iv_object
      iv_subobj = iv_subobj
      iv_descri = iv_descri
    ).

    ro_ballog->create( ).

  ENDMETHOD.


  METHOD show.

    DATA:
      lv_begda TYPE begda,
      lv_endda TYPE endda,
      lv_retcd TYPE c.

    lv_begda = iv_begda.
    lv_endda = iv_endda.

    IF iv_usact = abap_true.
      zcl_ballog=>user_confirm_date(
        CHANGING
          cv_begda = lv_begda
          cv_endda = lv_endda
          cv_retcd = lv_retcd
      ).
      CHECK lv_retcd <> 'A'.  "cancelado pelo usuario
    ENDIF.

    DATA(lo_ballog) = NEW zcl_ballog(
      iv_object = iv_object
      iv_subobj = iv_subobj
      iv_descri = iv_descri
    ).

    lo_ballog->load(
      iv_begda = lv_begda
      iv_endda = lv_endda
    ).

    IF iv_addstr IS NOT INITIAL.
      lo_ballog->set_addinfo_struc( iv_addstr ).
    ENDIF.

    lo_ballog->display(
      iv_begda = lv_begda
      iv_endda = lv_endda
      iv_popup = iv_popup
      iv_newwd = iv_newwd
    ).

    lo_ballog->finish( ).

  ENDMETHOD.
ENDCLASS.
