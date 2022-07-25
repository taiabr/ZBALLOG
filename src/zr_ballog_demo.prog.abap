*&---------------------------------------------------------------------*
*& Report ZR_BALLOG_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_ballog_demo.

PARAMETERS p_group TYPE bal_s_log-object   .
PARAMETERS p_subgr TYPE bal_s_log-subobject.
PARAMETERS p_extno TYPE bal_s_log-extnumber.
SELECTION-SCREEN SKIP.

PARAMETERS p_addtr TYPE baltabname.
SELECTION-SCREEN SKIP.

PARAMETERS any RADIOBUTTON GROUP g1 DEFAULT 'X'.
PARAMETERS show RADIOBUTTON GROUP g1.
PARAMETERS new RADIOBUTTON GROUP g1.
PARAMETERS simple RADIOBUTTON GROUP g1.
PARAMETERS encadead RADIOBUTTON GROUP g1.
PARAMETERS encadea2 RADIOBUTTON GROUP g1.

START-OF-SELECTION.
  DATA(lt_return)  = VALUE bapiret1_tab( ( type = 'A' id = '8B' number = '003' ) ).
  DATA(ls_return)  = VALUE bapireturn1( type = 'E' id = '8B' number = '003' ).
  DATA(ls_bsegkey) = VALUE bsegkey( bukrs = '0000' belnr = '1111111111' gjahr = '2222' buzei = '333' ).

  TRY.
      CASE abap_true.
        WHEN show.
          zcl_ballog_factory=>show(
            iv_object = p_group
            iv_subobj = p_subgr
            iv_descri = p_extno
            iv_addstr = p_addtr
*            iv_begda  = sy-datum
*            iv_endda  = sy-datum
*            iv_usact  = abap_true
*            iv_popup  = abap_true
*            iv_newwd  = abap_true
          ).

        WHEN new.
          DATA(lo_log) = zcl_ballog_factory=>new(
            iv_object = p_group
            iv_subobj = p_subgr
            iv_descri = p_extno
          ).

          lo_log->set_addinfo_struc( p_addtr ).

          MESSAGE e003(8b) INTO DATA(lv_dummy).
          lo_log->log_add( is_addinfo = ls_bsegkey ).

          lo_log->log_msg(
            iv_msgid   = '8B'
            iv_msgno   = '003'
            iv_msgty   = 'E'
            is_addinfo = ls_bsegkey
          ).

          lo_log->log_text(
            iv_text    = 'Erro text'
            iv_type    = 'X'
            is_addinfo = ls_bsegkey
          ).

          lo_log->log_bapireturn1(
            is_bapiret1 = ls_return
            is_addinfo = ls_bsegkey
          ).

          lo_log->display( ).

          lo_log->save( ).
          lo_log->delete( ).

          lo_log->finish( ).

        WHEN simple.
          DATA(lo_simple) = NEW zcl_ballog_simple(
            obj = p_group
            sub = p_subgr
            des = p_extno
          ).

          MESSAGE e003(8b) INTO lv_dummy.
          lo_simple->add( ).
          MESSAGE w003(8b) INTO lv_dummy.
          lo_simple->add( ).
          lo_simple->add( ls_return ).
          lo_simple->add( lt_return ).

          TRY.
              DATA(ls_retaux) = lt_return[ 3 ].
            CATCH cx_sy_itab_line_not_found INTO DATA(lx_notfound).
              lo_simple->add( lx_notfound ).
          ENDTRY.

          lo_simple->display( abap_true ).

        WHEN encadead.
          NEW zcl_ballog_simple( obj = p_group sub = p_subgr des = p_extno )->add( ls_return )->display( ).

        WHEN encadea2.
          zcl_ballog_factory=>new( iv_object = p_group iv_subobj = p_subgr iv_descri = p_extno
          )->set_addinfo_struc( p_addtr
          )->log_bapireturn1( is_bapiret1 = ls_return is_addinfo = ls_bsegkey
          )->display( ).

        WHEN any.
          zcl_ballog_factory=>new( iv_object = p_group iv_subobj = p_subgr iv_descri = p_extno
          )->log_add( i_logdata = ls_return
          )->log_add( i_logdata = lt_return
          )->log_add( i_logdata = 'Texto' )->display( ).

      ENDCASE.

    CATCH zcx_ballog INTO DATA(lx_log).
      MESSAGE lx_log->get_symsg( ) TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
