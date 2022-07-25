CLASS zcl_ballog DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS user_confirm_date
      CHANGING
        !cv_begda TYPE begda
        !cv_endda TYPE endda
        !cv_retcd TYPE c .
    METHODS constructor
      IMPORTING
        !iv_object TYPE bal_s_log-object OPTIONAL
        !iv_subobj TYPE bal_s_log-subobject OPTIONAL
        !iv_descri TYPE bal_s_log-extnumber OPTIONAL
      RAISING
        zcx_ballog .
    METHODS refresh
      RAISING
        zcx_ballog .
    METHODS create
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog
      RAISING
        zcx_ballog .
    METHODS load
      IMPORTING
        !iv_begda        TYPE begda DEFAULT sy-datum
        !iv_endda        TYPE endda DEFAULT sy-datum
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog
      RAISING
        zcx_ballog .
    METHODS save
      IMPORTING
        !iv_all          TYPE abap_bool OPTIONAL
        !iv_upd          TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog
      RAISING
        zcx_ballog .
    METHODS delete
      RAISING
        zcx_ballog .
    METHODS finish
      RAISING
        zcx_ballog .
    METHODS display
      IMPORTING
        !iv_begda TYPE begda DEFAULT sy-datum
        !iv_endda TYPE endda DEFAULT sy-datum
        !iv_popup TYPE abap_bool OPTIONAL
        !iv_newwd TYPE abap_bool OPTIONAL
      RAISING
        zcx_ballog .
    METHODS set_addinfo_struc
      IMPORTING
        !iv_ddicstruc    TYPE baltabname
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_add
      IMPORTING
        !i_logdata       TYPE any OPTIONAL
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog
      RAISING
        zcx_ballog .
    METHODS log_msg
      IMPORTING
        !iv_msgid        TYPE syst-msgid
        !iv_msgno        TYPE syst-msgno
        !iv_msgty        TYPE syst-msgty
        !iv_msgv1        TYPE syst-msgv1 OPTIONAL
        !iv_msgv2        TYPE syst-msgv2 OPTIONAL
        !iv_msgv3        TYPE syst-msgv3 OPTIONAL
        !iv_msgv4        TYPE syst-msgv4 OPTIONAL
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_symsg
      IMPORTING
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_text
      IMPORTING
        !iv_text         TYPE clike
        !iv_type         TYPE syst-msgty DEFAULT 'E'
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_exception
      IMPORTING
        !io_except       TYPE REF TO cx_root
        !iv_type         TYPE syst-msgty DEFAULT 'X'
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_bapiret2
      IMPORTING
        !is_bapiret2     TYPE bapiret2
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_bapiret2_t
      IMPORTING
        !it_bapiret2     TYPE bapiret2_tab
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_bapireturn
      IMPORTING
        !is_bapiret      TYPE bapireturn
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_bapireturn_t
      IMPORTING
        !it_bapiret      TYPE icl_t_bapireturn
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_bapireturn1
      IMPORTING
        !is_bapiret1     TYPE bapireturn1
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_bapireturn1_t
      IMPORTING
        !it_bapiret1     TYPE bapiret1_tab
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_bdcmsg
      IMPORTING
        !is_bdcmsg       TYPE bdcmsgcoll
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
    METHODS log_bdcmsg_t
      IMPORTING
        !it_bdcmsg       TYPE tab_bdcmsgcoll
        !is_addinfo      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ballog) TYPE REF TO zcl_ballog .
  PROTECTED SECTION.

    METHODS get_custom_fcat
      RETURNING
        VALUE(rt_fcat) TYPE bal_t_fcat .
    METHODS get_probclass
      IMPORTING
        !iv_type        TYPE c
      RETURNING
        VALUE(rv_class) TYPE balprobcl .
  PRIVATE SECTION.

    DATA t_balhdl TYPE bal_t_logh .
    DATA s_ballog TYPE bal_s_log .
    DATA s_balcont TYPE bal_s_cont .

    METHODS init .
ENDCLASS.



CLASS ZCL_BALLOG IMPLEMENTATION.


  METHOD constructor.

    me->init( ).
    me->s_ballog-object    = iv_object.
    me->s_ballog-subobject = iv_subobj.
    me->s_ballog-extnumber = iv_descri.

  ENDMETHOD.


  METHOD create.

    DATA:
      lv_log_handle TYPE balloghndl.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = me->s_ballog
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_ballog
        EXPORTING
          msgid = sy-msgid
          msgno = sy-msgno
          msgty = sy-msgty
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4.

    ENDIF.

    APPEND lv_log_handle TO me->t_balhdl[].

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD delete.

    CALL FUNCTION 'BAL_DB_DELETE'
      EXPORTING
        i_t_log_handle    = me->t_balhdl[]
      EXCEPTIONS
        no_logs_specified = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_ballog
        EXPORTING
          msgid = sy-msgid
          msgno = sy-msgno
          msgty = sy-msgty
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4.

    ENDIF.

    CLEAR me->t_balhdl[].


  ENDMETHOD.


  METHOD display.

    DATA:
      lt_loghandle TYPE bal_t_logh,
      ls_logprofil TYPE bal_s_prof,
      ls_logfilter TYPE bal_s_lfil,
      ls_msgfilter TYPE bal_s_mfil.

    CASE iv_popup.
      WHEN abap_true.
        CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
          IMPORTING
            e_s_display_profile = ls_logprofil
          EXCEPTIONS
            OTHERS              = 0.

        ls_logprofil-pop_adjst = abap_true.

      WHEN abap_false.
        CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
          IMPORTING
            e_s_display_profile = ls_logprofil
          EXCEPTIONS
            OTHERS              = 0.

    ENDCASE.

    "Define parametros adicionais de exibicao
    ls_logprofil-use_grid   = 'X'.  "grid ALV
    ls_logprofil-mess_mark  = 'X'.  "marcar msg
    ls_logprofil-show_all   = 'X'.  "exibe todas as msgs de toda a arvore
    ls_logprofil-tree_ontop = ' '.  "' ' = arvore lateral / 'X' = arvore em cima

    "Insere campos adicionais na ALV
    IF me->s_balcont IS NOT INITIAL.
      APPEND LINES OF me->get_custom_fcat( )
        TO ls_logprofil-mess_fcat[].
    ENDIF.

    "Define variante de exibicao
    ls_logprofil-disvariant-report = sy-repid.
    ls_logprofil-disvariant-handle = 'LOG'.

    "Define handler
    lt_loghandle[] = me->t_balhdl[].

    "Cria filtros
    CALL FUNCTION 'BAL_FILTER_CREATE'
      EXPORTING
        i_object       = me->s_ballog-object
        i_subobject    = me->s_ballog-subobject
        i_extnumber    = me->s_ballog-extnumber
        i_aldate_from  = iv_begda
        i_aldate_to    = iv_endda
      IMPORTING
        e_s_log_filter = ls_logfilter.

    "Exibe log
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = ls_logprofil
        i_t_log_handle       = lt_loghandle[]
        i_s_log_filter       = ls_logfilter
        i_s_msg_filter       = ls_msgfilter
        i_amodal             = iv_newwd  "nova janela
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_ballog
        EXPORTING
          msgid = sy-msgid
          msgno = sy-msgno
          msgty = sy-msgty
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4.

    ENDIF.

  ENDMETHOD.


  METHOD finish.

    me->refresh( ).

    "Application Log: Global: Import and insert memory
    CALL FUNCTION 'BAL_GLB_MEMORY_IMPORT'
      EXPORTING
        i_complete_overwrite = 'X'
      EXCEPTIONS
        OTHERS               = 1.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_ballog
        EXPORTING
          msgid = sy-msgid
          msgno = sy-msgno
          msgty = sy-msgty
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4.

    ENDIF.

  ENDMETHOD.


  METHOD get_custom_fcat.

    DATA:
      lt_fcat TYPE lvc_t_fcat.

    CLEAR: lt_fcat[], rt_fcat[].

    "Le campos da estrutura
    DATA(lt_objects) = cl_abap_typedescr=>describe_by_name(
      me->s_balcont-tabname
    )->get_ddic_object( ).

    "Move para fcat do log
    rt_fcat[] = CORRESPONDING #(
      lt_objects[] MAPPING ref_table = tabname
                           ref_field = fieldname
    ).

  ENDMETHOD.


  METHOD get_probclass.

    CASE iv_type.
      WHEN 'X'.    rv_class = '1'.
      WHEN 'A'.    rv_class = '1'.
      WHEN 'E'.    rv_class = '2'.
      WHEN 'W'.    rv_class = '3'.
      WHEN OTHERS. rv_class = '4'.
    ENDCASE.

  ENDMETHOD.


  METHOD init.

    CLEAR: me->t_balhdl[], me->s_ballog, me->s_balcont.

    "Application Log: Global: Export memory
    CALL FUNCTION 'BAL_GLB_MEMORY_EXPORT'.

    "Application Log: Global: (Partially) reset memory
    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
      EXPORTING
        i_refresh_all  = 'X'
      EXCEPTIONS
        not_authorized = 1
        OTHERS         = 2.

  ENDMETHOD.


  METHOD load.

    DATA:
      lt_logheader TYPE balhdr_t,
      lr_subobject TYPE RANGE OF balhdr-subobject,
      lr_extnumber TYPE RANGE OF balhdr-extnumber,
      lr_aldate    TYPE RANGE OF balhdr-aldate.

    lr_aldate[] = VALUE #(
      ( sign = 'I' option = 'BT' low = iv_begda high = iv_endda )
    ).

    IF me->s_ballog-subobject IS NOT INITIAL.
      lr_subobject[] = VALUE #(
        ( sign = 'I' option = 'EQ' low = me->s_ballog-subobject )
      ).
    ENDIF.

    IF me->s_ballog-extnumber IS NOT INITIAL.
      lr_extnumber[] = VALUE #(
        ( sign = 'I' option = 'EQ' low = me->s_ballog-extnumber )
      ).
    ENDIF.

    SELECT *
      FROM balhdr
      INTO TABLE @lt_logheader[]
      WHERE object     = @me->s_ballog-object
        AND subobject IN @lr_subobject[]
        AND extnumber IN @lr_extnumber[]
        AND aldate    IN @lr_aldate[].

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = lt_logheader[]
      IMPORTING
        e_t_log_handle     = me->t_balhdl[]
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 0
        OTHERS             = 4.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_ballog
        EXPORTING
          msgid = sy-msgid
          msgno = sy-msgno
          msgty = sy-msgty
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4.

    ENDIF.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_add.

    IF i_logdata IS NOT SUPPLIED.
      me->log_symsg( is_addinfo ).

    ELSE.
      DATA(lo_logtype) = cl_abap_typedescr=>describe_by_data( i_logdata ).
      CASE lo_logtype->kind.
        WHEN lo_logtype->kind_elem.
          me->log_text( iv_text = i_logdata is_addinfo = is_addinfo ).

        WHEN lo_logtype->kind_struct.
          CASE lo_logtype->absolute_name+6.
            WHEN 'BAPIRET2'.
              me->log_bapiret2( is_bapiret2 = i_logdata is_addinfo = is_addinfo ).
            WHEN 'BAPIRETURN'.
              me->log_bapireturn( is_bapiret = i_logdata is_addinfo = is_addinfo ).
            WHEN 'BAPIRETURN1'.
              me->log_bapireturn1( is_bapiret1 = i_logdata is_addinfo = is_addinfo ).
            WHEN 'BDCMSGCOLL'.
              me->log_bdcmsg( is_bdcmsg = i_logdata is_addinfo = is_addinfo ).
          ENDCASE.

        WHEN lo_logtype->kind_table.
          CASE lo_logtype->absolute_name+6.
            WHEN 'BAPIRET2_TAB'.
              me->log_bapiret2_t( it_bapiret2 = i_logdata is_addinfo = is_addinfo ).
            WHEN 'ICL_T_BAPIRETURN'.
              me->log_bapireturn_t( it_bapiret = i_logdata is_addinfo = is_addinfo ).
            WHEN 'bapiret1_tab'.
              me->log_bapireturn1_t( it_bapiret1 = i_logdata is_addinfo = is_addinfo ).
            WHEN 'TAB_BDCMSGCOLL'.
              me->log_bdcmsg_t( it_bdcmsg = i_logdata is_addinfo = is_addinfo ).
          ENDCASE.

        WHEN lo_logtype->kind_class OR lo_logtype->kind_ref.
          me->log_exception( io_except = i_logdata ).

        WHEN OTHERS.
          MESSAGE e600(f1) WITH 'Tipo de log não suportado' INTO DATA(lv_dummy).
          RAISE EXCEPTION TYPE zcx_ballog
            EXPORTING
              msgid = sy-msgid
              msgno = sy-msgno
              msgty = sy-msgty
              msgv1 = sy-msgv1
              msgv2 = sy-msgv2
              msgv3 = sy-msgv3
              msgv4 = sy-msgv4.

      ENDCASE.
    ENDIF.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_bapiret2.

    DATA:
      ls_balmsg TYPE bal_s_msg.

    ls_balmsg-msgty = is_bapiret2-type.
    ls_balmsg-msgid = is_bapiret2-id.
    ls_balmsg-msgno = is_bapiret2-number.
    ls_balmsg-msgv1 = is_bapiret2-message_v1.
    ls_balmsg-msgv2 = is_bapiret2-message_v2.
    ls_balmsg-msgv3 = is_bapiret2-message_v3.
    ls_balmsg-msgv4 = is_bapiret2-message_v4.
    ls_balmsg-probclass = me->get_probclass( ls_balmsg-msgty ).

    IF is_addinfo IS SUPPLIED.
      ls_balmsg-context = me->s_balcont.
      ls_balmsg-context-value = is_addinfo.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle  = me->t_balhdl[ 1 ]
        i_s_msg       = ls_balmsg
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_bapiret2_t.

    LOOP AT it_bapiret2[] ASSIGNING FIELD-SYMBOL(<fs_ret>).
      me->log_bapiret2( is_bapiret2 = <fs_ret> is_addinfo = is_addinfo ).
    ENDLOOP.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_bapireturn.

    DATA:
      ls_bapiret2 TYPE bapiret2.

    CALL FUNCTION 'BALW_RETURN_TO_RET2'
      EXPORTING
        return_in = is_bapiret
      IMPORTING
        return_ou = ls_bapiret2.

    me->log_bapiret2( is_bapiret2 = ls_bapiret2 is_addinfo = is_addinfo ).

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_bapireturn1.

    DATA:
      ls_bapiret2 TYPE bapiret2.

    CALL FUNCTION 'BALW_RET1_TO_RET2'
      EXPORTING
        return_in = is_bapiret1
      IMPORTING
        return_ou = ls_bapiret2.

    me->log_bapiret2( is_bapiret2 = ls_bapiret2 is_addinfo = is_addinfo ).

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_bapireturn1_t.

    LOOP AT it_bapiret1[] ASSIGNING FIELD-SYMBOL(<fs_ret>).
      me->log_bapireturn1( is_bapiret1 = <fs_ret> is_addinfo = is_addinfo ).
    ENDLOOP.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_bapireturn_t.

    LOOP AT it_bapiret[] ASSIGNING FIELD-SYMBOL(<fs_ret>).
      me->log_bapireturn( is_bapiret = <fs_ret> is_addinfo = is_addinfo ).
    ENDLOOP.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_bdcmsg.

    me->log_msg(
      EXPORTING
        iv_msgid   = is_bdcmsg-msgid
        iv_msgno   = CONV #( is_bdcmsg-msgnr )
        iv_msgty   = is_bdcmsg-msgtyp
        iv_msgv1   = CONV #( is_bdcmsg-msgv1 )
        iv_msgv2   = CONV #( is_bdcmsg-msgv2 )
        iv_msgv3   = CONV #( is_bdcmsg-msgv3 )
        iv_msgv4   = CONV #( is_bdcmsg-msgv4 )
        is_addinfo = is_addinfo
    ).

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_bdcmsg_t.

    LOOP AT it_bdcmsg[] ASSIGNING FIELD-SYMBOL(<fs_bdcmsg>).
      me->log_bdcmsg( is_bdcmsg = <fs_bdcmsg> is_addinfo = is_addinfo ).
    ENDLOOP.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_exception.

    DATA:
      ls_exc TYPE bal_s_exc.

    "Preenche parametros do log
    ls_exc-msgty     = iv_type.
    ls_exc-exception = io_except.
    ls_exc-probclass = me->get_probclass( ls_exc-msgty ).

    "Insere excecao no log
    CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
      EXPORTING
        i_log_handle     = me->t_balhdl[ 1 ]
        i_s_exc          = ls_exc
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_msg.

    DATA:
      ls_balmsg TYPE bal_s_msg.

    ls_balmsg-msgty = iv_msgty.
    ls_balmsg-msgid = iv_msgid.
    ls_balmsg-msgno = iv_msgno.
    ls_balmsg-msgv1 = iv_msgv1.
    ls_balmsg-msgv2 = iv_msgv2.
    ls_balmsg-msgv3 = iv_msgv3.
    ls_balmsg-msgv4 = iv_msgv4.
    ls_balmsg-probclass = me->get_probclass( ls_balmsg-msgty ).

    IF is_addinfo IS SUPPLIED.
      ls_balmsg-context = me->s_balcont.
      ls_balmsg-context-value = is_addinfo.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle  = me->t_balhdl[ 1 ]
        i_s_msg       = ls_balmsg
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_symsg.

    me->log_msg(
      EXPORTING
        iv_msgid   = sy-msgid
        iv_msgno   = sy-msgno
        iv_msgty   = sy-msgty
        iv_msgv1   = sy-msgv1
        iv_msgv2   = sy-msgv2
        iv_msgv3   = sy-msgv3
        iv_msgv4   = sy-msgv4
        is_addinfo = is_addinfo
    ).

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD log_text.

    DATA:
      ls_context   TYPE bal_s_cont,
      lv_probclass TYPE balprobcl.

    lv_probclass = me->get_probclass( iv_type ).

    IF is_addinfo IS SUPPLIED.
      ls_context = me->s_balcont.
      ls_context-value = is_addinfo.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = me->t_balhdl[ 1 ]
        i_msgty          = iv_type
        i_text           = iv_text
        i_s_context      = ls_context
        i_probclass      = lv_probclass
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD refresh.

    CHECK me->t_balhdl[] IS NOT INITIAL.

    LOOP AT me->t_balhdl[] ASSIGNING FIELD-SYMBOL(<fs_handle>).
      DATA(lv_tabix) = sy-tabix.

      "Clear protocol (in memory)
      CALL FUNCTION 'BAL_LOG_REFRESH'
        EXPORTING
          i_log_handle  = <fs_handle>
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.

        RAISE EXCEPTION TYPE zcx_ballog
          EXPORTING
            msgid = sy-msgid
            msgno = sy-msgno
            msgty = sy-msgty
            msgv1 = sy-msgv1
            msgv2 = sy-msgv2
            msgv3 = sy-msgv3
            msgv4 = sy-msgv4.

      ENDIF.

      DELETE me->t_balhdl[] INDEX lv_tabix.
    ENDLOOP.

  ENDMETHOD.


  METHOD save.

    DATA:
      lt_loghandle TYPE bal_t_logh,
      lv_saveall   TYPE boolean VALUE abap_false.

    CASE iv_all.
      WHEN abap_true.
        CLEAR lt_loghandle[].
        lv_saveall = abap_true.

      WHEN abap_false.
        lt_loghandle[] = me->t_balhdl[].
        lv_saveall = abap_false.

    ENDCASE.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_save_all       = lv_saveall
        i_t_log_handle   = lt_loghandle[]
        i_in_update_task = iv_upd
      EXCEPTIONS
        OTHERS           = 1.
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_ballog
        EXPORTING
          msgid = sy-msgid
          msgno = sy-msgno
          msgty = sy-msgty
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4.

    ENDIF.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD set_addinfo_struc.

    "Seta estrutura customizada
    me->s_balcont-tabname = iv_ddicstruc.

    "Retorna instancia para chamada encadeada
    ro_ballog = me.

  ENDMETHOD.


  METHOD user_confirm_date.

    DATA:
      lt_fields TYPE STANDARD TABLE OF sval.

    CLEAR cv_retcd.

    lt_fields[] = VALUE #(
      ( tabname = 'PA0000' fieldname = 'BEGDA' value = cv_begda field_obl = abap_true )
      ( tabname = 'PA0000' fieldname = 'ENDDA' value = cv_endda field_obl = abap_true )
    ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Selecionar datas'
      IMPORTING
        returncode      = cv_retcd
      TABLES
        fields          = lt_fields[]
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    CHECK cv_retcd <> 'A'.

    TRY.
        cv_begda = lt_fields[ fieldname = 'BEGDA' ]-value.
        cv_endda = lt_fields[ fieldname = 'ENDDA' ]-value.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
