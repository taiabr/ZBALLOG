CLASS zcx_ballog DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    DATA v_msgid TYPE sy-msgid .
    DATA v_msgno TYPE sy-msgno .
    DATA v_msgty TYPE sy-msgty .
    DATA v_msgv1 TYPE sy-msgv1 .
    DATA v_msgv2 TYPE sy-msgv2 .
    DATA v_msgv3 TYPE sy-msgv3 .
    DATA v_msgv4 TYPE sy-msgv4 .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgid    TYPE syst-msgid OPTIONAL
        !msgno    TYPE syst-msgno OPTIONAL
        !msgty    TYPE syst-msgty OPTIONAL
        !msgv1    TYPE syst-msgv1 OPTIONAL
        !msgv2    TYPE syst-msgv2 OPTIONAL
        !msgv3    TYPE syst-msgv3 OPTIONAL
        !msgv4    TYPE syst-msgv4 OPTIONAL .
    METHODS get_symsg
      RETURNING
        VALUE(rv_text) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BALLOG IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

*      IMPORTING textid   LIKE if_t100_message=>t100key OPTIONAL
*                previous LIKE previous OPTIONAL
*                msgid    TYPE syst-msgid OPTIONAL
*                msgno    TYPE syst-msgno OPTIONAL
*                msgty    TYPE syst-msgty OPTIONAL
*                msgv1    TYPE syst-msgv1 OPTIONAL
*                msgv2    TYPE syst-msgv2 OPTIONAL
*                msgv3    TYPE syst-msgv3 OPTIONAL
*                msgv4    TYPE syst-msgv4 OPTIONAL.

    super->constructor( previous = previous ).

    me->v_msgid = msgid.
    me->v_msgty = msgty.
    me->v_msgno = msgno.
    me->v_msgv1 = msgv1.
    me->v_msgv2 = msgv2.
    me->v_msgv3 = msgv3.
    me->v_msgv4 = msgv4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.


  ENDMETHOD.


  METHOD get_symsg.

*      RETURNING VALUE(rv_text) TYPE clike.

    CLEAR rv_text.
    MESSAGE ID me->v_msgid TYPE me->v_msgty NUMBER me->v_msgno
      WITH me->v_msgv1 me->v_msgv2 me->v_msgv3 me->v_msgv4
      INTO rv_text.


  ENDMETHOD.
ENDCLASS.
