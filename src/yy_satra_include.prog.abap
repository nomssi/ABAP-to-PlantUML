*&---------------------------------------------------------------------*
*&  Include           YY_SATRA_INCLUDE
*&---------------------------------------------------------------------*
* Custom logic to generate a PlantUML sequence diagram from an
* ABAP trace recorded in transaction SAT without aggregation.
*
* Usage:
* Start transaction SAT -> Evaluate -> Double Click on a measurement;
* on the next screen press F8 / click icon to Display sequence diagram
*
* Setup:
* 1) to add the following local classes to class CL_ATRA_UML_HANDLING
*    create a source code plug-in in include LS_ABAP_TRACE_DATAD07
*    e.g. as implicit enhancement at the end:
*
*ENHANCEMENT 1  YY_SATR_SEQUENCE.    "active version
*  INCLUDE YY_SATRA_INCLUDE.
*ENDENHANCEMENT.
*
* 2) add an implicit enhancement at the beginning of Method
*    SHOW_SEQ_DIAGRAM( ) of CL_ATRA_UML_HANDLING to replace the logic:
*
*ENHANCEMENT 2  YY_SATR_SEQUENCE.    "active version
*  TRY.
*      lcl_sequence=>to_diagram( lcl_configuration=>query( ) )->output( ).
*    CATCH cx_dynamic_check INTO DATA(gx_error).
*      MESSAGE gx_error TYPE 'I' DISPLAY LIKE 'E'.  "#EC CI_USE_WANTED
*  ENDTRY.
*  RETURN.
*ENDENHANCEMENT.
*&---------------------------------------------------------------------*
* Note: Unit test execution is disabled for standard classes, but you
* can add this include to a custom program and run the tests there.
* Glossary:   module = sequence diagram actor
*             event = sequence diagram message
* Convention: Most factory methods have NEW in their name

* Valid actor type: program, function group, class, logical databases
CONSTANTS:
  c_type_class TYPE trobjtype VALUE 'CLAS',
  c_type_prog  TYPE trobjtype VALUE 'PROG',
  c_type_fugr  TYPE trobjtype VALUE 'FUGR',
  c_type_ldba  TYPE trobjtype VALUE 'LDBA',  " Log. DB
  c_type_tran  TYPE trobjtype VALUE 'TRAN'.

* Caller / Called object
TYPES: BEGIN OF ts_object,
         global   TYPE program,
         local    TYPE program,
         type     TYPE trobjtype,
         instance TYPE satr_de_instance,
       END OF ts_object.

TYPES tv_index TYPE i.        " Object index in call

TYPES: BEGIN OF ts_path,
         caller TYPE tv_index,
         called TYPE tv_index,
       END OF ts_path.

* Trace step (Message): Caller -> Called object, message name (msg)
TYPES: BEGIN OF ts_message,
         id         TYPE satr_de_id,
         from_level TYPE satr_de_ebene.
    INCLUDE TYPE ts_path AS path.
TYPES: msg TYPE seocpdname,
       END OF ts_message.
TYPES tt_message TYPE STANDARD TABLE OF ts_message WITH KEY id
           WITH NON-UNIQUE SORTED KEY key_msg COMPONENTS table_line.

INTERFACE lif_unit_test.
ENDINTERFACE.

* Message - Called object information
TYPES:  BEGIN OF ts_call.
    INCLUDE TYPE ts_object AS object.
TYPES: mod TYPE seocpdname,
       END OF ts_call.

TYPES: BEGIN OF ts_seq,
         called    TYPE ts_call,   " Called fields
         aus_tabix TYPE i,         " Index for Table Access
       END OF ts_seq.

* Target structure for call hierarchy used in sequence diagrams, with the fields
*   ID - indicator describes the type of the recorded event
*        (call method, perform, call function, call screen, database operation,#)
*   from_level - The call level in the call hierarchy (an integer)
*   caller - source of the message (program, function group, class, etc.)
*   called - receiver of the message
*   system - system flag " only used in custom filter
TYPES: BEGIN OF ts_sat,
         id         TYPE satr_de_id,     " General Information.
         from_level TYPE satr_de_ebene,
         caller     TYPE ts_call.        " Caller fields
    INCLUDE TYPE ts_seq AS seq.
TYPES: system TYPE satr_de_sysflag,
       END OF ts_sat.

TYPES tv_text TYPE char128.
* Each actor (sender or receiver of a message) in the sequence diagram has a life line
TYPES: BEGIN OF ts_lifeline,
         index TYPE tv_index,    " secondary index in actor's table
         label TYPE tv_text,
       END OF ts_lifeline.

INTERFACE lif_actors.
  TYPES tt_lifeline TYPE SORTED TABLE OF ts_lifeline WITH UNIQUE KEY index.

  METHODS lifelines RETURNING VALUE(rt_lifeline) TYPE tt_lifeline.
  METHODS short_text IMPORTING is_message     TYPE ts_message
                     RETURNING VALUE(rv_text) TYPE tv_text.
ENDINTERFACE.

INTERFACE lif_path.
  METHODS to_path IMPORTING is_sat         TYPE ts_sat
                  RETURNING VALUE(rs_path) TYPE ts_path.
ENDINTERFACE.

TYPES tv_name TYPE char30.

* Abstract UML sequence diagram code generator - buffers the UML text
* representation and pass the complete string back to the caller
CLASS lcl_uml DEFINITION ABSTRACT FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES tv_uml TYPE i.
    CONSTANTS:
      c_uml_plant  TYPE tv_uml VALUE 0,
      c_uml_graph  TYPE tv_uml VALUE 2,
      c_uml_mscgen TYPE tv_uml VALUE 3,
      c_uml_mock   TYPE tv_uml VALUE 9.

    CLASS-METHODS new IMPORTING iv_uml        TYPE tv_uml
                                ii_actors     TYPE REF TO lif_actors
                      RETURNING VALUE(ro_uml) TYPE REF TO lcl_uml.
    METHODS top.
    METHODS bottom RETURNING VALUE(rv_diagram) TYPE string.
    "! Draws a message between two objects, with the given label.
    "! Self messages (where an object sends a message to itself) are supported.
    METHODS message IMPORTING is_message TYPE ts_message.
    METHODS return ABSTRACT IMPORTING iv_from TYPE i
                                      iv_to   TYPE i.
    METHODS begin_loop ABSTRACT IMPORTING iv_from  TYPE program OPTIONAL
                                          iv_times TYPE sytabix
                                          iv_name  TYPE tv_name OPTIONAL.
    METHODS end_loop ABSTRACT IMPORTING iv_to   TYPE program OPTIONAL
                                        iv_name TYPE tv_name OPTIONAL.
  PROTECTED SECTION.
    CONSTANTS c_txt_std TYPE string VALUE 'Standard SAP code has called some custom code ' ##NO_TEXT.
    DATA mv_diagram TYPE string.
    DATA mi_actors TYPE REF TO lif_actors.

    METHODS add IMPORTING iv_code TYPE string.

    METHODS call ABSTRACT IMPORTING is_message TYPE ts_message.
    METHODS skip_note ABSTRACT IMPORTING is_path TYPE ts_path.
    METHODS header ABSTRACT.
    METHODS footer ABSTRACT.
    "!  Defines an object is_lifeline-index, labeled on the diagram as is_lifeline-text
    METHODS participant ABSTRACT IMPORTING is_lifeline TYPE ts_lifeline
                                           iv_create   TYPE xsdboolean DEFAULT abap_false.
    "!  Object lifeline completion
    METHODS complete IMPORTING is_lifeline TYPE ts_lifeline.
    METHODS separator.  " between two participants
    METHODS delimiter.  " end of statement
ENDCLASS.

CLASS lcl_stack DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_call_level,
        actor_key  TYPE tv_index,
        from_level TYPE satr_de_ebene,
      END OF ts_call_level.

    METHODS push IMPORTING is_ref TYPE ts_call_level.
    METHODS pop RETURNING VALUE(rs_to) TYPE ts_call_level.
  PROTECTED SECTION.
    TYPES BEGIN OF ts_level.
    INCLUDE TYPE ts_call_level AS call_level.
    TYPES next TYPE REF TO data.
    TYPES END OF ts_level.

    DATA mr_top TYPE REF TO ts_level.

    METHODS has_next RETURNING VALUE(rv_flag) TYPE xsdboolean.
ENDCLASS.

CLASS lcl_uml_stack DEFINITION INHERITING FROM lcl_stack FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_uml TYPE REF TO lcl_uml.
    METHODS call IMPORTING is_message TYPE ts_message.
    METHODS return IMPORTING iv_to_level TYPE satr_de_ebene.
  PROTECTED SECTION.
    DATA mv_previous_level TYPE satr_de_ebene.
    DATA mo_uml TYPE REF TO lcl_uml.

    METHODS return_to IMPORTING iv_level TYPE satr_de_ebene.
ENDCLASS.

* Trace cycle description
TYPES: BEGIN OF ts_cycle,
         start TYPE sytabix,
         end   TYPE sytabix,
         last  TYPE sytabix,
         times TYPE i,
       END OF ts_cycle.

* iterator over an internal table of messages allowing to
* - skip a given number of entries (default 1).
* - look ahead to read the next value without updating the index
* - is_first to known if the current values is the first
CLASS lcl_messages DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS c_default_level TYPE satr_de_ebene VALUE 1.

    METHODS constructor IMPORTING it_messages TYPE tt_message.
    "! traverses the list of messages list and generates an "UML as text" diagram
    METHODS to_uml IMPORTING io_uml TYPE REF TO lcl_uml.
  PROTECTED SECTION.
    CONSTANTS c_first_key TYPE tv_index VALUE 1.   " Index of object in very first call
    DATA mv_first_level TYPE i.

    DATA mv_idx TYPE sytabix.
    DATA mv_size TYPE sytabix.
    DATA mt_list TYPE tt_message.

    METHODS message IMPORTING is_message TYPE ts_message
                              iv_idx     TYPE sytabix
                              io_stack   TYPE REF TO lcl_uml_stack.

    METHODS is_first RETURNING VALUE(rv_flag) TYPE xsdboolean.
    METHODS has_next RETURNING VALUE(rv_flag) TYPE xsdboolean.
    METHODS skip IMPORTING iv_count TYPE i DEFAULT 1.
    METHODS next RETURNING VALUE(rs_data) TYPE ts_message
                 RAISING   cx_sy_itab_error.
    METHODS first_message IMPORTING io_stack TYPE REF TO lcl_uml_stack.
    METHODS next_level RETURNING VALUE(rv_level) TYPE satr_de_ebene.
ENDCLASS.

CLASS lcl_messages_compact DEFINITION INHERITING FROM lcl_messages FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES tt_cycle TYPE SORTED TABLE OF ts_cycle WITH NON-UNIQUE KEY start.
    METHODS constructor IMPORTING it_messages TYPE tt_message
*                                  iv_start    TYPE sytabix DEFAULT 1
*                                  iv_stop     TYPE sytabix OPTIONAL
                                  it_cycles   TYPE tt_cycle
                                  io_uml      TYPE REF TO lcl_uml.
  PROTECTED SECTION.
    DATA mt_cycle TYPE tt_cycle.
    DATA mo_uml TYPE REF TO lcl_uml.

    METHODS message REDEFINITION.
    METHODS begin IMPORTING iv_idx  TYPE sytabix
                            iv_from TYPE tv_index.
    METHODS end IMPORTING iv_idx         TYPE sytabix
                          iv_to          TYPE tv_index
                RETURNING VALUE(rv_step) TYPE sytabix.
  PRIVATE SECTION.
    METHODS name IMPORTING iv_start       TYPE sytabix
                           iv_end         TYPE sytabix
                 RETURNING VALUE(rv_name) TYPE tv_name.
ENDCLASS.

CLASS lcl_actors DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_actors.
    INTERFACES lif_path.
  PROTECTED SECTION.
    DATA mv_last_number TYPE i.

    TYPES:  BEGIN OF ts_actor.
        INCLUDE TYPE ts_object AS object.
        INCLUDE TYPE ts_lifeline AS lifeline.
    TYPES:  END OF ts_actor.

    TYPES tt_actor TYPE SORTED TABLE OF ts_actor
      WITH UNIQUE KEY type instance global local
      WITH UNIQUE SORTED KEY obj_nr COMPONENTS index.
    DATA mt_actor TYPE tt_actor.

    METHODS lifeline IMPORTING is_object      TYPE ts_object
                     RETURNING VALUE(rv_text) TYPE tv_text.
    METHODS get_object_text IMPORTING iv_instance    TYPE satr_de_instance
                            RETURNING VALUE(rv_text) TYPE tv_text.
    METHODS get_function_group IMPORTING iv_program     TYPE program
                               RETURNING VALUE(rv_text) TYPE tv_text.
    METHODS class_name IMPORTING iv_index             TYPE sytabix
                       RETURNING VALUE(rv_short_name) TYPE tv_text.
    METHODS put IMPORTING is_object       TYPE ts_object
                RETURNING VALUE(rv_index) TYPE tv_index.
  PRIVATE SECTION.
    METHODS search IMPORTING is_object     TYPE ts_object
                   RETURNING VALUE(rv_idx) TYPE sytabix.
    METHODS new_object IMPORTING is_object       TYPE ts_object
                       RETURNING VALUE(rv_index) TYPE tv_index.
ENDCLASS.

TYPES tv_scale TYPE perct.
CONSTANTS c_default_scale TYPE tv_scale VALUE '0.5'.
TYPES: BEGIN OF ts_diagram_config,
         local_path        TYPE string,
         java_jar          TYPE string,
         java_appl         TYPE string,
         server_url        TYPE string,
         output_mode       TYPE char01,
         skip_dialog       TYPE flag,
         compact_trace     TYPE flag,
         system_events     TYPE flag,
         scale             TYPE tv_scale,
         pattern           TYPE sci_pattern,
         handwritten       TYPE flag,
         shadowing         TYPE flag,
         display_source    TYPE flag,
         teoz_architecture TYPE flag,
         uml_format        TYPE lcl_uml=>tv_uml,
         progress          TYPE flag,
         browser_size      TYPE string,
       END OF ts_diagram_config.

CLASS lcl_diagram_text DEFINITION ABSTRACT FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_aut TYPE char01 VALUE 'T',  " for ABAP Unit Test
      c_mode_url TYPE char01 VALUE 'U',
      c_mode_txt TYPE char01 VALUE space,
      c_mode_exe TYPE char01 VALUE 'E'.

    CLASS-METHODS new IMPORTING is_cfg            TYPE ts_diagram_config
                                iv_text           TYPE string
                      RETURNING VALUE(ro_diagram) TYPE REF TO lcl_diagram_text.
    METHODS constructor IMPORTING iv_diagram TYPE string
                                  iv_mode    TYPE char01.
    METHODS output  RAISING cx_dynamic_check.
  PROTECTED SECTION.
    DATA mv_diagram TYPE string.
    DATA ms_cfg TYPE ts_diagram_config.

    METHODS save_file IMPORTING iv_mode         TYPE char01
                      RETURNING VALUE(rv_fname) TYPE string
                      RAISING   cx_dynamic_check.
    METHODS to_xstring IMPORTING iv_string         TYPE string
                       RETURNING VALUE(rv_xstring) TYPE xstring
                       RAISING   cx_dynamic_check.
ENDCLASS.

INTERFACE lif_trace_filter.
  METHODS accepts IMPORTING is_sat         TYPE ts_sat
                  RETURNING VALUE(rv_flag) TYPE xsdboolean.
ENDINTERFACE.

CLASS lcl_file_name DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING iv_mode        TYPE char01
                      RETURNING VALUE(ro_file) TYPE REF TO lcl_file_name.
    METHODS constructor IMPORTING iv_mode TYPE char01.
    METHODS dialog RETURNING VALUE(rv_user_action) TYPE i.
    METHODS get_prefix RETURNING VALUE(rv_name) TYPE string
                       RAISING   cx_dynamic_check.
    METHODS get_fullpath RETURNING VALUE(rv_name) TYPE string.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_fullpath,
             title TYPE string,
             name  TYPE string,
             ext   TYPE string,
             path  TYPE string,
           END OF ts_fullpath.
    DATA ms_file TYPE ts_fullpath.
ENDCLASS.

CLASS lcl_file_name_test DEFINITION INHERITING FROM lcl_file_name FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS dialog REDEFINITION.
ENDCLASS.

CLASS lcl_file DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING iv_data         TYPE xstring
                io_name         TYPE REF TO lcl_file_name
      RETURNING VALUE(rv_subrc) TYPE sysubrc.
ENDCLASS.

CLASS lcl_diagram_plant_uml DEFINITION INHERITING FROM lcl_diagram_text FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING is_cfg     TYPE ts_diagram_config
                                  iv_diagram TYPE string.
    METHODS output REDEFINITION.
    CLASS-METHODS get_java_path RETURNING VALUE(rv_path) TYPE string.
  PROTECTED SECTION.
    TYPES tv_base64 TYPE c LENGTH 65.
    CONSTANTS:
      c_charset_standard TYPE tv_base64 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=' ##NO_TEXT,
      c_charset_plantuml TYPE tv_base64 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_0' ##NO_TEXT.

    METHODS to_url IMPORTING iv_base_url   TYPE string
                   RETURNING VALUE(rv_url) TYPE string
                   RAISING   cx_dynamic_check.
    METHODS to_png IMPORTING io_name        TYPE REF TO lcl_file_name
                   RETURNING VALUE(rv_name) TYPE string.
    METHODS display_html RAISING cx_dynamic_check.
    METHODS local_plant_uml RAISING cx_dynamic_check.
    METHODS parameter_string IMPORTING io_name         TYPE REF TO lcl_file_name
                             RETURNING VALUE(rv_param) TYPE string.
    METHODS png_file_name IMPORTING io_name        TYPE REF TO lcl_file_name
                          RETURNING VALUE(rv_name) TYPE string.
    METHODS encoded_url_suffix RETURNING VALUE(rv_url) TYPE string
                               RAISING   cx_dynamic_check.
    METHODS source RETURNING VALUE(rv_source) TYPE string.

    METHODS show_html IMPORTING iv_html TYPE string
                      RAISING   cx_dynamic_check.
  PRIVATE SECTION.
    CLASS-METHODS registry_local IMPORTING iv_key        TYPE string
                                           iv_value      TYPE string
                                 RETURNING VALUE(rv_reg) TYPE string
                                 RAISING   cx_dynamic_check.
ENDCLASS.

CLASS lcl_configuration DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS:
      get RETURNING VALUE(rs_cfg) TYPE ts_diagram_config,
      query RETURNING VALUE(rs_cfg) TYPE ts_diagram_config,
      class_constructor.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_param,
             local_path        TYPE localfile,
             java_jar          TYPE localfile,
             java_appl         TYPE localfile,
             server_url        TYPE localfile,
             output_mode       TYPE char01,
             skip_dialog       TYPE flag,
             compact_trace     TYPE flag,
             system_events     TYPE flag,
             scale             TYPE perct,
             handwritten       TYPE flag,
             shadowing         TYPE flag,
             teoz_architecture TYPE flag,
             display_source    TYPE flag,
             pattern           TYPE sci_pattern,
             uml_format        TYPE int4,
             progress          TYPE flag,
             browser_size      TYPE string,
           END OF ts_param.
    METHODS get_attributes RETURNING VALUE(rt_attr) TYPE sci_atttab.
    METHODS to_radiobutton.
    METHODS from_radiobutton.
    CLASS-DATA gs_cfg TYPE ts_param.
    DATA: mv_mode_url TYPE flag VALUE 'X',
          mv_mode_exe TYPE flag,
          mv_mode_txt TYPE flag.
    METHODS dialog.
ENDCLASS.

CLASS lcl_filter DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_trace_filter.
    ALIASES accepts FOR lif_trace_filter~accepts.
    CLASS-METHODS new IMPORTING is_cfg           TYPE ts_diagram_config
                      RETURNING VALUE(ri_filter) TYPE REF TO lif_trace_filter.
    METHODS constructor IMPORTING is_cfg TYPE ts_diagram_config.
  PROTECTED SECTION.
    DATA ms_cfg TYPE ts_diagram_config.
    DATA mv_config_id_reject TYPE string.
ENDCLASS.

CLASS lcl_filter_null DEFINITION INHERITING FROM lcl_filter FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS accepts REDEFINITION.
ENDCLASS.

CLASS lcl_filter_custom DEFINITION INHERITING FROM lcl_filter FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING is_cfg TYPE ts_diagram_config.
    METHODS accepts REDEFINITION.
  PRIVATE SECTION.
    DATA mv_regex TYPE string.

    METHODS is_custom_code IMPORTING iv_name        TYPE program
                           RETURNING VALUE(rv_flag) TYPE xsdboolean.
ENDCLASS.

CLASS lcl_bag DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS add IMPORTING iv_entry TYPE sytabix.
    METHODS remove IMPORTING iv_entry TYPE sytabix.
    METHODS contains IMPORTING iv_entry       TYPE sytabix
                     RETURNING VALUE(rv_flag) TYPE xsdboolean.
  PRIVATE SECTION.
    DATA mt_index TYPE SORTED TABLE OF sytabix WITH UNIQUE DEFAULT KEY.
ENDCLASS.

"! Class information
CLASS lcl_class_name DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS:
      is_global IMPORTING iv_name        TYPE sobj_name
                RETURNING VALUE(rv_flag) TYPE xsdboolean,
      external CHANGING cv_clas TYPE program,
      technical IMPORTING iv_name         TYPE program
                RETURNING VALUE(rv_class) TYPE program,
      to_internal IMPORTING iv_name         TYPE program
                  RETURNING VALUE(rv_class) TYPE program.
  PRIVATE SECTION.
*   Cache proxy for method IS_GLOBAL( )
    TYPES: BEGIN OF ts_class,
             name   TYPE sobj_name,
             global TYPE flag,
           END OF ts_class.
    TYPES tt_class TYPE HASHED TABLE OF ts_class WITH UNIQUE KEY name.
    DATA mt_class TYPE tt_class.

    METHODS fetch_is_global IMPORTING iv_name        TYPE sobj_name
                            RETURNING VALUE(rv_flag) TYPE xsdboolean.
ENDCLASS.

*----------------------------------------------------------------------*
"! Parse call hierarchy in ABAP trace stored as static attributes of
"! standard class CL_ATRA_TOOL_SE30_MAIN, extract trace details needed
"! for UML sequence diagram generation from internal tables
"!      IT_TRACEPROG - Table of Recorded Program Names
"!      IT_TRACETEXT - Table of TRACE Text Elements
"!      IT_TRACEMETH - Table of Recorded Method Names
"!      IT_AUSTAB_HIER - Call Hierarchy table with All TRACE Information
CLASS lcl_abap_trace DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS c_create_method TYPE seocpdname VALUE 'CONSTRUCTOR'.

    METHODS constructor.
    METHODS parse RETURNING VALUE(rv_flag) TYPE xsdboolean.
  PROTECTED SECTION.
    TYPES:  BEGIN OF ts_event.
        INCLUDE TYPE ts_object AS object.
    TYPES: event  TYPE seocpdname,                " Called component of object
           system TYPE satr_de_sysflag,
           END OF ts_event.

    DATA mo_atra TYPE REF TO cl_atra_tool_se30_main.
    DATA mr_src TYPE REF TO satr_austab_gesamt.
    DATA ms_target TYPE ts_event.                " Called actor
    DATA mo_class_name TYPE REF TO lcl_class_name.

    METHODS module IMPORTING iv_index        TYPE satr_de_pgix
                   RETURNING VALUE(rv_subrc) TYPE sysubrc.
    METHODS event RETURNING VALUE(rv_subrc) TYPE sysubrc.
*   the following methods are redefined in test double
    METHODS set_constructor.
    METHODS set_meth.
    METHODS set_form.
    METHODS set_function_module.

    METHODS set_submit.
    METHODS set_tran.
  PRIVATE SECTION.
    METHODS set_local_class IMPORTING iv_progname  TYPE program.
ENDCLASS.

INTERFACE lif_collector.
  "!  Add trace entry to collection
  METHODS collect IMPORTING is_sat         TYPE ts_sat
                  RETURNING VALUE(rv_flag) TYPE xsdboolean.
ENDINTERFACE.

CLASS lcl_progress_indicator DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS: c_enable  TYPE char01 VALUE 'X',
               c_disable TYPE char01 VALUE '0'.
    CLASS-METHODS new IMPORTING is_cfg         TYPE ts_diagram_config OPTIONAL
                      RETURNING VALUE(ro_prog) TYPE REF TO lcl_progress_indicator.
    CLASS-METHODS echo IMPORTING iv_percentage TYPE numeric DEFAULT 0
                                 iv_text       TYPE clike.
    METHODS set_mode IMPORTING iv_mode TYPE char01.
    METHODS restore.
  PRIVATE SECTION.
    CONSTANTS c_param_id_sin TYPE char20 VALUE 'SIN'.
    DATA mv_mode TYPE char01.

    METHODS constructor IMPORTING iv_progress TYPE flag.
    METHODS get_mode RETURNING VALUE(rv_mode) TYPE char01.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_trace DEFINITION                                     *
*----------------------------------------------------------------------*
CLASS lcl_trace DEFINITION INHERITING FROM lcl_abap_trace FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_progress TYPE REF TO lcl_progress_indicator.
    METHODS fill IMPORTING ii_collector TYPE REF TO lif_collector.
  PROTECTED SECTION.
    CONSTANTS c_event_entry TYPE satr_de_event VALUE '>'.
    TYPES tt_seq_k1 TYPE STANDARD TABLE OF ts_seq WITH KEY aus_tabix  " Performance
                    WITH NON-UNIQUE SORTED KEY k1 COMPONENTS aus_tabix.
    DATA mt_caller TYPE tt_seq_k1.
  PRIVATE SECTION.
    DATA mo_bag TYPE REF TO lcl_bag.
    DATA mi_collector TYPE REF TO lif_collector.

    METHODS skip_record.
    METHODS get_caller RETURNING VALUE(rs_sat) TYPE ts_sat.
    METHODS put_caller IMPORTING is_sat TYPE ts_sat.
    METHODS new_record RETURNING VALUE(rv_flag) TYPE xsdboolean.
ENDCLASS.

*-----------------------------------------------------------------------------------------*

INTERFACE lif_cycles.
  METHODS collect IMPORTING is_cycle TYPE ts_cycle.
  METHODS is_canonical_index IMPORTING iv_index       TYPE sytabix
                             RETURNING VALUE(rv_flag) TYPE xsdboolean.
ENDINTERFACE.

CLASS lcl_message_index DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
*   to benefit from the performance of the FIND IN TABLE statement
*   we implement a poor man's table index management here
    TYPES tv_x_tabix TYPE x LENGTH 4.
    TYPES tt_x_index TYPE STANDARD TABLE OF tv_x_tabix.

    METHODS constructor IMPORTING it_trace    TYPE tt_message
                                  ii_cycles   TYPE REF TO lif_cycles
                                  io_progress TYPE REF TO lcl_progress_indicator.
    METHODS to_trace CHANGING ct_trace TYPE tt_message.
    METHODS shrink IMPORTING iv_pass          TYPE syindex
                   RETURNING VALUE(rv_shrink) TYPE xsdboolean.
  PROTECTED SECTION.
    TYPES tt_components TYPE SORTED TABLE OF ts_message
      WITH UNIQUE KEY id from_level caller called msg.

    METHODS to_index.
    METHODS compact CHANGING  ct_xindex        TYPE tt_x_index
                    RETURNING VALUE(rv_shrink) TYPE xsdboolean.
  PRIVATE SECTION.
    DATA mt_trace TYPE tt_message.
    DATA mi_cycles TYPE REF TO lif_cycles.

    DATA mt_components TYPE tt_components.
    DATA mt_xindex TYPE tt_x_index.
ENDCLASS.

CLASS lcl_sequence DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_collector.

    CLASS-METHODS to_diagram IMPORTING is_cfg            TYPE ts_diagram_config
                             RETURNING VALUE(ro_diagram) TYPE REF TO lcl_diagram_text.
  PROTECTED SECTION.
    DATA mo_uml TYPE REF TO lcl_uml.
    DATA mv_compact_trace TYPE flag.

    METHODS to_uml RETURNING VALUE(rv_diagram) TYPE string.
  PRIVATE SECTION.
    DATA mi_filter TYPE REF TO lif_trace_filter.
    DATA mi_path TYPE REF TO lif_path.
    DATA mo_progress TYPE REF TO lcl_progress_indicator.

    DATA mt_trace TYPE tt_message.
    DATA ms_previous TYPE ts_sat.

    METHODS constructor IMPORTING is_cfg TYPE ts_diagram_config.
    METHODS add IMPORTING is_sat TYPE ts_sat.
    METHODS link_to_previous IMPORTING is_sat        TYPE ts_sat
                             RETURNING VALUE(rs_sat) TYPE ts_sat.
ENDCLASS.

CLASS lcl_uml_factory DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_cycles.

    METHODS constructor IMPORTING io_progress TYPE REF TO lcl_progress_indicator.
    CLASS-METHODS new_messages
      IMPORTING iv_compact_trace   TYPE xsdboolean
                it_trace           TYPE tt_message
                io_uml             TYPE REF TO lcl_uml
                io_progress        TYPE REF TO lcl_progress_indicator
      EXPORTING et_cycles          TYPE lcl_messages_compact=>tt_cycle
      RETURNING VALUE(ro_messages) TYPE REF TO lcl_messages.
  PROTECTED SECTION.
    DATA mt_cycles TYPE lcl_messages_compact=>tt_cycle.
    DATA mo_progress TYPE REF TO lcl_progress_indicator.

    METHODS new_iterator IMPORTING it_messages        TYPE tt_message
                                   io_uml             TYPE REF TO lcl_uml
                         RETURNING VALUE(ro_messages) TYPE REF TO lcl_messages.
    METHODS fold IMPORTING it_trace    TYPE tt_message
                 EXPORTING et_messages TYPE tt_message.
ENDCLASS.

CLASS lcl_cycles DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS shrink CHANGING ct_cycles TYPE lcl_messages_compact=>tt_cycle.
  PRIVATE SECTION.
    DATA mv_tabix TYPE sytabix.
    DATA mt_cycles TYPE lcl_messages_compact=>tt_cycle.
    DATA ms_stop TYPE ts_cycle.

    METHODS constructor IMPORTING it_cycles TYPE lcl_messages_compact=>tt_cycle.
    METHODS compress RETURNING VALUE(ro_cycles) TYPE REF TO lcl_cycles.
ENDCLASS.

"! Class LCL_PATTERN finds repetitions (cycles/loops) in the internal table.
"! Method CLONE( ) generates new iterators starting at the next repeated
"! entry in the given trace table and recursively checks within the match.
"! When a loop is found, an entry with
"!    the index of the starting entry of the repeated pattern  (e.g. 1)
"!    the index of the end entry of the repetition (e.g. 4)
"!    the number of loop counts (e.g. 3)
"!    the index of the last entry when the loop is exited  (e.g. 12)
"! is stored using the COLLECT( ) method of interface LIF_CYCLES.
CLASS lcl_pattern DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING it_xindex TYPE lcl_message_index=>tt_x_index
                                  iv_start  TYPE sytabix DEFAULT 1
                                  iv_stop   TYPE sytabix OPTIONAL.
    METHODS has_next RETURNING VALUE(rv_flag) TYPE xsdboolean.
    METHODS skip IMPORTING iv_count TYPE i DEFAULT 1.

    METHODS detect_cycles.

    CLASS-METHODS new IMPORTING it_xindex         TYPE lcl_message_index=>tt_x_index
                                iv_start          TYPE sytabix DEFAULT 1
                                iv_stop           TYPE sytabix OPTIONAL
                                ii_cycles         TYPE REF TO lif_cycles
                      RETURNING VALUE(ro_pattern) TYPE REF TO lcl_pattern.
  PROTECTED SECTION.
    CONSTANTS c_max_step TYPE i VALUE 1024.
    DATA mi_cycles TYPE REF TO lif_cycles.

    METHODS clone IMPORTING iv_start       TYPE sytabix DEFAULT 1
                            iv_stop        TYPE sytabix OPTIONAL
                              PREFERRED PARAMETER iv_start
                  RETURNING VALUE(ro_loop) TYPE REF TO lcl_pattern.
    METHODS next_occurrence_from IMPORTING iv_from       TYPE sytabix
                                 RETURNING VALUE(rv_row) TYPE sytabix.
    METHODS occurs_in IMPORTING iv_start       TYPE sytabix
                                iv_end         TYPE sytabix
                      RETURNING VALUE(rv_same) TYPE xsdboolean.
*   Condense loop logic
    METHODS match_pattern IMPORTING iv_end          TYPE sytabix
                          RETURNING VALUE(rv_match) TYPE xsdboolean.
  PRIVATE SECTION.
    DATA mv_idx TYPE sytabix.
    DATA mv_size TYPE sytabix.
    DATA mt_xindex TYPE lcl_message_index=>tt_x_index.
ENDCLASS.

* PlantUML code generator -
CLASS lcl_uml_plant DEFINITION INHERITING FROM lcl_uml FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS return REDEFINITION.
    METHODS begin_loop REDEFINITION.
    METHODS end_loop REDEFINITION.
  PROTECTED SECTION.
    CONSTANTS c_min_scale TYPE tv_scale VALUE '0.1'.
    METHODS call REDEFINITION.
    METHODS parameters IMPORTING is_cfg          TYPE ts_diagram_config
                       RETURNING VALUE(rv_param) TYPE string.
    METHODS participant REDEFINITION.
    METHODS header REDEFINITION.
    METHODS footer REDEFINITION.
    METHODS skip_note REDEFINITION.
  PRIVATE SECTION.
    DATA mv_param TYPE string.
    METHODS add_param IMPORTING iv_cond    TYPE xsdboolean DEFAULT abap_true
                                iv_command TYPE string.
    METHODS scale IMPORTING is_cfg          TYPE ts_diagram_config
                  RETURNING VALUE(rv_scale) TYPE string.
ENDCLASS.

* UMLGraph code generator -
CLASS lcl_uml_graph DEFINITION INHERITING FROM lcl_uml FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS return REDEFINITION.
    METHODS begin_loop REDEFINITION.
    METHODS end_loop REDEFINITION.
    METHODS top REDEFINITION.
  PROTECTED SECTION.
    METHODS call REDEFINITION.
    METHODS participant REDEFINITION.
    METHODS header REDEFINITION.
    METHODS complete REDEFINITION.
    METHODS footer REDEFINITION.
    METHODS skip_note REDEFINITION.
ENDCLASS.

CLASS lcl_uml_mscgen DEFINITION INHERITING FROM lcl_uml FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS return REDEFINITION.
    METHODS begin_loop REDEFINITION.
    METHODS end_loop REDEFINITION.
  PROTECTED SECTION.
    METHODS call REDEFINITION.
    METHODS header REDEFINITION.
    METHODS footer REDEFINITION.
    METHODS participant REDEFINITION.
    METHODS skip_note REDEFINITION.

    METHODS separator REDEFINITION.
    METHODS delimiter REDEFINITION.
ENDCLASS.

CLASS lcl_logger DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_log,
             method TYPE char30,
             params TYPE string,
           END OF ts_log.
    TYPES tt_log TYPE STANDARD TABLE OF ts_log WITH EMPTY KEY.

    CLASS-METHODS new RETURNING VALUE(ro_log) TYPE REF TO lcl_logger.
    METHODS info IMPORTING is_log TYPE ts_log.
    METHODS verify IMPORTING it_exp TYPE tt_log
                             iv_msg TYPE string.
  PROTECTED SECTION.
    DATA mt_log TYPE tt_log.
ENDCLASS.

* Mock UML object
CLASS lcl_uml_logger DEFINITION INHERITING FROM lcl_uml FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS return REDEFINITION.
    METHODS begin_loop REDEFINITION.
    METHODS end_loop REDEFINITION.
  PROTECTED SECTION.
    METHODS call REDEFINITION.
    METHODS participant REDEFINITION.
    METHODS header REDEFINITION.
    METHODS footer REDEFINITION.
    METHODS skip_note REDEFINITION.
  PRIVATE SECTION.
    DATA mo_log TYPE REF TO lcl_logger.

    METHODS log IMPORTING iv_method TYPE char30
                          iv_params TYPE string OPTIONAL.
ENDCLASS.

*--------------------------------------------------------------------------------*
* Object Identifier in a call
CONSTANTS:
  c_id_func    TYPE satr_de_id VALUE 'U',  " Function module
  c_id_form    TYPE satr_de_id VALUE 'F',
  c_id_class   TYPE satr_de_id VALUE 'R',  " Create object´/ Run time management
  c_id_method  TYPE satr_de_id VALUE 'm',
  c_id_record  TYPE satr_de_id VALUE 'O',  " Record of measurement
  c_id_program TYPE satr_de_id VALUE 'P',  " Program
  c_id_skip    TYPE satr_de_id VALUE 'K'.  " skip over SAP code
CONSTANTS:
  c_id_load   TYPE satr_de_id VALUE 'L',  " Load
  c_id_module TYPE satr_de_id VALUE 'M',  " Module call
  c_id_report TYPE satr_de_id VALUE 'C',  " CALL screen/report
  c_id_dynpro TYPE satr_de_id VALUE 'A',  " PBO / PAI
  c_id_flow   TYPE satr_de_id VALUE 'E'.  " Flow Logic
CONSTANTS:  " to be skipped
  c_id_title   TYPE satr_de_id VALUE 'Z',  " Dyn. Assign, Status/Title, SCAN, SET Local
  c_id_message TYPE satr_de_id VALUE 'W',  " Message/Continue/Wait
  c_id_db_oper TYPE satr_de_id VALUE 'V',  "  Database Operation

  c_id_sap_sql TYPE satr_de_id VALUE 'S',  " Open SQL
  c_id_db_sql  TYPE satr_de_id VALUE 'Q',  " Native SQL
  c_id_import  TYPE satr_de_id VALUE 'X',  " Export/Import
  c_id_db_tech TYPE satr_de_id VALUE 'N'.  " Buffer/Technical DB Operation

CLASS lcl_bag IMPLEMENTATION.

  METHOD add.
    INSERT iv_entry INTO TABLE mt_index.
  ENDMETHOD.

  METHOD remove.
    DELETE TABLE mt_index FROM iv_entry.
  ENDMETHOD.

  METHOD contains.
    rv_flag = xsdbool( line_exists( mt_index[ table_line = iv_entry ] ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_filter_null IMPLEMENTATION.

  METHOD accepts.
    rv_flag = abap_true.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_filter IMPLEMENTATION.

  METHOD constructor.
    ms_cfg = is_cfg.
    mv_config_id_reject = c_id_title " Z - Dyn. Assign, Status/Title, SCAN, SET Local
       && c_id_message         " W - Message/Continue/Wait
       && c_id_db_oper         " V - Database Operation
       && c_id_sap_sql         " S - Open SQL
       && c_id_db_sql          " Q - Native SQL
       && c_id_import          " X - Export/Import
       && c_id_db_tech         " N - Buffer/Technical DB Operation
       && c_id_program         " P - Program start point
       && c_id_record          " O - record of measurement
       && ` `.                 " space
  ENDMETHOD.

  METHOD new.
    IF is_cfg IS INITIAL.
      ri_filter = NEW lcl_filter_null( is_cfg ).
    ELSEIF is_cfg-pattern IS INITIAL.
      ri_filter = NEW lcl_filter( is_cfg ).
    ELSE.
      ri_filter = NEW lcl_filter_custom( is_cfg ).
    ENDIF.
  ENDMETHOD.

  METHOD accepts.
    CONSTANTS c_system_flag TYPE satr_de_sysflag VALUE 'S'.

    IF is_sat-id CA mv_config_id_reject.
      rv_flag = abap_false.
    ELSEIF ms_cfg-system_events EQ abap_false.
      rv_flag = xsdbool( is_sat-system NE c_system_flag ).
    ELSE.
      rv_flag = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_filter_custom IMPLEMENTATION.

  METHOD constructor.
    super->constructor( is_cfg ).
    mv_regex = concat_lines_of( table = ms_cfg-pattern sep = '|' ).
  ENDMETHOD.

  METHOD is_custom_code. " is object in customer name space ?
    TRY.
        rv_flag = xsdbool( mv_regex IS INITIAL OR 0 EQ numofchar( iv_name ) OR find_end( val = iv_name regex = mv_regex ) GT 0 ).
      CATCH cx_dynamic_check INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'E' DISPLAY LIKE 'I'.
        rv_flag = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD accepts.
*   Filter logic for custom code:
*      If called object is global, it must be custom code
*      If called object is local, its caller object must be custom code
    rv_flag = xsdbool( super->accepts( is_sat ) AND (
       ( is_sat-called-global IS NOT INITIAL AND is_custom_code( is_sat-called-global ) )  OR
       ( is_sat-called-global IS INITIAL AND is_custom_code( is_sat-caller-global ) )   )     ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_class_name IMPLEMENTATION.

  METHOD external.
    CHECK cv_clas+30(2) = 'CP'.
    cv_clas = translate( val = cv_clas(30) from = '=' to = ' ' ).
    cv_clas+30(2) = space.
  ENDMETHOD.

  METHOD technical.
    rv_class = to_internal( iv_name ).
    rv_class+30(2) = 'CP'.
  ENDMETHOD.

  METHOD to_internal.
    rv_class = iv_name.
*    rv_class = translate( val = rv_class(30) from = ` ` to = '=' ).  "DOES not work!?
    TRANSLATE rv_class(30) USING ' ='.
  ENDMETHOD.

  METHOD is_global.
    rv_flag = VALUE #( mt_class[ name = iv_name ]-global DEFAULT fetch_is_global( iv_name ) ).
  ENDMETHOD.

  METHOD fetch_is_global.
*    CLEAR rv_flag.
    SELECT SINGLE 'X' FROM tadir INTO @rv_flag            " Single Record buffered
      WHERE pgmid = 'R3TR'
      AND   object = 'CLAS'
      AND   obj_name = @iv_name.
*   add to cache proxy for table TADIR
    INSERT VALUE #( name = iv_name
                    global = rv_flag ) INTO TABLE mt_class.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_trace IMPLEMENTATION.

  METHOD constructor.
*   Query ABAP execution trace created by transaction SAT without aggregation
    mo_atra = NEW #( ).
    mo_class_name = NEW #( ).
  ENDMETHOD.

  METHOD module.   " Read table IT_TRACEPROG
    ASSIGN mo_atra->it_traceprog[ iv_index ] TO FIELD-SYMBOL(<ls_prog>).
    rv_subrc = sy-subrc.
    CHECK rv_subrc = 0.
    ms_target-global = <ls_prog>-cprog.
    ms_target-type = <ls_prog>-object.
    ms_target-system = <ls_prog>-sysflag.
  ENDMETHOD.

  METHOD event.   " Read table IT_TRACETEXT
    ASSIGN mo_atra->it_tracetext[ mr_src->textindex ] TO FIELD-SYMBOL(<ls_text>).
    rv_subrc = sy-subrc.
    CHECK rv_subrc = 0.
    ms_target-event = <ls_text>-tracetext.
    ms_target-system = <ls_text>-sysflag.
  ENDMETHOD.

  METHOD set_meth.  " Read table IT_TRACEMETH
*   METHINDEX  : Index for Method Name Table
    module( mr_src->progindex ).
*   CALLED-Data
    ms_target-event = VALUE #( mo_atra->it_tracemeth[ mr_src->methindex ]-methode OPTIONAL ).
    CHECK line_exists( mo_atra->it_tracetext[ mr_src->textindex ] ).
    DATA(lv_tracetext) = CONV program( mo_atra->it_tracetext[ mr_src->textindex ]-tracetext ).  " type conversion

    CHECK module( line_index( mo_atra->it_traceprog[ cprog = mo_class_name->to_internal( lv_tracetext ) ] ) ) NE 0.
    set_local_class( lv_tracetext ).
  ENDMETHOD.

  METHOD set_constructor.
    ASSIGN mo_atra->it_tracetext[ mr_src->textindex ] TO FIELD-SYMBOL(<ls_text>).
    CHECK sy-subrc = 0.
    ms_target-type = c_type_class.
    ms_target-event = c_create_method.
    ms_target-system = <ls_text>-sysflag.
    DATA(lv_tracetext) = CONV program( <ls_text>-tracetext ).

    CHECK NOT line_exists( mo_atra->it_traceprog[ cprog = lv_tracetext ] ).
    ms_target-global = mo_class_name->technical( lv_tracetext ).
    set_local_class( lv_tracetext ).
  ENDMETHOD.

  METHOD set_local_class.
    CHECK NOT mo_class_name->is_global( iv_progname ).
    ms_target-local = iv_progname.

    CHECK module( mr_src->progindex ) EQ 0.
    ms_target-type = c_type_class.           " overwrite TYPE IF found
  ENDMETHOD.

  METHOD set_form.
    event( ).
    module( mr_src->progindex ).
  ENDMETHOD.

  METHOD set_tran.
    CHECK event( ) EQ 0.
    ms_target-global = ms_target-event.
    ms_target-type = c_type_tran.
  ENDMETHOD.

  METHOD set_submit.
    CHECK event( ) EQ 0.
    module( line_index( mo_atra->it_traceprog[ cprog = ms_target-event ] ) ).
  ENDMETHOD.

  METHOD set_function_module.
    CHECK event( ) EQ 0.
    module( mr_src->pr_2index ).   " PR_2INDEX : Index of int. table TRACEPROG for Performs
  ENDMETHOD.

  METHOD parse.
    rv_flag = abap_true.
*   General data
    ms_target = VALUE #( instance = mr_src->instance ).

    CASE mr_src->id.             " ID of a Trace Record (Recorded Event)
      WHEN c_id_class.           " R - We only check for CREATE OBJECT (R/O) event
        set_constructor( ).
      WHEN c_id_method.          " m - Record of methods-calls
        set_meth( ).
      WHEN c_id_func.            " U - Record of Function Modules
        set_function_module( ).
      WHEN c_id_module.          " M - Module Calls
        set_form( ).
      WHEN c_id_load.            " L - Load
        IF mr_src->subid = 'D'.   " Dynpro
          set_form( ).
        ENDIF.
      WHEN c_id_report.          " CALL - e.g. Call SCREEN/CALL Report
        CASE mr_src->subid.      " Sub-ID of a TRACE Record
          WHEN 'D' OR 'S'.       " CALL Dialog OR Call Screen
            set_form( ).
          WHEN 'P'.              " CALL Program
            module( mr_src->progindex ).
          WHEN 'T'.              " CALL Transaction
            set_tran( ).
          WHEN 'R'.              " SUBMIT Report
            set_submit( ).
        ENDCASE.
      WHEN c_id_form.            " F - Record of Form-Routines
        CASE mr_src->subid.
          WHEN ' '.
            set_form( ).             " internal FORM
          WHEN 'E'.
            set_function_module( ).  " external FORM
          WHEN 'N'.  " Not Found. e.g. PERFORM IF FOUND failed
            rv_flag = abap_false.
        ENDCASE.
      WHEN c_id_record.          " O - Record of measurement
        CASE mr_src->subid.
          WHEN ' '.
            module( mr_src->progindex ).
          WHEN OTHERS.
        ENDCASE.
      WHEN c_id_dynpro           " A - Maybe PBO/PAI-Modules
        OR c_id_flow.            " E - Processing Logic (Flow Logic)
*       Screen handling does not add anything new, the module is needed
        module( mr_src->progindex ).

      WHEN OTHERS.                " Y - SAP GUI Events?
    ENDCASE.

    mo_class_name->external( CHANGING cv_clas = ms_target-global ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_trace IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    io_progress->echo( |Parse { lines( mo_atra->it_austab_hier ) } entries| ).
    mo_bag = NEW lcl_bag( ).
  ENDMETHOD.

  METHOD fill.
*   Parse call hierarchy, apply custom filters, fill collection of messages
    mi_collector = ii_collector.
    LOOP AT mo_atra->it_austab_hier REFERENCE INTO mr_src.
      CHECK new_record( ).
      put_caller( get_caller( ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD new_record.
    IF mr_src->event = c_event_entry.           " Event ID (> Entry, < Exit)
      mo_bag->add( mr_src->index ).
      rv_flag = parse( ).                       " Parse call hierarchy in trace
    ELSE.
      skip_record( ).
      rv_flag = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_caller.
    rs_sat = VALUE ts_sat(
      id            = mr_src->id      " Gen. data
      aus_tabix     = mr_src->index   " Index of DUMP List
      from_level    = mr_src->ebene   " Call Level (Call Hierarchy)
      caller        = VALUE #( mt_caller[ KEY k1 COMPONENTS aus_tabix = mr_src->caller ]-called OPTIONAL )
      called-object = ms_target-object
      called-mod    = ms_target-event
      system        = ms_target-system ).
  ENDMETHOD.

  METHOD put_caller.
    IF mi_collector->collect( is_sat ) EQ abap_false.
      skip_record( ).
    ENDIF.
    DATA(ls_seq) = is_sat-seq.
    IF ls_seq-called-global IS INITIAL.
      ls_seq-called = is_sat-caller.
    ENDIF.
    mo_class_name->external( CHANGING cv_clas = ls_seq-called-global ).
    APPEND ls_seq TO mt_caller.
  ENDMETHOD.

  METHOD skip_record.
    ASSIGN mt_caller[ KEY k1 COMPONENTS aus_tabix = mr_src->start ] TO FIELD-SYMBOL(<ls_seq>).
    IF sy-subrc EQ 0.

      IF mr_src->id = c_id_class AND mr_src->subid = 'O' AND mo_bag->contains( mr_src->start ).
        <ls_seq>-called-instance = mr_src->instance.
      ENDIF.

      APPEND VALUE #( called-instance = mr_src->instance
                      aus_tabix       = mr_src->index ) TO mt_caller.
    ELSE.
      mo_bag->remove( mr_src->start ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_actors IMPLEMENTATION.

  METHOD search.
    rv_idx = 0.
    IF is_object-type EQ c_type_class.
      IF is_object-instance NE 0.
*       Inheritance cases: do not check object name
        rv_idx = VALUE #( mt_actor[ type = c_type_class
                                    instance = is_object-instance ]-index OPTIONAL ).
      ENDIF.
    ELSEIF is_object-type IS NOT INITIAL.
      rv_idx = VALUE #( mt_actor[ type = is_object-type
                                  global = is_object-global
                                  local = is_object-local ]-index OPTIONAL ).
    ENDIF.
  ENDMETHOD.

  METHOD new_object.
    mv_last_number = rv_index = mv_last_number + 1.  " Next index - unique secondary key in actor collection
    INSERT VALUE #( object = is_object
                    index = rv_index
                    label = lifeline( is_object ) ) INTO TABLE mt_actor.
  ENDMETHOD.

  METHOD put.
    rv_index = VALUE #( mt_actor[ object = is_object ]-index DEFAULT search( is_object ) ).
    CHECK rv_index IS INITIAL.
    rv_index = new_object( is_object ).
  ENDMETHOD.

  METHOD lif_path~to_path.
    rs_path = VALUE #( caller = put( is_sat-caller-object )
                       called = put( is_sat-called-object ) ).
  ENDMETHOD.

  METHOD lifeline.
    CASE is_object-type.
      WHEN c_type_class.
        rv_text = |{ get_object_text( is_object-instance ) } of Class { is_object-local }\\n{ is_object-global }|.
      WHEN c_type_fugr.
        rv_text = |Function Group\\n{ get_function_group( is_object-global ) }|.
      WHEN OTHERS.
        rv_text = |{ is_object-type }\\n{ is_object-global }|.
    ENDCASE.
  ENDMETHOD.

  METHOD get_object_text.
    rv_text = SWITCH #( iv_instance WHEN 0 THEN |Static Methods|
                                    ELSE |ObjectId:{ iv_instance }| ).
  ENDMETHOD.

  METHOD get_function_group.
    rv_text = SWITCH #( iv_program+0(1) WHEN '/' THEN iv_program+0(6) && iv_program+10 ELSE iv_program+4 ).
  ENDMETHOD.

  METHOD class_name.
    DATA(ls_obj) = VALUE #( mt_actor[ KEY obj_nr COMPONENTS index = iv_index ] OPTIONAL ).
    rv_short_name = SWITCH program( ls_obj-local WHEN space THEN ls_obj-global ELSE ls_obj-local ).
  ENDMETHOD.

  METHOD lif_actors~lifelines.
    rt_lifeline = CORRESPONDING #( mt_actor ).
  ENDMETHOD.

  METHOD lif_actors~short_text.
    CASE is_message-id.
      WHEN c_id_class.
        rv_text = |Create instance of class { class_name( is_message-called ) }|.
      WHEN c_id_skip.
        rv_text = |<b>Skipping over SAP code until calling { is_message-msg }</b>|.
      WHEN OTHERS.
        rv_text = |{ SWITCH #( is_message-id
                        WHEN c_id_form   THEN |Perform|
                        WHEN c_id_func   THEN |Call FM|
                        WHEN c_id_method THEN |Call method|
                        ELSE space ) } { is_message-msg }|.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sequence IMPLEMENTATION.

  METHOD constructor.
    DATA(lo_actors) = NEW lcl_actors( ).
    mi_path = lo_actors.
    mi_filter = lcl_filter=>new( is_cfg ).            " Custom filter
    mo_progress = lcl_progress_indicator=>new( is_cfg ).
    mo_uml = lcl_uml=>new( iv_uml = is_cfg-uml_format
                           ii_actors = lo_actors ).
    mv_compact_trace = is_cfg-compact_trace.
  ENDMETHOD.

  METHOD link_to_previous.
*   add a link to last valid entry to fill gaps created by filter logic
    rs_sat = is_sat.
    IF ms_previous IS NOT INITIAL AND ms_previous-called NE is_sat-caller AND is_sat-from_level GT ms_previous-from_level.
      IF is_sat-caller IS INITIAL.
        rs_sat-caller = ms_previous-called.  " check initial?
      ELSE.
*       insert a new line into the table at this point (link to last valid line).
        add( VALUE #( id = c_id_skip
                      from_level = ms_previous-from_level
                      caller = ms_previous-called
                      called = is_sat-caller ) ).
      ENDIF.
    ENDIF.
    ms_previous = rs_sat.
  ENDMETHOD.

  METHOD lif_collector~collect.
    rv_flag = mi_filter->accepts( is_sat ).
    CHECK rv_flag EQ abap_true.
*   Update previous entry (fill gaps if needed)
    add( link_to_previous( is_sat ) ).
  ENDMETHOD.

  METHOD add.
    APPEND VALUE #( id = is_sat-id
                    from_level = is_sat-from_level
                    msg = is_sat-called-mod
                    path = mi_path->to_path( is_sat ) ) TO mt_trace.
  ENDMETHOD.

  METHOD to_diagram.
    ro_diagram = lcl_diagram_text=>new( is_cfg = is_cfg
                                        iv_text = NEW lcl_sequence( is_cfg )->to_uml( ) ).
  ENDMETHOD.

  METHOD to_uml.
*   filter and convert ABAP call hierarchy trace:
    NEW lcl_trace( mo_progress )->fill( me ).
*   UML header
    mo_uml->top( ).
*   Convert trace to call stack object
    lcl_uml_factory=>new_messages( iv_compact_trace = mv_compact_trace
                                   it_trace = mt_trace
                                   io_uml = mo_uml
                                   io_progress = mo_progress )->to_uml( mo_uml ).
*   add UML footer
    rv_diagram = mo_uml->bottom( ).

    mo_progress->restore( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_uml IMPLEMENTATION.

  METHOD new.
    CASE iv_uml.
      WHEN c_uml_graph.
        ro_uml = NEW lcl_uml_graph( ).
      WHEN c_uml_mscgen.
        ro_uml = NEW lcl_uml_mscgen( ).
      WHEN c_uml_mock.
        ro_uml = NEW lcl_uml_logger( ).
      WHEN OTHERS.
        ro_uml = NEW lcl_uml_plant( ).
    ENDCASE.
    ro_uml->mi_actors = ii_actors.
  ENDMETHOD.

  METHOD add.
    mv_diagram = mv_diagram && iv_code.
  ENDMETHOD.

  METHOD complete.
    RETURN.
  ENDMETHOD.

  METHOD separator.
    RETURN.
  ENDMETHOD.

  METHOD delimiter.
    RETURN.
  ENDMETHOD.

  METHOD top.
    header( ).
*   Init life lines
    LOOP AT mi_actors->lifelines( ) INTO DATA(ls_lifeline).
      participant( ls_lifeline ).
      AT LAST.
        delimiter( ).          "if needed: e.g. add( |;\n| ).
        EXIT.
      ENDAT.
      separator( ).            "if needed: e.g. add( |, | ).
    ENDLOOP.
  ENDMETHOD.

  METHOD bottom.
*   Complete all life lines
    LOOP AT mi_actors->lifelines( ) INTO DATA(ls_lifeline).
      complete( ls_lifeline ).
    ENDLOOP.
    footer( ).
    rv_diagram = mv_diagram.
  ENDMETHOD.

  METHOD message.
    CHECK is_message IS NOT INITIAL.
    call( is_message ).

    IF is_message-id = c_id_skip.
      skip_note( is_message-path ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_uml_plant IMPLEMENTATION.

  METHOD add_param.
    CHECK iv_cond EQ abap_true.
    mv_param = mv_param && iv_command.
  ENDMETHOD.

  METHOD scale.
    DATA lv_scale TYPE tv_scale.
    lv_scale = nmin( val1 = 1
                     val2 = nmax( val1 = is_cfg-scale
                                  val2 = c_min_scale ) ).
    rv_scale = |scale { lv_scale DECIMALS = 2 }\n|.
  ENDMETHOD.

  METHOD parameters.
    CLEAR mv_param.
    add_param( scale( is_cfg ) ).                   " Reduce the size of the output image
    add_param( iv_cond = is_cfg-teoz_architecture
               iv_command = |!pragma teoz true\n| ).
    add_param( |skinparam \{\n| ).
*   For performance: disable shadowing
    add_param( iv_cond = xsdbool( is_cfg-shadowing EQ abap_false )
               iv_command = |shadowing false\n| ).
    add_param( iv_cond = is_cfg-handwritten         " mimic hand writing
               iv_command = |handwritten true\n| ).
    add_param( |\}\n| ).
    rv_param = mv_param.
  ENDMETHOD.

  METHOD header.
*   Plant UML Header
    add( |@startuml\nhide footbox\nautonumber\n{ parameters( lcl_configuration=>get( ) ) }| ).
  ENDMETHOD.

  METHOD call.
    add( |{ is_message-caller } -> { is_message-called }: { mi_actors->short_text( is_message ) }\n| ).
    add( |activate { is_message-called }\n| ).
  ENDMETHOD.

  METHOD return.
*   return and deactivate the actor
    IF iv_from NE iv_to.
      add( |{ iv_from } --> { iv_to }\n| ).
    ENDIF.
    CHECK iv_from IS NOT INITIAL.
    add( |deactivate { iv_from }\n| ).
  ENDMETHOD.

  METHOD participant.
    add( |participant "{ is_lifeline-label }" as { is_lifeline-index }\n| ).
  ENDMETHOD.

  METHOD footer.
    add( |@enduml\n| ).
  ENDMETHOD.

  METHOD skip_note.
    add( |note over { is_path-caller },{ is_path-called }\n{ c_txt_std }\nend note\n| ).
  ENDMETHOD.

  METHOD begin_loop.
    add( |loop { iv_times } times\n| ).
  ENDMETHOD.

  METHOD end_loop.
    add( |end\n| ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_messages IMPLEMENTATION.

  METHOD constructor.
    CLEAR mv_idx.
    mt_list = it_messages.    " shared, never changed
    mv_size = lines( mt_list ).
    mv_first_level = next_level( ).
  ENDMETHOD.

  METHOD to_uml.
    DATA(lo_stack) = NEW lcl_uml_stack( io_uml ).
*   Convert calls to UML sequence in text format
    WHILE has_next( ).
      message( is_message = next( )
               iv_idx = sy-index
               io_stack = lo_stack ).
    ENDWHILE.
  ENDMETHOD.

  METHOD message.
    first_message( io_stack ).
    io_stack->call( is_message ).
    io_stack->return( next_level( ) ).
  ENDMETHOD.

  METHOD first_message.
    CHECK is_first( ).
    io_stack->call( VALUE #( caller = mv_first_level
                             called = c_first_key ) ). " very first call
  ENDMETHOD.

  METHOD next.
    skip( ).
    rs_data = mt_list[ mv_idx ].
  ENDMETHOD.

  METHOD has_next.
    rv_flag = xsdbool( mv_idx < mv_size ).
  ENDMETHOD.

  METHOD skip.
    ADD iv_count TO mv_idx.
  ENDMETHOD.

  METHOD is_first.
    rv_flag = xsdbool( mv_idx LE 1 ).
  ENDMETHOD.

  METHOD next_level.
    rv_level = VALUE #( mt_list[ mv_idx + 1 ]-from_level DEFAULT c_default_level ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_messages_compact IMPLEMENTATION.

  METHOD constructor.
    super->constructor( it_messages ).
    mo_uml = io_uml.
    mt_cycle = it_cycles.
  ENDMETHOD.

  METHOD message.
    begin( iv_idx = iv_idx
           iv_from = is_message-caller ).                " if needed
    super->message( is_message = is_message
                    iv_idx     = iv_idx
                    io_stack   = io_stack ).
*   after return so the loop encloses the return path
    skip( end( iv_idx = iv_idx
               iv_to = is_message-called ) ).
  ENDMETHOD.

  METHOD name.  " Loop ID
    rv_name = |{ iv_start }to{ iv_end }|.
  ENDMETHOD.

  METHOD begin.
    LOOP AT mt_cycle INTO DATA(ls_cycle) WHERE start = iv_idx.
      mo_uml->begin_loop( iv_from = |{ iv_from }|
                          iv_times = ls_cycle-times
                          iv_name = name( iv_start = iv_idx
                                          iv_end = ls_cycle-end ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD end.
    CLEAR rv_step.
    LOOP AT mt_cycle INTO DATA(ls_cycle) WHERE end = iv_idx. "#EC CI_SORTSEQ
      mo_uml->end_loop( iv_to = |{ iv_to }|
                        iv_name = name( iv_start = ls_cycle-start
                                        iv_end = iv_idx ) ).
      rv_step = nmax( val1 = ls_cycle-last - iv_idx
                      val2 = rv_step ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_stack IMPLEMENTATION.

  METHOD push.
    mr_top = NEW ts_level( call_level = is_ref
                           next = mr_top ).
  ENDMETHOD.

  METHOD pop.
    CLEAR rs_to.
    CHECK has_next( ).
    rs_to = mr_top->call_level.
    mr_top ?= mr_top->next.
  ENDMETHOD.

  METHOD has_next.
    rv_flag = xsdbool( mr_top IS BOUND ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_uml_stack IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_uml = io_uml.
    mv_previous_level = lcl_messages=>c_default_level.
  ENDMETHOD.

  METHOD call.
    push( VALUE #( actor_key = is_message-called
                   from_level = is_message-from_level ) ).
*   generate UML in text form
    mo_uml->message( is_message ).
  ENDMETHOD.

  METHOD return.
    IF iv_to_level <= mv_previous_level.
      return_to( iv_to_level ).
    ENDIF.
    mv_previous_level = iv_to_level.
  ENDMETHOD.

  METHOD return_to.
    DATA(ls_from) = pop( ).
    WHILE ls_from-from_level >= iv_level AND has_next( ).
      DATA(ls_to) = pop( ).
*     UML diagram
      mo_uml->return( iv_from = ls_from-actor_key
                      iv_to = ls_to-actor_key ).
      ls_from = ls_to.
    ENDWHILE.

    CHECK ls_to IS NOT INITIAL.
    push( ls_to ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_message_index IMPLEMENTATION.

  METHOD constructor.
    mt_trace = it_trace.
    mi_cycles = ii_cycles.
    to_index( ).
    io_progress->echo( |Compact trace with { lines( it_trace ) } entries| ).
  ENDMETHOD.

  METHOD to_index.
*    mt_components = VALUE #( FOR t IN mt_trace ( t-message ) ).  dump on duplicates! does not pass test
    CLEAR mt_components.
    LOOP AT mt_trace ASSIGNING FIELD-SYMBOL(<ls_trace>).
      INSERT <ls_trace> INTO TABLE mt_components.
    ENDLOOP.

    mt_xindex = VALUE #( FOR t IN mt_trace ( line_index( mt_components[ table_line = t ] ) ) ).
  ENDMETHOD.

  METHOD to_trace.
    ct_trace = VALUE #( FOR x_idx IN mt_xindex ( mt_components[ x_idx ] ) ).
  ENDMETHOD.

  METHOD shrink.
    lcl_pattern=>new( it_xindex = mt_xindex
                      ii_cycles = mi_cycles )->detect_cycles( ).
    rv_shrink = compact( CHANGING ct_xindex = mt_xindex ).
    lcl_progress_indicator=>echo( iv_percentage = iv_pass
                                  iv_text = |Compact trace pass { iv_pass } - { lines( mt_xindex ) } entries| ).
  ENDMETHOD.

  METHOD compact.
    DATA lt_xindex TYPE tt_x_index.

    rv_shrink = abap_false.
    LOOP AT ct_xindex ASSIGNING FIELD-SYMBOL(<lv_x_index>).
      CHECK mi_cycles->is_canonical_index( sy-tabix ).
      APPEND <lv_x_index> TO lt_xindex.
    ENDLOOP.
    IF ct_xindex NE lt_xindex.
      rv_shrink = abap_true.
      ct_xindex = lt_xindex.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_uml_factory IMPLEMENTATION.

  METHOD constructor.
    mo_progress = io_progress.
  ENDMETHOD.

  METHOD new_messages.
*   create UML sequence translator
    IF iv_compact_trace EQ abap_true.
      DATA(lo_factory) = NEW lcl_uml_factory( io_progress ).
      ro_messages = lo_factory->new_iterator( io_uml = io_uml
                                              it_messages = it_trace ).
      et_cycles = lo_factory->mt_cycles.
    ELSE.
      CLEAR et_cycles.
      ro_messages = NEW lcl_messages( it_trace ).
    ENDIF.
  ENDMETHOD.

  METHOD new_iterator.
    fold( EXPORTING it_trace = it_messages
          IMPORTING et_messages = DATA(lt_messages) ).
    ro_messages = NEW lcl_messages_compact( io_uml = io_uml
                                            it_messages = lt_messages
                                            it_cycles = mt_cycles ).
  ENDMETHOD.

  METHOD lif_cycles~collect.
*   duplicate entries are inserted BEFORE the existing row
    INSERT is_cycle INTO TABLE mt_cycles.
  ENDMETHOD.

  METHOD lif_cycles~is_canonical_index.
    LOOP AT mt_cycles TRANSPORTING NO FIELDS            "#EC CI_SORTSEQ
      WHERE end LT iv_index AND last GE iv_index.
      rv_flag = abap_false.
      RETURN.
    ENDLOOP.
    rv_flag = abap_true.
  ENDMETHOD.

  METHOD fold.
    CLEAR et_messages.

    DATA(lo_evt) = NEW lcl_message_index( it_trace = it_trace
                                          ii_cycles = me
                                          io_progress = mo_progress ).
    WHILE lo_evt->shrink( sy-index ).
      lcl_cycles=>shrink( CHANGING ct_cycles = mt_cycles ).
    ENDWHILE.
    lo_evt->to_trace( CHANGING ct_trace = et_messages ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_cycles IMPLEMENTATION.

  METHOD constructor.
    mt_cycles = it_cycles.

    CLEAR mv_tabix.
    LOOP AT it_cycles INTO DATA(ls_cycle).
      mv_tabix = sy-tabix.
*     Change needed in subsequent entries?
      CHECK ls_cycle-last GT ls_cycle-end.
      ms_stop = ls_cycle.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD shrink.
    DATA(lo_cycles) = NEW lcl_cycles( ct_cycles ).

    WHILE lo_cycles->ms_stop IS NOT INITIAL.  " Change needed ?
      lo_cycles = lo_cycles->compress( ).     " Compress loop reference table:
    ENDWHILE.
    ct_cycles = lo_cycles->mt_cycles.
  ENDMETHOD.

  METHOD compress.
    DATA lt_shrinked TYPE SORTED TABLE OF ts_cycle WITH UNIQUE KEY start end last times.
*   after removing repetition from the active loop in the trace table,
*   rebuild (shrink) the loop reference table
    LOOP AT mt_cycles INTO DATA(ls_cycle).
      AT FIRST.
        DATA(lv_delta) = ms_stop-last - ms_stop-end.
      ENDAT.
      IF ls_cycle-last LT ms_stop-start.
*       Loops entries before the active loop - verbatim
      ELSEIF ls_cycle-start LT ms_stop-start AND ls_cycle-end GE ms_stop-last.
*       Loops entries starting before the active loop to be compacted and
*       that contains it (i.e. they enclose it ) - adjust
        SUBTRACT lv_delta FROM: ls_cycle-end,
                                ls_cycle-last.
      ELSEIF sy-tabix EQ mv_tabix.
*       Update the active loop entry
        ls_cycle = VALUE #( BASE ms_stop last = ms_stop-end ).
      ELSEIF ls_cycle-start GE ms_stop-start AND ls_cycle-last LE ms_stop-end.
*       Loops entries completely enclosed by the active loop entry - verbatim
      ELSEIF ls_cycle-start GT ms_stop-last.
*       Loops entries after the active loop - shifted by Delta
        SUBTRACT lv_delta FROM: ls_cycle-start,
                                ls_cycle-end,
                                ls_cycle-last.
      ELSE.
        CONTINUE.
      ENDIF.
      INSERT ls_cycle INTO TABLE lt_shrinked.
    ENDLOOP.
*   result in new object
    ro_cycles = NEW lcl_cycles( CONV #( lt_shrinked ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_pattern IMPLEMENTATION.

  METHOD new.
    ro_pattern = NEW #( it_xindex = it_xindex
                        iv_start = iv_start
                        iv_stop = iv_stop ).
    ro_pattern->mi_cycles = ii_cycles.
  ENDMETHOD.

  METHOD constructor.
    CLEAR mv_idx.
    mt_xindex = it_xindex.         " Copied or Shared??? never changed
    mv_size = COND #( WHEN iv_stop IS INITIAL THEN lines( mt_xindex )
                      ELSE iv_stop ).
    skip( iv_start - 1 ).
  ENDMETHOD.

  METHOD clone.
    ro_loop = new( iv_start = iv_start
                   iv_stop = COND #( WHEN iv_stop IS SUPPLIED THEN iv_stop ELSE mv_size )
                   it_xindex = mt_xindex
                   ii_cycles = mi_cycles ).
  ENDMETHOD.

  METHOD skip.
    ADD iv_count TO mv_idx.
  ENDMETHOD.

  METHOD has_next.
    rv_flag = xsdbool( mv_idx < mv_size ).
  ENDMETHOD.

  METHOD occurs_in.
    DATA(lv_other) = iv_end.
    rv_same = abap_false.
    LOOP AT mt_xindex FROM iv_start TO iv_end ASSIGNING FIELD-SYMBOL(<lv_index>).
      ADD 1 TO lv_other.
      CHECK mt_xindex[ lv_other ] NE <lv_index>.
      RETURN.
    ENDLOOP.
*   returning abap_true for zero length tests will lead to infinite loop
    rv_same = xsdbool( sy-subrc EQ 0 ).
  ENDMETHOD.

  METHOD match_pattern.
    rv_match = abap_false.

    DATA(lv_dist) = iv_end - mv_idx + 1.
    CHECK iv_end GE 0 AND lv_dist GT 0.

    DATA(ls_group) = VALUE ts_cycle( start = mv_idx
                                     end = iv_end
                                     last = iv_end
                                     times = 1 ).
*   Find consecutive occurrences of group
    WHILE ( ls_group-last + lv_dist <= mv_size )
      AND occurs_in( iv_start = mv_idx
                     iv_end = ls_group-last ).
      ADD 1 TO ls_group-times.
      ADD lv_dist TO ls_group-last.
      skip( lv_dist ).
    ENDWHILE.
    CHECK ls_group-times GT 1.
    rv_match = abap_true.
    skip( lv_dist - 1 ).
    mi_cycles->collect( ls_group ).
*   repeat search in a subrange (recursive call)
    clone( iv_start = ls_group-start + 1
           iv_stop = ls_group-end )->detect_cycles( ).
  ENDMETHOD.

  METHOD detect_cycles.
*   identify blocks of repeated messages (cycles)
*   Pass the cycle information to interface LIF_CYCLES
    WHILE has_next( ).
      skip( ).
      DATA(lv_index) = mv_idx.
      DO.                                               "#EC CI_NESTED.
        lv_index = next_occurrence_from( lv_index ).
        CHECK lv_index IS INITIAL OR match_pattern( lv_index - 1 ).
        EXIT.
      ENDDO.
    ENDWHILE.
  ENDMETHOD.

  METHOD next_occurrence_from.
    CLEAR rv_row.
    DATA(lv_until) = nmin( val1 = iv_from + c_max_step
                           val2 = mv_size ).
    CHECK lv_until GT iv_from.
    ASSIGN mt_xindex[ iv_from ] TO FIELD-SYMBOL(<lv_org>).
    CHECK sy-subrc EQ 0.
    FIND FIRST OCCURRENCE OF <lv_org> IN TABLE mt_xindex
      FROM iv_from + 1 TO lv_until IN BYTE MODE
      MATCH LINE rv_row.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_file_name IMPLEMENTATION.

  METHOD new.
    CASE iv_mode.
      WHEN lcl_diagram_text=>c_mode_aut.
        ro_file = NEW lcl_file_name_test( iv_mode ).
      WHEN OTHERS.
        ro_file = NEW lcl_file_name( iv_mode ).
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.
    CASE iv_mode.
      WHEN lcl_diagram_text=>c_mode_txt.
        ms_file = VALUE #( title = |Save UML text source|
                           ext = |.txt| ).
      WHEN OTHERS.
        ms_file = VALUE #( title = |Save As...|
                           ext = |.txt| ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_prefix.
    rv_name = shift_right( val = ms_file-name
                           places = strlen( ms_file-ext ) ).
  ENDMETHOD.

  METHOD get_fullpath.
    rv_name = ms_file-path.
  ENDMETHOD.

  METHOD dialog.
    DATA lv_path TYPE string.

    CLEAR rv_user_action.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title = ms_file-title                " Window Title
        default_extension = ms_file-ext             " Default Extension
      CHANGING
        filename = ms_file-name          " File Name to Save
        path = lv_path                   " Path to File
        fullpath = ms_file-path          " Path + File Name
        user_action = rv_user_action
" User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*   file_encoding =
      EXCEPTIONS
        OTHERS = 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_file_name_test IMPLEMENTATION.

  METHOD dialog.
    ms_file-path = |test.txt|.
    rv_user_action = cl_gui_frontend_services=>action_cancel.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_file IMPLEMENTATION.

  METHOD create.
    rv_subrc = 1.
    CHECK io_name->dialog( ) NE cl_gui_frontend_services=>action_cancel.

    rv_subrc = cl_uml_utilities=>save_xml_local( xml = iv_data
                                                 filename = io_name->get_fullpath( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_diagram_text IMPLEMENTATION.

  METHOD new.
    CASE is_cfg-uml_format.
      WHEN OTHERS.   "default: create PlantUML diagram
        ro_diagram = NEW lcl_diagram_plant_uml( is_cfg = is_cfg
                                                iv_diagram = iv_text ).
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.
    mv_diagram = iv_diagram.
    ms_cfg-output_mode = iv_mode.
  ENDMETHOD.

  METHOD output.
    save_file( ms_cfg-output_mode ).
  ENDMETHOD.

  METHOD to_xstring.
    cl_abap_conv_out_ce=>create( encoding = 'UTF-8' )->convert( EXPORTING data = iv_string
                                                                IMPORTING buffer = rv_xstring ).
  ENDMETHOD.

  METHOD save_file.
*   export (PlantUML) source
    rv_fname = lcl_file=>create( iv_data = to_xstring( mv_diagram )
                                 io_name = lcl_file_name=>new( iv_mode ) ).
  ENDMETHOD.                    "save_file

ENDCLASS.

CLASS lcl_diagram_plant_uml IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_diagram = iv_diagram
                        iv_mode = is_cfg-output_mode ).
    ms_cfg = is_cfg.
  ENDMETHOD.

  METHOD output.
    CASE ms_cfg-output_mode.
      WHEN c_mode_url.
        display_html( ).
      WHEN c_mode_exe.
        local_plant_uml( ).
      WHEN OTHERS.
        super->output( ).
    ENDCASE.
  ENDMETHOD.

  METHOD show_html.
    cl_abap_browser=>show_html( html_string = iv_html
                                size = ms_cfg-browser_size
                                context_menu = abap_true ).
  ENDMETHOD.

  METHOD display_html.
    show_html( |<img src="{ to_url( ms_cfg-server_url ) }"/>\n{ source( ) }| ).
  ENDMETHOD.                    "display_html

  METHOD to_url.
    rv_url = iv_base_url && encoded_url_suffix( ).
  ENDMETHOD.

  METHOD encoded_url_suffix.
*   for PlantUML Server: Convert to UTF-8, then deflate, then encode (base64 variant)
    cl_abap_gzip=>compress_binary(
      EXPORTING
        raw_in         = to_xstring( mv_diagram )  ##TYPE  " UTF-8
        compress_level = 9
      IMPORTING
        gzip_out       = DATA(lv_bin) ).

    rv_url = translate( val = cl_http_utility=>encode_x_base64( lv_bin )
                        from = c_charset_standard
                        to =   c_charset_plantuml ).
  ENDMETHOD.

  METHOD local_plant_uml.
    DATA(lo_name) = lcl_file_name=>new( c_mode_txt ).
    CHECK lcl_file=>create( iv_data = to_xstring( mv_diagram )
                            io_name = lo_name ) IS INITIAL.
    show_html( |<img src="{ to_png( lo_name ) }"/>\n{ source( ) }| ).
  ENDMETHOD.

  METHOD parameter_string.
    rv_param = |-jar { ms_cfg-java_jar } -o { ms_cfg-local_path } "{ io_name->get_fullpath( ) }"|.
  ENDMETHOD.

  METHOD png_file_name.
    TRY.
        rv_name = |{ ms_cfg-local_path }{ io_name->get_prefix( ) }.png|.
      CATCH cx_dynamic_check.
        CLEAR rv_name.
    ENDTRY.
  ENDMETHOD.

  METHOD source.
    CASE ms_cfg-display_source.
      WHEN abap_false.
        CLEAR rv_source.
      WHEN OTHERS.
        rv_source = |<p>{ mv_diagram }</p>|.
    ENDCASE.
  ENDMETHOD.

  METHOD to_png.
    CLEAR rv_name.
    cl_gui_frontend_services=>execute( EXPORTING application = ms_cfg-java_appl
                                                 parameter = parameter_string( io_name )
                                                 synchronous = 'X'
                                       EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc EQ 0.
      rv_name = png_file_name( io_name ).
    ENDIF.
  ENDMETHOD.

  METHOD registry_local.
    CLEAR rv_reg.
    cl_gui_frontend_services=>registry_get_value(
      EXPORTING
        root = cl_gui_frontend_services=>hkey_local_machine
        key = iv_key
        value = iv_value
      IMPORTING
        reg_value = rv_reg
      EXCEPTIONS
        OTHERS               = 5 ).
    CHECK sy-subrc NE 0.
    RAISE EXCEPTION TYPE cx_sy_itab_error. "any cx_dynamic_check exception will do
  ENDMETHOD.

  METHOD get_java_path.
    CONSTANTS c_java_base_key TYPE string
      VALUE 'SOFTWARE\JavaSoft\Java Runtime Environment' ##NO_TEXT.
    TRY.
        DATA(lv_base) = registry_local( iv_key = c_java_base_key
                                        iv_value = 'CurrentVersion' ).
        rv_path = |{ registry_local( iv_key = |{ c_java_base_key }\\{ lv_base }|
                                     iv_value = 'JavaHome' ) }\\bin\\java|.
      CATCH cx_dynamic_check.
        CLEAR rv_path.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_progress_indicator IMPLEMENTATION.

  METHOD new.
    DATA(ls_cfg) = COND #( WHEN is_cfg IS SUPPLIED THEN is_cfg
                           ELSE lcl_configuration=>get( ) ).
    ro_prog = NEW #( ls_cfg-progress ).
  ENDMETHOD.

  METHOD constructor.
    mv_mode = get_mode( ).
    set_mode( COND #( WHEN iv_progress EQ abap_true THEN c_enable
                      ELSE c_disable ) ).
  ENDMETHOD.

  METHOD restore.
    set_mode( mv_mode ).
  ENDMETHOD.

  METHOD get_mode.
    GET PARAMETER ID c_param_id_sin FIELD rv_mode.
  ENDMETHOD.

  METHOD set_mode.
    SET PARAMETER ID c_param_id_sin FIELD iv_mode.
  ENDMETHOD.

  METHOD echo.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = iv_percentage
        text       = iv_text.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_configuration IMPLEMENTATION.

  METHOD class_constructor.
    gs_cfg = VALUE #( java_appl = lcl_diagram_plant_uml=>get_java_path( )
*                     PlantUML jar file and output path
                      local_path = `C:\Temp\Dokumente\UML\`
                      java_jar = `C:\Temp\Dokumente\UML\plantuml.jar`
*                     PlantUML server URL
                      server_url = `http://www.plantuml.com/plantuml/img/` ##NO_TEXT
                      output_mode = lcl_diagram_text=>c_mode_url
                      skip_dialog = space
                      compact_trace = abap_true
                      scale = c_default_scale
                      pattern = VALUE #( ( 'Y*' ) ( 'Z*' ) ( '/BAY*' ) )
                      progress = abap_true
                      handwritten = abap_false
                      shadowing = abap_false
                      display_source = abap_true
                      teoz_architecture = abap_false
                      browser_size = cl_abap_browser=>xlarge ).
*   Windows: Local Java installation
    IF gs_cfg-java_appl IS INITIAL.
      gs_cfg-java_appl = `C:\Windows\System32\java`.
    ENDIF.
  ENDMETHOD.

  METHOD get_attributes.
    DEFINE m_fill_att.
      INSERT VALUE #( ref = REF #( &1 )
                      text = &2
                      kind = &3 ) INTO TABLE rt_attr.
    END-OF-DEFINITION.
    DEFINE m_fill_radio.
      INSERT VALUE #( ref = REF #( &1 )
                      text = &2
                      kind = 'R'
                      button_group = &3 ) INTO TABLE rt_attr.
    END-OF-DEFINITION.
* Table Type has type 'T' - patterns SCI_PATTERN
*                     ' ' - ?? private attributes?
*                     'I' - ?? Integer?
    m_fill_att gs_cfg-skip_dialog        'Remember my settings'(c00)    'C'.

    m_fill_att: ''                       'Trace Settings'(c01)          'G',
              gs_cfg-progress            'Progress indicator'(c02)      'C',
              gs_cfg-system_events       'System events'(c03)           'C',
              gs_cfg-compact_trace       'Loops compacted'(c04)         'C',
              gs_cfg-pattern             'Customer namespace'(c05)      'T'.

    m_fill_att: sy-index                 'PlantUML Execution Mode'(c10) 'G'.   " Group
    m_fill_radio: mv_mode_url            'PlantUML web service'(c11)  'MOD',
                  mv_mode_txt            'Save text file'(c12)        'MOD',
                  mv_mode_exe            'Local PlantUML '(c13)       'MOD'.

    m_fill_att: ''                       'PlantUML Settings'(c20)       'G',
                gs_cfg-scale             'Scale '(c21)                  'S'.

    m_fill_att: gs_cfg-server_url        'PlantUML Server'(c25)         'S',
                gs_cfg-local_path        'Local PlantUML path'(c26)     'S',
                gs_cfg-java_jar          'Local PlantUML jar file'(c27) 'S',
                gs_cfg-java_appl         'Local Java path'(c28)         'S'.  " Select-Options

    m_fill_att: gs_cfg-handwritten       'Handwritten '(c30)           'C',
                gs_cfg-shadowing         'Shadowing '(c31)             'C',
                gs_cfg-display_source    'Display source '(c32)        'C',
                gs_cfg-teoz_architecture 'Use TEOZ architecture '(c33) 'C'.
  ENDMETHOD.

  METHOD to_radiobutton.
    mv_mode_url = xsdbool( gs_cfg-output_mode EQ lcl_diagram_text=>c_mode_url ).
    mv_mode_exe = xsdbool( gs_cfg-output_mode EQ lcl_diagram_text=>c_mode_exe ).
    mv_mode_txt = xsdbool( gs_cfg-output_mode EQ lcl_diagram_text=>c_mode_txt ).
  ENDMETHOD.

  METHOD from_radiobutton.
    IF mv_mode_url EQ abap_true.
      gs_cfg-output_mode = lcl_diagram_text=>c_mode_url.
    ELSEIF mv_mode_exe EQ abap_true.
      gs_cfg-output_mode = lcl_diagram_text=>c_mode_exe.
    ELSEIF mv_mode_txt EQ abap_true.
      gs_cfg-output_mode = lcl_diagram_text=>c_mode_txt.
    ENDIF.
  ENDMETHOD.

  METHOD get.
    rs_cfg = CORRESPONDING #( gs_cfg ).
  ENDMETHOD.

  METHOD query.
    DATA(lo_config) = NEW lcl_configuration( ).
    lo_config->dialog( ).
    rs_cfg = lo_config->get( ).
  ENDMETHOD.

  METHOD dialog.
    to_radiobutton( ).
    CHECK gs_cfg-skip_dialog EQ abap_false.
    CHECK cl_ci_query_attributes=>generic(
        p_name       = CONV #( sy-repid )                    " unique screen ID
        p_title      = 'Sequence Diagram Parameters'         " Screen title
        p_attributes = get_attributes( )                     " Screen fields
        p_display    = abap_false                            " Edit / Display only
       ) EQ abap_false.   " Do not cancel
    from_radiobutton( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_uml_graph IMPLEMENTATION.

  METHOD header.
    add( |.PS\n| ). " GNU plot pic2plot macro header for sequence diagram
    add( |copy "sequence.pic";\n| ).
    add( |\n| ).
  ENDMETHOD.

  METHOD top.
    super->top( ).
    add( |step();\n| ).
  ENDMETHOD.

  METHOD call.
* alternative: create_message(from_object,to_object,object_label);
*   Has from_object create the to_object, labeled with object_label.
*   The message is labeled with the «create» stereotype. Can also be written as cmessage.
    add( |message({ is_message-caller },{ is_message-called },"{
        mi_actors->short_text( is_message ) }");\n| ).

    add( |active({ is_message-called });\n| ).
  ENDMETHOD.

  METHOD return.
* - return_message(from_object,to_object,label)
*   Draws a return message between two objects, with the given label. Can also be written as rmessage.
    IF iv_from NE iv_to.
      add( |return_message({ iv_from },{ iv_to });\n| ).
    ENDIF.
    CHECK iv_from IS NOT INITIAL.
    add( |inactive({ iv_from });\n| ).
  ENDMETHOD.

  METHOD participant.
* - object(name,label);   OR  actor(name,label);
*   Defines an object / actor with the given name, labeled on the diagram as specified.
*   Actors are typically used instead of objects to indicate operations initiated by human action
    add( |object({ is_lifeline-index },"{ is_lifeline-label }");\n| ).
  ENDMETHOD.

  METHOD complete.
    add( |complete({ is_lifeline-index });\n| ).
  ENDMETHOD.

  METHOD footer.
    add( |.PE\n| ).
  ENDMETHOD.

  METHOD skip_note.
* - lifeline_constraint(object,label);
*   Displays a constraint label (typically given inside curly braces) for the given object.
*   The constraint will appear on the right of the object's lifeline at the time it appears.
*   Can also be used to place an message label on the left of a message arrow, rather than its center.
*   Can also be written as lconstraint.
    add( |lifeline_constraint({ is_path-caller },"{ c_txt_std }")\n| ).
  ENDMETHOD.

  METHOD begin_loop.
* - begin_frame(left_object,name,label_text);
*   Begins a frame with the upper left corner at left_object column and the current line.
*   The specified label_text is shown in the upper left corner.
    add( |begin_frame({ iv_from },{ iv_name },"Loop { iv_times } times");\n| ).        " pop
  ENDMETHOD.

  METHOD end_loop.
* - end_frame(right_object,name);
*   Ends a frame with the lower right corner at right_object column and the current line.
*   The name must correspond to a begin_frame's name.
    add( |end_frame({ iv_to }, { iv_name });\n| ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_uml_mscgen IMPLEMENTATION.

  METHOD header.
    add( |msc \{\n| ).
  ENDMETHOD.

  METHOD delimiter.
    add( |;\n| ).
  ENDMETHOD.

  METHOD separator.
    add( |, | ).
  ENDMETHOD.

  METHOD call.
    add( |{ is_message-caller }=>{ is_message-called } [label="{
      mi_actors->short_text( is_message ) }"];\n| ).
  ENDMETHOD.

  METHOD return.
    CHECK iv_from NE iv_to.
    add( |{ iv_to }<<{ iv_from }\n| ).
  ENDMETHOD.

  METHOD participant.
    add( |{ is_lifeline-index } [label="{ is_lifeline-label }"]| ).
  ENDMETHOD.

  METHOD footer.
    add( |\}\n| ).
  ENDMETHOD.

  METHOD skip_note.
    add( |{ is_path-called } note { is_path-caller } [label="{ c_txt_std
      }", textbgcolour="#7fff7f"];\n| ).
  ENDMETHOD.

  METHOD begin_loop.
*    add( |[c:loop  { iv_times } times]\n| ).        " pop
    RETURN.
  ENDMETHOD.

  METHOD end_loop.
*    add( |[/c]\n| ).
    RETURN.
  ENDMETHOD.

ENDCLASS.

*---------------- Unit Tests ----------------------------------------------------*

CLASS lcl_logger IMPLEMENTATION.

  METHOD new.
    ro_log = NEW lcl_logger( ).
  ENDMETHOD.

  METHOD verify.
    cl_abap_unit_assert=>assert_equals( act = mt_log
                                        exp = it_exp
                                        msg = iv_msg ).
  ENDMETHOD.

  METHOD info.
    APPEND is_log TO mt_log.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_uml_logger IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_log = lcl_logger=>new( ).
  ENDMETHOD.

  METHOD log.
    mo_log->info( VALUE #( method = iv_method
                           params = iv_params ) ).
  ENDMETHOD.

  METHOD header.
    log( 'HEADER' ).
  ENDMETHOD.

  METHOD call.
    log( iv_method = 'CALL'
         iv_params = |{ is_message-id } { is_message-caller } { is_message-called
                          } { mi_actors->short_text( is_message ) }| ).
  ENDMETHOD.

  METHOD return.
    log( iv_method = 'RETURN'
         iv_params = |{ iv_from } { iv_to }| ).
  ENDMETHOD.

  METHOD participant.
    log( iv_method = 'PARTICIPANT'
         iv_params = |{ is_lifeline-index } { is_lifeline-label }| ).
  ENDMETHOD.

  METHOD footer.
    log( 'FOOTER' ).
  ENDMETHOD.

  METHOD skip_note.
    log( iv_method = 'SKIP_NOTE'
         iv_params = |{ is_path-called } { is_path-caller }| ).
  ENDMETHOD.

  METHOD begin_loop.
    log( iv_method = 'BEGIN_LOOP'
         iv_params = |{ iv_times }| ).
  ENDMETHOD.

  METHOD end_loop.
    log( 'END_LOOP' ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_trace_logger DEFINITION INHERITING FROM lcl_abap_trace
  FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS set_constructor REDEFINITION.
    METHODS set_meth REDEFINITION.
    METHODS set_form REDEFINITION.
    METHODS set_function_module REDEFINITION.
    METHODS set_submit REDEFINITION.
    METHODS set_tran REDEFINITION.

    METHODS module REDEFINITION.
    METHODS event REDEFINITION.
  PRIVATE SECTION.
    DATA mo_log TYPE REF TO lcl_logger.

    METHODS log IMPORTING iv_method TYPE lcl_logger=>ts_log-method
                          iv_params TYPE lcl_logger=>ts_log-params OPTIONAL.
ENDCLASS.

CLASS lcl_abap_trace_logger IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_log = lcl_logger=>new( ).
  ENDMETHOD.

  METHOD log.
    mo_log->info( VALUE #( method = iv_method
                           params = iv_params ) ).
  ENDMETHOD.

  METHOD set_constructor.
    log( iv_method = 'SET_CONSTRUCTOR'
         iv_params = |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_constructor( ).
  ENDMETHOD.

  METHOD set_meth.
    log( iv_method = 'SET_METH'
         iv_params =  |{ mr_src->progindex } { mr_src->textindex } { mr_src->methindex }| ).
    super->set_meth( ).
  ENDMETHOD.

  METHOD set_form.
    log( iv_method = 'SET_FORM'
         iv_params =  |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_form( ).
  ENDMETHOD.

  METHOD set_function_module.
    log( iv_method = 'SET_FUNCTION_MODULE'
         iv_params =  |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_function_module( ).
  ENDMETHOD.

  METHOD set_submit.
    log( iv_method = 'SET_SUBMIT'
         iv_params =  |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_submit( ).
  ENDMETHOD.

  METHOD set_tran.
    log( iv_method = 'SET_TRAN'
         iv_params = |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_tran( ).
  ENDMETHOD.

  METHOD module.
    log( iv_method = 'MODULE'
         iv_params = |{ iv_index }| ).
    super->module( iv_index ).
  ENDMETHOD.

  METHOD event.
    log( iv_method = 'EVENT'
         iv_params = |{ mr_src->textindex }| ).
    super->event( ).
  ENDMETHOD.

ENDCLASS.

CONSTANTS:
  c_my_program TYPE program VALUE 'Y_TEST_PROG'.

CLASS ltc_bag DEFINITION FOR TESTING CREATE PRIVATE RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    CONSTANTS c_index TYPE i VALUE 42.
    TYPES tt_index TYPE STANDARD TABLE OF sytabix WITH EMPTY KEY.
    DATA mo_bag TYPE REF TO lcl_bag.

    METHODS fixture IMPORTING it_index TYPE tt_index.
    METHODS teardown.

    METHODS remove_value FOR TESTING.
    METHODS remove_empty FOR TESTING.
    METHODS contains_empty FOR TESTING.
    METHODS contains_value FOR TESTING.
ENDCLASS.

CLASS ltc_bag IMPLEMENTATION.

  METHOD fixture.
    mo_bag = NEW #( ).
    LOOP AT it_index INTO DATA(lv_idx).
      mo_bag->add( lv_idx ).
    ENDLOOP.
  ENDMETHOD.

  METHOD teardown.
    FREE mo_bag.
  ENDMETHOD.

  METHOD contains_empty.
    fixture( VALUE #( ) ).
    cl_abap_unit_assert=>assert_false( act = mo_bag->contains( c_index )
                                       msg = 'LCL_BAG CONTAINS( ) No Entries Error' ).
  ENDMETHOD.

  METHOD contains_value.
    fixture( VALUE #( ( c_index ) ) ).
    cl_abap_unit_assert=>assert_true( act = mo_bag->contains( c_index )
                                      msg = 'LCL_BAG CONTAINS( ) One Entry Error' ).
  ENDMETHOD.

  METHOD remove_empty.
    fixture( VALUE #( ) ).
    mo_bag->remove( c_index ).
    cl_abap_unit_assert=>assert_false( act = mo_bag->contains( c_index )
                                       msg = 'LCL_BAG REMOVE( ) Empty Error' ).
  ENDMETHOD.

  METHOD remove_value.
    fixture( VALUE #( ( c_index ) ) ).
    mo_bag->remove( c_index ).
    cl_abap_unit_assert=>assert_false( act = mo_bag->contains( c_index )
                                       msg = 'LCL_BAG REMOVE( ) Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_satr_state DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_atra TYPE REF TO cl_atra_tool_se30_main.
    METHODS save.
    METHODS fill IMPORTING iv_textindex TYPE sytabix OPTIONAL
                           iv_methindex TYPE sytabix OPTIONAL
                           iv_progindex TYPE sytabix OPTIONAL.
    METHODS restore.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_satr_data,
             it_austab_hier TYPE satr_t_austab_gesamt,
             it_traceprog   TYPE satr_t_traceprog,
             it_tracetext   TYPE satr_t_tracetext,
             it_tracemeth   TYPE satr_t_tracemeth,
           END OF ts_satr_data.
    DATA mo_atra TYPE REF TO cl_atra_tool_se30_main.
    DATA ms_satr TYPE ts_satr_data.
ENDCLASS.

CLASS lcl_satr_state IMPLEMENTATION.

  METHOD constructor.
    mo_atra = io_atra.
    save( ).
  ENDMETHOD.

  METHOD fill.
    DEFINE m_fill.
      DO &1 TIMES.
        APPEND VALUE #( ) TO mo_atra->&2.
      ENDDO.
    END-OF-DEFINITION.

    m_fill: iv_textindex it_tracetext,
            iv_progindex it_traceprog,
            iv_methindex it_tracemeth.
  ENDMETHOD.

  METHOD save.
    ms_satr-it_austab_hier = mo_atra->it_austab_hier[].
    ms_satr-it_traceprog = mo_atra->it_traceprog[].
    ms_satr-it_tracetext = mo_atra->it_tracetext[].
    ms_satr-it_tracemeth = mo_atra->it_tracemeth[].
  ENDMETHOD.

  METHOD restore.
    mo_atra->it_austab_hier[] = ms_satr-it_austab_hier.
    mo_atra->it_traceprog[] = ms_satr-it_traceprog.
    mo_atra->it_tracetext[] = ms_satr-it_tracetext.
    mo_atra->it_tracemeth[] = ms_satr-it_tracemeth.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_abap_trace DEFINITION FOR TESTING
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    CONSTANTS:
      c_textindex TYPE sytabix VALUE 1,
      c_progindex TYPE sytabix VALUE 2,  " > 1
      c_methindex TYPE sytabix VALUE 3,
      c_fm_index  TYPE sytabix VALUE 4.
    DATA mo_trace TYPE REF TO lcl_abap_trace_logger.
    DATA mo_log TYPE REF TO lcl_logger.
    DATA mo_state TYPE REF TO lcl_satr_state.

    METHODS setup.
    METHODS teardown.
    METHODS parse IMPORTING iv_id        TYPE satr_austab_gesamt-id
                            iv_subid     TYPE satr_austab_gesamt-subid OPTIONAL
                            iv_methindex TYPE sytabix OPTIONAL
                            iv_fm_index  TYPE sytabix OPTIONAL.
    METHODS expected_after_set_form RETURNING VALUE(rt_exp) TYPE lcl_logger=>tt_log.

    METHODS parse_form_in_module FOR TESTING.
    METHODS parse_form_at_load FOR TESTING.
    METHODS parse_form FOR TESTING.
    METHODS parse_form_in_call_screen FOR TESTING.
    METHODS parse_form_in_call_dialog FOR TESTING.

    METHODS parse_function_module FOR TESTING.
    METHODS parse_method FOR TESTING.
    METHODS parse_constructor FOR TESTING.
    METHODS set_constructor FOR TESTING.
    METHODS parse_dynpro FOR TESTING.
    METHODS parse_module FOR TESTING.

    METHODS parse_submit FOR TESTING.
    METHODS parse_transaction FOR TESTING.
ENDCLASS.

CLASS ltc_abap_trace IMPLEMENTATION.

  METHOD setup.
    mo_trace = NEW lcl_abap_trace_logger( ).
    mo_log = mo_trace->mo_log.
    mo_state = NEW #( mo_trace->mo_atra ).
    mo_state->fill( iv_textindex = c_textindex
                    iv_progindex = nmax( val1 = c_progindex val2 = c_fm_index )
                    iv_methindex = c_methindex ).
  ENDMETHOD.

  METHOD teardown.
    mo_state->restore( ).
    FREE mo_state.
    FREE mo_trace.
  ENDMETHOD.

  METHOD parse.
    mo_trace->mo_atra->it_traceprog[ c_progindex ] = 'MY_PROG'.
    mo_trace->mr_src = NEW satr_austab_gesamt( id = iv_id
                                               subid = iv_subid
                                               textindex = c_textindex
                                               progindex = c_progindex
                                               methindex = iv_methindex
                                               pr_2index = iv_fm_index ).
    CLEAR mo_trace->ms_target.                                   " Called actor
    mo_trace->parse( ).
  ENDMETHOD.

  METHOD expected_after_set_form.
    rt_exp = VALUE #( ( method = 'SET_FORM' params = |{ c_progindex } { c_textindex }| )
                      ( method = 'EVENT'    params = |{ c_textindex }| )
                      ( method = 'MODULE'   params = |{ c_progindex }| ) ).
  ENDMETHOD.

  METHOD parse_form_in_module.
    parse( iv_id = c_id_module ).
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM in Module Error'  ).
  ENDMETHOD.

  METHOD parse_form_at_load.
    parse( iv_id = c_id_load
           iv_subid = 'D' ).  " Dynpro
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM at LOAD Error'  ).
  ENDMETHOD.

  METHOD parse_form_in_call_dialog.
    parse( iv_id = c_id_report     " CALL - e.g. Call SCREEN/CALL Report
           iv_subid = 'D' ).       " CALL Dialog OR Call Screen
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM in CALL Dialog' ).
  ENDMETHOD.

  METHOD parse_form_in_call_screen.
    parse( iv_id = c_id_report     " CALL - e.g. Call SCREEN/CALL Report
           iv_subid = 'S' ).       " CALL Dialog OR Call Screen ).       " Called actor
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM in CALL Screen' ).
  ENDMETHOD.

  METHOD parse_form.
    parse( iv_id = c_id_form ).
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM' ).
  ENDMETHOD.

  METHOD parse_constructor.
    parse( iv_id = c_id_class ).
    mo_log->verify( it_exp = VALUE #( ( method = 'SET_CONSTRUCTOR'
                                        params = |{ c_progindex } { c_textindex }| ) )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) SET CONSTRUCTOR Call Error' ).
  ENDMETHOD.

  METHOD set_constructor.
    parse( iv_id = c_id_class ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_trace->ms_target
      exp = VALUE lcl_abap_trace=>ts_event( type = c_type_class
                                            event = lcl_abap_trace=>c_create_method
                                            global = space )
      msg = 'LCL_ABAP_TRACE=>SET_CONSTRUCTOR( ) Error' ).
  ENDMETHOD.

  METHOD parse_method.
    parse( iv_id = c_id_method
           iv_methindex = c_methindex ).
    mo_log->verify( it_exp = VALUE #(
      ( method = 'SET_METH' params = |{ c_progindex } { c_textindex } { c_methindex }| )
      ( method = 'MODULE'   params = |{ c_progindex }| )
      ( method = 'MODULE'   params = |{ 0 }| )  )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) METHOD' ).
  ENDMETHOD.

  METHOD parse_function_module.
    parse( iv_id = c_id_func
           iv_fm_index = c_fm_index ).
    mo_log->verify( it_exp = VALUE #(
      ( method = 'SET_FUNCTION_MODULE' params = |{ c_progindex } { c_textindex }| )
      ( method = 'EVENT'               params = |{ c_textindex }| )
      ( method = 'MODULE'              params = |{ c_fm_index }| )  )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FUNCTION_MODULE' ).
  ENDMETHOD.

  METHOD parse_module.
    parse( iv_id = c_id_report
           iv_subid = 'P' ).
    mo_log->verify( it_exp = VALUE #( ( method = 'MODULE'  params = |{ c_progindex }| ) )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) MODULE' ).
  ENDMETHOD.

  METHOD parse_dynpro.
    parse( iv_id = c_id_dynpro
           iv_subid = 'P' ).
    mo_log->verify( it_exp = VALUE #( ( method = 'MODULE'  params = |{ c_progindex }| ) )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) MODULE Dynpro' ).
  ENDMETHOD.

  METHOD parse_submit.
    CONSTANTS:
      c_mod_index TYPE sytabix VALUE 1,    " larger 0, smaller than c_progindex
      c_my_event  TYPE program VALUE 'MY_EVENT'.
    mo_trace->mo_atra->it_tracetext[ c_textindex ]-tracetext = c_my_event.
    mo_trace->mo_atra->it_traceprog[ c_mod_index ] = c_my_event.

    parse( iv_id = c_id_report
           iv_subid = 'R' ).
    mo_log->verify( it_exp = VALUE #( ( method = 'SET_SUBMIT' params = |{ c_progindex } { c_textindex }| )
                                      ( method = 'EVENT'   params = |{ c_textindex }| )
                                      ( method = 'MODULE'  params = |{ c_mod_index }| )  )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) SET_SUBMIT Error' ).
  ENDMETHOD.

  METHOD parse_transaction.
    parse( iv_id = c_id_report
           iv_subid = 'T' ).
    mo_log->verify( it_exp = VALUE #( ( method = 'SET_TRAN' params = |{ c_progindex } { c_textindex }| )
                                      ( method = 'EVENT'    params = |{ c_textindex }| ) )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) SET_TRAN Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_class DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mo_class_name TYPE REF TO lcl_class_name.

    METHODS setup.
    METHODS teardown.

    METHODS section_name FOR TESTING.
    METHODS technical_name FOR TESTING.
    METHODS external_name FOR TESTING.

    METHODS is_global FOR TESTING.
    METHODS is_not_global FOR TESTING.
ENDCLASS.

CLASS ltc_sequence DEFINITION FOR TESTING CREATE PRIVATE RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.

    TYPES tt_sat TYPE STANDARD TABLE OF ts_sat WITH KEY aus_tabix
                 WITH NON-UNIQUE SORTED KEY k1 COMPONENTS aus_tabix. " Performance ?

    CLASS-METHODS new_sequence IMPORTING is_cfg             TYPE ts_diagram_config
                                         it_sat             TYPE tt_sat
                               RETURNING VALUE(ro_sequence) TYPE REF TO lcl_sequence.

    CLASS-METHODS new_collection IMPORTING is_cfg             TYPE ts_diagram_config
                                           it_sat             TYPE tt_sat
                                 RETURNING VALUE(ro_sequence) TYPE REF TO lcl_sequence.

    CLASS-METHODS sat_fixture_events RETURNING VALUE(rt_sat) TYPE tt_sat.
  PRIVATE SECTION.
    DATA mo_sequence TYPE REF TO lcl_sequence.

    METHODS setup.
    METHODS teardown.

    METHODS collect FOR TESTING.
    METHODS add FOR TESTING.
ENDCLASS.

CLASS ltc_sequence IMPLEMENTATION.

  METHOD new_sequence.
    ro_sequence = NEW lcl_sequence( is_cfg ).
*   insert into collection
    LOOP AT it_sat ASSIGNING FIELD-SYMBOL(<ls_sat>).
      ro_sequence->lif_collector~collect( <ls_sat> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD new_collection.
    ro_sequence = NEW lcl_sequence( is_cfg ).
*   add to collection
    LOOP AT it_sat ASSIGNING FIELD-SYMBOL(<ls_sat>).
      ro_sequence->add( <ls_sat> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD sat_fixture_events.
    CONSTANTS:
      c_prog  TYPE program VALUE 'Y_TEST_PROG',
      c_ctrl  TYPE program VALUE 'LCL_CTRL',
      c_model TYPE program VALUE 'LCL_MODEL'.

    rt_sat = VALUE tt_sat( id = 'm'
     ( from_level = 1
       caller = VALUE #( global = c_prog type = c_type_prog )
       called = VALUE #( global = c_prog type = c_type_class
                         local = c_ctrl mod = 'MAIN' ) )
     ( from_level = 2
       caller = VALUE #( global = c_prog type = c_type_class
                         local = c_ctrl mod = 'MAIN' )
       called = VALUE #( global = 'SAPPROG' type = c_type_class instance = 6
                         local = c_model mod = lcl_abap_trace=>c_create_method ) )
     ( from_level = 2
       caller = VALUE #( global = 'SAPPROG' type = c_type_class
                         local = c_model mod = 'SETUP' )
       called = VALUE #( global = c_prog type = c_type_class instance = 0
                         local = c_ctrl mod = 'CHANGE' ) )        ).
  ENDMETHOD.

  METHOD setup.
    mo_sequence = new_sequence( it_sat = sat_fixture_events( )
                                is_cfg = lcl_configuration=>get( ) ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_sequence.
  ENDMETHOD.

  METHOD collect.
    cl_abap_unit_assert=>assert_equals( act = mo_sequence->mt_trace
                                        exp = VALUE tt_message(
     ( id = 'm' from_level = 1  caller = 1  called = 2 msg = 'MAIN'   )
     ( id = 'K' from_level = 1  caller = 2  called = 3 msg = 'SETUP' )  "<- Skipped
     ( id = 'm' from_level = 2  caller = 3  called = 2 msg = 'CHANGE' ) )
                                        msg = 'LCL_SEQUENCE->COLLECT( ) Error' ).
  ENDMETHOD.

  METHOD add.
    mo_sequence = new_collection( it_sat = sat_fixture_events( )
                                  is_cfg = lcl_configuration=>get( ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_sequence->mt_trace
                                        exp = VALUE tt_message(
     ( id = 'm' from_level = 1  caller = 1  called = 2 msg = 'MAIN'     )
     ( id = 'm' from_level = 2  caller = 2  called = 3 msg = lcl_abap_trace=>c_create_method )
     ( id = 'm' from_level = 2  caller = 4  called = 2 msg = 'CHANGE'   ) )
                                        msg = 'LCL_SEQUENCE->ADD( ) Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_class IMPLEMENTATION.

  METHOD setup.
    mo_class_name = NEW #( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_class_name.
  ENDMETHOD.

  METHOD section_name.
    cl_abap_unit_assert=>assert_equals( act = mo_class_name->to_internal( '/IDV0/JN_CL_ASM_ACT' )
                                        exp = '/IDV0/JN_CL_ASM_ACT==========='
                                        msg = 'LCL_CLASS SECTION_NAME( ) Translate Error' ).
  ENDMETHOD.

  METHOD technical_name.
    cl_abap_unit_assert=>assert_equals( act = mo_class_name->technical( 'CL_GUI_ALV_GRID' )
                                        exp = 'CL_GUI_ALV_GRID===============CP'
                                        msg = 'LCL_CLASS TECHNICAL_NAME( ) Error' ).
  ENDMETHOD.

  METHOD external_name.
    DATA lv_class TYPE program VALUE '/IDV0/JN_CL_ASM_ACT===========CP'.
    mo_class_name->external( CHANGING cv_clas = lv_class ).
    cl_abap_unit_assert=>assert_equals( act = lv_class
                                        exp = '/IDV0/JN_CL_ASM_ACT'
                                        msg = 'LCL_CLASS EXTERNAL_NAME( ) Error' ).
  ENDMETHOD.

  METHOD is_global.
    cl_abap_unit_assert=>assert_true( act = mo_class_name->is_global( 'CL_ABAP_MEMORY_X_WRITER' )
                                      msg = 'LCL_ABAP_TRACE=>IS_GLOBAL( ) Error' ).
  ENDMETHOD.

  METHOD is_not_global.
    cl_abap_unit_assert=>assert_false( act = mo_class_name->is_global( 'LCL_BAG' )
                                       msg = 'LCL_ABAP_TRACE=>IS_GLOBAL( ) Local Class Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_actors_factory DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    CONSTANTS:
      c_prog  TYPE program VALUE 'Y_TEST_PROG',
      c_ctrl  TYPE program VALUE 'LCL_CTRL',
      c_model TYPE program VALUE 'LCL_MODEL'.
    INTERFACES lif_unit_test.

    CLASS-METHODS sat_fixture_record RETURNING VALUE(rs_sat) TYPE ts_sat.
    CLASS-METHODS sat_fixture_plant_uml  RETURNING VALUE(rt_sat) TYPE ltc_sequence=>tt_sat.
    CLASS-METHODS sat_fixture_messages RETURNING VALUE(rt_sat) TYPE ltc_sequence=>tt_sat.
  PRIVATE SECTION.
    DATA mi_path TYPE REF TO lif_path.
    DATA ms_exp TYPE ts_sat.

    METHODS constructor.
    METHODS setup.
    METHODS teardown.

    METHODS get_count RETURNING VALUE(rv_count) TYPE i.
    METHODS put FOR TESTING.
ENDCLASS.

CLASS ltc_actors_factory IMPLEMENTATION.

  METHOD constructor.
    ms_exp = VALUE #( id = 1 from_level = 1
                      caller = VALUE #( global = c_my_program
                                        type = c_type_fugr )
                      called = VALUE #( global = c_my_program local = c_model
                                        type = c_type_class
                                        instance = 1
                                        mod = lcl_abap_trace=>c_create_method ) ).
  ENDMETHOD.

  METHOD sat_fixture_messages.
    DATA(ls_main) = VALUE ts_call( global = c_prog type = c_type_class local = c_ctrl mod = 'MAIN' ).
    rt_sat = VALUE ltc_sequence=>tt_sat( id = 'm'
       ( from_level = 1
         caller = VALUE #( global = c_prog type = c_type_prog )
         called = ls_main )
       ( from_level = 2
         caller = ls_main
         called = VALUE #( global = c_prog type = c_type_class instance = 6
                           local = c_model mod = lcl_abap_trace=>c_create_method ) )
       ( from_level = 2
         caller = ls_main
         called = VALUE #( global = c_prog type = c_type_class  local = c_ctrl  mod = 'CHANGE' ) )
       ( from_level = 3
         caller = VALUE #( global = c_prog type = c_type_class  local = c_ctrl  mod = 'CHANGE' )
         called = VALUE #( global = c_prog type = c_type_class  instance = 6 local = c_model mod = 'UP' ) )
        ) .
  ENDMETHOD.

  METHOD sat_fixture_plant_uml.
    rt_sat = VALUE ltc_sequence=>tt_sat(
      ( id = 1 from_level = 1
        caller = VALUE #( global = c_my_program type = c_type_prog )
        called = VALUE #( type = c_type_class instance = 0
                          global = c_my_program local = 'LCL_CTRL' mod = 'MAIN' ) )
      ( id = 'm' from_level = 2
        caller = VALUE #( type = c_type_class instance = 0
                          global = c_my_program local = 'LCL_CTRL' mod = 'MAIN' )
        called = VALUE #( type = c_type_class instance = 1
                          global = c_my_program local = 'LCL_MODEL' mod = lcl_abap_trace=>c_create_method ) )
     ( id = ' ' from_level = 3
         caller = VALUE #( type = c_type_class instance = 447
                           global = c_my_program local = 'LCL_CALL_STACK' mod = 'PLANTUML' )
         called = VALUE #( type = c_type_class instance = 448
                           global = c_my_program local = 'LCL_ITERATOR' mod = 'HAS_NEXT' ) )  ).
  ENDMETHOD.

  METHOD sat_fixture_record.
    rs_sat = VALUE ts_sat( id = 'm' from_level = 1
                           caller = VALUE #( global = c_prog type = c_type_prog )
                           called = VALUE #( global = c_prog type = c_type_class
                                             local = 'LCL_CTRL' mod = 'MAIN' )
                           aus_tabix = 1 ).
  ENDMETHOD.

  METHOD setup.
    mi_path = NEW lcl_actors( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mi_path.
  ENDMETHOD.

  METHOD get_count.
    rv_count = lines( CAST lcl_actors( mi_path )->mt_actor ).
  ENDMETHOD.

  METHOD put.
    mi_path->to_path( ms_exp ).
    DATA(lv_count) = get_count( ).
    mi_path->to_path( ms_exp ).  " <-- Duplicate
    cl_abap_unit_assert=>assert_equals( act = get_count( )
                                        exp = lv_count
                                        msg = 'LCL_ACTORS->PUT( ) Duplicate Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_actors DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    CONSTANTS c_my_class TYPE program VALUE 'ZCL_TEST_CLASS'.
    DATA mo_actors TYPE REF TO lcl_actors.
    DATA ms_exp TYPE ts_sat.

    METHODS setup.
    METHODS teardown.

    METHODS call_constructor FOR TESTING.

    METHODS get_text_class IMPORTING iv_id  TYPE satr_de_id
                                     iv_exp TYPE string
                                     iv_msg TYPE string.
    METHODS get_text_global_class FOR TESTING.
    METHODS get_text_local_class FOR TESTING.
ENDCLASS.

CLASS ltc_actors IMPLEMENTATION.

  METHOD setup.
    mo_actors = NEW lcl_actors( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_actors.
  ENDMETHOD.

  METHOD get_text_class.
    ms_exp = VALUE #( id = 1 from_level = 1
                      caller = VALUE #( global = c_my_program
                                        type = c_type_fugr )
                      called = VALUE #( global = c_my_class
                                        type = c_type_class
                                        instance = 1
                                        mod = lcl_abap_trace=>c_create_method ) ).
    mo_actors->lif_path~to_path( ms_exp ).
    DATA(ls_msg) = VALUE ts_message( id = iv_id from_level = 1
      path-caller = 1 path-called = 2 msg = 'CONSTRUCTOR' ).
    cl_abap_unit_assert=>assert_equals( act = mo_actors->lif_actors~short_text( ls_msg )
                                        exp = iv_exp
                                        msg = |LCL_ACTORS->GET_TEXT( ) { iv_msg }| ).
  ENDMETHOD.

  METHOD get_text_global_class.
    get_text_class( iv_id = c_id_class
                    iv_exp = |Create instance of class { c_my_class }|
                    iv_msg = 'Global Class Get Text Error' ).
  ENDMETHOD.

  METHOD get_text_local_class.
    get_text_class( iv_id = c_id_method
                    iv_exp = |Call method CONSTRUCTOR|
                    iv_msg = 'Local Class Get Text Error' ).
  ENDMETHOD.

  METHOD call_constructor.
    DATA(ls_path) = mo_actors->lif_path~to_path(
      VALUE ts_sat( called = VALUE #( local    = 'LCL_TEST'
                                      type     = c_type_class ) ) ).
    DATA(lo_uml) = lcl_uml=>new( iv_uml = lcl_uml=>c_uml_plant
                                 ii_actors = mo_actors ).
    DATA(lv_exp) = lo_uml->mv_diagram &&
      |3 -> { ls_path-called }: Create instance of class LCL_TEST\nactivate { ls_path-called }\n|.
    lo_uml->message( VALUE ts_message( id = c_id_class
                                       caller = 3
                                       called = ls_path-called
                                       from_level = 2
                                       msg = 'CONSTRUCTOR' ) ).
    cl_abap_unit_assert=>assert_equals( act = lo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT MESSAGE( ) Factory' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_stack DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA ms_exp TYPE lcl_stack=>ts_call_level.
    DATA mo_stack TYPE REF TO lcl_stack.   " CUT

    METHODS setup.
    METHODS teardown.

    METHODS is_empty FOR TESTING.
    METHODS is_empty_set FOR TESTING.
    METHODS is_empty_reset FOR TESTING.
    METHODS push FOR TESTING.
    METHODS pop FOR TESTING.
    METHODS pop_empty FOR TESTING.
ENDCLASS.

CLASS ltc_stack IMPLEMENTATION.

  METHOD setup.
    mo_stack = NEW #( ).
    ms_exp = VALUE #( actor_key = 1
                      from_level = 1 ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_stack.
  ENDMETHOD.

  METHOD is_empty.
    cl_abap_unit_assert=>assert_not_bound( act = mo_stack->mr_top
                                           msg = 'LCL_STACK is not Empty' ).
  ENDMETHOD.

  METHOD pop.
    mo_stack->push( ms_exp ).
    cl_abap_unit_assert=>assert_equals( act = mo_stack->pop( )
                                        exp = ms_exp
                                        msg = 'LCL_STACK POP( ) Error' ).
  ENDMETHOD.

  METHOD push.
    mo_stack->push( ms_exp ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( ( ms_exp EQ mo_stack->pop( ) )
                                                AND NOT mo_stack->has_next( ) )
                                      msg = 'LCL_STACK PUSH( ) Error' ).
  ENDMETHOD.

  METHOD pop_empty.
    cl_abap_unit_assert=>assert_initial( act = mo_stack->pop( )
                                         msg = 'LCL_STACK POP( ) Empty' ).
  ENDMETHOD.

  METHOD is_empty_reset.
    mo_stack->push( ms_exp ).
    cl_abap_unit_assert=>assert_bound( act = mo_stack->mr_top
                                       msg = 'LCL_STACK Empty not reset/PUSH( )' ).
  ENDMETHOD.

  METHOD is_empty_set.
    mo_stack->push( ms_exp ).
    mo_stack->pop( ).
    cl_abap_unit_assert=>assert_not_bound( act = mo_stack->mr_top
                                           msg = 'LCL_STACK Empty not set/POP( )' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_call DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PROTECTED SECTION.
    DATA mo_uml TYPE REF TO lcl_uml_logger.
    DATA mo_actors TYPE REF TO lcl_actors.
    DATA ms_exp TYPE lcl_stack=>ts_call_level.

    METHODS create_uml.
    METHODS free_uml.
  PRIVATE SECTION.
    CONSTANTS c_level TYPE i VALUE 42.

    DATA mo_stack TYPE REF TO lcl_uml_stack.   " CUT

    METHODS setup.
    METHODS teardown.
    METHODS fixture_message RETURNING VALUE(rs_message) TYPE ts_message.

    METHODS call FOR TESTING.
    METHODS return FOR TESTING.
    METHODS first_call FOR TESTING.
ENDCLASS.

CLASS ltc_call IMPLEMENTATION.

  METHOD create_uml.
    mo_actors = NEW #( ).
    mo_uml ?= lcl_uml=>new( iv_uml = lcl_uml=>c_uml_mock
                           ii_actors = mo_actors ).
  ENDMETHOD.

  METHOD setup.
    create_uml( ).
    mo_stack = NEW #( io_uml = mo_uml ).
    ms_exp = VALUE #( actor_key = 1
                      from_level = 1 ).
  ENDMETHOD.

  METHOD free_uml.
    FREE mo_uml.
    FREE mo_actors.
  ENDMETHOD.

  METHOD teardown.
    FREE mo_stack.
    free_uml( ).
  ENDMETHOD.

  METHOD call.
    mo_stack->call( fixture_message( ) ).
    mo_uml->mo_log->verify( it_exp = VALUE lcl_logger=>tt_log(
                                         ( method = 'CALL'
                                           params = 'm 1 2 Call method Test Call' )  )
                            iv_msg = 'LCL_UML_STACK MESSAGE Error' ).
  ENDMETHOD.

  METHOD fixture_message.
    rs_message = VALUE ts_message( id         = 'm'
                                   from_level = c_level
                                   caller = 1
                                   called = 2
                                   msg  = 'Test Call' ).
  ENDMETHOD.

  METHOD return.
    DATA(ls_message) = fixture_message( ).
    mo_stack->call( ls_message ).
    ls_message-caller = 2.
    ls_message-called = 3.
    mo_stack->call( ls_message ).
    mo_stack->return_to( c_level ).
    mo_uml->mo_log->verify( it_exp = VALUE lcl_logger=>tt_log(
                                         ( method = 'CALL'
                                           params = 'm 1 2 Call method Test Call' )
                                         ( method = 'CALL'
                                           params = 'm 2 3 Call method Test Call' )
                                         ( method = 'RETURN' params = '3 2' )  )
                            iv_msg = 'LCL_UML_STACK RETURN Error' ).
  ENDMETHOD.

  METHOD first_call.
    create_uml( ).
    NEW lcl_messages( it_messages = VALUE #( ( id = 'm'
                                               from_level = c_level
                                               caller = 1
                                               called = 2
                                               msg = 'TEST' ) ) )->to_uml( mo_uml ).
    mo_uml->mo_log->verify( it_exp = VALUE lcl_logger=>tt_log(
          ( method = 'CALL'   params = | { c_level } { lcl_messages=>c_first_key } | )
          ( method = 'CALL'   params = 'm 1 2 Call method TEST' )
          ( method = 'RETURN' params = '2 1' ) )
                            iv_msg = 'LCL_UML_STACK MESSAGE FIRST CALL Error' ).
    free_uml( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_iterator DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mo_iter TYPE REF TO lcl_messages.   " CUT
    DATA mt_messages TYPE tt_message.

    METHODS constructor.
    METHODS setup.
    METHODS teardown.

    METHODS has_next_after_init FOR TESTING.
    METHODS has_next_after_end FOR TESTING.
    METHODS skip_once FOR TESTING.
    METHODS skip_3_times FOR TESTING.
    METHODS next FOR TESTING.
    METHODS next_error FOR TESTING.
    METHODS:
      has_previous_after_init FOR TESTING,
      has_previous_at_first FOR TESTING,
      has_previous_after_first FOR TESTING.
ENDCLASS.

CLASS ltc_iterator IMPLEMENTATION.

  METHOD constructor.
    CONSTANTS c_main TYPE tv_index VALUE 2.
    mt_messages = VALUE #(
      ( id = 1 from_level = 1 caller = 1 called = c_main msg = 'MAIN' )  " PROG -> Main
      ( id = 2 from_level = 2 caller = c_main called = 2 msg = lcl_abap_trace=>c_create_method )  " Main -> Model=>Constructor
      ( id = 3 from_level = 1 caller = c_main called = 3 msg = 'QUERY' )  " Main -> Model->Query
      ( id = 4 from_level = 2 caller = 3 called = 4 msg = 'FILTER' )   ).  " Model->Query -> Model->Filter
  ENDMETHOD.

  METHOD setup.
    mo_iter = NEW lcl_messages( mt_messages ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_iter.
  ENDMETHOD.

  METHOD skip_once.
    mo_iter->skip( ).
    cl_abap_unit_assert=>assert_equals( act = mo_iter->mv_idx
                                        exp = 1
                                        msg = 'LCL_MESSAGES SKIP( ) Error' ).
  ENDMETHOD.

  METHOD skip_3_times.
    mo_iter->skip( 3 ).
    cl_abap_unit_assert=>assert_equals( act = mo_iter->mv_idx
                                        exp = 3
                                        msg = 'LCL_MESSAGES SKIP( 2 ) Error' ).
  ENDMETHOD.

  METHOD has_next_after_init.
    cl_abap_unit_assert=>assert_true( act = mo_iter->has_next( )
                                      msg = 'LCL_MESSAGES HAS_NEXT( ) Start Error' ).
  ENDMETHOD.

  METHOD has_next_after_end.
    mo_iter->skip( mo_iter->mv_size ).
    cl_abap_unit_assert=>assert_false( act = mo_iter->has_next( )
                                       msg = 'LCL_MESSAGES HAS_NEXT( ) End Error' ).
  ENDMETHOD.

  METHOD next.
    cl_abap_unit_assert=>assert_equals( act = mo_iter->next( )
                                        exp = VALUE #( mo_iter->mt_list[ 1 ] )
                                        msg = 'LCL_MESSAGES NEXT( ) Error' ).
  ENDMETHOD.

  METHOD next_error.
    TRY.
        mo_iter->skip( mo_iter->mv_size ).
        mo_iter->next( ).
        cl_abap_unit_assert=>fail( msg = 'LCL_MESSAGES NEXT( ) No Exception' ).
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER. " Do nothing
    ENDTRY.
  ENDMETHOD.

  METHOD has_previous_after_init.
    cl_abap_unit_assert=>assert_true( act = mo_iter->is_first( )
                                      msg = 'LCL_MESSAGES IS_FIRST( ) Error' ).
  ENDMETHOD.

  METHOD has_previous_at_first.
    mo_iter->next( ).
    cl_abap_unit_assert=>assert_true( act = mo_iter->is_first( )
                                      msg = 'LCL_MESSAGES IS_FIRST( ) First' ).
  ENDMETHOD.

  METHOD has_previous_after_first.
    mo_iter->next( ).
    mo_iter->next( ).
    cl_abap_unit_assert=>assert_false( act = mo_iter->is_first( )
                                       msg = 'LCL_MESSAGES IS_FIRST( ) Next' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_uml DEFINITION FOR TESTING ABSTRACT
  CREATE PROTECTED RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PROTECTED SECTION.
    DATA mi_actors TYPE REF TO lif_actors.
    DATA mo_uml TYPE REF TO lcl_uml.   " CUT
    DATA mv_obj_name TYPE sobj_name.

    METHODS test_participant IMPORTING iv_exp TYPE string.
    METHODS test_message IMPORTING iv_exp TYPE string.
    METHODS test_return IMPORTING iv_exp TYPE string.
    METHODS test_footer IMPORTING iv_exp TYPE string.

    METHODS setup_uml IMPORTING iv_uml  TYPE lcl_uml=>tv_uml
                                iv_name TYPE sobj_name.
    METHODS teardown_uml.
ENDCLASS.

CLASS ltc_uml IMPLEMENTATION.

  METHOD setup_uml.
    mi_actors = NEW lcl_actors( ).
    mo_uml = lcl_uml=>new( iv_uml = iv_uml
                           ii_actors = mi_actors ).
    mv_obj_name = iv_name.
  ENDMETHOD.

  METHOD teardown_uml.
    FREE mo_uml.
    FREE mi_actors.
  ENDMETHOD.

  METHOD test_participant.
    DATA(lv_exp) = mo_uml->mv_diagram && iv_exp.
    mo_uml->participant( is_lifeline = VALUE #( label = `Test Participant`
                                                index = 42 ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = mv_obj_name && ` PARTICIPANT( ) Error` ).
  ENDMETHOD.

  METHOD test_message.
    DATA(lv_exp) = mo_uml->mv_diagram && iv_exp.
    mo_uml->message( VALUE ts_message( id = c_id_method
                                       caller = 3
                                       called = 5
                                       from_level = 2
                                       msg = 'MAIN' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = mv_obj_name && ` MESSAGE( ) Error` ).
  ENDMETHOD.

  METHOD test_return.
    DATA(lv_exp) = mo_uml->mv_diagram && iv_exp.
    mo_uml->return( iv_from = 2
                    iv_to   = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = mv_obj_name && ` RETURN( ) Error` ).
  ENDMETHOD.

  METHOD test_footer.
    DATA(lv_exp) = mo_uml->mv_diagram && iv_exp.
    mo_uml->footer( ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = mv_obj_name && ` FOOTER( ) Error` ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_plant_uml DEFINITION INHERITING FROM ltc_uml
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS setup.
    METHODS teardown.

    METHODS add FOR TESTING.

    METHODS message FOR TESTING.
    METHODS call_and_skip FOR TESTING.
    METHODS initial_message FOR TESTING.
    METHODS first_message FOR TESTING.

    METHODS return FOR TESTING.
    METHODS return_from_initial FOR TESTING.
    METHODS return_same_level FOR TESTING.
    METHODS participant FOR TESTING.
    METHODS plantuml_header FOR TESTING.
    METHODS footer FOR TESTING.
ENDCLASS.

CLASS ltc_plant_uml IMPLEMENTATION.

  METHOD setup.
    setup_uml( iv_uml = lcl_uml=>c_uml_plant
               iv_name = 'LCL_PLANT_UML' ).
  ENDMETHOD.

  METHOD teardown.
    teardown_uml( ).
  ENDMETHOD.

  METHOD add.
    CONSTANTS c_code TYPE string VALUE 'New PlantUML Code'.

    DATA(lv_before) = mo_uml->mv_diagram.
    mo_uml->add( c_code ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_before && c_code
                                        msg = 'LCL_UML_PLANT ADD( ) Error' ).
  ENDMETHOD.

  METHOD message.
    test_message( |3 -> 5: Call method MAIN\nactivate 5\n| ).
  ENDMETHOD.

  METHOD call_and_skip.
    DATA(lv_exp) = mo_uml->mv_diagram &&
                  |1 -> 2: <b>Skipping over SAP code until calling MODIFIER</b>\n|
               && |activate 2\nnote over 1,2\n{ lcl_uml_plant=>c_txt_std }\nend note\n|.

    mo_uml->message( VALUE ts_message( id = c_id_skip
                                       from_level = 1
                                       msg = 'MODIFIER'
                                       caller = 1
                                       called = 2 ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT MESSAGE( ) Skip' ).
  ENDMETHOD.

  METHOD first_message.
    DATA(lv_exp) = mo_uml->mv_diagram && |{ lcl_messages=>c_first_key } -> 2: \nactivate 2\n|.
    mo_uml->message( is_message = VALUE ts_message( caller = lcl_messages=>c_first_key
                                                    called = 2 ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT MESSAGE( ) First' ).
  ENDMETHOD.

  METHOD initial_message.
    DATA(lv_exp) = mo_uml->mv_diagram.
    mo_uml->message( is_message = VALUE ts_message(  ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT MESSAGE( ) Initial' ).
  ENDMETHOD.

  METHOD return.
    test_return( |2 --> 1\ndeactivate 2\n| ).
  ENDMETHOD.

  METHOD return_from_initial.
    DATA(lv_exp) = mo_uml->mv_diagram && |0 --> 1\n|.
    mo_uml->return( iv_from = 0
                    iv_to   = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT RETURN( ) From Initial Error' ).
  ENDMETHOD.

  METHOD return_same_level.
    DATA(lv_exp) = mo_uml->mv_diagram && |deactivate 3\n|.
    mo_uml->return( iv_from = 3
                    iv_to   = 3 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT RETURN( ) Same Level' ).
  ENDMETHOD.

  METHOD participant.
    test_participant( |participant "Test Participant" as 42\n| ).
  ENDMETHOD.

  METHOD plantuml_header.
    cl_abap_unit_assert=>assert_equals( act = CAST lcl_uml_plant( mo_uml )->scale( lcl_configuration=>get( ) )
                                        exp = |scale 0.50\n|
                                        msg = 'LCL_UML_PLANT->SCALE( ) Error' ).
  ENDMETHOD.

  METHOD footer.
    test_footer( |@enduml\n| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_graph_uml DEFINITION INHERITING FROM ltc_uml
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS setup.
    METHODS teardown.

    METHODS call FOR TESTING.
    METHODS return FOR TESTING.
    METHODS participant FOR TESTING.
    METHODS footer FOR TESTING.
ENDCLASS.

CLASS ltc_graph_uml IMPLEMENTATION.

  METHOD setup.
    setup_uml( iv_uml = lcl_uml=>c_uml_graph
               iv_name = 'LCL_GRAPH_UML' ).
  ENDMETHOD.

  METHOD teardown.
    teardown_uml( ).
  ENDMETHOD.

  METHOD call.
    test_message( |message(3,5,"Call method MAIN");\nactive(5);\n| ).
  ENDMETHOD.

  METHOD return.
    test_return( |return_message(2,1);\ninactive(2);\n| ).
  ENDMETHOD.

  METHOD participant.
    test_participant( |object(42,"Test Participant");\n| ).
  ENDMETHOD.

  METHOD footer.
    test_footer( |.PE\n| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_mscgen_uml DEFINITION INHERITING FROM ltc_uml
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS setup.
    METHODS teardown.

    METHODS call FOR TESTING.
    METHODS return FOR TESTING.
    METHODS participant FOR TESTING.
    METHODS footer FOR TESTING.
ENDCLASS.

CLASS ltc_mscgen_uml IMPLEMENTATION.

  METHOD setup.
    setup_uml( iv_uml = lcl_uml=>c_uml_mscgen
               iv_name = 'LCL_MSCGEN_UML' ).
  ENDMETHOD.

  METHOD teardown.
    teardown_uml( ).
  ENDMETHOD.

  METHOD call.
    test_message( |3=>5 [label="Call method MAIN"];\n| ).
  ENDMETHOD.

  METHOD return.
    test_return( |1<<2\n| ).
  ENDMETHOD.

  METHOD participant.
    test_participant( |42 [label="Test Participant"]| ).
  ENDMETHOD.

  METHOD footer.
    test_footer( |\}\n| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_file_name DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    CONSTANTS:
      c_path   TYPE string VALUE 'C:\Temp\Dokumente\UML\',
      c_prefix TYPE string VALUE 'test' ##NO_TEXT.
    DATA mo_name TYPE REF TO lcl_file_name.

    METHODS setup.
    METHODS teardown.
    METHODS fixture.

    METHODS prefix_with_txt_extension FOR TESTING.
    METHODS prefix_without_extension FOR TESTING.
    METHODS get_fullpath FOR TESTING.
ENDCLASS.

CLASS ltc_file_name IMPLEMENTATION.

  METHOD setup.
    mo_name = lcl_file_name=>new( lcl_diagram_text=>c_mode_txt ).
    fixture( ).
  ENDMETHOD.

  METHOD fixture.
    mo_name->ms_file-name = c_prefix && mo_name->ms_file-ext.
    mo_name->ms_file-path = c_path && mo_name->ms_file-name.
  ENDMETHOD.

  METHOD teardown.
    FREE mo_name.
  ENDMETHOD.

  METHOD prefix_with_txt_extension.
    cl_abap_unit_assert=>assert_equals( exp = c_prefix
                                        act = mo_name->get_prefix( )
                                        msg = 'LCL_FILE_NAME->GET_PREFIX( ) Error' ).
  ENDMETHOD.

  METHOD prefix_without_extension.
    mo_name->ms_file-ext = space.
    fixture( ).
    cl_abap_unit_assert=>assert_equals( exp = c_prefix
                                        act = mo_name->get_prefix( )
                                        msg = 'LCL_FILE_NAME->GET_PREFIX( ) Error No Prefix' ).
  ENDMETHOD.

  METHOD get_fullpath.
    cl_abap_unit_assert=>assert_equals( exp = mo_name->ms_file-path
                                        act = mo_name->get_fullpath( )
                                        msg = 'LCL_FILE_NAME->GET_FULLPATH( ) Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_diagram DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mo_gen TYPE REF TO lcl_diagram_plant_uml.

    METHODS setup.
    METHODS teardown.
    METHODS set_diagram IMPORTING iv_text TYPE string.

    METHODS to_xstring FOR TESTING.
    METHODS to_url FOR TESTING.
ENDCLASS.

CLASS ltc_diagram IMPLEMENTATION.

  METHOD setup.
    lcl_configuration=>gs_cfg-skip_dialog = abap_true.
    lcl_configuration=>gs_cfg-output_mode = lcl_diagram_text=>c_mode_aut.
    mo_gen ?= lcl_sequence=>to_diagram( lcl_configuration=>get( ) ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_gen.
  ENDMETHOD.

  METHOD set_diagram.
    mo_gen->mv_diagram = iv_text.
  ENDMETHOD.

  METHOD to_url.
    set_diagram( |Bob -> Alice : hello| ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'SyfFKj2rKt3CoKnELR1Io4ZDoSa70000'
      act = mo_gen->to_url( iv_base_url = space )
      msg = 'LCL_SEQ_GEN TO_URL( ) Error' ).
  ENDMETHOD.

  METHOD to_xstring.
    set_diagram( 'Polyfon zwitschernd aßen Mäxchens Vögel Rüben, Joghurt und Quark' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'UG9seWZvbiB6d2l0c2NoZXJuZCBhw59lbiBNw6R4Y2hlbnMgVsO2Z2VsIFLDvGJlbiwgSm9naHVydCB1bmQgUXVhcms='
      act = cl_http_utility=>encode_x_base64( mo_gen->to_xstring( mo_gen->mv_diagram ) )
      msg = 'LCL_SEQ_GEN TO_XSTRING( ) Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_messages DEFINITION FOR TESTING
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
    METHODS constructor.
  PROTECTED SECTION.
    DATA mt_sat TYPE ltc_sequence=>tt_sat.
    DATA mo_uml TYPE REF TO lcl_uml.

    METHODS to_uml IMPORTING it_sat           TYPE ltc_sequence=>tt_sat
                             iv_uml           TYPE lcl_uml=>tv_uml DEFAULT lcl_uml=>c_uml_plant
                             iv_compact_trace TYPE char01 DEFAULT abap_true
                   RETURNING VALUE(rv_uml)    TYPE string.

    METHODS get_log RETURNING VALUE(ro_log) TYPE REF TO lcl_logger.
  PRIVATE SECTION.
    METHODS plantuml_header RETURNING VALUE(rv_header) TYPE string.
    METHODS expected_plantuml_source RETURNING VALUE(rv_exp) TYPE string.

    METHODS:
      calls_for_empty_diagram FOR TESTING,
      plantuml_for_empty_diagram FOR TESTING.

    METHODS sample_plantuml_body FOR TESTING.
    METHODS calls_for_sample_output FOR TESTING.
ENDCLASS.

CLASS ltc_messages IMPLEMENTATION.

  METHOD constructor.
    mt_sat = ltc_actors_factory=>sat_fixture_messages( ).
  ENDMETHOD.

  METHOD to_uml.
    DATA(lo_calls) = ltc_sequence=>new_sequence(
      it_sat = it_sat
      is_cfg = VALUE #( uml_format = iv_uml
                        compact_trace = iv_compact_trace ) ).
    rv_uml = lo_calls->to_uml( ).
    mo_uml = lo_calls->mo_uml.
  ENDMETHOD.

  METHOD get_log.
    ro_log = CAST lcl_uml_logger( mo_uml )->mo_log.
  ENDMETHOD.

  METHOD calls_for_empty_diagram.
    to_uml( it_sat = VALUE #( )
            iv_uml = lcl_uml=>c_uml_mock ).
    get_log( )->verify( it_exp = VALUE lcl_logger=>tt_log( ( method = 'HEADER' ) ( method = 'FOOTER' ) )
                        iv_msg = 'LCL_MESSAGES TO_UML( ) Call Stack Empty Diagram Header/Footer Error' ).
  ENDMETHOD.

  METHOD calls_for_sample_output.
    to_uml( it_sat = mt_sat
            iv_uml = lcl_uml=>c_uml_mock ).

    get_log( )->verify( it_exp = VALUE lcl_logger=>tt_log(
      ( method = 'HEADER' )
      ( method = 'PARTICIPANT' params = |1 PROG\\nY_TEST_PROG| )
      ( method = 'PARTICIPANT' params = |2 Static Methods of Class LCL_CTRL\\nY_TEST_PROG| )
      ( method = 'PARTICIPANT' params = |3 ObjectId:6 of Class LCL_MODEL\\nY_TEST_PROG| )
      ( method = 'CALL' params = | 1 1 | )                       " First Call w/o type
      ( method = 'CALL' params = |m 1 2 Call method MAIN| )
      ( method = 'CALL' params = |m 2 3 Call method CONSTRUCTOR| )
      ( method = 'RETURN' params = |3 2| )
      ( method = 'CALL' params = |m 2 2 Call method CHANGE| )
      ( method = 'CALL' params = |m 2 3 Call method UP| )
      ( method = 'RETURN' params = |3 2| )
      ( method = 'RETURN' params = |2 2| )                     " Why ???
      ( method = 'RETURN' params = |2 1| )
      ( method = 'FOOTER' ) )
                     iv_msg = 'LCL_MESSAGES TO_UML( ) Call Stack Error' ).
  ENDMETHOD.

  METHOD plantuml_header.
    DATA(lo_uml) = NEW lcl_uml_plant( ).
    lo_uml->header( ).
    rv_header = lo_uml->mv_diagram.
  ENDMETHOD.

  METHOD expected_plantuml_source.
    rv_exp = plantuml_header( ) &&
|participant "PROG\\nY_TEST_PROG" as 1\nparticipant "Static Methods of Class LCL_CTRL\\nY_TEST_PROG" as 2\n| &&
|participant "ObjectId:6 of Class LCL_MODEL\\nY_TEST_PROG" as 3\n1 -> 1: \nactivate 1\n| &&
|1 -> 2: Call method MAIN\nactivate 2\n2 -> 3: Call method CONSTRUCTOR\nactivate 3\n3 --> 2\ndeactivate 3\n2 -> 2: Call method CHANGE\n| &&
|activate 2\n2 -> 3: Call method UP\nactivate 3\n3 --> 2\ndeactivate 3\ndeactivate 2\n2 --> 1\n| &&
|deactivate 2\n@enduml\n|.
  ENDMETHOD.

  METHOD sample_plantuml_body.
    cl_abap_unit_assert=>assert_equals(
      act = to_uml( mt_sat )
      exp = expected_plantuml_source( )
      msg = 'LCL_MESSAGES->TO_UML( ) PlantUML Error' ).
  ENDMETHOD.

  METHOD plantuml_for_empty_diagram.
    cl_abap_unit_assert=>assert_char_cp(
      act = to_uml( VALUE #( ) )
      exp = |@startuml\nhide footbox\nautonumber\n*skinparam*@enduml\n|
      msg = 'LCL_MESSAGES TO_UML( ) Empty Diagram Header/Footer Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_uml_compactor DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mo_iterator TYPE REF TO lcl_messages_compact.
    DATA mo_uml TYPE REF TO lcl_uml.
    DATA mt_cycle TYPE lcl_messages_compact=>tt_cycle.

    METHODS setup.
    METHODS teardown.

    METHODS cycle_trace RETURNING VALUE(rt_trace) TYPE tt_message.
    METHODS fixture
      IMPORTING iv_compact_trace TYPE xsdboolean
                iv_uml           TYPE lcl_uml=>tv_uml DEFAULT lcl_uml=>c_uml_plant
      RAISING   cx_sy_move_cast_error.
    METHODS create_cycle FOR TESTING.
    METHODS cycle_no_compress FOR TESTING.

    METHODS begin_loop FOR TESTING.
    METHODS calls_for_begin_loop FOR TESTING.
    METHODS end_loop FOR TESTING.
    METHODS no_end_loop FOR TESTING.
ENDCLASS.

CLASS ltc_uml_compactor IMPLEMENTATION.

  METHOD cycle_trace.
    rt_trace = VALUE #(
      ( id = 'm' from_level = 1 caller = 1  called = 2 msg = 'MAIN' )
      ( id = 'm' from_level = 2 caller = 2  called = 3 msg = lcl_abap_trace=>c_create_method )  ).
    APPEND LINES OF rt_trace TO rt_trace.  " Create Loop
  ENDMETHOD.

  METHOD setup.
    fixture( iv_compact_trace = abap_true
             iv_uml = lcl_uml=>c_uml_plant ).
  ENDMETHOD.

  METHOD fixture.
    mo_uml = lcl_uml=>new( iv_uml = iv_uml
                           ii_actors = NEW lcl_actors( ) ).
    mo_iterator ?= lcl_uml_factory=>new_messages(
                     EXPORTING iv_compact_trace = iv_compact_trace
                               it_trace = cycle_trace( )
                               io_uml = mo_uml
                               io_progress = lcl_progress_indicator=>new( VALUE #( progress = abap_true ) )
                     IMPORTING et_cycles = mt_cycle ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_iterator.
    FREE mo_uml.
  ENDMETHOD.

  METHOD create_cycle.
    cl_abap_unit_assert=>assert_equals( act = lines( mt_cycle )
                                        exp = 1
                                        msg = 'LCL_UML_FACTORY CONSTRUCTOR( ) Cycle Error' ).
  ENDMETHOD.

  METHOD cycle_no_compress.
    TRY.
        fixture( abap_false ).
        cl_abap_unit_assert=>fail( msg = 'LCL_MESSAGES_COMPACT CONSTRUCTOR( ) No Cycle Error' ).
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
*     Expected: Object of type LCL_MESSAGES created, not compatible with LCL_MESSAGE_COMPACT
    ENDTRY.
  ENDMETHOD.

  METHOD begin_loop.
    mo_iterator->next( ).
    mo_iterator->begin( iv_idx = mo_iterator->mv_idx
                        iv_from = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = |loop 2 times\n|
                                        msg = 'LCL_MESSAGES_COMPACT BEGIN_LOOP( ) Error' ).
  ENDMETHOD.

  METHOD calls_for_begin_loop.
    fixture( iv_compact_trace = abap_true
             iv_uml = lcl_uml=>c_uml_mock ).
    mo_iterator->next( ).
    mo_iterator->begin( iv_idx = mo_iterator->mv_idx
                        iv_from = 1 ).
    CAST lcl_uml_logger( mo_uml )->mo_log->verify(
      it_exp = VALUE lcl_logger=>tt_log( ( method = 'BEGIN_LOOP'
                                           params = '2' )  )
      iv_msg = 'LCL_MESSAGES_COMPACT BEGIN_LOOP( ) Calls Error' ).
  ENDMETHOD.

  METHOD end_loop.
    mo_iterator->skip( 2 ).
    mo_iterator->end( iv_idx = mo_iterator->mv_idx
                      iv_to = 2 ).     " need a MOCK object here
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = |end\n|
                                        msg = 'LCL_MESSAGES_COMPACT END_LOOP( ) Error' ).
  ENDMETHOD.

  METHOD no_end_loop.
    fixture( abap_true ).
    mo_iterator->skip( 4 ).
    mo_iterator->end( iv_idx = mo_iterator->mv_idx
                      iv_to = 2 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = space
                                        msg = 'LCL_MESSAGES_COMPACT END_LOOP( ) No End Loop Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_cycles DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
    INTERFACES lif_cycles.
  PRIVATE SECTION.
    DATA mt_messages TYPE tt_message.
    DATA mo_index TYPE REF TO lcl_message_index.
    DATA mt_cycles TYPE lcl_messages_compact=>tt_cycle.

    METHODS fixture IMPORTING iv_cycles TYPE n DEFAULT 1.
    METHODS detect_cycles IMPORTING iv_cycles TYPE n DEFAULT 1.
    METHODS new_factory RETURNING VALUE(ro_factory) TYPE REF TO lcl_uml_factory.

    METHODS next_match FOR TESTING.
    METHODS detect FOR TESTING.
    METHODS detect_4 FOR TESTING.
    METHODS fold FOR TESTING.

    METHODS clone_rest FOR TESTING.
    METHODS clone_resize FOR TESTING.
ENDCLASS.

CLASS ltc_cycles IMPLEMENTATION.

  METHOD fixture.
    mt_messages = VALUE #( id = 'm'
         ( from_level = 1 caller = 1 called = 2 msg = 'MAIN' )
         ( from_level = 2 caller = 2 called = 3 msg = lcl_abap_trace=>c_create_method )
         ( from_level = 2 caller = 2 called = 4 msg = 'CHANGE' )
         ( from_level = 2 caller = 2 called = 4 msg = 'CHANGE' )
         ( from_level = 3 caller = 4 called = 6 msg = 'UP' )  ).
    DATA(lt_msg) = mt_messages.
    DO iv_cycles TIMES.
      APPEND LINES OF lt_msg TO mt_messages.
    ENDDO.
    lt_msg =  VALUE #( id = 'm'
      ( from_level = 1 caller = 10 called = 11 msg = 'MAIN' )
      ( from_level = 2 caller = 11 called = 16 msg = lcl_abap_trace=>c_create_method ) ).
    APPEND LINES OF lt_msg TO mt_messages.

    mo_index = NEW #( it_trace = mt_messages
                      ii_cycles = me
                      io_progress = lcl_progress_indicator=>new( VALUE #( progress = abap_false ) ) ).
  ENDMETHOD.

  METHOD lif_cycles~collect.
    INSERT is_cycle INTO TABLE mt_cycles.
  ENDMETHOD.

  METHOD lif_cycles~is_canonical_index.
    rv_flag = abap_true.
  ENDMETHOD.

  METHOD detect_cycles.
    fixture( iv_cycles ).
    lcl_pattern=>new( it_xindex = mo_index->mt_xindex
                      ii_cycles = me )->detect_cycles( ).
  ENDMETHOD.

  METHOD next_match.
    TRY.
        fixture( 1 ).
        lcl_pattern=>new( it_xindex  = mo_index->mt_xindex
                          ii_cycles = me )->next_occurrence_from( -10 ).
*       Should not throw an exception.
      CATCH cx_dynamic_check.
*       If an exception is triggered, the run-time will catch it (not propagated)
*       It will appear in the result list without a proper message.
*       So this is just for developer information, for now
        cl_abap_unit_assert=>fail( msg = 'LCL_PATTERN NEXT_OCCURRENCE_FROM( ) Index Error' ).
    ENDTRY.
  ENDMETHOD.

  METHOD detect.
    detect_cycles( ).
    cl_abap_unit_assert=>assert_equals( act = lines( mt_cycles )
                                        exp = 2
                                        msg = 'LCL_PATTERN DETECT_CYCLES( ) Error' ).
  ENDMETHOD.

  METHOD detect_4.
    detect_cycles( 4 ).
    cl_abap_unit_assert=>assert_equals( act = lines( mt_cycles )
                                        exp = 2
                                        msg = 'LCL_PATTERN DETECT_CYCLES( ) Level 4 Error' ).
  ENDMETHOD.

  METHOD new_factory.
    ro_factory = NEW lcl_uml_factory( lcl_progress_indicator=>new( VALUE #( progress = abap_true ) ) ).
  ENDMETHOD.

  METHOD fold.
    detect_cycles( 3 ).
    new_factory( )->fold( EXPORTING it_trace = mt_messages
                          IMPORTING et_messages = DATA(lt_act) ).
    DATA(lt_exp) = VALUE tt_message( id = 'm'
         ( from_level = 1 caller = 1 called = 2 msg = 'MAIN'    )
         ( from_level = 2 caller = 2 called = 3 msg = lcl_abap_trace=>c_create_method   )
         ( from_level = 2 caller = 2 called = 4 msg = 'CHANGE'  )
         ( from_level = 3 caller = 4 called = 6 msg = 'UP'      )
         ( from_level = 1 caller = 10 called = 11 msg = 'MAIN'  )
         ( from_level = 2 caller = 11 called = 16 msg = lcl_abap_trace=>c_create_method  ) ).

    cl_abap_unit_assert=>assert_equals( act = lt_act
                                        exp = lt_exp
                                        msg = 'LCL_UML_FACTORY->FOLD( ) LOGIC Error' ).
  ENDMETHOD.

  METHOD clone_rest.
    CONSTANTS c_start TYPE i VALUE 2.
    fixture( 1 ).
    cl_abap_unit_assert=>assert_equals( act = NEW lcl_pattern( it_xindex = mo_index->mt_xindex
                                                               iv_start = c_start )->mv_idx
                                        exp = c_start - 1
                                        msg = 'LCL_PATTERN CONSTRUCTOR( ) Index Error' ).
  ENDMETHOD.

  METHOD clone_resize.
    CONSTANTS: c_start TYPE i VALUE 2,
               c_size  TYPE i VALUE 3.
    fixture( 1 ).
    cl_abap_unit_assert=>assert_equals( act = NEW lcl_pattern( it_xindex = mo_index->mt_xindex
                                                               iv_start = c_start
                                                               iv_stop = c_size )->mv_size
                                        exp = c_size
                                        msg = 'LCL_PATTERN CONSTRUCTOR( ) Size Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_shrinkage DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    METHODS reconcile_loop_references FOR TESTING.
ENDCLASS.

CLASS ltc_shrinkage IMPLEMENTATION.

  METHOD reconcile_loop_references.
    DATA(lt_cycles) = VALUE lcl_messages_compact=>tt_cycle(
      ( start = 1    end = 48   last = 192    times = 4 )    "   1 +  48 x 4 - 1 = 192
      ( start = 3    end = 24   last = 46     times = 2 )    "   3 +  22 x 2 - 1 = 46
      ( start = 19   end = 20   last = 22     times = 2 )    "  19 +   2 x 2 - 1 = 22
      ( start = 193  end = 194  last = 200    times = 4 )    " 193 +   2 x 4 - 1 = 200
      ( start = 201  end = 632  last = 476264 times = 1102 ) " 201 + 432 x 1102 - 1 = 476264
      ( start = 217  end = 218  last = 220    times = 2 ) ). " 217 +   2 x 2 - 1 = 220
    lcl_cycles=>shrink( CHANGING ct_cycles = lt_cycles ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_cycles
      exp = VALUE lcl_messages_compact=>tt_cycle(
        ( start = 1    end = 24   last = 24     times = 4 )
        ( start = 3    end = 22   last = 22     times = 2 )
        ( start = 19   end = 20   last = 20     times = 2 )
        ( start = 25   end = 26   last = 26     times = 4 )
        ( start = 27   end = 456  last = 456    times = 1102 )
        ( start = 43   end = 44   last = 44     times = 2 )    )
      msg = 'LCL_CYCLES->SHRINK( ) Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_trace DEFINITION FOR TESTING
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
    INTERFACES lif_collector.
  PRIVATE SECTION.
    DATA ms_cfg TYPE ts_diagram_config.
    DATA mo_trace TYPE REF TO lcl_trace.  " CUT
    DATA mo_state TYPE REF TO lcl_satr_state.
    DATA mt_act_trace TYPE tt_message.
    DATA mt_sat TYPE STANDARD TABLE OF ts_sat WITH EMPTY KEY.

    METHODS setup.
    METHODS teardown.
    METHODS fill_trace.
    METHODS fixture_add_more.
    METHODS fixture_caller.
    METHODS fixture_prepare.

    METHODS add_message FOR TESTING.
    METHODS add_more FOR TESTING.
    METHODS skip_message FOR TESTING.
    METHODS filter_message FOR TESTING.
    METHODS save_caller FOR TESTING.
    METHODS test_output FOR TESTING.
    METHODS skip_record FOR TESTING.
ENDCLASS.

CLASS ltc_trace IMPLEMENTATION.

  METHOD setup.
    CLEAR mt_act_trace.
    CLEAR mt_sat.
    ms_cfg = VALUE ts_diagram_config( pattern = VALUE #( ( 'Y*' ) )
                                      compact_trace = abap_true
                                      output_mode = lcl_diagram_text=>c_mode_aut ).
    mo_trace = NEW #( lcl_progress_indicator=>new( ms_cfg ) ).
    mo_state = NEW #( mo_trace->mo_atra ).
  ENDMETHOD.

  METHOD teardown.
    mo_state->restore( ).
    FREE mo_state.
    FREE mo_trace.
  ENDMETHOD.

  METHOD fill_trace.
    DATA(lo_collector) = NEW lcl_sequence( ms_cfg ).
    mo_trace->fill( lo_collector ).
    mt_act_trace = lo_collector->mt_trace.
  ENDMETHOD.

  METHOD lif_collector~collect.
*   No filter / No conversion
    APPEND is_sat TO mt_sat.
  ENDMETHOD.

  METHOD add_message.
    mo_trace->mo_atra->it_austab_hier = VALUE #( ( id = 'm'
         index = 1  ebene = 2  caller = 3 event = '>'  ) ).
    mo_trace->fill( me ).
    cl_abap_unit_assert=>assert_equals( act = lines( mt_sat )
                                        exp = 1
                                        msg = 'LCL_TRACE ADD_MESSAGE( ) Error' ).
  ENDMETHOD.

  METHOD filter_message.
    mo_trace->mo_atra->it_traceprog = VALUE #(
         ( cprog = 'Y_PROG'   object = c_type_prog  )
         ( cprog = 'SAPPROG'  object = c_type_class )  ).
    mo_trace->mo_atra->it_austab_hier = VALUE #(
      ( id = 'O'             index = 1  ebene = 0  caller = 0 event = '>' )
      ( id = 'V' subid = 'O' index = 2  ebene = 1  caller = 1 event = '>' )  "DB Op.
      ( id = 'P'             index = 3  ebene = 1  caller = 1 event = '>' progindex = 1 textindex = 1 )
      ( id = 'm'             index = 4  ebene = 2  caller = 1 event = '>' progindex = 1 methindex = 1 )
      ( id = 'm' subid = 'x' index = 5  ebene = 2  caller = 1 event = '<' ) ).

    fill_trace( ).
    cl_abap_unit_assert=>assert_equals( act = lines( mt_act_trace )
                                        exp = 1
                                        msg = 'LCL_TRACE ADD_MESSAGE( ) Filter Error' ).
  ENDMETHOD.

  METHOD skip_message.
    mo_trace->mo_atra->it_austab_hier = VALUE #( ( id = 'm'
         index = 1  ebene = 2  caller = 2 event = '<'  ) ).
    fill_trace( ).
    cl_abap_unit_assert=>assert_initial( act = lines( mt_act_trace )
                                         msg = 'LCL_TRACE ADD_MESSAGE( ) Skip Error' ).
  ENDMETHOD.

  METHOD save_caller.
    mo_trace->mo_atra->it_austab_hier = VALUE #( id = 'm'
         ( index = 1  ebene = 2  caller = 3 event = '>'  )
         ( index = 1  ebene = 2  caller = 2 event = '<'  ) ).
    fill_trace( ).
    cl_abap_unit_assert=>assert_equals( act = lines( mo_trace->mt_caller )
                                        exp = 1
                                        msg = 'LCL_TRACE ADD_MESSAGE( ) Save Caller' ).
  ENDMETHOD.

  METHOD test_output.
    lcl_configuration=>gs_cfg-skip_dialog = abap_true.
    lcl_configuration=>gs_cfg-output_mode = lcl_diagram_text=>c_mode_aut.
    lcl_sequence=>to_diagram( lcl_configuration=>get( ) )->output( ).
  ENDMETHOD.

  METHOD fixture_prepare.
    mo_trace->mo_atra->it_traceprog = VALUE #(
         ( cprog = ltc_actors_factory=>c_prog  object = c_type_prog  )
         ( cprog = ltc_actors_factory=>c_prog  object = c_type_class  )
         ( cprog = 'SAPPROG'  object = c_type_class )  ).
    mo_trace->mo_atra->it_tracetext = VALUE #(
         ( tracetext = 'MAIN' )
         ( tracetext = lcl_abap_trace=>c_create_method )
         ( tracetext = 'CHANGE' )
         ( tracetext = ltc_actors_factory=>c_ctrl )
         ( tracetext = ltc_actors_factory=>c_model )
         ( tracetext = ltc_actors_factory=>c_prog )  ).
    mo_trace->mo_atra->it_tracemeth = VALUE #(
         ( methode = 'MAIN'  )
         ( methode = lcl_abap_trace=>c_create_method )
         ( methode = 'CHANGE'  )  ).
  ENDMETHOD.

  METHOD fixture_add_more.
*   Fixture: Scenario from ltc_objects=>sat_fixture_events( )
    fixture_prepare( ).

    mo_trace->mo_atra->it_austab_hier = VALUE #(
      ( id = 'O'             index = 1  ebene = 0  caller = 0 event = '>' )
      ( id = 'C' subid = 'R' index = 2  ebene = 1  caller = 1 event = '>'               textindex = 6 )  "Submit
      ( id = 'V' subid = 'O' index = 3  ebene = 2  caller = 2 event = '>' )  "DB Op.
      ( id = 'P'             index = 4  ebene = 2  caller = 2 event = '>' progindex = 1 textindex = 1 )  "Program

      ( id = 'm'             index = 5  ebene = 3  caller = 4 event = '>' progindex = 2 textindex = 4 methindex = 1 )
      ( id = 'm' subid = 'x' index = 6  ebene = 4  caller = 5 event = '>' progindex = 3 textindex = 5 methindex = 2 )
      ( id = 'm' subid = 'x' index = 7  ebene = 4  caller = 6 event = '>' progindex = 1 textindex = 4 methindex = 3 ) ).
    fill_trace( ).
  ENDMETHOD.

  METHOD add_more.
    fixture_add_more( ).

    DATA(lt_exp)  = VALUE tt_message(
     ( id = 'C'       from_level = 1  caller = 1  called = 2 msg = ltc_actors_factory=>c_prog )
     ( id = 'm'       from_level = 3  caller = 2  called = 3 msg = 'MAIN'              )
     ( id = c_id_skip from_level = 3  caller = 3  called = 4 msg = 'CONSTRUCTOR'       )
     ( id = 'm'       from_level = 4  caller = 4  called = 3 msg = 'CHANGE'            ) ).

    cl_abap_unit_assert=>assert_equals( act = mt_act_trace
                                        exp = lt_exp
                                        msg = 'LCL_TRACE ADD_MESSAGE( ) More - Error' ).
  ENDMETHOD.

  METHOD fixture_caller.
    fixture_prepare( ).

    mo_trace->mo_atra->it_austab_hier = VALUE #(
      ( id = 'O'             index = 1  ebene = 0  caller = 0 event = '>' )
      ( id = 'C' subid = 'R' index = 2  ebene = 1  caller = 1 event = '>'    textindex = 6 )  "Submit
      ( id = 'C' subid = 'R' index = 2  ebene = 1  caller = 1 event = '<'    textindex = 6 ) ). "Submit
    fill_trace( ).
  ENDMETHOD.

  METHOD skip_record.
    fixture_caller( ).
*   TO DO: check coverage/review fixture
    cl_abap_unit_assert=>assert_equals(
      act = mo_trace->mt_caller
      exp = VALUE lcl_trace=>tt_seq_k1( ( aus_tabix       = 1 )
                ( called = VALUE #( global = ltc_actors_factory=>c_prog type = c_type_prog
                                    mod = ltc_actors_factory=>c_prog )
                  aus_tabix       = 2 )  )
      msg = 'LCL_TRACE SKIP_RECORD( ) Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_filter_null DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mi_filter TYPE REF TO lif_trace_filter.
    METHODS setup.
    METHODS teardown.

    METHODS filter_empty FOR TESTING.
    METHODS filter_valid FOR TESTING.
ENDCLASS.

CLASS ltc_filter_null IMPLEMENTATION.

  METHOD setup.
    mi_filter = lcl_filter=>new( VALUE #( ) ).
  ENDMETHOD.

  METHOD teardown.
    FREE mi_filter.
  ENDMETHOD.

  METHOD filter_empty.
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( VALUE #(  ) )
                                      msg = 'LCL_FILTER ACCEPTS( ) Empty - Null Filter Error' ).
  ENDMETHOD.

  METHOD filter_valid.
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( ltc_actors_factory=>sat_fixture_record( ) )
                                      msg = 'LCL_FILTER ACCEPTS( ) Valid - Null Filter Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_filter_custom DEFINITION FOR TESTING
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mi_filter TYPE REF TO lif_trace_filter.

    METHODS setup.
    METHODS teardown.
    METHODS sample_sat RETURNING VALUE(rs_sat) TYPE ts_sat.

    METHODS filter_empty FOR TESTING.
    METHODS filter_valid FOR TESTING.
    METHODS accepts FOR TESTING.
    METHODS system_events FOR TESTING.
    METHODS skip_constructor FOR TESTING.
ENDCLASS.

CLASS ltc_filter_custom IMPLEMENTATION.

  METHOD setup.
    mi_filter = lcl_filter=>new( VALUE #( pattern = VALUE #( ( 'Y*' ) ) ) ).
  ENDMETHOD.

  METHOD teardown.
    FREE mi_filter.
  ENDMETHOD.

  METHOD filter_empty.
    cl_abap_unit_assert=>assert_false( act = mi_filter->accepts( VALUE #(  ) )
                                       msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Empty - Error' ).
  ENDMETHOD.

  METHOD filter_valid.
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( ltc_actors_factory=>sat_fixture_record( ) )
                                      msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Valid - Error' ).
  ENDMETHOD.

  METHOD sample_sat.
    rs_sat = VALUE ts_sat( id = 'L' from_level = 8  aus_tabix = 21
       caller = VALUE #( global = 'SAPMSSYD' type = 'PROG' mod = '%_IMODE_INIT' )
       called = VALUE #( ) ).
  ENDMETHOD.

  METHOD accepts.
    cl_abap_unit_assert=>assert_false( act = mi_filter->accepts( sample_sat( ) )
                                       msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Custom - Filter Error' ).
  ENDMETHOD.

  METHOD system_events.
    mi_filter = lcl_filter=>new( VALUE #( system_events = abap_true
                                          pattern = VALUE #( ( 'SAPM*' ) ) ) ).
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( sample_sat( ) )
                                      msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Custom - <system Event' ).
  ENDMETHOD.

  METHOD skip_constructor.
    mi_filter = lcl_filter=>new( VALUE #( system_events = abap_true
                                          pattern = VALUE #( ( 'SAPM*' ) ) ) ).
    DATA(ls_sat) = sample_sat( ).
    DATA(ls_ref) = ls_sat.
    ls_ref-caller-mod = 'CONSTRUCTOR'.
    mi_filter->accepts( ls_ref ).
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( ls_sat )
                                      msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Custom - Filter Error' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_configuration DEFINITION FOR TESTING
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mo_config TYPE REF TO lcl_configuration.

    METHODS setup.
    METHODS teardown.
    METHODS get_result RETURNING VALUE(rv_result) TYPE char3.

    METHODS radiobutton_url FOR TESTING.
    METHODS radiobutton_txt FOR TESTING.
    METHODS radiobutton_exe FOR TESTING.
    METHODS from_radiobutton_txt FOR TESTING.
ENDCLASS.

CLASS ltc_configuration IMPLEMENTATION.

  METHOD setup.
    mo_config = NEW #( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_config.
  ENDMETHOD.

  METHOD get_result.
    rv_result+0(1) = mo_config->mv_mode_url.
    rv_result+1(1) = mo_config->mv_mode_txt.
    rv_result+2(1) = mo_config->mv_mode_exe.
  ENDMETHOD.

  METHOD radiobutton_url.
    mo_config->gs_cfg-output_mode = lcl_diagram_text=>c_mode_url.
    mo_config->to_radiobutton( ).
    cl_abap_unit_assert=>assert_equals( act = get_result( )
                                        exp = 'X  '
                                        msg = 'LCL_CONFIGURATION->TO_RADIOBUTTON( ) - URL Error' ).
  ENDMETHOD.

  METHOD radiobutton_exe.
    mo_config->gs_cfg-output_mode = lcl_diagram_text=>c_mode_exe.
    mo_config->to_radiobutton( ).
    cl_abap_unit_assert=>assert_equals( act = get_result( )
                                        exp = '  X'
                                        msg = 'LCL_CONFIGURATION->TO_RADIOBUTTON( ) - EXE Error' ).
  ENDMETHOD.

  METHOD radiobutton_txt.
    mo_config->gs_cfg-output_mode = lcl_diagram_text=>c_mode_txt.
    mo_config->to_radiobutton( ).
    cl_abap_unit_assert=>assert_equals( act = get_result( )
                                        exp = ' X '
                                        msg = 'LCL_CONFIGURATION->TO_RADIOBUTTON( ) - TXT Error' ).
  ENDMETHOD.

  METHOD from_radiobutton_txt.
    mo_config->gs_cfg-output_mode = lcl_diagram_text=>c_mode_txt.
    mo_config->to_radiobutton( ).
    mo_config->from_radiobutton( ).
    cl_abap_unit_assert=>assert_equals( act = mo_config->gs_cfg-output_mode
                                        exp = lcl_diagram_text=>c_mode_txt
                                        msg = 'LCL_CONFIGURATION->FROM_RADIOBUTTON( ) - TXT Error' ).
  ENDMETHOD.

ENDCLASS.

*-------------------------------------------------------------------------------------------------*
