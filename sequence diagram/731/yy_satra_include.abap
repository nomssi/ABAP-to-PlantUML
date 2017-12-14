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
*    create a source code plugin in include LS_ABAP_TRACE_DATAD07
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
*    CATCH cx_dynamic_check INTO DATA(lx_error).
*      MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.  "#EC CI_USE_WANTED
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
TYPES:  BEGIN OF ts_object,
          global   TYPE program,
          local    TYPE program,
          type     TYPE trobjtype,
          instance TYPE satr_de_instance,
        END OF ts_object.

TYPES tv_key TYPE i.        " Object numbers in call

TYPES: BEGIN OF ts_path,
         caller TYPE tv_key,
         called TYPE tv_key,
       END OF ts_path.

* Trace step (Message): Caller -> Called object, message name (msg)
TYPES: BEGIN OF ts_message,
         id         TYPE satr_de_id,
         from_level TYPE satr_de_ebene.
        INCLUDE TYPE ts_path AS path.
TYPES:   msg TYPE seocpdname,
         END OF ts_message.
TYPES tt_message TYPE STANDARD TABLE OF ts_message WITH KEY id
           WITH NON-UNIQUE SORTED KEY key_msg COMPONENTS table_line.

*----------------------------------------------------------------------*
*       INTERFACE lif_unit_test
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_unit_test.
ENDINTERFACE.                    "lif_unit_test

* Message - Called object information
TYPES:  BEGIN OF ts_call.
        INCLUDE TYPE ts_object AS object.
TYPES:    mod TYPE seocpdname,
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
TYPES:   system TYPE satr_de_sysflag,
         END OF ts_sat.

TYPES tv_text TYPE char128.
* Each actor (sender or receiver of a message) in the sequence diagram has a life line
TYPES:  BEGIN OF ts_lifeline,
          index TYPE tv_key,    " secondary index in table of ts_actor
          label TYPE tv_text,
        END OF ts_lifeline.

*----------------------------------------------------------------------*
*       INTERFACE lif_actors
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_actors.
  TYPES tt_lifeline TYPE SORTED TABLE OF ts_lifeline WITH UNIQUE KEY index.

  METHODS lifelines RETURNING value(rt_lifeline) TYPE tt_lifeline.
  METHODS short_text IMPORTING is_message     TYPE ts_message
                     RETURNING value(rv_text) TYPE tv_text.
  METHODS new_path IMPORTING is_sat         TYPE ts_sat
                   RETURNING value(rs_path) TYPE ts_path.
ENDINTERFACE.                    "lif_actors

TYPES tv_element TYPE char30.

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
                      RETURNING value(ro_uml) TYPE REF TO lcl_uml.
    METHODS top.
    METHODS bottom RETURNING value(rv_diagram) TYPE string.
    "! Draws a message between two objects, with the given label.
    "! Self messages (where an object sends a message to itself) are supported.
    METHODS message IMPORTING is_message TYPE ts_message.
    METHODS return ABSTRACT IMPORTING iv_from TYPE i
                                      iv_to   TYPE i.
    METHODS begin_loop ABSTRACT IMPORTING iv_from  TYPE program OPTIONAL
                                          iv_times TYPE sytabix
                                          iv_name  TYPE tv_element OPTIONAL.
    METHODS end_loop ABSTRACT IMPORTING iv_to   TYPE program OPTIONAL
                                        iv_name TYPE tv_element OPTIONAL.
  PROTECTED SECTION.
    CONSTANTS c_txt_std TYPE string VALUE 'Standard SAP code has called some custom code ' ##no_text.
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
ENDCLASS.                    "lcl_uml DEFINITION

* iterator over an internal table of messages allowing to
* - skip a given number of entries (default 1).
* - look ahead to read the next value without updating the index
* - is_first to known if the current values is the first
CLASS lcl_messages DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS c_default_level TYPE satr_de_ebene VALUE 1.

    METHODS constructor IMPORTING it_messages TYPE tt_message
                                  iv_start    TYPE sytabix DEFAULT 1
                                  iv_stop     TYPE sytabix OPTIONAL.
    METHODS is_first RETURNING value(rv_flag) TYPE xsdboolean.
    METHODS has_next RETURNING value(rv_flag) TYPE xsdboolean.
    METHODS skip IMPORTING iv_count TYPE i DEFAULT 1.
    METHODS next RETURNING value(rs_data) TYPE ts_message
                 RAISING   cx_sy_itab_error.
    METHODS first_level RETURNING value(rv_level) TYPE satr_de_ebene.
    METHODS next_level RETURNING value(rv_level) TYPE satr_de_ebene.
  PROTECTED SECTION.
    DATA mv_idx TYPE sytabix.
    DATA mv_size TYPE sytabix.
    DATA mt_list TYPE tt_message.
  PRIVATE SECTION.
    DATA mv_first_level TYPE i.
ENDCLASS.                    "lcl_messages DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_stack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_stack DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_call_level,
        actor_key  TYPE tv_key,
        from_level TYPE satr_de_ebene,
      END OF ts_call_level.

    METHODS push IMPORTING is_ref TYPE ts_call_level.
    METHODS pop RETURNING value(rs_to) TYPE ts_call_level.
  PROTECTED SECTION.
    DATA mv_empty TYPE xsdboolean VALUE abap_true.
  PRIVATE SECTION.
    TYPES BEGIN OF ts_level.
            INCLUDE TYPE ts_call_level AS call_level.
    TYPES next TYPE REF TO data.
    TYPES END OF ts_level.

    DATA mr_level TYPE REF TO ts_level.
ENDCLASS.                    "lcl_stack DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_stack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_stack DEFINITION INHERITING FROM lcl_stack FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_uml TYPE REF TO lcl_uml.
    METHODS call IMPORTING is_message TYPE ts_message.
    METHODS return IMPORTING iv_to_level TYPE satr_de_ebene.
  PROTECTED SECTION.
    DATA mv_previous_level TYPE satr_de_ebene.
    DATA mo_uml TYPE REF TO lcl_uml.

    METHODS return_to IMPORTING iv_level TYPE satr_de_ebene.
ENDCLASS.                    "lcl_uml_stack DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_call_stack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_call_stack DEFINITION INHERITING FROM lcl_uml_stack FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_uml      TYPE REF TO lcl_uml
                                  io_messages TYPE REF TO lcl_messages.
    "! traverses the list of messages list and generates an "UML as text" diagram
    METHODS to_uml RETURNING value(rv_diagram) TYPE string.
  PROTECTED SECTION.
    CONSTANTS c_first_key TYPE tv_key VALUE 1.      " Code of very first call
    DATA mo_messages TYPE REF TO lcl_messages.

    METHODS message IMPORTING is_message TYPE ts_message
                              iv_idx     TYPE sytabix.
ENDCLASS.                    "lcl_call_stack DEFINITION

* Trace cycle description
TYPES: BEGIN OF ts_cycle,
         start TYPE sytabix,
         end   TYPE sytabix,
         last  TYPE sytabix,
         times TYPE i,
       END OF ts_cycle.

*----------------------------------------------------------------------*
*       CLASS lcl_call_stack_compact DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_call_stack_compact DEFINITION INHERITING FROM lcl_call_stack FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES tt_cycle TYPE SORTED TABLE OF ts_cycle WITH NON-UNIQUE KEY start.
    METHODS constructor IMPORTING io_uml      TYPE REF TO lcl_uml
                                  io_messages TYPE REF TO lcl_messages
                                  it_cycles   TYPE tt_cycle.
  PROTECTED SECTION.
    DATA mt_cycle TYPE tt_cycle.

    METHODS message REDEFINITION.
    METHODS begin IMPORTING iv_idx  TYPE sytabix
                            iv_from TYPE tv_key.
    METHODS end IMPORTING iv_idx         TYPE sytabix
                          iv_to          TYPE tv_key
                RETURNING value(rv_step) TYPE sytabix.
  PRIVATE SECTION.
    METHODS name IMPORTING iv_start       TYPE sytabix
                           iv_end         TYPE sytabix
                 RETURNING value(rv_name) TYPE tv_element.
ENDCLASS.                    "lcl_call_stack_compact DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_actors DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_actors DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_actors.
  PROTECTED SECTION.
    TYPES:  BEGIN OF ts_actor.
            INCLUDE TYPE ts_object AS object.
            INCLUDE TYPE ts_lifeline AS lifeline.
    TYPES:  END OF ts_actor.

    TYPES tt_actor TYPE SORTED TABLE OF ts_actor
      WITH UNIQUE KEY type instance global local
      WITH UNIQUE SORTED KEY obj_nr COMPONENTS index.
    DATA mt_actor TYPE tt_actor.
    DATA mv_last_number TYPE i.

    METHODS index_of IMPORTING is_object     TYPE ts_object
                     RETURNING value(rv_idx) TYPE sytabix.
    METHODS next_number RETURNING value(rv_key) TYPE tv_key.
    METHODS lifeline IMPORTING is_object      TYPE ts_object
                     RETURNING value(rv_text) TYPE tv_text.
    METHODS call_short_text IMPORTING is_message     TYPE ts_message
                            RETURNING value(rv_text) TYPE tv_text.
    METHODS class_name IMPORTING iv_index             TYPE sytabix
                       RETURNING value(rv_short_name) TYPE tv_text.
    METHODS put IMPORTING is_object     TYPE ts_object
                RETURNING value(rv_key) TYPE tv_key.
ENDCLASS.                    "lcl_actors DEFINITION

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
       END OF ts_diagram_config.

*----------------------------------------------------------------------*
*       CLASS lcl_diagram_text DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diagram_text DEFINITION ABSTRACT FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_aut TYPE char01 VALUE 'T',  " for ABAP Unit Test
      c_mode_url TYPE char01 VALUE 'U',
      c_mode_txt TYPE char01 VALUE space,
      c_mode_exe TYPE char01 VALUE 'E'.

    CLASS-METHODS new IMPORTING is_cfg            TYPE ts_diagram_config
                                iv_text           TYPE string
                      RETURNING value(ro_diagram) TYPE REF TO lcl_diagram_text.
    METHODS constructor IMPORTING iv_diagram TYPE string
                                  iv_mode    TYPE char01.
    METHODS output  RAISING cx_dynamic_check.
  PROTECTED SECTION.
    DATA mv_diagram TYPE string.
    DATA ms_cfg TYPE ts_diagram_config.

    METHODS save_file IMPORTING iv_mode         TYPE char01
                      RETURNING value(rv_fname) TYPE string
                      RAISING   cx_dynamic_check.
    METHODS to_xstring IMPORTING iv_string         TYPE string
                       RETURNING value(rv_xstring) TYPE xstring
                       RAISING   cx_dynamic_check.
ENDCLASS.                    "lcl_diagram_text DEFINITION

*----------------------------------------------------------------------*
*       INTERFACE lif_trace_filter DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_trace_filter.
  METHODS accepts IMPORTING is_sat         TYPE ts_sat
                  RETURNING value(rv_flag) TYPE xsdboolean.
ENDINTERFACE.                    "lif_trace_filter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING iv_mode        TYPE char01
                      RETURNING value(ro_file) TYPE REF TO lcl_file_name.
    METHODS constructor IMPORTING iv_mode TYPE char01.
    METHODS dialog RETURNING value(rv_user_action) TYPE i.
    METHODS get_prefix RETURNING value(rv_name) TYPE string
                       RAISING   cx_dynamic_check.
    METHODS get_fullpath RETURNING value(rv_name) TYPE string.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_fullpath,
             title TYPE string,
             name  TYPE string,
             ext   TYPE string,
             path  TYPE string,
           END OF ts_fullpath.
    DATA ms_file TYPE ts_fullpath.
ENDCLASS.                    "lcl_file_name DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file_test_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_test_name DEFINITION INHERITING FROM lcl_file_name FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS dialog REDEFINITION.
ENDCLASS.                    "lcl_file_test_name DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS download
      IMPORTING iv_data         TYPE xstring
                io_name         TYPE REF TO lcl_file_name
      RETURNING value(rv_subrc) TYPE sysubrc.
ENDCLASS.                    "lcl_file DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_diagram_plant_uml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diagram_plant_uml DEFINITION INHERITING FROM lcl_diagram_text FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING is_cfg     TYPE ts_diagram_config
                                  iv_diagram TYPE string.
    METHODS output REDEFINITION.
    CLASS-METHODS get_java_path RETURNING value(rv_fullpath) TYPE string.
  PROTECTED SECTION.
    TYPES tv_base64 TYPE c LENGTH 65.
    CONSTANTS:
      c_charset_standard TYPE tv_base64 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=' ##no_text,
      c_charset_plantuml TYPE tv_base64 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_0' ##no_text.

    METHODS to_url IMPORTING iv_base_url   TYPE string
                   RETURNING value(rv_url) TYPE string
                   RAISING   cx_dynamic_check.
    METHODS to_png IMPORTING io_name        TYPE REF TO lcl_file_name
                   RETURNING value(rv_name) TYPE string.
    METHODS display_html RAISING cx_dynamic_check.
    METHODS local_plant_uml RAISING cx_dynamic_check.
    METHODS parameter_string IMPORTING io_name         TYPE REF TO lcl_file_name
                             RETURNING value(rv_param) TYPE string.
    METHODS png_file_name IMPORTING io_name        TYPE REF TO lcl_file_name
                          RETURNING value(rv_name) TYPE string.
    METHODS encoded_url_suffix RETURNING value(rv_url) TYPE string
                               RAISING   cx_dynamic_check.
    METHODS source RETURNING value(rv_source) TYPE string.

    METHODS display_url IMPORTING iv_url TYPE string
                        RAISING   cx_dynamic_check.
ENDCLASS.                    "lcl_diagram_plant_uml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_configuration DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_configuration DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS:
      get RETURNING value(rs_cfg) TYPE ts_diagram_config,
      query RETURNING value(rs_cfg) TYPE ts_diagram_config,
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
           END OF ts_param.
    METHODS get_attributes RETURNING value(rt_attr) TYPE sci_atttab.
    METHODS to_radiobutton.
    METHODS from_radiobutton.
    CLASS-DATA gs_cfg TYPE ts_param.
    DATA: mv_mode_url TYPE flag VALUE 'X',
          mv_mode_exe TYPE flag,
          mv_mode_txt TYPE flag.
    METHODS dialog.
ENDCLASS.                    "lcl_configuration DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_filter DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_filter DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_trace_filter.
    ALIASES accepts FOR lif_trace_filter~accepts.
    CLASS-METHODS new IMPORTING is_cfg           TYPE ts_diagram_config
                      RETURNING value(ri_filter) TYPE REF TO lif_trace_filter.
    METHODS constructor IMPORTING is_cfg TYPE ts_diagram_config.
  PROTECTED SECTION.
    DATA ms_cfg TYPE ts_diagram_config.
    DATA mv_config_id_reject TYPE string.
ENDCLASS.                    "lcl_filter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_filter_null DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_filter_null DEFINITION INHERITING FROM lcl_filter FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS accepts REDEFINITION.
ENDCLASS.                    "lcl_filter_null DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_filter_custom DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_filter_custom DEFINITION INHERITING FROM lcl_filter FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING is_cfg TYPE ts_diagram_config.
    METHODS accepts REDEFINITION.
  PRIVATE SECTION.
    DATA mv_regex TYPE string.

    METHODS customer_namespace
      IMPORTING iv_name        TYPE program
      RETURNING value(rv_flag) TYPE xsdboolean.
    METHODS get_regex IMPORTING it_pattern      TYPE sci_pattern
                      RETURNING value(rv_regex) TYPE string.
ENDCLASS.                    "lcl_filter_custom DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_bag DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bag DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS add IMPORTING iv_entry TYPE sytabix.
    METHODS remove IMPORTING iv_entry TYPE sytabix.
    METHODS contains IMPORTING iv_entry       TYPE sytabix
                     RETURNING value(rv_flag) TYPE xsdboolean.
  PRIVATE SECTION.
    DATA mt_index TYPE SORTED TABLE OF sytabix WITH UNIQUE DEFAULT KEY.
ENDCLASS.                    "lcl_bag DEFINITION

"! Class information
*----------------------------------------------------------------------*
*       CLASS lcl_class_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class_name DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS:
      is_global IMPORTING iv_name        TYPE sobj_name
                RETURNING value(rv_flag) TYPE xsdboolean,
      external CHANGING cv_clas TYPE program,
      technical IMPORTING iv_name         TYPE program
                RETURNING value(rv_class) TYPE program,
      to_internal IMPORTING iv_name         TYPE program
                  RETURNING value(rv_class) TYPE program.
  PRIVATE SECTION.
*   Cache proxy for method IS_GLOBAL( )
    TYPES: BEGIN OF ts_class,
             name   TYPE sobj_name,
             global TYPE flag,
           END OF ts_class.
    TYPES tt_class TYPE HASHED TABLE OF ts_class WITH UNIQUE KEY name.
    DATA mt_class TYPE tt_class.

    METHODS is_global_class
      IMPORTING iv_name        TYPE sobj_name
      RETURNING value(rv_flag) TYPE xsdboolean.
ENDCLASS.                    "lcl_class_name DEFINITION

*----------------------------------------------------------------------*
"! Parse call hierarchy in ABAP trace stored as static attributes of
"! standard class CL_ATRA_TOOL_SE30_MAIN, extract trace details needed
"! for UML sequence diagram generation from internal tables
"!      IT_TRACEPROG - Table of Recorded Program Names
"!      IT_TRACETEXT - Table of TRACE Text Elements
"!      IT_TRACEMETH - Table of Recorded Method Names
"!      IT_AUSTAB_HIER - Call Hierarchy table with All TRACE Information
*----------------------------------------------------------------------*
*       CLASS lcl_abap_trace DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_trace DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS c_create_method TYPE seocpdname VALUE 'CONSTRUCTOR'.

    METHODS constructor.
    METHODS parse RETURNING value(rv_flag) TYPE xsdboolean.
  PROTECTED SECTION.
    TYPES:  BEGIN OF ts_event.
            INCLUDE TYPE ts_object AS object.
    TYPES:    event  TYPE seocpdname,       " Called component of object
              system TYPE satr_de_sysflag,
              END OF ts_event.

    DATA mo_atra TYPE REF TO cl_atra_tool_se30_main.
    DATA mr_src TYPE REF TO satr_austab_gesamt.
    DATA ms_target TYPE ts_event.                " Called actor
    DATA mo_class_name TYPE REF TO lcl_class_name.

    METHODS module IMPORTING iv_index        TYPE satr_de_pgix
                   RETURNING value(rv_subrc) TYPE sysubrc.
    METHODS event RETURNING value(rv_subrc) TYPE sysubrc.
*   the following methods are redefined in test double
    METHODS set_constructor.
    METHODS set_meth.
    METHODS set_form.
    METHODS set_function_module.

    METHODS set_submit.
    METHODS set_tran.
  PRIVATE SECTION.
    METHODS set_local_class IMPORTING iv_progname  TYPE program.
ENDCLASS.                    "lcl_abap_trace DEFINITION

*----------------------------------------------------------------------*
*       INTERFACE lif_collector DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_collector.
  "!  Add trace entry to collection
  METHODS collect IMPORTING is_sat         TYPE ts_sat
                  RETURNING value(rv_flag) TYPE xsdboolean.
ENDINTERFACE.                    "lif_collector DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_trace DEFINITION                                     *
*----------------------------------------------------------------------*
CLASS lcl_trace DEFINITION INHERITING FROM lcl_abap_trace FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS fill IMPORTING ii_collector TYPE REF TO lif_collector.
  PROTECTED SECTION.
    CONSTANTS c_event_entry TYPE satr_de_event VALUE '>'.
    TYPES tt_seq_k1 TYPE STANDARD TABLE OF ts_seq WITH KEY aus_tabix  " Performance
                    WITH NON-UNIQUE SORTED KEY k1 COMPONENTS aus_tabix.
    DATA mt_caller TYPE tt_seq_k1.
  PRIVATE SECTION.
    DATA mo_bag TYPE REF TO lcl_bag.

    METHODS skip_record.
    METHODS put_caller IMPORTING is_sat TYPE ts_sat.
    METHODS get_sat RETURNING value(rs_sat) TYPE ts_sat.
    METHODS new_record RETURNING value(rv_flag) TYPE xsdboolean.
ENDCLASS.                    "lcl_trace DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_progress_indicator DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_progress_indicator DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS: c_enable  TYPE char01 VALUE 'X',
               c_disable TYPE char01 VALUE '0'.
    CLASS-METHODS new IMPORTING is_cfg         TYPE ts_diagram_config OPTIONAL
                      RETURNING value(ro_prog) TYPE REF TO lcl_progress_indicator.
    CLASS-METHODS echo IMPORTING iv_percentage TYPE numeric DEFAULT 0
                                 iv_text       TYPE clike.
    METHODS set_mode IMPORTING iv_mode TYPE char01.
    METHODS restore.
  PRIVATE SECTION.
    CONSTANTS c_param_id_sin TYPE char20 VALUE 'SIN'.
    DATA mv_mode TYPE char01.

    METHODS constructor IMPORTING iv_progress TYPE flag.
    METHODS get_mode RETURNING value(rv_mode) TYPE char01.
ENDCLASS.                    "lcl_progress_indicator DEFINITION

*-----------------------------------------------------------------------------------------*

INTERFACE lif_cycles.
  METHODS collect IMPORTING is_cycle TYPE ts_cycle.
  METHODS accepts IMPORTING iv_index       TYPE sytabix
                  RETURNING value(rv_flag) TYPE xsdboolean.
ENDINTERFACE.                    "lif_cycles DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_trace_index DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_trace_index DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
*   to benefit from the performance of the FIND IN TABLE statement
*   we implement a poor man's table index management here
    TYPES tv_x_tabix TYPE x LENGTH 4.
    TYPES tt_x_index TYPE STANDARD TABLE OF tv_x_tabix.

    METHODS constructor IMPORTING it_trace  TYPE tt_message
                                  ii_cycles TYPE REF TO lif_cycles.
    METHODS to_trace CHANGING ct_trace TYPE tt_message.
    METHODS shrink IMPORTING iv_pass          TYPE syindex
                   RETURNING value(rv_shrink) TYPE xsdboolean.
  PROTECTED SECTION.
    TYPES tt_components TYPE SORTED TABLE OF ts_message
      WITH UNIQUE KEY id from_level caller called msg.

    METHODS to_index.
    METHODS filter RETURNING value(rv_shrink) TYPE xsdboolean.
    METHODS compact IMPORTING it_xindex TYPE tt_x_index
                    EXPORTING et_xindex TYPE tt_x_index.
  PRIVATE SECTION.
    DATA mt_trace TYPE tt_message.
    DATA mi_cycles TYPE REF TO lif_cycles.

    DATA mt_components TYPE tt_components.
    DATA mt_xindex TYPE tt_x_index.
ENDCLASS.                    "lcl_trace_index DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_sequence DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sequence DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_collector.

    CLASS-METHODS to_diagram IMPORTING is_cfg            TYPE ts_diagram_config
                             RETURNING value(ro_diagram) TYPE REF TO lcl_diagram_text.
    METHODS get_calls RETURNING value(ro_calls) TYPE REF TO lcl_call_stack.
  PROTECTED SECTION.
    DATA mi_filter TYPE REF TO lif_trace_filter.
    DATA mi_actors TYPE REF TO lif_actors.

    DATA mt_trace TYPE tt_message.
    DATA ms_previous TYPE ts_sat.
    DATA mo_uml TYPE REF TO lcl_uml.
    DATA mv_compact_trace TYPE flag.

    METHODS constructor IMPORTING is_cfg TYPE ts_diagram_config.
    METHODS add IMPORTING is_sat TYPE ts_sat.
    METHODS to_call_stack RETURNING value(ro_calls) TYPE REF TO lcl_call_stack.
    METHODS link_to_previous IMPORTING is_sat TYPE ts_sat
                             RETURNING value(rs_sat) TYPE ts_sat.
ENDCLASS.                    "lcl_sequence DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_factory DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_factory DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS new
      IMPORTING iv_compact_trace  TYPE xsdboolean
      RETURNING value(ro_factory) TYPE REF TO lcl_uml_factory.
    CLASS-METHODS new_calls
      IMPORTING iv_compact_trace TYPE xsdboolean
                it_trace         TYPE tt_message
                io_uml           TYPE REF TO lcl_uml
      RETURNING value(ro_calls)  TYPE REF TO lcl_call_stack.
  PROTECTED SECTION.
    METHODS new_call_stack
      IMPORTING io_uml          TYPE REF TO lcl_uml
                it_trace        TYPE tt_message
      RETURNING value(ro_calls) TYPE REF TO lcl_call_stack.
    METHODS new_iterator IMPORTING it_trace           TYPE tt_message
                         RETURNING value(ro_iterator) TYPE REF TO lcl_messages.
    METHODS fold IMPORTING it_trace    TYPE tt_message
                 EXPORTING et_messages TYPE tt_message.
ENDCLASS.                    "lcl_uml_factory DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_shrinkage DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_shrinkage DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS shrink_references CHANGING ct_cycles TYPE lcl_call_stack_compact=>tt_cycle.
  PRIVATE SECTION.
    TYPES tt_sorted_loop TYPE SORTED TABLE OF ts_cycle
             WITH UNIQUE KEY start end last times.
    DATA mt_shrinked_loop TYPE tt_sorted_loop.
    DATA mv_tabix TYPE sytabix.
    DATA mt_cycles TYPE lcl_call_stack_compact=>tt_cycle.
    DATA ms_stop TYPE ts_cycle.

    METHODS constructor IMPORTING it_cycles TYPE lcl_call_stack_compact=>tt_cycle.
    METHODS insert IMPORTING ls_cycle TYPE ts_cycle.
    METHODS renew RETURNING value(ro_shrink) TYPE REF TO lcl_shrinkage.
    METHODS rebuild RETURNING value(ro_shrink) TYPE REF TO lcl_shrinkage.
ENDCLASS.                    "lcl_shrinkage DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_compactor DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_compactor DEFINITION INHERITING FROM lcl_uml_factory FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_cycles.
  PROTECTED SECTION.
    METHODS new_call_stack REDEFINITION.
    METHODS fold REDEFINITION.
  PRIVATE SECTION.
    DATA mt_cycles TYPE lcl_call_stack_compact=>tt_cycle.
ENDCLASS.                    "lcl_uml_compactor DEFINITION

"! Class LCL_PATTERN finds repetitions (cycles/loops) in the internal table.
"! Method CLONE( ) generates new iterators starting at the next repeated
"! entry in the given trace table and recursively checks within the match.
"! When a loop is found, an entry with
"!    the index of the starting entry of the repeated pattern  (e.g. 1)
"!    the index of the end entry of the repetition (e.g. 4)
"!    the number of loop counts (e.g. 3)
"!    the index of the last entry when the loop is exited  (e.g. 12)
"! is stored using the COLLECT( ) method of interface LIF_CYCLES.
*----------------------------------------------------------------------*
*       CLASS lcl_pattern DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pattern DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING it_xindex TYPE lcl_trace_index=>tt_x_index
                                  iv_start  TYPE sytabix DEFAULT 1
                                  iv_stop   TYPE sytabix OPTIONAL.
    METHODS has_next RETURNING value(rv_flag) TYPE xsdboolean.
    METHODS skip IMPORTING iv_count TYPE i DEFAULT 1.

    METHODS detect_cycles.

    CLASS-METHODS new IMPORTING it_xindex         TYPE lcl_trace_index=>tt_x_index
                                iv_start          TYPE sytabix DEFAULT 1
                                iv_stop           TYPE sytabix OPTIONAL
                                ii_cycles         TYPE REF TO lif_cycles
                      RETURNING value(ro_pattern) TYPE REF TO lcl_pattern.
  PROTECTED SECTION.
    CONSTANTS c_max_step TYPE i VALUE 1024.
    DATA mi_cycles TYPE REF TO lif_cycles.

    METHODS clone IMPORTING iv_start       TYPE sytabix DEFAULT 1
                            iv_stop        TYPE sytabix OPTIONAL
                              PREFERRED PARAMETER iv_start
                  RETURNING value(ro_loop) TYPE REF TO lcl_pattern.
    METHODS next_occurrence_from IMPORTING iv_from       TYPE sytabix
                                 RETURNING value(rv_row) TYPE sytabix.
    METHODS occurs_in IMPORTING iv_start       TYPE sytabix
                                iv_end         TYPE sytabix
                      RETURNING value(rv_same) TYPE xsdboolean.
*   Condense loop logic
    METHODS match_pattern IMPORTING iv_end          TYPE sytabix
                          RETURNING value(rv_match) TYPE xsdboolean.
  PRIVATE SECTION.
    DATA mv_idx TYPE sytabix.
    DATA mv_size TYPE sytabix.
    DATA mt_xindex TYPE lcl_trace_index=>tt_x_index.
ENDCLASS.                    "lcl_pattern DEFINITION

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
                       RETURNING value(rv_param) TYPE string.
    METHODS participant REDEFINITION.
    METHODS header REDEFINITION.
    METHODS footer REDEFINITION.
    METHODS skip_note REDEFINITION.
  PRIVATE SECTION.
    DATA mv_param TYPE string.
    METHODS add_param IMPORTING iv_cond    TYPE xsdboolean DEFAULT abap_true
                                iv_command TYPE string.
    METHODS scale IMPORTING is_cfg          TYPE ts_diagram_config
                  RETURNING value(rv_scale) TYPE string.
ENDCLASS.                    "lcl_uml_plant DEFINITION

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
ENDCLASS.                    "lcl_uml_graph DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_mscgen DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
ENDCLASS.                    "lcl_uml_mscgen DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_logger DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_logger DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_log,
             method TYPE char30,
             params TYPE string,
           END OF ts_log.
    TYPES tt_log TYPE STANDARD TABLE OF ts_log WITH DEFAULT KEY.

    CLASS-METHODS new RETURNING value(ro_log) TYPE REF TO lcl_logger.
    METHODS info IMPORTING is_log TYPE ts_log.
    METHODS verify IMPORTING it_exp TYPE tt_log
                             iv_msg TYPE string.
  PROTECTED SECTION.
    DATA mt_log TYPE tt_log.
ENDCLASS.                    "lcl_logger DEFINITION

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
ENDCLASS.                    "lcl_uml_logger DEFINITION

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

*----------------------------------------------------------------------*
*       CLASS lcl_bag IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bag IMPLEMENTATION.

  METHOD add.
    INSERT iv_entry INTO TABLE mt_index.
  ENDMETHOD.                    "add

  METHOD remove.
    DELETE TABLE mt_index FROM iv_entry.
  ENDMETHOD.                    "remove

  METHOD contains.
    READ TABLE mt_index TRANSPORTING NO FIELDS WITH KEY table_line = iv_entry.
    rv_flag = boolc( sy-subrc EQ 0 ).
  ENDMETHOD.                    "contains

ENDCLASS.                    "lcl_bag IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_filter_null IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_filter_null IMPLEMENTATION.

  METHOD accepts.
    rv_flag = abap_true.
  ENDMETHOD.                    "accepts

ENDCLASS.                    "lcl_filter_null IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_filter IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
  ENDMETHOD.                    "constructor

  METHOD new.
    IF is_cfg IS INITIAL.
      CREATE OBJECT ri_filter TYPE lcl_filter_null
        EXPORTING
          is_cfg = is_cfg.
    ELSEIF is_cfg-pattern IS INITIAL.
      CREATE OBJECT ri_filter TYPE lcl_filter
        EXPORTING
          is_cfg = is_cfg.
    ELSE.
      CREATE OBJECT ri_filter TYPE lcl_filter_custom
        EXPORTING
          is_cfg = is_cfg.
    ENDIF.
  ENDMETHOD.                    "new

  METHOD accepts.
    CONSTANTS c_system_flag TYPE satr_de_sysflag VALUE 'S'.

    IF is_sat-id CA mv_config_id_reject.
      rv_flag = abap_false.
    ELSEIF ms_cfg-system_events EQ abap_false.
      rv_flag = boolc( is_sat-system NE c_system_flag ).
    ELSE.
      rv_flag = abap_true.
    ENDIF.
  ENDMETHOD.                    "accepts

ENDCLASS.                    "lcl_filter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_filter_custom IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_filter_custom IMPLEMENTATION.

  METHOD constructor.
    super->constructor( is_cfg ).
    mv_regex = get_regex( ms_cfg-pattern ).
  ENDMETHOD.                    "constructor

  METHOD get_regex.
    FIELD-SYMBOLS <lv_pattern> LIKE LINE OF it_pattern.
    CLEAR rv_regex.
    LOOP AT it_pattern ASSIGNING <lv_pattern> WHERE table_line IS NOT INITIAL.
      IF rv_regex IS INITIAL.
        rv_regex = <lv_pattern>.
      ELSE.
        rv_regex = |{ rv_regex }\|{ <lv_pattern> }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_regex

  METHOD customer_namespace.
*   Check if IV_NAME satisfies MV_REGEX pattern
    DATA lv_offset TYPE i.
    DATA lv_mlen TYPE i.

    rv_flag = boolc( mv_regex IS INITIAL ).
    CHECK rv_flag EQ abap_false.
    FIND REGEX mv_regex IN iv_name MATCH OFFSET lv_offset
      MATCH LENGTH lv_mlen.
    rv_flag = boolc( 0 EQ sy-subrc AND 0 EQ lv_offset AND lv_mlen GT 0 ).
  ENDMETHOD.                    "customer_namespace

  METHOD accepts.
    rv_flag = super->accepts( is_sat ).
    CHECK rv_flag EQ abap_true.
*   Filter logic
    rv_flag = boolc(
*      Called object is custom code
       ( is_sat-called-global IS NOT INITIAL AND customer_namespace( is_sat-called-global ) EQ abap_true )
       OR
*      If initial, caller object must be custom code
       ( is_sat-called-global IS INITIAL AND customer_namespace( is_sat-caller-global ) EQ abap_true )
       ).
  ENDMETHOD.                    "accepts

ENDCLASS.                    "lcl_filter_custom IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_class_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class_name IMPLEMENTATION.

  METHOD external.
    CHECK cv_clas+30(2) = 'CP'.
    cv_clas = translate( val = cv_clas(30) from = '=' to = ' ' ).
    cv_clas+30(2) = space.
  ENDMETHOD.                    "external

  METHOD technical.
    rv_class = to_internal( iv_name ).
    rv_class+30(2) = 'CP'.
  ENDMETHOD.                    "technical

  METHOD to_internal.
    rv_class = iv_name.
*    rv_class = translate( val = rv_class(30) from = ' ' to = '=' ).  "DOES not work!?
    TRANSLATE rv_class(30) USING ' ='.
  ENDMETHOD.                    "to_internal

  METHOD is_global.
    DATA ls_class LIKE LINE OF mt_class.
*   Cache Proxy for table TADIR
    READ TABLE mt_class INTO ls_class WITH KEY name = iv_name.
    IF sy-subrc EQ 0.
      rv_flag = ls_class-global.
    ELSE.
      rv_flag = is_global_class( iv_name ).
    ENDIF.
  ENDMETHOD.                    "is_global

  METHOD is_global_class.
    DATA lv_pgmid TYPE tadir-pgmid.
    DATA ls_class LIKE LINE OF mt_class.

    CLEAR rv_flag.
    SELECT SINGLE pgmid FROM tadir INTO lv_pgmid            " Single Record buffered
      WHERE pgmid = 'R3TR'
      AND   object = 'CLAS'
      AND   obj_name = iv_name.
    rv_flag = boolc( sy-subrc EQ 0 ).
*   add to cache proxy
    ls_class-name = iv_name.
    ls_class-global = rv_flag.
    INSERT ls_class INTO TABLE mt_class.
  ENDMETHOD.                    "is_global_class

ENDCLASS.                    "lcl_class_name IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_abap_trace IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_trace IMPLEMENTATION.

  METHOD constructor.
*   Query ABAP execution trace created by transaction SAT without aggregation
    CREATE OBJECT: mo_atra,
                   mo_class_name.
  ENDMETHOD.                    "constructor

  METHOD module.
    FIELD-SYMBOLS <ls_prog> TYPE satr_prog.
*   PROGINDEX  : Index for Table TRACEPROG
    READ TABLE mo_atra->it_traceprog INDEX iv_index ASSIGNING <ls_prog>.
    rv_subrc = sy-subrc.
    CHECK rv_subrc = 0.
    ms_target-global = <ls_prog>-cprog.
    ms_target-type = <ls_prog>-object.
    ms_target-system = <ls_prog>-sysflag.
  ENDMETHOD.                    "module

  METHOD event.
    FIELD-SYMBOLS <ls_text> TYPE satr_tracetext.
*   TEXTINDEX  : Index for Internal Table TRACETEXT
    READ TABLE mo_atra->it_tracetext INDEX mr_src->textindex ASSIGNING <ls_text>.
    rv_subrc = sy-subrc.
    CHECK rv_subrc = 0.
    ms_target-event = <ls_text>-tracetext.
    ms_target-system = <ls_text>-sysflag.
  ENDMETHOD.                    "event

  METHOD set_meth.
    FIELD-SYMBOLS <ls_meth> TYPE satr_tracmeth.
    FIELD-SYMBOLS <ls_text> TYPE satr_tracetext.
    DATA lv_tracetext TYPE program.
*   METHINDEX  : Index for Method Name Table SATR_T_TRACEMETH
    module( mr_src->progindex ).
*   CALLED-Data
    READ TABLE mo_atra->it_tracemeth INDEX mr_src->methindex ASSIGNING <ls_meth>.
    IF sy-subrc = 0.
      ms_target-event = <ls_meth>-methode.
    ENDIF.

    READ TABLE mo_atra->it_tracetext INDEX mr_src->textindex ASSIGNING <ls_text>.
    CHECK sy-subrc = 0.
    lv_tracetext = <ls_text>-tracetext.  " type conversion

    READ TABLE mo_atra->it_traceprog TRANSPORTING NO FIELDS
      WITH KEY cprog = mo_class_name->to_internal( lv_tracetext ).
    CHECK module( sy-tabix ) NE 0.
    set_local_class( lv_tracetext ).
  ENDMETHOD.                    "set_meth

  METHOD set_constructor.
    FIELD-SYMBOLS <ls_text> TYPE satr_tracetext.
    DATA lv_tracetext TYPE program.

    READ TABLE mo_atra->it_tracetext INDEX mr_src->textindex ASSIGNING <ls_text>.
    CHECK sy-subrc = 0.
    ms_target-type = c_type_class.
    ms_target-event = c_create_method.
    ms_target-system = <ls_text>-sysflag.
    lv_tracetext = <ls_text>-tracetext.             " type conversion

    READ TABLE mo_atra->it_traceprog TRANSPORTING NO FIELDS
      WITH KEY cprog = lv_tracetext.
    CHECK sy-subrc NE 0.
    ms_target-global = mo_class_name->technical( lv_tracetext ).
    set_local_class( lv_tracetext ).
  ENDMETHOD.                    "set_constructor

  METHOD set_local_class.
    CHECK mo_class_name->is_global( iv_progname ) EQ abap_false.
    ms_target-local = iv_progname.

    CHECK module( mr_src->progindex ) EQ 0.
    ms_target-type = c_type_class.           " overwrite TYPE IF found
  ENDMETHOD.                    "set_local_class

  METHOD set_form.
    event( ).
    module( mr_src->progindex ).
  ENDMETHOD.                    "set_form

  METHOD set_tran.
    CHECK event( ) EQ 0.
    ms_target-global = ms_target-event.
    ms_target-type = c_type_tran.
  ENDMETHOD.                    "set_tran

  METHOD set_submit.
    CHECK event( ) EQ 0.
    READ TABLE mo_atra->it_traceprog TRANSPORTING NO FIELDS
      WITH KEY cprog = ms_target-event.
    module( sy-tabix ).
  ENDMETHOD.                    "set_submit

  METHOD set_function_module.
*   PR_2INDEX  : Index of Int. Table TRACEPROG for Performs
    CHECK event( ) EQ 0.
    module( mr_src->pr_2index ).
  ENDMETHOD.                    "set_function_module

  METHOD parse.
    rv_flag = abap_true.
*   General data
    CLEAR ms_target.
    ms_target-instance = mr_src->instance.

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
  ENDMETHOD.                    "parse

ENDCLASS.                    "lcl_abap_trace IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_trace IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_trace IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    lcl_progress_indicator=>echo( iv_text = |Parse { lines( mo_atra->it_austab_hier ) } entries| ).
    CREATE OBJECT mo_bag.
  ENDMETHOD.                    "constructor

  METHOD fill.
    DATA ls_sat TYPE ts_sat.
*   Parse call hierarchy, apply custom filters, fill collection of messages
    LOOP AT mo_atra->it_austab_hier REFERENCE INTO mr_src.

      CHECK new_record( ) EQ abap_true.
      ls_sat = get_sat( ).

      IF ii_collector->collect( ls_sat ) EQ abap_false.
        skip_record( ).
      ENDIF.

      put_caller( ls_sat ).
    ENDLOOP.
  ENDMETHOD.                    "fill

  METHOD new_record.
    rv_flag = abap_false.

    IF mr_src->event = c_event_entry.           " Event ID (> Entry, < Exit)
      mo_bag->add( mr_src->index ).

*     Parse call hierarchy in trace
      rv_flag = parse( ).
    ELSE.
      skip_record( ).
    ENDIF.
  ENDMETHOD.                    "new_record

  METHOD get_sat.
    DATA ls_seq TYPE ts_seq.

    CLEAR rs_sat.
    rs_sat-id            = mr_src->id.    " Gen. data
    rs_sat-aus_tabix     = mr_src->index.   " Index of DUMP List
    rs_sat-from_level    = mr_src->ebene.   " Call Level (Call Hierarchy)
    rs_sat-called-object = ms_target-object.
    rs_sat-called-mod    = ms_target-event.
    rs_sat-system        = ms_target-system.

*   IV_CALLER  : Entry No. of CALLER in Table TRACE
    READ TABLE mt_caller INTO ls_seq WITH KEY k1 COMPONENTS aus_tabix = mr_src->caller.
    IF sy-subrc = 0.
      rs_sat-caller = ls_seq-called.
    ENDIF.
  ENDMETHOD.                    "get_sat

  METHOD put_caller.
    DATA ls_seq TYPE ts_seq.

    ls_seq = is_sat-seq.
    IF ls_seq-called-global IS INITIAL.
      ls_seq-called = is_sat-caller.
    ENDIF.
    mo_class_name->external( CHANGING cv_clas = ls_seq-called-global ).
    APPEND ls_seq TO mt_caller.
  ENDMETHOD.                    "put_caller

  METHOD skip_record.
    FIELD-SYMBOLS <ls_seq> TYPE ts_seq.
    DATA ls_seq TYPE ts_seq.
*   Aussprungsätze.
    READ TABLE mt_caller ASSIGNING <ls_seq> WITH KEY k1 COMPONENTS aus_tabix = mr_src->start.
    IF sy-subrc EQ 0.

      IF mr_src->id = c_id_class AND mr_src->subid = 'O'.
        IF mo_bag->contains( mr_src->start ) EQ abap_true.
          <ls_seq>-called-instance = mr_src->instance.
        ENDIF.
      ENDIF.

      CLEAR ls_seq.
      ls_seq-called-instance = mr_src->instance.
      ls_seq-aus_tabix       = mr_src->index.
      APPEND ls_seq TO mt_caller.
    ELSE.
      mo_bag->remove( mr_src->start ).
    ENDIF.
  ENDMETHOD.                    "skip_record

ENDCLASS.                    "lcl_trace IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_actors IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_actors IMPLEMENTATION.

  METHOD next_number.
    ADD 1 TO mv_last_number.
    rv_key = mv_last_number.
  ENDMETHOD.                    "next_number

  METHOD index_of.
    DATA ls_actor TYPE ts_actor.

    CLEAR rv_idx.
    READ TABLE mt_actor INTO ls_actor WITH KEY object = is_object.
    IF sy-subrc EQ 0.
      rv_idx = sy-tabix.
    ELSE.
      IF is_object-type EQ c_type_class.
        IF is_object-instance IS NOT INITIAL.
*         Inheritance cases: do not check object name
          READ TABLE mt_actor INTO ls_actor WITH KEY instance = is_object-instance.
        ELSE.
          READ TABLE mt_actor INTO ls_actor
            WITH KEY instance = is_object-instance
                     local = is_object-local
                     global = is_object-global.
        ENDIF.
      ELSEIF is_object-type IS NOT INITIAL.
        READ TABLE mt_actor INTO ls_actor
          WITH KEY local = is_object-local
                   global = is_object-global
                   type = is_object-type.
      ENDIF.
      IF sy-subrc EQ 0.
        rv_idx = sy-tabix.
      ENDIF.
    ENDIF.
    CHECK rv_idx IS NOT INITIAL.
    rv_idx = ls_actor-index.  " mapping
  ENDMETHOD.                    "index_of

  METHOD put.
    DATA ls_actor TYPE ts_actor.

    rv_key = index_of( is_object ).
    CHECK rv_key IS INITIAL.
    rv_key = next_number( ).                      " Next primary key field or SORTED

    ls_actor-object = is_object.
    ls_actor-index = rv_key.
    ls_actor-label = lifeline( is_object ).
    INSERT ls_actor INTO TABLE mt_actor.
  ENDMETHOD.                    "put

  METHOD lif_actors~new_path.
    rs_path-caller = put( is_sat-caller-object ).
    rs_path-called = put( is_sat-called-object ).
  ENDMETHOD.                    "lif_actors~new_path

  METHOD lifeline.
    DATA lv_name TYPE string.

    CASE is_object-type.
      WHEN c_type_class.
        lv_name = |{ is_object-local }\\n{ is_object-global }|.

        IF is_object-instance EQ 0.
          rv_text = |Static Methods of Class { lv_name }|.
        ELSE.
          rv_text = |ObjectId:{ is_object-instance } of Class { lv_name }|.
        ENDIF.
      WHEN c_type_fugr.
        IF is_object-global+0(1) EQ '/'.
          lv_name = is_object-global+0(6) && is_object-global+10.
        ELSE.
          lv_name = is_object-global+4.
        ENDIF.
        rv_text = |Function Group\\n{ lv_name }|.
      WHEN OTHERS.
        rv_text = |{ is_object-type }\\n{ is_object-global }|.
    ENDCASE.
  ENDMETHOD.                    "lifeline

  METHOD class_name.
    DATA ls_actor TYPE ts_actor.

    CLEAR rv_short_name.
    READ TABLE mt_actor WITH TABLE KEY obj_nr COMPONENTS index = iv_index INTO ls_actor.
    CHECK sy-subrc EQ 0.

    IF ls_actor-local EQ space.
      rv_short_name = ls_actor-global.
    ELSE.
      rv_short_name = ls_actor-local.
    ENDIF.
  ENDMETHOD.                    "class_name

  METHOD lif_actors~lifelines.
    DATA ls_actor TYPE ts_actor.

    CLEAR rt_lifeline.
    LOOP AT mt_actor INTO ls_actor.
      INSERT ls_actor-lifeline INTO TABLE rt_lifeline.
    ENDLOOP.
  ENDMETHOD.                    "lif_actors~lifelines

  METHOD lif_actors~short_text.
    CASE is_message-id.
      WHEN c_id_skip.
        rv_text = |<b>Skipping over SAP code until calling { is_message-msg }</b>|.
      WHEN c_id_class.
        rv_text = |Create instance of class { class_name( is_message-called ) }|.
      WHEN OTHERS.
        rv_text = call_short_text( is_message ).
    ENDCASE.
  ENDMETHOD.                    "lif_actors~short_text

  METHOD call_short_text.
    DATA lv_call_text TYPE string.

    CASE is_message-id.
      WHEN c_id_form.
        lv_call_text = |Perform|.
      WHEN c_id_func.
        lv_call_text = |Call FM|.
      WHEN c_id_method.
        lv_call_text = |Call method|.
      WHEN OTHERS.
        lv_call_text = space.
    ENDCASE.
    rv_text = |{ lv_call_text } { is_message-msg }|.
  ENDMETHOD.                    "call_short_text

ENDCLASS.                    "lcl_actors IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_sequence IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sequence IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mi_actors TYPE lcl_actors.
    mi_filter = lcl_filter=>new( is_cfg ).            " Custom filter
    mo_uml = lcl_uml=>new( iv_uml = is_cfg-uml_format
                           ii_actors = mi_actors ).
    mv_compact_trace = is_cfg-compact_trace.
  ENDMETHOD.                    "constructor

  METHOD link_to_previous.
    DATA ls_sat TYPE ts_sat.
*   add a link to last valid entry to fill gaps created by filter logic
    rs_sat = is_sat.
    IF ms_previous IS NOT INITIAL AND ms_previous-called NE is_sat-caller
      AND is_sat-from_level GT ms_previous-from_level.
      IF is_sat-caller IS INITIAL.
        rs_sat-caller = ms_previous-called.  " check initial?
      ELSE.
*       insert a new line into the table at this point (link to last valid line).
        CLEAR ls_sat.
        ls_sat-id            = c_id_skip.
        ls_sat-from_level    = ms_previous-from_level.   " Call Level (Call Hierarchy)
        ls_sat-caller        = ms_previous-called.
        ls_sat-called        = is_sat-caller.
        add( ls_sat ).
      ENDIF.
    ENDIF.
    ms_previous = rs_sat.
  ENDMETHOD.                    "link_to_previous

  METHOD lif_collector~collect.
    rv_flag = mi_filter->accepts( is_sat ).
    CHECK rv_flag EQ abap_true.
*   Update previous entry (fill gaps if needed)
    add( link_to_previous( is_sat ) ).
  ENDMETHOD.                    "lif_collector~collect

  METHOD add.
    DATA ls_message TYPE ts_message.

    ls_message-id = is_sat-id.
    ls_message-from_level = is_sat-from_level.
    ls_message-msg = is_sat-called-mod.
    ls_message-path = mi_actors->new_path( is_sat ).
    APPEND ls_message TO mt_trace.
  ENDMETHOD.                    "add

  METHOD to_diagram.
    DATA lo_progress TYPE REF TO lcl_progress_indicator.
    DATA lo_sequence TYPE REF TO lcl_sequence.

    lo_progress = lcl_progress_indicator=>new( is_cfg ).
    CREATE OBJECT lo_sequence
      EXPORTING
        is_cfg = is_cfg.
    ro_diagram = lcl_diagram_text=>new( is_cfg = is_cfg
                                        iv_text = lo_sequence->get_calls( )->to_uml( ) ).
    lo_progress->restore( ).
  ENDMETHOD.                    "to_diagram

  METHOD get_calls.
    DATA lo_trace TYPE REF TO lcl_trace.
*   filter and convert ABAP call hierarchy trace:
    CREATE OBJECT lo_trace.
    lo_trace->fill( me ).
    ro_calls = to_call_stack( ).
  ENDMETHOD.                    "get_calls

  METHOD to_call_stack.
*   Convert trace to call stack object
    ro_calls = lcl_uml_factory=>new_calls( iv_compact_trace = mv_compact_trace
                                           it_trace = mt_trace
                                           io_uml = mo_uml ).
  ENDMETHOD.                    "to_call_stack

ENDCLASS.                    "lcl_sequence IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml IMPLEMENTATION.

  METHOD new.
    CASE iv_uml.
      WHEN c_uml_graph.
        CREATE OBJECT ro_uml TYPE lcl_uml_graph.
      WHEN c_uml_mscgen.
        CREATE OBJECT ro_uml TYPE lcl_uml_mscgen.
      WHEN c_uml_mock.
        CREATE OBJECT ro_uml TYPE lcl_uml_logger.
      WHEN OTHERS.
        CREATE OBJECT ro_uml TYPE lcl_uml_plant.
    ENDCASE.
    ro_uml->mi_actors = ii_actors.
  ENDMETHOD.                    "new

  METHOD add.
    mv_diagram = mv_diagram && iv_code.
  ENDMETHOD.                    "add

  METHOD complete.
    RETURN.
  ENDMETHOD.                    "complete

  METHOD separator.
    RETURN.
  ENDMETHOD.                    "separator

  METHOD delimiter.
    RETURN.
  ENDMETHOD.                    "delimiter

  METHOD top.
    DATA ls_lifeline TYPE ts_lifeline.
    DATA lt_lifeline TYPE lif_actors=>tt_lifeline.

    header( ).
*   Init life lines
    lt_lifeline = mi_actors->lifelines( ).
    LOOP AT lt_lifeline INTO ls_lifeline.
      participant( ls_lifeline ).
      AT LAST.
        delimiter( ).          "if needed: e.g. add( |;\n| ).
        EXIT.
      ENDAT.
      separator( ).            "if needed: e.g. add( |, | ).
    ENDLOOP.
  ENDMETHOD.                    "top

  METHOD bottom.
    DATA ls_lifeline TYPE ts_lifeline.
    DATA lt_lifeline TYPE lif_actors=>tt_lifeline.

*   Complete all life lines
    lt_lifeline = mi_actors->lifelines( ).
    LOOP AT lt_lifeline INTO ls_lifeline.
      complete( ls_lifeline ).
    ENDLOOP.
    footer( ).
    rv_diagram = mv_diagram.
  ENDMETHOD.                    "bottom

  METHOD message.
    CHECK is_message IS NOT INITIAL.
    call( is_message ).

    IF is_message-id = c_id_skip.
      skip_note( is_message-path ).
    ENDIF.
  ENDMETHOD.                    "message

ENDCLASS.                    "lcl_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_plant IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_plant IMPLEMENTATION.

  METHOD add_param.
    CHECK iv_cond EQ abap_true.
    mv_param = mv_param && iv_command.
  ENDMETHOD.                    "add_param

  METHOD scale.
    DATA lv_scale TYPE tv_scale.
    lv_scale = nmin( val1 = 1
                     val2 = nmax( val1 = is_cfg-scale
                                  val2 = c_min_scale ) ).
    rv_scale = |scale { lv_scale DECIMALS = 2 }\n|.
  ENDMETHOD.                    "scale

  METHOD parameters.
    CLEAR mv_param.
    add_param( scale( is_cfg ) ). " Reduce the size of the output image
    add_param( iv_cond = is_cfg-teoz_architecture
               iv_command = |!pragma teoz true\n| ).
    add_param( |skinparam \{\n| ).
*   For performance: disable shadowing
    add_param( iv_cond = boolc( is_cfg-shadowing EQ abap_false )
               iv_command = |shadowing false\n| ).
*   mimic hand writting
    add_param( iv_cond = is_cfg-handwritten
               iv_command = |handwritten true\n| ).
    add_param( |\}\n| ).
    rv_param = mv_param.
  ENDMETHOD.                    "parameters

  METHOD header.
*   Plant UML Header
    add( |@startuml\nhide footbox\nautonumber\n{ parameters( lcl_configuration=>get( ) ) }| ).
  ENDMETHOD.                    "header

  METHOD call.
    add( |{ is_message-caller } -> { is_message-called }: { mi_actors->short_text( is_message ) }\n| ).
    add( |activate { is_message-called }\n| ).
  ENDMETHOD.                    "call

  METHOD return.
*   return and deactivate the actor
    IF iv_from NE iv_to.
      add( |{ iv_from } --> { iv_to }\n| ).
    ENDIF.
    CHECK iv_from IS NOT INITIAL.
    add( |deactivate { iv_from }\n| ).
  ENDMETHOD.                    "return

  METHOD participant.
    add( |participant "{ is_lifeline-label }" as { is_lifeline-index }\n| ).
  ENDMETHOD.                    "participant

  METHOD footer.
    add( |@enduml\n| ).
  ENDMETHOD.                    "footer

  METHOD skip_note.
    add( |note over { is_path-caller },{ is_path-called }\n{ c_txt_std }\nend note\n| ).
  ENDMETHOD.                    "skip_note

  METHOD begin_loop.
    add( |loop { iv_times } times\n| ).        " pop
  ENDMETHOD.                    "begin_loop

  METHOD end_loop.
    add( |end\n| ).
  ENDMETHOD.                    "end_loop

ENDCLASS.                    "lcl_uml_plant IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_messages IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_messages IMPLEMENTATION.

  METHOD constructor.
    CLEAR mv_idx.
    mt_list = it_messages.    " shared, never changed
    IF iv_stop IS INITIAL.
      mv_size = lines( mt_list ).
    ELSE.
      mv_size = iv_stop.
    ENDIF.
    skip( iv_start - 1 ).
    mv_first_level = next_level( ).
  ENDMETHOD.                    "constructor

  METHOD next.
    skip( ).
    READ TABLE mt_list INDEX mv_idx INTO rs_data.
    CHECK sy-subrc NE 0.
    RAISE EXCEPTION TYPE cx_sy_itab_error.
  ENDMETHOD.                    "next

  METHOD has_next.
    rv_flag = boolc( mv_idx < mv_size ).
  ENDMETHOD.                    "has_next

  METHOD skip.
    ADD iv_count TO mv_idx.
  ENDMETHOD.                    "skip

  METHOD is_first.
    rv_flag = boolc( mv_idx LE 1 ).
  ENDMETHOD.                    "is_first

  METHOD first_level.
    rv_level = mv_first_level.
  ENDMETHOD.                    "first_level

  METHOD next_level.
    DATA ls_data TYPE ts_message.

    IF mv_idx < mv_size.
      READ TABLE mt_list INDEX mv_idx + 1 INTO ls_data.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ELSE.
        rv_level = ls_data-from_level.
      ENDIF.
    ELSE.
      rv_level = c_default_level.
    ENDIF.
  ENDMETHOD.                    "next_level

ENDCLASS.                    "lcl_messages IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_stack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_stack IMPLEMENTATION.

  METHOD push.
    DATA lr_level LIKE mr_level.
*   First create new node
    CREATE DATA lr_level.
    lr_level->call_level = is_ref.
    lr_level->next = mr_level.
*   then overwrite old node
    mr_level = lr_level.

    mv_empty = abap_false.
  ENDMETHOD.                    "push

  METHOD pop.
    FIELD-SYMBOLS <lr_next> TYPE ts_level.
    CLEAR rs_to.
    IF mr_level IS BOUND.
      rs_to = mr_level->call_level.
      mr_level ?= mr_level->next.
    ENDIF.
    mv_empty = boolc( mr_level IS NOT BOUND ).
  ENDMETHOD.                    "pop

ENDCLASS.                    "lcl_stack IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_stack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_stack IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_uml = io_uml.
    mv_previous_level = lcl_messages=>c_default_level.
  ENDMETHOD.                    "constructor

  METHOD call.
    DATA ls_call TYPE ts_call_level.

    ls_call-actor_key = is_message-called.
    ls_call-from_level = is_message-from_level.
    push( ls_call ).
*   UML text form generator
    mo_uml->message( is_message ).
  ENDMETHOD.                    "call

  METHOD return.
    IF iv_to_level <= mv_previous_level.
      return_to( iv_to_level ).
    ENDIF.
    mv_previous_level = iv_to_level.
  ENDMETHOD.                    "return

  METHOD return_to.
    DATA ls_from TYPE ts_call_level.
    DATA ls_to TYPE ts_call_level.

    ls_from = pop( ).
    WHILE ls_from-from_level >= iv_level AND mv_empty EQ abap_false.
      ls_to = pop( ).
*     UML diagram
      mo_uml->return( iv_from = ls_from-actor_key
                      iv_to = ls_to-actor_key ).
      ls_from = ls_to.
    ENDWHILE.

    CHECK ls_to IS NOT INITIAL.
    push( ls_to ).
  ENDMETHOD.                    "return_to

ENDCLASS.                    "lcl_uml_stack IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_call_stack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_call_stack IMPLEMENTATION.

  METHOD constructor.
    super->constructor( io_uml ).
    mo_messages = io_messages.
  ENDMETHOD.                    "constructor

  METHOD message.
    DATA ls_message TYPE ts_message.

    IF mo_messages->is_first( ) EQ abap_true.
*     very first call
      ls_message-caller = mo_messages->first_level( ).
      ls_message-called = c_first_key.
      call( ls_message ).
    ENDIF.
    call( is_message ).
    return( mo_messages->next_level( ) ).
  ENDMETHOD.                    "message

  METHOD to_uml.
*   UML header
    mo_uml->top( ).
*   Convert call stack to UML sequence diagram in text format
    WHILE mo_messages->has_next( ) EQ abap_true.
      message( is_message = mo_messages->next( )
               iv_idx = sy-index ).
    ENDWHILE.
*   add UML footer
    rv_diagram = mo_uml->bottom( ).
  ENDMETHOD.                    "to_uml

ENDCLASS.                    "lcl_call_stack IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_call_stack_compact IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_call_stack_compact IMPLEMENTATION.

  METHOD constructor.
    super->constructor( io_uml = io_uml
                        io_messages = io_messages ).
    mt_cycle = it_cycles.
  ENDMETHOD.                    "constructor

  METHOD message.
    begin( iv_idx = iv_idx
           iv_from = is_message-caller ).                " if needed
    super->message( is_message = is_message
                    iv_idx = iv_idx ).
*   after return so the loop encloses the return path
    mo_messages->skip( end( iv_idx = iv_idx
                            iv_to = is_message-called ) ).
  ENDMETHOD.                    "message

  METHOD name.
*   Loop ID
    rv_name = |{ iv_start }to{ iv_end }|.
  ENDMETHOD.                    "name

  METHOD begin.
    DATA ls_cycle TYPE ts_cycle.
    LOOP AT mt_cycle INTO ls_cycle WHERE start = iv_idx.
      mo_uml->begin_loop( iv_from = |{ iv_from }|
                          iv_times = ls_cycle-times
                          iv_name = name( iv_start = iv_idx
                                          iv_end = ls_cycle-end ) ).
    ENDLOOP.
  ENDMETHOD.                    "begin

  METHOD end.
    DATA ls_cycle TYPE ts_cycle.
    CLEAR rv_step.
    LOOP AT mt_cycle INTO ls_cycle WHERE end = iv_idx.  "#EC CI_SORTSEQ
      mo_uml->end_loop( iv_to = |{ iv_to }|
                        iv_name = name( iv_start = ls_cycle-start
                                        iv_end = iv_idx ) ).
      rv_step = nmax( val1 = ls_cycle-last - iv_idx
                      val2 = rv_step ).
    ENDLOOP.
  ENDMETHOD.                    "end

ENDCLASS.                    "lcl_call_stack_compact IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_trace_index IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_trace_index IMPLEMENTATION.

  METHOD constructor.
    mt_trace = it_trace.
    mi_cycles = ii_cycles.
    to_index( ).
    lcl_progress_indicator=>echo( iv_text = |Compact trace with { lines( it_trace ) } entries| ).
  ENDMETHOD.                    "constructor

  METHOD to_index.
    TYPES tt_trace_index TYPE STANDARD TABLE OF sytabix.
    DATA lt_trace_index TYPE tt_trace_index.
    FIELD-SYMBOLS <ls_trace> LIKE LINE OF mt_trace.

*    mt_components = VALUE #( FOR t IN mt_trace ( t-message ) ).  dump on duplicates! does not pass test
    CLEAR mt_components.
    LOOP AT mt_trace ASSIGNING <ls_trace>.
      INSERT <ls_trace> INTO TABLE mt_components.
    ENDLOOP.

    CLEAR lt_trace_index.
    LOOP AT mt_trace ASSIGNING <ls_trace>.
      READ TABLE mt_components FROM <ls_trace> TRANSPORTING NO FIELDS.
      APPEND sy-tabix TO lt_trace_index.
    ENDLOOP.

    mt_xindex = lt_trace_index.
  ENDMETHOD.                    "to_index

  METHOD to_trace.
    DATA lv_x_idx TYPE tv_x_tabix.
    DATA ls_trace TYPE ts_message.

    CLEAR ct_trace.
    LOOP AT mt_xindex INTO lv_x_idx.
      CLEAR ls_trace.
      READ TABLE mt_components INDEX lv_x_idx INTO ls_trace.

      APPEND ls_trace TO ct_trace.
    ENDLOOP.
  ENDMETHOD.                    "to_trace

  METHOD shrink.
    lcl_pattern=>new( it_xindex = mt_xindex
                      ii_cycles = mi_cycles )->detect_cycles( ).
    rv_shrink = filter( ).
    lcl_progress_indicator=>echo( iv_percentage = iv_pass
                                  iv_text = |Compact trace pass { iv_pass } - { lines( mt_xindex ) } entries| ).
  ENDMETHOD.                    "shrink

  METHOD filter.
    DATA lt_xorg LIKE mt_xindex.

    lt_xorg = mt_xindex.
    compact( EXPORTING it_xindex = lt_xorg
             IMPORTING et_xindex = mt_xindex ).

    rv_shrink = boolc( lt_xorg NE mt_xindex ).
  ENDMETHOD.                    "filter

  METHOD compact.
    FIELD-SYMBOLS <lv_x_index> TYPE tv_x_tabix.
    CLEAR et_xindex.
    LOOP AT it_xindex ASSIGNING <lv_x_index>.
      CHECK mi_cycles->accepts( sy-tabix ) EQ abap_true.
      APPEND <lv_x_index> TO et_xindex.
    ENDLOOP.
  ENDMETHOD.                    "compact

ENDCLASS.                    "lcl_trace_index IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_factory IMPLEMENTATION.

  METHOD new.
*   create UML sequence diagram translator
    IF iv_compact_trace EQ abap_true.
      CREATE OBJECT ro_factory TYPE lcl_uml_compactor.
    ELSE.
      CREATE OBJECT ro_factory TYPE lcl_uml_factory.
    ENDIF.
  ENDMETHOD.                    "new

  METHOD new_calls.
    ro_calls = lcl_uml_factory=>new( iv_compact_trace )->new_call_stack( io_uml = io_uml
                                                                         it_trace = it_trace ).
  ENDMETHOD.                    "new_calls

  METHOD new_iterator.
    DATA lt_messages TYPE tt_message.

    fold( EXPORTING it_trace = it_trace
          IMPORTING et_messages = lt_messages ).
    CREATE OBJECT ro_iterator
      EXPORTING
        it_messages = lt_messages.
  ENDMETHOD.                    "new_iterator

  METHOD fold.
    et_messages = it_trace.
  ENDMETHOD.                    "fold

  METHOD new_call_stack.
    CREATE OBJECT ro_calls
      EXPORTING
        io_uml      = io_uml
        io_messages = new_iterator( it_trace ).
  ENDMETHOD.                    "new_call_stack

ENDCLASS.                    "lcl_uml_factory IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_compactor IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_compactor IMPLEMENTATION.

  METHOD fold.
    DATA lo_evt TYPE REF TO lcl_trace_index.

    CLEAR et_messages.
    CREATE OBJECT lo_evt
      EXPORTING
        it_trace  = it_trace
        ii_cycles = me.
    WHILE lo_evt->shrink( sy-index ) EQ abap_true.
      lcl_shrinkage=>shrink_references( CHANGING ct_cycles = mt_cycles ).
    ENDWHILE.
    lo_evt->to_trace( CHANGING ct_trace = et_messages ).
  ENDMETHOD.                    "fold

  METHOD new_call_stack.
    CREATE OBJECT ro_calls TYPE lcl_call_stack_compact
      EXPORTING
        io_uml      = io_uml
        io_messages = new_iterator( it_trace )
        it_cycles   = mt_cycles.
  ENDMETHOD.                    "new_call_stack

  METHOD lif_cycles~collect.
*   duplicate entries are inserted BEFORE the existing row
    INSERT is_cycle INTO TABLE mt_cycles.
  ENDMETHOD.                    "lif_cycles~collect

  METHOD lif_cycles~accepts.
    LOOP AT mt_cycles TRANSPORTING NO FIELDS            "#EC CI_SORTSEQ
      WHERE end LT iv_index AND last GE iv_index.
      rv_flag = abap_false.
      RETURN.
    ENDLOOP.
    rv_flag = abap_true.
  ENDMETHOD.                    "lif_cycles~accepts

ENDCLASS.                    "lcl_uml_compactor IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_shrinkage IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_shrinkage IMPLEMENTATION.

  METHOD shrink_references.
    DATA lo_shrink TYPE REF TO lcl_shrinkage.
*   reconcile the loop reference table
    CREATE OBJECT lo_shrink
      EXPORTING
        it_cycles = ct_cycles.

*   Change needed ?
    WHILE lo_shrink->ms_stop IS NOT INITIAL.
*     Rebuild loop references table:
      lo_shrink = lo_shrink->rebuild( ).
    ENDWHILE.
    ct_cycles = lo_shrink->mt_cycles.
  ENDMETHOD.                    "shrink_references

  METHOD constructor.
    DATA ls_cycle TYPE ts_cycle.
    mt_cycles = it_cycles.

    CLEAR mv_tabix.
    LOOP AT it_cycles INTO ls_cycle.
      mv_tabix = sy-tabix.
*     Change needed in subsequent entries?
      CHECK ls_cycle-last GT ls_cycle-end.
      ms_stop = ls_cycle.
      EXIT.
    ENDLOOP.
  ENDMETHOD.                    "constructor

  METHOD renew.
    DATA lt_cycles TYPE  lcl_call_stack_compact=>tt_cycle.

    lt_cycles = mt_shrinked_loop.
    CREATE OBJECT ro_shrink
      EXPORTING
        it_cycles = lt_cycles.
  ENDMETHOD.                    "renew

  METHOD rebuild.
    DATA ls_cycle TYPE ts_cycle.
    DATA lv_delta TYPE i.
*   after removing repetition from the active loop in the trace table,
*   rebuild (compress) the loop reference table
    LOOP AT mt_cycles INTO ls_cycle.
      AT FIRST.
        lv_delta = ms_stop-last - ms_stop-end.
      ENDAT.
      IF ls_cycle-last LT ms_stop-start.
*       Loops entries before the active loop - verbatim
        insert( ls_cycle ).
      ENDIF.

      IF ls_cycle-start LT ms_stop-start AND ls_cycle-end GE ms_stop-last.
*       Loops entries starting before the active loop to be compacted and
*       that contains it (i.e. they enclose it ) - adjust
        ls_cycle-end = ls_cycle-end - lv_delta.
        ls_cycle-last = ls_cycle-last - lv_delta.
        insert( ls_cycle ).
      ENDIF.

      IF sy-tabix EQ mv_tabix.
*       Update the active loop entry
        ls_cycle = ms_stop.
        ls_cycle-last = ms_stop-end.
        insert( ls_cycle ).
      ELSEIF ls_cycle-start GE ms_stop-start AND ls_cycle-last LE ms_stop-end.
*       Loops entries completely enclosed by the active loop entry - verbatim
        insert( ls_cycle ).
      ENDIF.

      IF ls_cycle-start GT ms_stop-last.
*       Loops entries after the active loop - shifted by Offset
        ls_cycle-start = ls_cycle-start - lv_delta.
        ls_cycle-end = ls_cycle-end - lv_delta.
        ls_cycle-last = ls_cycle-last - lv_delta.
        insert( ls_cycle ).
      ENDIF.

    ENDLOOP.

*   Write back
    ro_shrink = renew( ).
  ENDMETHOD.                    "rebuild

  METHOD insert.
    INSERT ls_cycle INTO TABLE mt_shrinked_loop.
  ENDMETHOD.                    "insert

ENDCLASS.                    "lcl_shrinkage IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pattern IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pattern IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_pattern
      EXPORTING
        it_xindex = it_xindex
        iv_start  = iv_start
        iv_stop   = iv_stop.
    ro_pattern->mi_cycles = ii_cycles.
  ENDMETHOD.                    "new

  METHOD constructor.
    CLEAR mv_idx.
    mt_xindex = it_xindex.         " Copied or Shared??? never changed
    IF iv_stop IS INITIAL.
      mv_size = lines( mt_xindex ).
    ELSE.
      mv_size = iv_stop.
    ENDIF.
    skip( iv_start - 1 ).
  ENDMETHOD.                    "constructor

  METHOD clone.
    DATA lv_stop LIKE iv_stop.

    IF iv_stop IS SUPPLIED.
      lv_stop = iv_stop.
    ELSE.
      lv_stop = mv_size.
    ENDIF.
    ro_loop = new( iv_start = iv_start
                   iv_stop = lv_stop
                   it_xindex = mt_xindex
                   ii_cycles = mi_cycles ).
  ENDMETHOD.                    "clone

  METHOD skip.
    ADD iv_count TO mv_idx.
  ENDMETHOD.                    "skip

  METHOD has_next.
    rv_flag = boolc( mv_idx < mv_size ).
  ENDMETHOD.                    "has_next

  METHOD occurs_in.
    FIELD-SYMBOLS <lv_index> LIKE LINE OF mt_xindex.
    DATA lv_new LIKE LINE OF mt_xindex.
    DATA lv_other LIKE iv_end.

    lv_other = iv_end.
    rv_same = abap_false.
    LOOP AT mt_xindex FROM iv_start TO iv_end ASSIGNING <lv_index>.
      ADD 1 TO lv_other.
      READ TABLE mt_xindex INDEX lv_other INTO lv_new.
      CHECK lv_new NE <lv_index>.
      RETURN.
    ENDLOOP.
*   returning abap_true for zero length tests will lead to infinite loop
    rv_same = boolc( sy-subrc EQ 0 ).
  ENDMETHOD.                    "occurs_in

  METHOD match_pattern.
    DATA ls_group TYPE ts_cycle.
    DATA lv_dist TYPE i.

    rv_match = abap_false.

    lv_dist = iv_end - mv_idx + 1.
    CHECK iv_end GE 0 AND lv_dist GT 0.

    ls_group-start = mv_idx.
    ls_group-end = iv_end.
    ls_group-last = iv_end.
    ls_group-times = 1.
*   Find consecutive occurrences of group
    WHILE ( ls_group-last + lv_dist <= mv_size )
      AND occurs_in( iv_start = mv_idx
                     iv_end = ls_group-last ) EQ abap_true.
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
  ENDMETHOD.                    "match_pattern

  METHOD detect_cycles.
    DATA lv_index TYPE sytabix.
*   identify blocks of repeated messages (cycles)
*   Pass the cycle information to interface LIF_CYCLES
    WHILE has_next( ) EQ abap_true.
      skip( ).
      lv_index = mv_idx.
      DO.                                               "#EC CI_NESTED.
        lv_index = next_occurrence_from( lv_index ).
        CHECK lv_index IS INITIAL OR match_pattern( lv_index - 1 ) EQ abap_true.
        EXIT.
      ENDDO.
    ENDWHILE.
  ENDMETHOD.                    "detect_cycles

  METHOD next_occurrence_from.
    DATA lv_until TYPE sytabix.
    FIELD-SYMBOLS <lv_org> LIKE LINE OF mt_xindex.

    CLEAR rv_row.
    lv_until = nmin( val1 = iv_from + c_max_step
                     val2 = mv_size ).
    CHECK lv_until GT iv_from.
    READ TABLE mt_xindex INDEX iv_from ASSIGNING <lv_org>.
    CHECK sy-subrc EQ 0.
    FIND FIRST OCCURRENCE OF <lv_org> IN TABLE mt_xindex
      FROM iv_from + 1 TO lv_until IN BYTE MODE
      MATCH LINE rv_row.
  ENDMETHOD.                    "next_occurrence_from

ENDCLASS.                    "lcl_pattern IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name IMPLEMENTATION.

  METHOD new.
    CASE iv_mode.
      WHEN lcl_diagram_text=>c_mode_aut.
        CREATE OBJECT ro_file TYPE lcl_file_test_name
          EXPORTING
            iv_mode = iv_mode.
      WHEN OTHERS.
        CREATE OBJECT ro_file TYPE lcl_file_name
          EXPORTING
            iv_mode = iv_mode.
    ENDCASE.
  ENDMETHOD.                    "new

  METHOD constructor.
    CASE iv_mode.
      WHEN lcl_diagram_text=>c_mode_txt.
        ms_file-title = |Save UML text source|.
        ms_file-ext = |.txt|.
      WHEN OTHERS.
        ms_file-title = |Save As...|.
        ms_file-ext = |.txt|.
    ENDCASE.
  ENDMETHOD.                    "constructor

  METHOD get_prefix.
    rv_name = shift_right( val = ms_file-name
                           places = strlen( ms_file-ext ) ).
  ENDMETHOD.                    "get_prefix

  METHOD get_fullpath.
    rv_name = ms_file-path.
  ENDMETHOD.                    "get_fullpath

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
  ENDMETHOD.                    "dialog

ENDCLASS.                    "lcl_file_name IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file_test_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_test_name IMPLEMENTATION.

  METHOD dialog.
    ms_file-path = |test.txt|.
    rv_user_action = cl_gui_frontend_services=>action_cancel.
  ENDMETHOD.                    "dialog

ENDCLASS.                    "lcl_file_test_name IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file IMPLEMENTATION.

  METHOD download.
    rv_subrc = 1.
    CHECK io_name->dialog( ) NE cl_gui_frontend_services=>action_cancel.

    rv_subrc = cl_uml_utilities=>save_xml_local( xml = iv_data
                                                 filename = io_name->get_fullpath( ) ).
  ENDMETHOD.                    "download

ENDCLASS.                    "lcl_file IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_diagram_text IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diagram_text IMPLEMENTATION.

  METHOD new.
    CASE is_cfg-uml_format.
      WHEN OTHERS.
*       create PlantUML diagram by default
        CREATE OBJECT ro_diagram TYPE lcl_diagram_plant_uml
          EXPORTING
            is_cfg     = is_cfg
            iv_diagram = iv_text.
    ENDCASE.
  ENDMETHOD.                    "new

  METHOD constructor.
    mv_diagram = iv_diagram.
    ms_cfg-output_mode = iv_mode.
  ENDMETHOD.                    "constructor

  METHOD output.
    save_file( ms_cfg-output_mode ).
  ENDMETHOD.                    "output

  METHOD to_xstring.
    cl_abap_conv_out_ce=>create( encoding = 'UTF-8' )->convert(
      EXPORTING data = iv_string
      IMPORTING buffer = rv_xstring ).
  ENDMETHOD.                    "to_xstring

  METHOD save_file.
*   export data as (PlantUML) source
    rv_fname = lcl_file=>download( iv_data = to_xstring( mv_diagram )
                                   io_name = lcl_file_name=>new( iv_mode ) ).
  ENDMETHOD.                    "save_file

ENDCLASS.                    "lcl_diagram_text IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_diagram_plant_uml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diagram_plant_uml IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_diagram = iv_diagram
                        iv_mode = is_cfg-output_mode ).
    ms_cfg = is_cfg.
  ENDMETHOD.                    "constructor

  METHOD output.
    CASE ms_cfg-output_mode.
      WHEN c_mode_url.
        display_html( ).
      WHEN c_mode_exe.
        local_plant_uml( ).
      WHEN OTHERS.
        super->output( ).
    ENDCASE.
  ENDMETHOD.                    "output

  METHOD display_url.
    cl_abap_browser=>show_html( html_string = iv_url
                                size = cl_abap_browser=>xlarge
                                context_menu = abap_true ).
  ENDMETHOD.                    "display_url

  METHOD display_html.
    display_url( |<img src="{ to_url( ms_cfg-server_url ) }"/>\n{ source( ) }| ).
  ENDMETHOD.                    "display_html

  METHOD to_url.
    rv_url = iv_base_url && encoded_url_suffix( ).
  ENDMETHOD.                    "to_url

  METHOD encoded_url_suffix.
    DATA lv_bin TYPE xstring.
*   for PlantUML Server: Convert to UTF-8, then deflate, then encode (base64 variant)
    cl_abap_gzip=>compress_binary(
      EXPORTING
        raw_in         = to_xstring( mv_diagram )  ##type  " UTF-8
        compress_level = 9
      IMPORTING
        gzip_out       = lv_bin ).

    rv_url = translate( val = cl_http_utility=>encode_x_base64( lv_bin )
                        from = c_charset_standard
                        to =   c_charset_plantuml ).
  ENDMETHOD.                    "encoded_url_suffix

  METHOD local_plant_uml.
    DATA lo_name TYPE REF TO lcl_file_name.
    lo_name = lcl_file_name=>new( c_mode_txt ).
    CHECK lcl_file=>download( iv_data = to_xstring( mv_diagram )
                              io_name = lo_name ) IS INITIAL.
    display_url( |<img src="{ to_png( lo_name ) }"/>\n{ source( ) }| ).
  ENDMETHOD.                    "local_plant_uml

  METHOD parameter_string.
    rv_param = |-jar { ms_cfg-java_jar } -o { ms_cfg-local_path } "{ io_name->get_fullpath( ) }"|.
  ENDMETHOD.                    "parameter_string

  METHOD png_file_name.
    TRY.
        rv_name = |{ ms_cfg-local_path }{ io_name->get_prefix( ) }.png|.
      CATCH cx_dynamic_check.
        CLEAR rv_name.
    ENDTRY.
  ENDMETHOD.                    "png_file_name

  METHOD source.
    CASE ms_cfg-display_source.
      WHEN abap_false.
        CLEAR rv_source.
      WHEN OTHERS.
        rv_source = |<p>{ mv_diagram }</p>|.
    ENDCASE.
  ENDMETHOD.                    "source

  METHOD to_png.
    CLEAR rv_name.
    cl_gui_frontend_services=>execute(
      EXPORTING application = ms_cfg-java_appl
                parameter = parameter_string( io_name )
                synchronous = 'X'
      EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc EQ 0.
      rv_name = png_file_name( io_name ).
    ENDIF.
  ENDMETHOD.                    "to_png

  METHOD get_java_path.
    CONSTANTS c_registry_java_base_key TYPE string
      VALUE 'SOFTWARE\JavaSoft\Java Runtime Environment'  ##no_text.
    DATA lv_path TYPE string.

    CLEAR rv_fullpath.
    cl_gui_frontend_services=>registry_get_value(
      EXPORTING
        root = cl_gui_frontend_services=>hkey_local_machine
        key = c_registry_java_base_key
        value = 'CurrentVersion'
      IMPORTING
        reg_value = lv_path
      EXCEPTIONS
        OTHERS               = 5 ).
    CHECK sy-subrc EQ 0.
    cl_gui_frontend_services=>registry_get_value(
      EXPORTING
        root = cl_gui_frontend_services=>hkey_local_machine
        key = |{ c_registry_java_base_key }\\{ lv_path }|
        value = 'JavaHome'
      IMPORTING
        reg_value = lv_path
      EXCEPTIONS
        OTHERS               = 5 ).
    CHECK sy-subrc EQ 0.
    rv_fullpath = |{ lv_path }\\bin\\java|.
  ENDMETHOD.                    "get_java_path

ENDCLASS.                    "lcl_diagram_plant_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_progress_indicator IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_progress_indicator IMPLEMENTATION.

  METHOD new.
    DATA ls_cfg LIKE is_cfg.
    IF is_cfg IS SUPPLIED.
      ls_cfg = is_cfg.
    ELSE.
      ls_cfg = lcl_configuration=>get( ).
    ENDIF.
    CREATE OBJECT ro_prog
      EXPORTING
        iv_progress = ls_cfg-progress.
  ENDMETHOD.                    "new

  METHOD constructor.
    mv_mode = get_mode( ).
    IF iv_progress EQ abap_true.
      set_mode( c_enable ).
    ELSE.
      set_mode( c_disable ).
    ENDIF.
  ENDMETHOD.                    "constructor

  METHOD restore.
    set_mode( mv_mode ).
  ENDMETHOD.                    "restore

  METHOD get_mode.
    GET PARAMETER ID c_param_id_sin FIELD rv_mode.
  ENDMETHOD.                    "get_mode

  METHOD set_mode.
    SET PARAMETER ID c_param_id_sin FIELD iv_mode.
  ENDMETHOD.                    "set_mode

  METHOD echo.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = iv_percentage
        text       = iv_text.
  ENDMETHOD.                    "echo

ENDCLASS.                    "lcl_progress_indicator IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_configuration IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_configuration IMPLEMENTATION.

  METHOD class_constructor.
    DATA ls_pattern LIKE LINE OF gs_cfg-pattern.
*   Windows: Local Java installation
    gs_cfg-java_appl = lcl_diagram_plant_uml=>get_java_path( ).
    IF gs_cfg-java_appl IS INITIAL.
      gs_cfg-java_appl = `C:\Windows\System32\java`.
    ENDIF.
*   PlantUML jar file and output path
    gs_cfg-local_path = `C:\Temp\Dokumente\UML\`.
    gs_cfg-java_jar = 'C:\Temp\Dokumente\UML\plantuml.jar'.
*   PlantUML server URL
    gs_cfg-server_url = `http://www.plantuml.com/plantuml/img/` ##no_text.
*   Output mode: c_mode_aut for ABAP Unit Test
    gs_cfg-output_mode = lcl_diagram_text=>c_mode_url.
    gs_cfg-skip_dialog = space.
    gs_cfg-compact_trace = abap_true.
    gs_cfg-scale = c_default_scale.
    ls_pattern = 'Y*'.
    APPEND ls_pattern TO gs_cfg-pattern.
    ls_pattern = 'Z*'.
    APPEND ls_pattern TO gs_cfg-pattern.
    gs_cfg-progress = abap_true.
    gs_cfg-handwritten = abap_false.
    gs_cfg-shadowing = abap_false.
    gs_cfg-display_source = abap_true.
    gs_cfg-teoz_architecture = abap_false.
  ENDMETHOD.                    "class_constructor


  METHOD get_attributes.
    DATA ls_attr LIKE LINE OF rt_attr.

    DEFINE fill_att.
      clear ls_attr.
      get reference of &1 into ls_attr-ref.
      ls_attr-text = &2.
      ls_attr-kind = &3.
      insert ls_attr into table rt_attr.
    END-OF-DEFINITION.
    DEFINE fill_radio.
      clear ls_attr.
      get reference of &1 into ls_attr-ref.
      ls_attr-text = &2.
      ls_attr-kind = 'R'.
      ls_attr-button_group = &3.
      insert ls_attr into table rt_attr.
    END-OF-DEFINITION.

* Table Type has type 'T' - patterns SCI_PATTERN
*                     ' ' - ?? private attributes?
*                     'I' - ?? Integer?
    fill_att   gs_cfg-skip_dialog 'Remember my settings'(c00)     'C'.

    fill_att: ''                   'Trace Settings'(c01)          'G',
              gs_cfg-progress      'Progress indicator'(c02)      'C',
              gs_cfg-system_events 'System events'(c03)           'C',
              gs_cfg-compact_trace 'Loops compacted'(c04)         'C',
              gs_cfg-pattern       'Customer namespace'(c05)      'T'.

    fill_att:   sy-index      'PlantUML Execution Mode'(c10) 'G'.   " Group
    fill_radio: mv_mode_url   'PlantUML web service'(c11)  'MOD',
                mv_mode_txt   'Save text file'(c12)        'MOD',
                mv_mode_exe   'Local PlantUML '(c13)       'MOD'.

    fill_att: ''              'PlantUML Settings'(c20)         'G',
              gs_cfg-scale       'Scale '(c21)                 'S'.
    fill_att: gs_cfg-server_url  'PlantUML Server'(c25)         'S',
              gs_cfg-local_path  'Local PlantUML path'(c26)     'S',
              gs_cfg-java_jar    'Local PlantUML jar file'(c27) ' ',
              gs_cfg-java_appl   'Local Java path'(c28)         'S'.  " Select-Options
    fill_att: gs_cfg-handwritten       'Handwritten '(c30)           'C',
              gs_cfg-shadowing         'Shadowing '(c31)             'C',
              gs_cfg-display_source    'Display source '(c32)        'C',
              gs_cfg-teoz_architecture 'Use TEOZ architecture '(c33) 'C'.
  ENDMETHOD.                    "get_attributes

  METHOD to_radiobutton.
    mv_mode_url = boolc( gs_cfg-output_mode EQ lcl_diagram_text=>c_mode_url ).
    mv_mode_exe = boolc( gs_cfg-output_mode EQ lcl_diagram_text=>c_mode_exe ).
    mv_mode_txt = boolc( gs_cfg-output_mode EQ lcl_diagram_text=>c_mode_txt ).
  ENDMETHOD.                    "to_radiobutton

  METHOD from_radiobutton.
    IF mv_mode_url EQ abap_true.
      gs_cfg-output_mode = lcl_diagram_text=>c_mode_url.
    ELSEIF mv_mode_exe EQ abap_true.
      gs_cfg-output_mode = lcl_diagram_text=>c_mode_exe.
    ELSEIF mv_mode_txt EQ abap_true.
      gs_cfg-output_mode = lcl_diagram_text=>c_mode_txt.
    ENDIF.
  ENDMETHOD.                    "from_radiobutton

  METHOD get.
    MOVE-CORRESPONDING gs_cfg TO rs_cfg.
  ENDMETHOD.                    "get

  METHOD query.
    DATA lo_config TYPE REF TO lcl_configuration.

    CREATE OBJECT lo_config.
    lo_config->dialog( ).
    rs_cfg = lo_config->get( ).
  ENDMETHOD.                    "query

  METHOD dialog.
    DATA lv_screen_id TYPE sychar30.

    to_radiobutton( ).
    CHECK gs_cfg-skip_dialog EQ abap_false.
    lv_screen_id = sy-repid.
    CHECK cl_ci_query_attributes=>generic(
        p_name       = lv_screen_id                          " unique screen ID
        p_title      = 'Sequence Diagram Parameters'         " Screen title
        p_attributes = get_attributes( )                     " Screen fields
        p_display    = abap_false                            " Edit / Display only
       ) EQ abap_false.   " Do not cancel
    from_radiobutton( ).
  ENDMETHOD.                    "dialog

ENDCLASS.                    "lcl_configuration IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_graph IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_graph IMPLEMENTATION.

  METHOD header.
    add( |.PS\n| ). " GNU plot pic2plot macro header for sequence diagram
    add( |copy "sequence.pic";\n| ).
    add( |\n| ).
  ENDMETHOD.                    "header

  METHOD top.
    super->top( ).
    add( |step();\n| ).
  ENDMETHOD.                    "top

  METHOD call.
* alternative:
* - create_message(from_object,to_object,object_label);
*   Has from_object create the to_object, labeled with object_label.
*   The message is labeled with the «create» stereotype. Can also be written as cmessage.
    add( |message({ is_message-caller },{ is_message-called },"{
        mi_actors->short_text( is_message ) }");\n| ).

    add( |active({ is_message-called });\n| ).
  ENDMETHOD.                    "call

  METHOD return.
* - return_message(from_object,to_object,label)
*   Draws a return message between two objects, with the given label. Can also be written as rmessage.
    IF iv_from NE iv_to.
      add( |return_message({ iv_from },{ iv_to });\n| ).
    ENDIF.
    CHECK iv_from IS NOT INITIAL.
    add( |inactive({ iv_from });\n| ).
  ENDMETHOD.                    "return

  METHOD participant.
* - object(name,label);   OR  actor(name,label);
*   Defines an object / actor with the given name, labeled on the diagram as specified.
*   Actors are typically used instead of objects to indicate operations initiated by human action
    add( |object({ is_lifeline-index },"{ is_lifeline-label }");\n| ).
  ENDMETHOD.                    "participant

  METHOD complete.
    add( |complete({ is_lifeline-index });\n| ).
  ENDMETHOD.                    "complete

  METHOD footer.
    add( |.PE\n| ).
  ENDMETHOD.                    "footer

  METHOD skip_note.
* - lifeline_constraint(object,label);
*   Displays a constraint label (typically given inside curly braces) for the given object.
*   The constraint will appear on the right of the object's lifeline at the time it appears.
*   Can also be used to place an message label on the left of a message arrow, rather than its center.
*   Can also be written as lconstraint.
    add( |lifeline_constraint({ is_path-caller },"{ c_txt_std }")\n| ).
  ENDMETHOD.                    "skip_note

  METHOD begin_loop.
* - begin_frame(left_object,name,label_text);
*   Begins a frame with the upper left corner at left_object column and the current line.
*   The specified label_text is shown in the upper left corner.
    add( |begin_frame({ iv_from },{ iv_name },"Loop { iv_times } times");\n| ).        " pop
  ENDMETHOD.                    "begin_loop

  METHOD end_loop.
* - end_frame(right_object,name);
*   Ends a frame with the lower right corner at right_object column and the current line.
*   The name must correspond to a begin_frame's name.
    add( |end_frame({ iv_to }, { iv_name });\n| ).
  ENDMETHOD.                    "end_loop

ENDCLASS.                    "lcl_uml_graph IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_mscgen IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_mscgen IMPLEMENTATION.

  METHOD header.
    add( |msc \{\n| ).
  ENDMETHOD.                    "header

  METHOD delimiter.
    add( |;\n| ).
  ENDMETHOD.                    "delimiter

  METHOD separator.
    add( |, | ).
  ENDMETHOD.                    "separator

  METHOD call.
    add( |{ is_message-caller }=>{ is_message-called } [label="{
      mi_actors->short_text( is_message ) }"];\n| ).
  ENDMETHOD.                    "call

  METHOD return.
    CHECK iv_from NE iv_to.
    add( |{ iv_to }<<{ iv_from }\n| ).
  ENDMETHOD.                    "return

  METHOD participant.
    add( |{ is_lifeline-index } [label="{ is_lifeline-label }"]| ).
  ENDMETHOD.                    "participant

  METHOD footer.
    add( |\}\n| ).
  ENDMETHOD.                    "footer

  METHOD skip_note.
    add( |{ is_path-called } note { is_path-caller } [label="{ c_txt_std
      }", textbgcolour="#7fff7f"];\n| ).
  ENDMETHOD.                    "skip_note

  METHOD begin_loop.
*    add( |[c:loop  { iv_times } times]\n| ).        " pop
    RETURN.
  ENDMETHOD.                    "begin_loop

  METHOD end_loop.
*    add( |[/c]\n| ).
    RETURN.
  ENDMETHOD.                    "end_loop

ENDCLASS.                    "lcl_uml_mscgen IMPLEMENTATION

*---------------- Unit Tests ----------------------------------------------------*

CLASS lcl_logger IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_log.
  ENDMETHOD.                    "new

  METHOD verify.
    cl_abap_unit_assert=>assert_equals( act = mt_log
                                        exp = it_exp
                                        msg = iv_msg ).
  ENDMETHOD.                    "verify

  METHOD info.
    APPEND is_log TO mt_log.
  ENDMETHOD.                    "info

ENDCLASS.                    "lcl_logger IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_logger IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uml_logger IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_log = lcl_logger=>new( ).
  ENDMETHOD.                    "constructor

  METHOD log.
    DATA ls_log TYPE lcl_logger=>ts_log.

    ls_log-method = iv_method.
    ls_log-params = iv_params.
    mo_log->info( ls_log ).
  ENDMETHOD.                    "log

  METHOD header.
    log( 'HEADER' ).
  ENDMETHOD.                    "header

  METHOD call.
    log( iv_method = 'CALL'
         iv_params = |{ is_message-id } { is_message-caller } { is_message-called
                          } { mi_actors->short_text( is_message ) }| ).
  ENDMETHOD.                    "call

  METHOD return.
    log( iv_method = 'RETURN'
         iv_params = |{ iv_from } { iv_to }| ).
  ENDMETHOD.                    "return

  METHOD participant.
    log( iv_method = 'PARTICIPANT'
         iv_params = |{ is_lifeline-index } { is_lifeline-label }| ).
  ENDMETHOD.                    "participant

  METHOD footer.
    log( 'FOOTER' ).
  ENDMETHOD.                    "footer

  METHOD skip_note.
    log( iv_method = 'SKIP_NOTE'
         iv_params = |{ is_path-called } { is_path-caller }| ).
  ENDMETHOD.                    "skip_note

  METHOD begin_loop.
    log( iv_method = 'BEGIN_LOOP'
         iv_params = |{ iv_times }| ).
  ENDMETHOD.                    "begin_loop

  METHOD end_loop.
    log( 'END_LOOP' ).
  ENDMETHOD.                    "end_loop

ENDCLASS.                    "lcl_uml_logger IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_abap_trace_logger DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
ENDCLASS.                    "lcl_abap_trace_logger DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_abap_trace_logger IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_trace_logger IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_log = lcl_logger=>new( ).
  ENDMETHOD.                    "constructor

  METHOD log.
    DATA ls_log TYPE lcl_logger=>ts_log.
    ls_log-method = iv_method.
    ls_log-params = iv_params.
    mo_log->info( ls_log ).
  ENDMETHOD.                    "log

  METHOD set_constructor.
    log( iv_method = 'SET_CONSTRUCTOR'
         iv_params = |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_constructor( ).
  ENDMETHOD.                    "set_constructor

  METHOD set_meth.
    log( iv_method = 'SET_METH'
         iv_params =  |{ mr_src->progindex } { mr_src->textindex } { mr_src->methindex }| ).
    super->set_meth( ).
  ENDMETHOD.                    "set_meth

  METHOD set_form.
    log( iv_method = 'SET_FORM'
         iv_params =  |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_form( ).
  ENDMETHOD.                    "set_form

  METHOD set_function_module.
    log( iv_method = 'SET_FUNCTION_MODULE'
         iv_params =  |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_function_module( ).
  ENDMETHOD.                    "set_function_module

  METHOD set_submit.
    log( iv_method = 'SET_SUBMIT'
         iv_params =  |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_submit( ).
  ENDMETHOD.                    "set_submit

  METHOD set_tran.
    log( iv_method = 'SET_TRAN'
         iv_params = |{ mr_src->progindex } { mr_src->textindex }| ).
    super->set_tran( ).
  ENDMETHOD.                    "set_tran

  METHOD module.
    log( iv_method = 'MODULE'
         iv_params = |{ iv_index }| ).
    super->module( iv_index ).
  ENDMETHOD.                    "module

  METHOD event.
    log( iv_method = 'EVENT'
         iv_params = |{ mr_src->textindex }| ).
    super->event( ).
  ENDMETHOD.                    "event

ENDCLASS.                    "lcl_abap_trace_logger IMPLEMENTATION

CONSTANTS:
  c_my_program TYPE program VALUE 'Y_TEST_PROG'.

*----------------------------------------------------------------------*
*       CLASS ltc_bag DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_bag DEFINITION FOR TESTING CREATE PRIVATE RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    CONSTANTS c_index TYPE i VALUE 42.
    TYPES tt_index TYPE STANDARD TABLE OF sytabix.
    DATA mo_bag TYPE REF TO lcl_bag.

    METHODS fixture IMPORTING it_index TYPE tt_index OPTIONAL.
    METHODS teardown.

    METHODS remove_value FOR TESTING.
    METHODS remove_empty FOR TESTING.
    METHODS contains_empty FOR TESTING.
    METHODS contains_value FOR TESTING.
ENDCLASS.                    "ltc_bag DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_bag IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_bag IMPLEMENTATION.

  METHOD fixture.
    DATA lv_idx TYPE sytabix.

    CREATE OBJECT mo_bag.
    LOOP AT it_index INTO lv_idx.
      mo_bag->add( lv_idx ).
    ENDLOOP.
  ENDMETHOD.                    "fixture

  METHOD teardown.
    FREE mo_bag.
  ENDMETHOD.                    "teardown

  METHOD contains_empty.
    fixture( ).
    cl_abap_unit_assert=>assert_false( act = mo_bag->contains( c_index )
                                       msg = 'LCL_BAG CONTAINS( ) No Entries Error' ).
  ENDMETHOD.                    "contains_empty

  METHOD contains_value.
    DATA lt_index TYPE tt_index.
    APPEND c_index TO lt_index.
    fixture( lt_index ).
    cl_abap_unit_assert=>assert_true( act = mo_bag->contains( c_index )
                                      msg = 'LCL_BAG CONTAINS( ) One Entry Error' ).
  ENDMETHOD.                    "contains_value

  METHOD remove_empty.
    fixture( ).
    mo_bag->remove( c_index ).
    cl_abap_unit_assert=>assert_false( act = mo_bag->contains( c_index )
                                       msg = 'LCL_BAG REMOVE( ) Empty Error' ).
  ENDMETHOD.                    "remove_empty

  METHOD remove_value.
    DATA lt_index TYPE tt_index.
    APPEND c_index TO lt_index.
    fixture( lt_index ).
    mo_bag->remove( c_index ).
    cl_abap_unit_assert=>assert_false( act = mo_bag->contains( c_index )
                                       msg = 'LCL_BAG REMOVE( ) Error' ).
  ENDMETHOD.                    "remove_value

ENDCLASS.                    "ltc_bag IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_satr_state DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
ENDCLASS.                    "lcl_satr_state DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_satr_state IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_satr_state IMPLEMENTATION.

  METHOD constructor.
    mo_atra = io_atra.
    save( ).
  ENDMETHOD.                    "constructor

  METHOD fill.
    DEFINE macro_fill.
      do &1 times.
        append initial line to mo_atra->&2.
      enddo.
    END-OF-DEFINITION.

    macro_fill: iv_textindex it_tracetext,
                iv_progindex it_traceprog,
                iv_methindex it_tracemeth.
  ENDMETHOD.                    "fill

  METHOD save.
    ms_satr-it_austab_hier = mo_atra->it_austab_hier[].
    ms_satr-it_traceprog = mo_atra->it_traceprog[].
    ms_satr-it_tracetext = mo_atra->it_tracetext[].
    ms_satr-it_tracemeth = mo_atra->it_tracemeth[].
  ENDMETHOD.                    "save

  METHOD restore.
    mo_atra->it_austab_hier[] = ms_satr-it_austab_hier.
    mo_atra->it_traceprog[] = ms_satr-it_traceprog.
    mo_atra->it_tracetext[] = ms_satr-it_tracetext.
    mo_atra->it_tracemeth[] = ms_satr-it_tracemeth.
  ENDMETHOD.                    "restore

ENDCLASS.                    "lcl_satr_state IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_abap_trace DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
    METHODS expected_after_set_form RETURNING value(rt_exp) TYPE lcl_logger=>tt_log.

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
ENDCLASS.                    "ltc_abap_trace DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_abap_trace IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_trace IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_trace TYPE lcl_abap_trace_logger.
    mo_log = mo_trace->mo_log.
    CREATE OBJECT mo_state
      EXPORTING
        io_atra = mo_trace->mo_atra.
    mo_state->fill( iv_textindex = c_textindex
                    iv_progindex = nmax( val1 = c_progindex
                                         val2 = c_fm_index )
                    iv_methindex = c_methindex ).
  ENDMETHOD.                    "setup

  METHOD teardown.
    mo_state->restore( ).
    FREE mo_state.
    FREE mo_trace.
  ENDMETHOD.                    "teardown

  METHOD parse.
    DATA lr_gesamt TYPE REF TO satr_austab_gesamt.
    FIELD-SYMBOLS <ls_prog> TYPE satr_prog.

    CREATE DATA lr_gesamt.
    lr_gesamt->id = iv_id.
    lr_gesamt->subid = iv_subid.
    lr_gesamt->textindex = c_textindex.
    lr_gesamt->progindex = c_progindex.
    lr_gesamt->methindex = iv_methindex.
    lr_gesamt->pr_2index = iv_fm_index.

    mo_trace->mr_src = lr_gesamt.
    READ TABLE mo_trace->mo_atra->it_traceprog INDEX c_progindex
      ASSIGNING <ls_prog>.
    <ls_prog> = 'MY_PROG'.
    CLEAR mo_trace->ms_target.                                   " Called actor
    mo_trace->parse( ).
  ENDMETHOD.                    "parse

  METHOD expected_after_set_form.
    DATA ls_log TYPE lcl_logger=>ts_log.
    ls_log-method = 'SET_FORM'.
    ls_log-params = |{ c_progindex } { c_textindex }|.
    APPEND ls_log TO rt_exp.
    ls_log-method = 'EVENT'.
    ls_log-params = |{ c_textindex }|.
    APPEND ls_log TO rt_exp.
    ls_log-method = 'MODULE'.
    ls_log-params = |{ c_progindex }|.
    APPEND ls_log TO rt_exp.
  ENDMETHOD.                    "expected_after_set_form

  METHOD parse_form_in_module.
    parse( iv_id = c_id_module ).
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM in Module Error'  ).
  ENDMETHOD.                    "parse_form_in_module

  METHOD parse_form_at_load.
    parse( iv_id = c_id_load
           iv_subid = 'D' ).  " Dynpro
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM at LOAD Error'  ).
  ENDMETHOD.                    "parse_form_at_load

  METHOD parse_form_in_call_dialog.
    parse( iv_id = c_id_report     " CALL - e.g. Call SCREEN/CALL Report
           iv_subid = 'D' ).       " CALL Dialog OR Call Screen
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM in CALL Dialog' ).
  ENDMETHOD.                    "parse_form_in_call_dialog

  METHOD parse_form_in_call_screen.
    parse( iv_id = c_id_report     " CALL - e.g. Call SCREEN/CALL Report
           iv_subid = 'S' ).       " CALL Dialog OR Call Screen ).       " Called actor
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM in CALL Screen' ).
  ENDMETHOD.                    "parse_form_in_call_screen

  METHOD parse_form.
    parse( iv_id = c_id_form ).
    mo_log->verify( it_exp = expected_after_set_form( )
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FORM' ).
  ENDMETHOD.                    "parse_form

  METHOD parse_constructor.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    ls_log-method = 'SET_CONSTRUCTOR'.
    ls_log-params = |{ c_progindex } { c_textindex }|.
    APPEND ls_log TO lt_log.

    parse( iv_id = c_id_class ).
    mo_log->verify( it_exp = lt_log
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) SET CONSTRUCTOR Call Error' ).
  ENDMETHOD.                    "parse_constructor

  METHOD set_constructor.
    DATA ls_event TYPE lcl_abap_trace=>ts_event.

    ls_event-type = c_type_class.
    ls_event-event = lcl_abap_trace=>c_create_method.
    ls_event-global = space.

    parse( iv_id = c_id_class ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_trace->ms_target
      exp = ls_event
      msg = 'LCL_ABAP_TRACE=>SET_CONSTRUCTOR( ) Error' ).
  ENDMETHOD.                    "set_constructor

  METHOD parse_method.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    ls_log-method = 'SET_METH'.
    ls_log-params = |{ c_progindex } { c_textindex } { c_methindex }|.
    APPEND ls_log TO lt_log.
    ls_log-method = 'MODULE'.
    ls_log-params = |{ c_progindex }|.
    APPEND ls_log TO lt_log.
    ls_log-method = 'MODULE'.
    ls_log-params = |{ 0 }|.
    APPEND ls_log TO lt_log.

    parse( iv_id = c_id_method
           iv_methindex = c_methindex ).
    mo_log->verify( it_exp = lt_log
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) METHOD' ).
  ENDMETHOD.                    "parse_method

  METHOD parse_function_module.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    ls_log-method = 'SET_FUNCTION_MODULE'.
    ls_log-params = |{ c_progindex } { c_textindex }|.
    APPEND ls_log TO lt_log.
    ls_log-method = 'EVENT'.
    ls_log-params = |{ c_textindex }|.
    APPEND ls_log TO lt_log.
    ls_log-method = 'MODULE'.
    ls_log-params = |{ c_fm_index }|.
    APPEND ls_log TO lt_log.

    parse( iv_id = c_id_func
           iv_fm_index = c_fm_index ).
    mo_log->verify( it_exp = lt_log
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) FUNCTION_MODULE' ).
  ENDMETHOD.                    "parse_function_module

  METHOD parse_module.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    ls_log-method = 'MODULE'.
    ls_log-params = |{ c_progindex }|.
    APPEND ls_log TO lt_log.

    parse( iv_id = c_id_report
           iv_subid = 'P' ).
    mo_log->verify( it_exp = lt_log
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) MODULE' ).
  ENDMETHOD.                    "parse_module

  METHOD parse_dynpro.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    ls_log-method = 'MODULE'.
    ls_log-params = |{ c_progindex }|.
    APPEND ls_log TO lt_log.

    parse( iv_id = c_id_dynpro
           iv_subid = 'P' ).
    mo_log->verify( it_exp = lt_log
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) MODULE Dynpro' ).
  ENDMETHOD.                    "parse_dynpro

  METHOD parse_submit.
    CONSTANTS:
      c_mod_index TYPE sytabix VALUE 1,    " larger 0, smaller than c_progindex
      c_my_event  TYPE program VALUE 'MY_EVENT'.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.
    FIELD-SYMBOLS <ls_prog> TYPE satr_prog.
    FIELD-SYMBOLS <ls_text> TYPE satr_tracetext.

    ls_log-method = 'SET_SUBMIT'.
    ls_log-params = |{ c_progindex } { c_textindex }|.
    APPEND ls_log TO lt_log.
    ls_log-method = 'EVENT'.
    ls_log-params = |{ c_textindex }|.
    APPEND ls_log TO lt_log.
    ls_log-method = 'MODULE'.
    ls_log-params = |{ c_mod_index }|.
    APPEND ls_log TO lt_log.

    READ TABLE mo_trace->mo_atra->it_tracetext INDEX c_textindex ASSIGNING <ls_text>.
    <ls_text>-tracetext = c_my_event.
    READ TABLE mo_trace->mo_atra->it_traceprog INDEX c_mod_index ASSIGNING <ls_prog>.
    <ls_prog> = c_my_event.

    parse( iv_id = c_id_report
           iv_subid = 'R' ).
    mo_log->verify( it_exp = lt_log
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) SET_SUBMIT Error' ).
  ENDMETHOD.                    "parse_submit

  METHOD parse_transaction.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    ls_log-method = 'SET_TRAN'.
    ls_log-params = |{ c_progindex } { c_textindex }|.
    APPEND ls_log TO lt_log.
    ls_log-method = 'EVENT'.
    ls_log-params = |{ c_textindex }|.
    APPEND ls_log TO lt_log.

    parse( iv_id = c_id_report
           iv_subid = 'T' ).
    mo_log->verify( it_exp = lt_log
                    iv_msg = 'LCL_ABAP_TRACE->PARSE( ) SET_TRAN Error' ).
  ENDMETHOD.                    "parse_transaction

ENDCLASS.                    "ltc_abap_trace IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
ENDCLASS.                    "ltc_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_sequence DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_sequence DEFINITION FOR TESTING CREATE PRIVATE RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.

    TYPES tt_sat TYPE STANDARD TABLE OF ts_sat WITH KEY aus_tabix
                 WITH NON-UNIQUE SORTED KEY k1 COMPONENTS aus_tabix. " Performance ?

    CLASS-METHODS new_sequence IMPORTING is_cfg             TYPE ts_diagram_config
                                         it_sat             TYPE tt_sat
                               RETURNING value(ro_sequence) TYPE REF TO lcl_sequence.

    CLASS-METHODS new_collection IMPORTING is_cfg             TYPE ts_diagram_config
                                           it_sat             TYPE tt_sat
                                 RETURNING value(ro_sequence) TYPE REF TO lcl_sequence.

    CLASS-METHODS sat_fixture_events RETURNING value(rt_sat) TYPE tt_sat.
  PRIVATE SECTION.
    DATA mo_sequence TYPE REF TO lcl_sequence.

    METHODS setup.
    METHODS teardown.

    METHODS collect FOR TESTING.
    METHODS add FOR TESTING.
ENDCLASS.                    "ltc_sequence DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_sequence IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_sequence IMPLEMENTATION.

  METHOD new_sequence.
    FIELD-SYMBOLS <ls_sat> TYPE ts_sat.

    CREATE OBJECT ro_sequence
      EXPORTING
        is_cfg = is_cfg.
*   insert into collection
    LOOP AT it_sat ASSIGNING <ls_sat>.
      ro_sequence->lif_collector~collect( <ls_sat> ).
    ENDLOOP.
  ENDMETHOD.                    "new_sequence

  METHOD new_collection.
    FIELD-SYMBOLS <ls_sat> TYPE ts_sat.

    CREATE OBJECT ro_sequence
      EXPORTING
        is_cfg = is_cfg.
*   add to collection
    LOOP AT it_sat ASSIGNING <ls_sat>.
      ro_sequence->add( <ls_sat> ).
    ENDLOOP.
  ENDMETHOD.                    "new_collection

  METHOD sat_fixture_events.
    CONSTANTS:
      c_prog  TYPE program VALUE 'Y_TEST_PROG',
      c_ctrl  TYPE program VALUE 'LCL_CTRL',
      c_model TYPE program VALUE 'LCL_MODEL'.
    DATA ls_sat TYPE ts_sat.

    CLEAR ls_sat.
    ls_sat-id = 'm'.
    ls_sat-from_level = 1.
    ls_sat-caller-global = c_prog.
    ls_sat-caller-type = c_type_prog.
    ls_sat-called-global = c_prog.
    ls_sat-called-type = c_type_class.
    ls_sat-called-local = c_ctrl.
    ls_sat-called-mod = 'MAIN'.
    APPEND ls_sat TO rt_sat.

    CLEAR ls_sat.
    ls_sat-id = 'm'.
    ls_sat-from_level = 2.
    ls_sat-caller-global = c_prog.
    ls_sat-caller-type = c_type_class.
    ls_sat-caller-local = c_ctrl.
    ls_sat-caller-mod = 'MAIN'.
    ls_sat-called-global = 'SAPPROG'.
    ls_sat-called-type = c_type_class.
    ls_sat-called-instance = 6.
    ls_sat-called-local = c_model.
    ls_sat-called-mod = lcl_abap_trace=>c_create_method.
    APPEND ls_sat TO rt_sat.

    CLEAR ls_sat.
    ls_sat-id = 'm'.
    ls_sat-from_level = 2.

    ls_sat-caller-global = 'SAPPROG'.
    ls_sat-caller-type = c_type_class.
    ls_sat-caller-local = c_model.
    ls_sat-caller-mod = 'SETUP'.
    ls_sat-called-global = c_prog.
    ls_sat-called-type = c_type_class.
    ls_sat-called-local = c_ctrl.
    ls_sat-called-instance = 0.
    ls_sat-called-mod = 'CHANGE'.
    APPEND ls_sat TO rt_sat.
  ENDMETHOD.                    "sat_fixture_events

  METHOD setup.
    mo_sequence = new_sequence( it_sat = sat_fixture_events( )
                                is_cfg = lcl_configuration=>get( ) ).
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mo_sequence.
  ENDMETHOD.                    "teardown

  METHOD collect.
    DATA lt_message TYPE tt_message.
    DATA ls_message TYPE ts_message.

    ls_message-id = 'm'.
    ls_message-from_level = 1.
    ls_message-caller = 1.
    ls_message-called = 2.
    ls_message-msg = 'MAIN'.
    APPEND ls_message TO lt_message.

    ls_message-id = 'K'.  "<--- skipped
    ls_message-from_level = 1.
    ls_message-caller = 2.
    ls_message-called = 3.
    ls_message-msg = 'SETUP'.
    APPEND ls_message TO lt_message.

    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 3.
    ls_message-called = 2.
    ls_message-msg = 'CHANGE'.
    APPEND ls_message TO lt_message.

    cl_abap_unit_assert=>assert_equals( act = mo_sequence->mt_trace
                                        exp = lt_message
                                        msg = 'LCL_SEQUENCE->COLLECT( ) Error' ).
  ENDMETHOD.                    "collect

  METHOD add.
    DATA lt_message TYPE tt_message.
    DATA ls_message TYPE ts_message.

    ls_message-id = 'm'.
    ls_message-from_level = 1.
    ls_message-caller = 1.
    ls_message-called = 2.
    ls_message-msg = 'MAIN'.
    APPEND ls_message TO lt_message.

    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 2.
    ls_message-called = 3.
    ls_message-msg = lcl_abap_trace=>c_create_method.
    APPEND ls_message TO lt_message.

    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 4.
    ls_message-called = 2.
    ls_message-msg = 'CHANGE'.
    APPEND ls_message TO lt_message.

    mo_sequence = new_collection( it_sat = sat_fixture_events( )
                                  is_cfg = lcl_configuration=>get( ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_sequence->mt_trace
                                        exp = lt_message
                                        msg = 'LCL_SEQUENCE->ADD( ) Error' ).
  ENDMETHOD.                    "add

ENDCLASS.                    "ltc_sequence IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_class IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_class_name.
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mo_class_name.
  ENDMETHOD.                    "teardown

  METHOD section_name.
    cl_abap_unit_assert=>assert_equals( act = mo_class_name->to_internal( '/IDV0/JN_CL_ASM_ACT' )
                                        exp = '/IDV0/JN_CL_ASM_ACT==========='
                                        msg = 'LCL_CLASS SECTION_NAME( ) Error' ).
  ENDMETHOD.                    "section_name

  METHOD technical_name.
    cl_abap_unit_assert=>assert_equals( act = mo_class_name->technical( 'CL_GUI_ALV_GRID' )
                                        exp = 'CL_GUI_ALV_GRID===============CP'
                                        msg = 'LCL_CLASS TECHNICAL_NAME( ) Error' ).
  ENDMETHOD.                    "technical_name

  METHOD external_name.
    DATA lv_class TYPE program VALUE '/IDV0/JN_CL_ASM_ACT===========CP'.
    mo_class_name->external( CHANGING cv_clas = lv_class ).
    cl_abap_unit_assert=>assert_equals( act = lv_class
                                        exp = '/IDV0/JN_CL_ASM_ACT'
                                        msg = 'LCL_CLASS EXTERNAL_NAME( ) Error' ).
  ENDMETHOD.                    "external_name

  METHOD is_global.
    cl_abap_unit_assert=>assert_true( act = mo_class_name->is_global( 'CL_ABAP_MEMORY_X_WRITER' )
                                      msg = 'LCL_ABAP_TRACE=>IS_GLOBAL_CLASS( ) Error' ).
  ENDMETHOD.                    "is_global

  METHOD is_not_global.
    cl_abap_unit_assert=>assert_false( act = mo_class_name->is_global( 'LCL_BAG' )
                                       msg = 'LCL_ABAP_TRACE=>IS_GLOBAL_CLASS( ) Local Class Error' ).
  ENDMETHOD.                    "is_not_global

ENDCLASS.                    "ltc_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_actors DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_actors DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    CONSTANTS:
      c_prog  TYPE program VALUE 'Y_TEST_PROG',
      c_ctrl  TYPE program VALUE 'LCL_CTRL',
      c_model TYPE program VALUE 'LCL_MODEL'.
    INTERFACES lif_unit_test.

    CLASS-METHODS sat_fixture_record RETURNING value(rs_sat) TYPE ts_sat.
    CLASS-METHODS sat_fixture_plant_uml  RETURNING value(rt_sat) TYPE ltc_sequence=>tt_sat.
    CLASS-METHODS sat_fixture_messages RETURNING value(rt_sat) TYPE ltc_sequence=>tt_sat.
  PRIVATE SECTION.
    CONSTANTS c_my_class TYPE program VALUE 'YCL_TEST_CLASS'.
    DATA mi_actors TYPE REF TO lif_actors.
    DATA ms_exp TYPE ts_sat.

    METHODS constructor.
    METHODS setup.
    METHODS teardown.

    METHODS get_count RETURNING value(rv_count) TYPE i.
    METHODS get_text_class IMPORTING iv_id  TYPE satr_de_id
                                     iv_exp TYPE string
                                     iv_msg TYPE string.
    METHODS put FOR TESTING.
    METHODS get_text_global_class FOR TESTING.
    METHODS get_text_local_class FOR TESTING.
ENDCLASS.                    "ltc_actors DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_actors IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_actors IMPLEMENTATION.

  METHOD constructor.
    ms_exp-id = 1.
    ms_exp-from_level = 1.
    ms_exp-caller-global = c_my_program.
    ms_exp-caller-type = c_type_fugr.
    ms_exp-called-global = c_my_program.
    ms_exp-called-local = c_model.
    ms_exp-called-type = c_type_class.
    ms_exp-called-instance = 1.
    ms_exp-called-mod = lcl_abap_trace=>c_create_method.
  ENDMETHOD.                    "constructor

  METHOD sat_fixture_messages.
    DATA ls_main TYPE ts_call.
    DATA ls_sat TYPE ts_sat.

    ls_main-global = c_prog.
    ls_main-type = c_type_class.
    ls_main-local = c_ctrl.
    ls_main-mod = 'MAIN'.

    CLEAR ls_sat.
    ls_sat-id = 'm'.
    ls_sat-from_level = 1.
    ls_sat-caller-global = c_prog.
    ls_sat-caller-type = c_type_prog.
    ls_sat-called = ls_main.
    APPEND ls_sat TO rt_sat.

    CLEAR ls_sat.
    ls_sat-id = 'm'.
    ls_sat-from_level = 2.
    ls_sat-caller = ls_main.
    ls_sat-called-global = c_prog.
    ls_sat-called-type = c_type_class.
    ls_sat-called-instance = 6.
    ls_sat-called-local = c_model.
    ls_sat-called-mod = lcl_abap_trace=>c_create_method.
    APPEND ls_sat TO rt_sat.

    CLEAR ls_sat.
    ls_sat-id = 'm'.
    ls_sat-from_level = 2.
    ls_sat-caller = ls_main.
    ls_sat-called-global = c_prog.
    ls_sat-called-type = c_type_class.
    ls_sat-called-local = c_ctrl.
    ls_sat-called-mod = 'CHANGE'.
    APPEND ls_sat TO rt_sat.

    CLEAR ls_sat.
    ls_sat-id = 'm'.
    ls_sat-from_level = 3.
    ls_sat-caller-global = c_prog.
    ls_sat-caller-type = c_type_class.
    ls_sat-caller-local = c_ctrl.
    ls_sat-caller-mod = 'CHANGE'.
    ls_sat-called-global = c_prog.
    ls_sat-called-type = c_type_class.
    ls_sat-called-instance = 6.
    ls_sat-called-local = c_model.
    ls_sat-called-mod = 'UP'.
    APPEND ls_sat TO rt_sat.
  ENDMETHOD.                    "sat_fixture_messages

  METHOD sat_fixture_plant_uml.
    DATA ls_sat TYPE ts_sat.

    CLEAR ls_sat.
    ls_sat-id = 1.     "<------ ?
    ls_sat-from_level = 1.
    ls_sat-caller-global = c_my_program.
    ls_sat-caller-type = c_type_prog.
    ls_sat-called-global = c_my_program.
    ls_sat-called-type = c_type_class.
    ls_sat-called-instance = 0.
    ls_sat-called-local = 'LCL_CTRL'.
    ls_sat-called-mod = 'MAIN'.
    APPEND ls_sat TO rt_sat.

    CLEAR ls_sat.
    ls_sat-id = 'm'.
    ls_sat-from_level = 2.
    ls_sat-caller-global = c_my_program.
    ls_sat-caller-type = c_type_class.
    ls_sat-caller-instance = 0.
    ls_sat-caller-local = 'LCL_CTRL'.
    ls_sat-caller-mod = 'MAIN'.

    ls_sat-called-global = c_my_program.
    ls_sat-called-type = c_type_class.
    ls_sat-called-instance = 1.
    ls_sat-called-local = 'LCL_MODEL'.
    ls_sat-called-mod = lcl_abap_trace=>c_create_method.
    APPEND ls_sat TO rt_sat.

    CLEAR ls_sat.
    ls_sat-id = space.
    ls_sat-from_level = 3.
    ls_sat-caller-global = c_my_program.
    ls_sat-caller-type = c_type_class.
    ls_sat-caller-instance = 447.
    ls_sat-caller-local = 'LCL_CALL_STACK'.
    ls_sat-caller-mod = 'PLANTUML'.

    ls_sat-called-global = c_my_program.
    ls_sat-called-type = c_type_class.
    ls_sat-called-instance = 448.
    ls_sat-called-local = 'LCL_ITERATOR'.
    ls_sat-called-mod = 'HAS_NEXT'.
    APPEND ls_sat TO rt_sat.
  ENDMETHOD.                    "sat_fixture_plant_uml

  METHOD sat_fixture_record.
    CLEAR rs_sat.
    rs_sat-id = 'm'.
    rs_sat-from_level = 1.
    rs_sat-aus_tabix = 1.

    rs_sat-caller-global = c_prog.
    rs_sat-caller-type = c_type_prog.

    rs_sat-called-global = c_prog.
    rs_sat-called-type = c_type_class.
    rs_sat-called-local = 'LCL_CTRL'.
    rs_sat-called-mod = 'MAIN'.
  ENDMETHOD.                    "sat_fixture_record

  METHOD setup.
    CREATE OBJECT mi_actors TYPE lcl_actors.
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mi_actors.
  ENDMETHOD.                    "teardown

  METHOD get_count.
    DATA lo_actors TYPE REF TO lcl_actors.
    lo_actors ?= mi_actors.
    rv_count = lines( lo_actors->mt_actor ).
  ENDMETHOD.                    "get_count

  METHOD put.
    DATA lv_count TYPE i.

    mi_actors->new_path( ms_exp ).
    lv_count = get_count( ).
    mi_actors->new_path( ms_exp ).  " <-- Duplicate
    cl_abap_unit_assert=>assert_equals( act = get_count( )
                                        exp = lv_count
                                        msg = 'LCL_ACTORS->PUT( ) Duplicate Error' ).
  ENDMETHOD.                    "put

  METHOD get_text_class.
    DATA ls_msg TYPE ts_message.

    CLEAR ms_exp.
    ms_exp-id = 1.
    ms_exp-from_level = 1.
    ms_exp-caller-global = c_my_program.
    ms_exp-caller-type = c_type_fugr.

    ms_exp-called-global = c_my_class.
    ms_exp-called-type = c_type_class.
    ms_exp-called-instance = 1.
    ms_exp-called-mod = lcl_abap_trace=>c_create_method.

    mi_actors->new_path( ms_exp ).
    ls_msg-id = iv_id.
    ls_msg-from_level = 1.
    ls_msg-caller = 1.
    ls_msg-called = 2.
    ls_msg-msg = lcl_abap_trace=>c_create_method.
    cl_abap_unit_assert=>assert_equals( act = mi_actors->short_text( ls_msg )
                                        exp = iv_exp
                                        msg = |LCL_ACTORS->GET_TEXT( ) { iv_msg }| ).
  ENDMETHOD.                    "get_text_class

  METHOD get_text_global_class.
    get_text_class( iv_id = c_id_class
                    iv_exp = |Create instance of class { c_my_class }|
                    iv_msg = 'Global Class Get Text Error' ).
  ENDMETHOD.                    "get_text_global_class

  METHOD get_text_local_class.
    get_text_class( iv_id = c_id_method
                    iv_exp = |Call method CONSTRUCTOR|
                    iv_msg = 'Local Class Get Text Error' ).
  ENDMETHOD.                    "get_text_local_class

ENDCLASS.                    "ltc_actors IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_stack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
ENDCLASS.                    "ltc_stack DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_stack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_stack IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_stack.
    ms_exp-actor_key = 1.
    ms_exp-from_level = 1.
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mo_stack.
  ENDMETHOD.                    "teardown

  METHOD is_empty.
    cl_abap_unit_assert=>assert_true( act = mo_stack->mv_empty
                                      msg = 'LCL_STACK is not Empty' ).
  ENDMETHOD.                    "is_empty

  METHOD pop.
    mo_stack->push( ms_exp ).
    cl_abap_unit_assert=>assert_equals( act = mo_stack->pop( )
                                        exp = ms_exp
                                        msg = 'LCL_STACK POP( ) Error' ).
  ENDMETHOD.                    "pop

  METHOD push.
    DATA ls_act TYPE lcl_stack=>ts_call_level.
    mo_stack->push( ms_exp ).
    ls_act = mo_stack->pop( ).
    cl_abap_unit_assert=>assert_true(
      act = boolc( ( ms_exp EQ ls_act ) AND mo_stack->mv_empty EQ abap_true )
      msg = 'LCL_STACK PUSH( ) Error' ).
  ENDMETHOD.                    "push

  METHOD pop_empty.
    cl_abap_unit_assert=>assert_initial( act = mo_stack->pop( )
                                         msg = 'LCL_STACK POP( ) Empty' ).
  ENDMETHOD.                    "pop_empty

  METHOD is_empty_reset.
    mo_stack->push( ms_exp ).
    cl_abap_unit_assert=>assert_false( act = mo_stack->mv_empty
                                       msg = 'LCL_STACK Empty not reset/PUSH( )' ).
  ENDMETHOD.                    "is_empty_reset

  METHOD is_empty_set.
    mo_stack->push( ms_exp ).
    mo_stack->pop( ).
    cl_abap_unit_assert=>assert_true( act = mo_stack->mv_empty
                                      msg = 'LCL_STACK Empty not set/POP( )' ).
  ENDMETHOD.                    "is_empty_set

ENDCLASS.                    "ltc_stack IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_call DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
    CONSTANTS c_level TYPE i VALUE 1.
    DATA mo_stack TYPE REF TO lcl_uml_stack.   " CUT

    METHODS setup.
    METHODS teardown.
    METHODS fixture_message RETURNING value(rs_message) TYPE ts_message.

    METHODS call FOR TESTING.
    METHODS return FOR TESTING.
ENDCLASS.                    "ltc_call DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_call IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_call IMPLEMENTATION.

  METHOD create_uml.
    CREATE OBJECT mo_actors.
    mo_uml ?= lcl_uml=>new( iv_uml = lcl_uml=>c_uml_mock
                           ii_actors = mo_actors ).
  ENDMETHOD.                    "create_uml

  METHOD setup.
    create_uml( ).
    CREATE OBJECT mo_stack
      EXPORTING
        io_uml = mo_uml.
    ms_exp-actor_key = 1.
    ms_exp-from_level = 1.
  ENDMETHOD.                    "setup

  METHOD free_uml.
    FREE mo_uml.
    FREE mo_actors.
  ENDMETHOD.                    "free_uml

  METHOD teardown.
    FREE mo_stack.
    free_uml( ).
  ENDMETHOD.                    "teardown

  METHOD call.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    CLEAR ls_log.
    ls_log-method = 'CALL'.
    ls_log-params = 'm 1 2 Call method Test Call'.
    APPEND ls_log TO lt_log.

    mo_stack->call( fixture_message( ) ).

    mo_uml->mo_log->verify( it_exp = lt_log
                            iv_msg = 'LCL_UML_STACK MESSAGE Error' ).
  ENDMETHOD.                    "call

  METHOD fixture_message.
    CLEAR rs_message.
    rs_message-id         = 'm'.
    rs_message-from_level = c_level.
    rs_message-caller = 1.
    rs_message-called = 2.
    rs_message-msg  = 'Test Call'.
  ENDMETHOD.                    "fixture_message

  METHOD return.
    DATA ls_message TYPE ts_message.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    CLEAR ls_log.
    ls_log-method = 'CALL'.
    ls_log-params = 'm 1 2 Call method Test Call'.
    APPEND ls_log TO lt_log.
    CLEAR ls_log.
    ls_log-method = 'CALL'.
    ls_log-params = 'm 2 3 Call method Test Call'.
    APPEND ls_log TO lt_log.
    CLEAR ls_log.
    ls_log-method = 'RETURN'.
    ls_log-params = '3 2'.
    APPEND ls_log TO lt_log.

    ls_message = fixture_message( ).
    mo_stack->call( ls_message ).
    ls_message-caller = 2.
    ls_message-called = 3.
    mo_stack->call( ls_message ).
    mo_stack->return_to( c_level ).
    mo_uml->mo_log->verify( it_exp = lt_log
                            iv_msg = 'LCL_UML_STACK RETURN Error' ).
  ENDMETHOD.                    "return

ENDCLASS.                    "ltc_call IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_call_stack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_call_stack DEFINITION FOR TESTING
  INHERITING FROM ltc_call RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    CONSTANTS c_level TYPE i VALUE 42.
    DATA mo_messages TYPE REF TO lcl_messages.
    DATA mo_call TYPE REF TO lcl_call_stack.

    METHODS setup.
    METHODS teardown.

    METHODS first_call FOR TESTING.
ENDCLASS.                    "ltc_call_stack DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_call_stack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_call_stack IMPLEMENTATION.

  METHOD setup.
    DATA lt_message TYPE tt_message.
    DATA ls_message TYPE ts_message.

    ls_message-id = 'm'.
    ls_message-from_level = c_level.
    ls_message-caller = 1.
    ls_message-called = 2.
    ls_message-msg = 'TEST'.
    APPEND ls_message TO lt_message.

    create_uml( ).
    CREATE OBJECT mo_messages
      EXPORTING
        it_messages = lt_message.
    CREATE OBJECT mo_call
      EXPORTING
        io_uml      = mo_uml
        io_messages = mo_messages.
    ms_exp-actor_key = 1.
    ms_exp-from_level = 1.
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mo_messages.
    FREE mo_call.
    free_uml( ).
  ENDMETHOD.                    "teardown

  METHOD first_call.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    CLEAR ls_log.
    ls_log-method = 'CALL'.
    ls_log-params = | { c_level } { lcl_call_stack=>c_first_key } |.
    APPEND ls_log TO lt_log.
    CLEAR ls_log.
    ls_log-method = 'CALL'.
    ls_log-params = 'm 1 2 Call method TEST'.
    APPEND ls_log TO lt_log.
    CLEAR ls_log.
    ls_log-method = 'RETURN'.
    ls_log-params = '2 1'.
    APPEND ls_log TO lt_log.

    mo_call->message( is_message = mo_messages->next( )
                      iv_idx = 1 ).
    mo_uml->mo_log->verify( it_exp = lt_log
                            iv_msg = 'LCL_CALL_STACK MESSAGE FIRST CALL Error' ).
  ENDMETHOD.                    "first_call

ENDCLASS.                    "ltc_call_stack IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_iterator DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
    METHODS clone_rest FOR TESTING.
    METHODS clone_resize FOR TESTING.
    METHODS:
      has_previous_after_init FOR TESTING,
      has_previous_at_first FOR TESTING,
      has_previous_after_first FOR TESTING.
ENDCLASS.                    "ltc_iterator DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_iterator IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_iterator IMPLEMENTATION.

  METHOD constructor.
    CONSTANTS c_main TYPE tv_key VALUE 2.
    DATA ls_message TYPE ts_message.

    CLEAR ls_message.              "Prog. MAIN
    ls_message-id = 1.
    ls_message-from_level = 1.
    ls_message-caller = 1.
    ls_message-called = c_main.
    ls_message-msg = 'MAIN'.
    APPEND ls_message TO mt_messages.

    CLEAR ls_message.
    ls_message-id = 2.
    ls_message-from_level = 2.
    ls_message-caller = c_main.
    ls_message-called = 2.
    ls_message-msg = lcl_abap_trace=>c_create_method.  " Main -> Model=>Constructor.
    APPEND ls_message TO mt_messages.

    CLEAR ls_message.              " Main -> Model->Query
    ls_message-id = 3.
    ls_message-from_level = 1.
    ls_message-caller = c_main.
    ls_message-called = 3.
    ls_message-msg = 'QUERY'.
    APPEND ls_message TO mt_messages.

    CLEAR ls_message.
    ls_message-id = 4.
    ls_message-from_level = 2.
    ls_message-caller = 3.
    ls_message-called = 4.
    ls_message-msg = 'FILTER'.      " Model->Query -> Model->Filter
    APPEND ls_message TO mt_messages.
  ENDMETHOD.                    "constructor

  METHOD setup.
    CREATE OBJECT mo_iter
      EXPORTING
        it_messages = mt_messages.
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mo_iter.
  ENDMETHOD.                    "teardown

  METHOD skip_once.
    mo_iter->skip( ).
    cl_abap_unit_assert=>assert_equals( act = mo_iter->mv_idx
                                        exp = 1
                                        msg = 'LCL_MESSAGES SKIP( ) Error' ).
  ENDMETHOD.                    "skip_once

  METHOD skip_3_times.
    mo_iter->skip( 3 ).
    cl_abap_unit_assert=>assert_equals( act = mo_iter->mv_idx
                                        exp = 3
                                        msg = 'LCL_MESSAGES SKIP( 2 ) Error' ).
  ENDMETHOD.                    "skip_3_times

  METHOD has_next_after_init.
    cl_abap_unit_assert=>assert_true( act = mo_iter->has_next( )
                                      msg = 'LCL_MESSAGES HAS_NEXT( ) Start Error' ).
  ENDMETHOD.                    "has_next_after_init

  METHOD has_next_after_end.
    mo_iter->skip( mo_iter->mv_size ).
    cl_abap_unit_assert=>assert_false( act = mo_iter->has_next( )
                                       msg = 'LCL_MESSAGES HAS_NEXT( ) End Error' ).
  ENDMETHOD.                    "has_next_after_end

  METHOD next.
    DATA ls_exp TYPE ts_message.
    READ TABLE mo_iter->mt_list INDEX 1 INTO ls_exp.
    cl_abap_unit_assert=>assert_equals( act = mo_iter->next( )
                                        exp = ls_exp
                                        msg = 'LCL_MESSAGES_ITERATOR NEXT( ) Error' ).
  ENDMETHOD.                    "next

  METHOD next_error.
    TRY.
        mo_iter->skip( mo_iter->mv_size ).
        mo_iter->next( ).
        cl_abap_unit_assert=>fail( msg = 'LCL_MESSAGES NEXT( ) No Exception' ).
      CATCH cx_sy_itab_error ##no_handler. " Do nothing
    ENDTRY.
  ENDMETHOD.                    "next_error

  METHOD has_previous_after_init.
    cl_abap_unit_assert=>assert_true( act = mo_iter->is_first( )
                                      msg = 'LCL_MESSAGES IS_FIRST( ) Error' ).
  ENDMETHOD.                    "has_previous_after_init

  METHOD has_previous_at_first.
    mo_iter->next( ).
    cl_abap_unit_assert=>assert_true( act = mo_iter->is_first( )
                                      msg = 'LCL_MESSAGES IS_FIRST( ) First' ).
  ENDMETHOD.                    "has_previous_at_first

  METHOD has_previous_after_first.
    mo_iter->next( ).
    mo_iter->next( ).
    cl_abap_unit_assert=>assert_false( act = mo_iter->is_first( )
                                       msg = 'LCL_MESSAGES IS_FIRST( ) Next' ).
  ENDMETHOD.                    "has_previous_after_first

  METHOD clone_rest.
    CONSTANTS c_start TYPE i VALUE 2.
    DATA lo_messages TYPE REF TO lcl_messages.
    CREATE OBJECT lo_messages
      EXPORTING
        it_messages = mo_iter->mt_list
        iv_start    = c_start.
    cl_abap_unit_assert=>assert_equals( act = lo_messages->mv_idx
                                        exp = c_start - 1
                                        msg = 'LCL_MESSAGESCONSTRUCTOR( ) Index Error' ).
  ENDMETHOD.                    "clone_rest

  METHOD clone_resize.
    CONSTANTS: c_start TYPE i VALUE 2,
               c_size  TYPE i VALUE 3.
    DATA lo_messages TYPE REF TO lcl_messages.
    CREATE OBJECT lo_messages
      EXPORTING
        it_messages = mo_iter->mt_list
        iv_start    = c_start
        iv_stop     = c_size.
    cl_abap_unit_assert=>assert_equals( act = lo_messages->mv_size
                                        exp = c_size
                                        msg = 'LCL_MESSAGES CONSTRUCTOR( ) Size Error' ).
  ENDMETHOD.                    "clone_resize

ENDCLASS.                    "ltc_iterator IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_uml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
ENDCLASS.                    "ltc_uml DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_uml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_uml IMPLEMENTATION.

  METHOD setup_uml.
    CREATE OBJECT mi_actors TYPE lcl_actors.
    mo_uml = lcl_uml=>new( iv_uml = iv_uml
                           ii_actors = mi_actors ).
    mv_obj_name = iv_name.
  ENDMETHOD.                    "setup_uml

  METHOD teardown_uml.
    FREE mo_uml.
    FREE mi_actors.
  ENDMETHOD.                    "teardown_uml

  METHOD test_participant.
    DATA lv_exp TYPE string.
    DATA ls_lifeline TYPE ts_lifeline.

    ls_lifeline-label = `Test Participant`.
    ls_lifeline-index = 42.
    lv_exp = mo_uml->mv_diagram && iv_exp.
    mo_uml->participant( is_lifeline = ls_lifeline ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = mv_obj_name && ` PARTICIPANT( ) Error` ).
  ENDMETHOD.                    "test_participant

  METHOD test_message.
    DATA lv_exp TYPE string.
    DATA ls_message TYPE ts_message.

    ls_message-id = c_id_method.
    ls_message-caller = 3.
    ls_message-called = 5.
    ls_message-from_level = 2.
    ls_message-msg = 'MAIN'.

    lv_exp = mo_uml->mv_diagram && iv_exp.
    mo_uml->message( ls_message ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = mv_obj_name && ` MESSAGE( ) Error` ).
  ENDMETHOD.                    "test_message

  METHOD test_return.
    DATA lv_exp TYPE string.
    lv_exp = mo_uml->mv_diagram && iv_exp.
    mo_uml->return( iv_from = 2
                    iv_to   = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = mv_obj_name && ` RETURN( ) Error` ).
  ENDMETHOD.                    "test_return

  METHOD test_footer.
    DATA lv_exp TYPE string.
    lv_exp = mo_uml->mv_diagram && iv_exp.
    mo_uml->footer( ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = mv_obj_name && ` FOOTER( ) Error` ).
  ENDMETHOD.                    "test_footer

ENDCLASS.                    "ltc_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_plant_uml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
    METHODS call_factory FOR TESTING.

    METHODS return FOR TESTING.
    METHODS return_from_initial FOR TESTING.
    METHODS return_same_level FOR TESTING.
    METHODS participant FOR TESTING.
    METHODS plantuml_header FOR TESTING.
    METHODS footer FOR TESTING.
ENDCLASS.                    "ltc_plant_uml DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_plant_uml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_plant_uml IMPLEMENTATION.

  METHOD setup.
    setup_uml( iv_uml = lcl_uml=>c_uml_plant
               iv_name = 'LCL_PLANT_UML' ).
  ENDMETHOD.                    "setup

  METHOD teardown.
    teardown_uml( ).
  ENDMETHOD.                    "teardown

  METHOD add.
    CONSTANTS c_code TYPE string VALUE 'New PlantUML Code'.
    DATA lv_before TYPE string.

    lv_before = mo_uml->mv_diagram.
    mo_uml->add( c_code ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_before && c_code
                                        msg = 'LCL_UML_PLANT ADD( ) Error' ).
  ENDMETHOD.                    "add

  METHOD message.
    test_message( |3 -> 5: Call method MAIN\nactivate 5\n| ).
  ENDMETHOD.                    "message

  METHOD call_and_skip.
    DATA lv_exp TYPE string.
    DATA ls_message TYPE ts_message.

    lv_exp = mo_uml->mv_diagram &&
                  |1 -> 2: <b>Skipping over SAP code until calling MODIFIER</b>\n|
               && |activate 2\nnote over 1,2\n{ lcl_uml_plant=>c_txt_std }\nend note\n|.

    ls_message-id = c_id_skip.
    ls_message-from_level = 1.
    ls_message-msg = 'MODIFIER'.
    ls_message-caller = 1.
    ls_message-called = 2.

    mo_uml->message( ls_message ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT MESSAGE( ) Skip' ).
  ENDMETHOD.                    "call_and_skip

  METHOD first_message.
    DATA lv_exp TYPE string.
    DATA ls_message TYPE ts_message.

    lv_exp = mo_uml->mv_diagram && |{ lcl_call_stack=>c_first_key } -> 2: \nactivate 2\n|.

    ls_message-caller = lcl_call_stack=>c_first_key.
    ls_message-called = 2.
    mo_uml->message( is_message = ls_message ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT MESSAGE( ) First' ).
  ENDMETHOD.                    "first_message

  METHOD initial_message.
    DATA lv_exp TYPE string.
    DATA ls_message TYPE ts_message.

    lv_exp = mo_uml->mv_diagram.
    mo_uml->message( is_message = ls_message ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT MESSAGE( ) Initial' ).
  ENDMETHOD.                    "initial_message

  METHOD call_factory.
    DATA ls_path TYPE ts_path.
    DATA lv_exp TYPE string.
    DATA ls_message TYPE ts_message.
    DATA ls_sat TYPE ts_sat.

    ls_sat-called-local = 'LCL_TEST'.
    ls_sat-called-type = c_type_class .
    ls_path = mi_actors->new_path( ls_sat ).

    lv_exp = mo_uml->mv_diagram &&
      |3 -> { ls_path-called }: Create instance of class LCL_TEST\nactivate { ls_path-called }\n|.

    ls_message-id = c_id_class.
    ls_message-from_level = 2.
    ls_message-msg = lcl_abap_trace=>c_create_method.
    ls_message-caller = 3.
    ls_message-called = ls_path-called.
    mo_uml->message( ls_message ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT MESSAGE( ) Factory' ).
  ENDMETHOD.                    "call_factory

  METHOD return.
    test_return( |2 --> 1\ndeactivate 2\n| ).
  ENDMETHOD.                    "return

  METHOD return_from_initial.
    DATA lv_exp TYPE string.

    lv_exp = mo_uml->mv_diagram && |0 --> 1\n|.
    mo_uml->return( iv_from = 0
                    iv_to   = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT RETURN( ) From Initial Error' ).
  ENDMETHOD.                    "return_from_initial

  METHOD return_same_level.
    DATA lv_exp TYPE string.

    lv_exp = mo_uml->mv_diagram && |deactivate 3\n|.
    mo_uml->return( iv_from = 3
                    iv_to   = 3 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = lv_exp
                                        msg = 'LCL_UML_PLANT RETURN( ) Same Level' ).
  ENDMETHOD.                    "return_same_level

  METHOD participant.
    test_participant( |participant "Test Participant" as 42\n| ).
  ENDMETHOD.                    "participant

  METHOD plantuml_header.
    DATA lo_uml TYPE REF TO lcl_uml_plant.
    lo_uml ?= mo_uml.
    cl_abap_unit_assert=>assert_equals( act = lo_uml->scale( lcl_configuration=>get( ) )
                                        exp = |scale 0.50\n|
                                        msg = 'LCL_UML_PLANT->SCALE( ) Error' ).
  ENDMETHOD.                    "plantuml_header

  METHOD footer.
    test_footer( |@enduml\n| ).
  ENDMETHOD.                    "footer

ENDCLASS.                    "ltc_plant_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_graph_uml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_graph_uml DEFINITION INHERITING FROM ltc_uml
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS setup.
    METHODS teardown.

    METHODS call FOR TESTING.
    METHODS return FOR TESTING.
    METHODS participant FOR TESTING.
    METHODS footer FOR TESTING.
ENDCLASS.                    "ltc_graph_uml DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_graph_uml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_graph_uml IMPLEMENTATION.

  METHOD setup.
    setup_uml( iv_uml = lcl_uml=>c_uml_graph
               iv_name = 'LCL_GRAPH_UML' ).
  ENDMETHOD.                    "setup

  METHOD teardown.
    teardown_uml( ).
  ENDMETHOD.                    "teardown

  METHOD call.
    test_message( |message(3,5,"Call method MAIN");\nactive(5);\n| ).
  ENDMETHOD.                    "call

  METHOD return.
    test_return( |return_message(2,1);\ninactive(2);\n| ).
  ENDMETHOD.                    "return

  METHOD participant.
    test_participant( |object(42,"Test Participant");\n| ).
  ENDMETHOD.                    "participant

  METHOD footer.
    test_footer( |.PE\n| ).
  ENDMETHOD.                    "footer

ENDCLASS.                    "ltc_graph_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_mscgen_uml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_mscgen_uml DEFINITION INHERITING FROM ltc_uml
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS setup.
    METHODS teardown.

    METHODS call FOR TESTING.
    METHODS return FOR TESTING.
    METHODS participant FOR TESTING.
    METHODS footer FOR TESTING.
ENDCLASS.                    "ltc_mscgen_uml DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_mscgen_uml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_mscgen_uml IMPLEMENTATION.

  METHOD setup.
    setup_uml( iv_uml = lcl_uml=>c_uml_mscgen
               iv_name = 'LCL_MSCGEN_UML' ).
  ENDMETHOD.                    "setup

  METHOD teardown.
    teardown_uml( ).
  ENDMETHOD.                    "teardown

  METHOD call.
    test_message( |3=>5 [label="Call method MAIN"];\n| ).
  ENDMETHOD.                    "call

  METHOD return.
    test_return( |1<<2\n| ).
  ENDMETHOD.                    "return

  METHOD participant.
    test_participant( |42 [label="Test Participant"]| ).
  ENDMETHOD.                    "participant

  METHOD footer.
    test_footer( |\}\n| ).
  ENDMETHOD.                    "footer

ENDCLASS.                    "ltc_mscgen_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_file_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_file_name DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    CONSTANTS:
      c_path   TYPE string VALUE 'C:\Temp\Dokumente\UML\',
      c_prefix TYPE string VALUE 'test' ##no_text.
    DATA mo_name TYPE REF TO lcl_file_name.

    METHODS setup.
    METHODS teardown.
    METHODS fixture.

    METHODS prefix_with_txt_extension FOR TESTING.
    METHODS prefix_without_extension FOR TESTING.
    METHODS get_fullpath FOR TESTING.
ENDCLASS.                    "ltc_file_name DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_file_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_file_name IMPLEMENTATION.

  METHOD setup.
    mo_name = lcl_file_name=>new( lcl_diagram_text=>c_mode_txt ).
    fixture( ).
  ENDMETHOD.                    "setup

  METHOD fixture.
    mo_name->ms_file-name = c_prefix && mo_name->ms_file-ext.
    mo_name->ms_file-path = c_path && mo_name->ms_file-name.
  ENDMETHOD.                    "fixture

  METHOD teardown.
    FREE mo_name.
  ENDMETHOD.                    "teardown

  METHOD prefix_with_txt_extension.
    cl_abap_unit_assert=>assert_equals( exp = c_prefix
                                        act = mo_name->get_prefix( )
                                        msg = 'LCL_FILE_NAME->GET_PREFIX( ) Error' ).
  ENDMETHOD.                    "prefix_with_txt_extension

  METHOD prefix_without_extension.
    mo_name->ms_file-ext = space.
    fixture( ).
    cl_abap_unit_assert=>assert_equals( exp = c_prefix
                                        act = mo_name->get_prefix( )
                                        msg = 'LCL_FILE_NAME->GET_PREFIX( ) Error No Prefix' ).
  ENDMETHOD.                    "prefix_without_extension

  METHOD get_fullpath.
    cl_abap_unit_assert=>assert_equals( exp = mo_name->ms_file-path
                                        act = mo_name->get_fullpath( )
                                        msg = 'LCL_FILE_NAME->GET_FULLPATH( ) Error' ).
  ENDMETHOD.                    "get_fullpath

ENDCLASS.                    "ltc_file_name IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_diagram DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
ENDCLASS.                    "ltc_diagram DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_diagram IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_diagram IMPLEMENTATION.

  METHOD setup.
    lcl_configuration=>gs_cfg-skip_dialog = abap_true.
    lcl_configuration=>gs_cfg-output_mode = lcl_diagram_text=>c_mode_aut.
    mo_gen ?= lcl_sequence=>to_diagram( lcl_configuration=>get( ) ).
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mo_gen.
  ENDMETHOD.                    "teardown

  METHOD set_diagram.
    mo_gen->mv_diagram = iv_text.
  ENDMETHOD.                    "set_diagram

  METHOD to_url.
    set_diagram( |Bob -> Alice : hello| ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'SyfFKj2rKt3CoKnELR1Io4ZDoSa70000'
      act = mo_gen->to_url( iv_base_url = space )
      msg = 'LCL_SEQ_GEN TO_URL( ) Error' ).
  ENDMETHOD.                    "to_url

  METHOD to_xstring.
    set_diagram( 'Polyfon zwitschernd aßen Mäxchens Vögel Rüben, Joghurt und Quark' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'UG9seWZvbiB6d2l0c2NoZXJuZCBhw59lbiBNw6R4Y2hlbnMgVsO2Z2VsIFLDvGJlbiwgSm9naHVydCB1bmQgUXVhcms='
      act = cl_http_utility=>encode_x_base64( mo_gen->to_xstring( mo_gen->mv_diagram ) )
      msg = 'LCL_SEQ_GEN TO_XSTRING( ) Error' ).
  ENDMETHOD.                    "to_xstring

ENDCLASS.                    "ltc_diagram IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_messages DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
                   RETURNING value(rv_uml)    TYPE string.

    METHODS get_log RETURNING value(ro_log) TYPE REF TO lcl_logger.
  PRIVATE SECTION.
    METHODS plantuml_header RETURNING value(rv_header) TYPE string.
    METHODS expected_plantuml_source RETURNING value(rv_exp) TYPE string.

    METHODS:
      calls_for_empty_diagram FOR TESTING,
      plantuml_for_empty_diagram FOR TESTING.

    METHODS sample_plantuml_body FOR TESTING.
    METHODS calls_for_sample_output FOR TESTING.
ENDCLASS.                    "ltc_messages DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_messages IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_messages IMPLEMENTATION.

  METHOD constructor.
    mt_sat = ltc_actors=>sat_fixture_messages( ).
  ENDMETHOD.                    "constructor

  METHOD to_uml.
    DATA lo_calls TYPE REF TO lcl_call_stack.
    DATA ls_cfg TYPE ts_diagram_config.

    ls_cfg-uml_format = iv_uml.
    ls_cfg-compact_trace = iv_compact_trace.

    lo_calls = ltc_sequence=>new_sequence( it_sat = it_sat
                                           is_cfg = ls_cfg )->to_call_stack( ).
    rv_uml = lo_calls->to_uml( ).
    mo_uml = lo_calls->mo_uml.
  ENDMETHOD.                    "to_uml

  METHOD get_log.
    DATA lo_logger TYPE REF TO lcl_uml_logger.
    lo_logger ?= mo_uml.
    ro_log = lo_logger->mo_log.
  ENDMETHOD.                    "get_log

  METHOD calls_for_empty_diagram.
    DATA lt_sat TYPE ltc_sequence=>tt_sat.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    CLEAR ls_log.
    ls_log-method = 'HEADER'.
    APPEND ls_log TO lt_log.
    CLEAR ls_log.
    ls_log-method = 'FOOTER'.
    APPEND ls_log TO lt_log.

    to_uml( it_sat = lt_sat
            iv_uml = lcl_uml=>c_uml_mock ).
    get_log( )->verify( it_exp = lt_log
                        iv_msg = 'LCL_MESSAGES TO_UML( ) Call Stack Empty Diagram Header/Footer Error' ).
  ENDMETHOD.                    "calls_for_empty_diagram

  METHOD calls_for_sample_output.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    DEFINE add_log.
      clear ls_log.
      ls_log-method = &1.
      ls_log-params = &2.
      append ls_log to lt_log.
    END-OF-DEFINITION.

    CLEAR ls_log.
    ls_log-method = 'HEADER'.
    APPEND ls_log TO lt_log.
    CLEAR ls_log.
    ls_log-method = 'PARTICIPANT'.
    ls_log-params = |1 PROG\\nY_TEST_PROG|.
    APPEND ls_log TO lt_log.
    CLEAR ls_log.
    ls_log-method = 'PARTICIPANT'.
    ls_log-params = |2 Static Methods of Class LCL_CTRL\\nY_TEST_PROG|.
    APPEND ls_log TO lt_log.
    CLEAR ls_log.
    ls_log-method = 'PARTICIPANT'.
    ls_log-params = |3 ObjectId:6 of Class LCL_MODEL\\nY_TEST_PROG|.
    APPEND ls_log TO lt_log.
    add_log: 'CALL' ` 1 1 `,
             'CALL' `m 1 2 Call method MAIN`,
             'CALL' `m 2 3 Call method CONSTRUCTOR`,
             'RETURN' `3 2`,
             'CALL' `m 2 2 Call method CHANGE`,
             'CALL' `m 2 3 Call method UP`,
             'RETURN' `3 2`,
             'RETURN' `2 2`,
             'RETURN' `2 1`.
    CLEAR ls_log.
    ls_log-method = 'FOOTER'.
    APPEND ls_log TO lt_log.

    to_uml( it_sat = mt_sat
            iv_uml = lcl_uml=>c_uml_mock ).
    get_log( )->verify( it_exp = lt_log
                        iv_msg = 'LCL_MESSAGES TO_UML( ) Call Stack Error' ).
  ENDMETHOD.                    "calls_for_sample_output

  METHOD plantuml_header.
    DATA lo_uml TYPE REF TO lcl_uml_plant.
    CREATE OBJECT lo_uml.
    lo_uml->header( ).
    rv_header = lo_uml->mv_diagram.
  ENDMETHOD.                    "plantuml_header

  METHOD expected_plantuml_source.
    rv_exp = plantuml_header( ) &&
|participant "PROG\\nY_TEST_PROG" as 1\nparticipant "Static Methods of Class LCL_CTRL\\nY_TEST_PROG" as 2\n| &&
|participant "ObjectId:6 of Class LCL_MODEL\\nY_TEST_PROG" as 3\n1 -> 1: \nactivate 1\n| &&
|1 -> 2: Call method MAIN\nactivate 2\n2 -> 3: Call method CONSTRUCTOR\nactivate 3\n3 --> 2\ndeactivate 3\n2 -> 2: Call method CHANGE\n| &&
|activate 2\n2 -> 3: Call method UP\nactivate 3\n3 --> 2\ndeactivate 3\ndeactivate 2\n2 --> 1\n| &&
|deactivate 2\n@enduml\n|.
  ENDMETHOD.                    "expected_plantuml_source

  METHOD sample_plantuml_body.
    cl_abap_unit_assert=>assert_equals(
      act = to_uml( mt_sat )
      exp = expected_plantuml_source( )
      msg = 'LCL_MESSAGES->TO_UML( ) PlantUML Error' ).
  ENDMETHOD.                    "sample_plantuml_body

  METHOD plantuml_for_empty_diagram.
    DATA lt_sat TYPE ltc_sequence=>tt_sat.
    cl_abap_unit_assert=>assert_char_cp(
      act = to_uml( lt_sat )
      exp = |@startuml\nhide footbox\nautonumber\n*skinparam*@enduml\n|
      msg = 'LCL_MESSAGES TO_UML( ) Empty Diagram Header/Footer Error' ).
  ENDMETHOD.                    "plantuml_for_empty_diagram

ENDCLASS.                    "ltc_messages IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_call_compact DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_call_compact DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mo_calls TYPE REF TO lcl_call_stack_compact.   " CUT
    DATA mo_iterator TYPE REF TO lcl_messages.
    DATA mo_uml TYPE REF TO lcl_uml.

    METHODS setup.
    METHODS teardown.

    METHODS cycle_trace RETURNING value(rt_trace) TYPE tt_message.
    METHODS fixture
      IMPORTING iv_compact_trace TYPE xsdboolean
                iv_uml           TYPE lcl_uml=>tv_uml DEFAULT lcl_uml=>c_uml_plant
      RETURNING value(ro_calls)  TYPE REF TO lcl_call_stack.
*
    METHODS create_cycle FOR TESTING.
    METHODS cycle_no_compress FOR TESTING.

    METHODS begin_loop FOR TESTING.
    METHODS calls_for_begin_loop FOR TESTING.
    METHODS end_loop FOR TESTING.
    METHODS no_end_loop FOR TESTING.
ENDCLASS.                    "ltc_call_compact DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_call_compact IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_call_compact IMPLEMENTATION.

  METHOD cycle_trace.
    DATA ls_msg TYPE ts_message.

    ls_msg-id = 'm'.
    ls_msg-from_level = 1.
    ls_msg-caller = 1.
    ls_msg-called = 2.
    ls_msg-msg = 'MAIN'.
    APPEND ls_msg TO rt_trace.

    CLEAR ls_msg.
    ls_msg-id = 'm'.
    ls_msg-from_level = 2.
    ls_msg-caller = 2.
    ls_msg-called = 3.
    ls_msg-msg = lcl_abap_trace=>c_create_method.
    APPEND ls_msg TO rt_trace.

    APPEND LINES OF rt_trace TO rt_trace.  " Create Loop
  ENDMETHOD.                    "cycle_trace

  METHOD setup.
    mo_calls ?= fixture( iv_compact_trace = abap_true
                         iv_uml = lcl_uml=>c_uml_plant ).
  ENDMETHOD.                    "setup

  METHOD fixture.
    DATA lo_actors TYPE REF TO lcl_actors.
    CREATE OBJECT lo_actors.
    mo_uml = lcl_uml=>new( iv_uml = iv_uml
                           ii_actors = lo_actors ).
    ro_calls = lcl_uml_factory=>new_calls( iv_compact_trace = iv_compact_trace
                                           it_trace = cycle_trace( )
                                           io_uml = mo_uml ).
    mo_iterator = ro_calls->mo_messages.
  ENDMETHOD.                    "fixture

  METHOD teardown.
    FREE mo_iterator.
    FREE mo_calls.
    FREE mo_uml.
  ENDMETHOD.                    "teardown

  METHOD create_cycle.
    cl_abap_unit_assert=>assert_equals( act = lines( mo_calls->mt_cycle )
                                        exp = 1
                                        msg = 'LCL_CALL_STACK_COMPACT CONSTRUCTOR( ) Cycle Error' ).
  ENDMETHOD.                    "create_cycle

  METHOD cycle_no_compress.
    TRY.
        mo_calls ?= fixture( iv_compact_trace = abap_false ).
        cl_abap_unit_assert=>fail( msg = 'LCL_CALL_STACK_COMPACT CONSTRUCTOR( ) No Cycle Error' ).
      CATCH cx_sy_move_cast_error ##no_handler.
*     Expected: Object of type LCL_MESSAGES created, not compatible with LCL_MESSAGE_COMPACT
    ENDTRY.
  ENDMETHOD.                    "cycle_no_compress

  METHOD begin_loop.
    mo_iterator->next( ).
    mo_calls->begin( iv_idx = mo_iterator->mv_idx
                     iv_from = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = |loop 2 times\n|
                                        msg = 'LCL_CALL_STACK_COMPACT BEGIN_LOOP( ) Error' ).
  ENDMETHOD.                    "begin_loop

  METHOD calls_for_begin_loop.
    DATA lo_logger TYPE REF TO lcl_uml_logger.
    DATA ls_log TYPE lcl_logger=>ts_log.
    DATA lt_log TYPE lcl_logger=>tt_log.

    ls_log-method = 'BEGIN_LOOP'.
    ls_log-params = '2'.
    APPEND ls_log TO lt_log.

    mo_calls ?= fixture( iv_compact_trace = abap_true
                         iv_uml = lcl_uml=>c_uml_mock ).
    mo_iterator->next( ).
    mo_calls->begin( iv_idx = mo_iterator->mv_idx
                     iv_from = 1 ).
    lo_logger ?= mo_uml.
    lo_logger->mo_log->verify( it_exp = lt_log
                               iv_msg = 'LCL_CALL_STACK_COMPACT BEGIN_LOOP( ) Calls Error' ).
  ENDMETHOD.                    "calls_for_begin_loop

  METHOD end_loop.
    mo_iterator->skip( 2 ).
    mo_calls->end( iv_idx = mo_iterator->mv_idx
                   iv_to = 2 ).     " need a MOCK object here
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = |end\n|
                                        msg = 'LCL_CALL_STACK_COMPACT END_LOOP( ) Error' ).
  ENDMETHOD.                    "end_loop

  METHOD no_end_loop.
    mo_calls ?= fixture( abap_true ).
    mo_iterator->skip( 4 ).
    mo_calls->end( iv_idx = mo_iterator->mv_idx
                   iv_to = 2 ).
    cl_abap_unit_assert=>assert_equals( act = mo_uml->mv_diagram
                                        exp = space
                                        msg = 'LCL_CALL_STACK_COMPACT END_LOOP( ) No End Loop Error' ).
  ENDMETHOD.                    "no_end_loop

ENDCLASS.                    "ltc_call_compact IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_cycles DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_cycles DEFINITION FOR TESTING CREATE PRIVATE
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
    INTERFACES lif_cycles.
  PRIVATE SECTION.
    DATA mt_messages TYPE tt_message.
    DATA mo_index TYPE REF TO lcl_trace_index.
    DATA mt_cycles TYPE lcl_call_stack_compact=>tt_cycle.

    METHODS fixture IMPORTING iv_cycles TYPE n DEFAULT 1.
    METHODS detect_cycles IMPORTING iv_cycles TYPE n DEFAULT 1.

    METHODS next_match FOR TESTING.
    METHODS detect FOR TESTING.
    METHODS detect_4 FOR TESTING.
    METHODS fold FOR TESTING.
ENDCLASS.                    "ltc_cycles DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_cycles IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_cycles IMPLEMENTATION.

  METHOD fixture.
    DATA ls_message TYPE ts_message.
    DATA lt_msg LIKE mt_messages.

    ls_message-id = 'm'.
    ls_message-from_level = 1.
    ls_message-caller = 1.
    ls_message-called = 2.
    ls_message-msg = 'MAIN'.
    APPEND ls_message TO mt_messages.

    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 2.
    ls_message-called = 3.
    ls_message-msg = lcl_abap_trace=>c_create_method.
    APPEND ls_message TO mt_messages.

    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 2.
    ls_message-called = 4.
    ls_message-msg = 'CHANGE'.
    APPEND ls_message TO mt_messages.

    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 2.
    ls_message-called = 4.
    ls_message-msg = 'CHANGE'.
    APPEND ls_message TO mt_messages.

    ls_message-id = 'm'.
    ls_message-from_level = 3.
    ls_message-caller = 4.
    ls_message-called = 6.
    ls_message-msg = 'UP'.
    APPEND ls_message TO mt_messages.

    lt_msg = mt_messages.
    DO iv_cycles TIMES.
      APPEND LINES OF lt_msg TO mt_messages.
    ENDDO.

    CLEAR lt_msg.

    ls_message-id = 'm'.
    ls_message-from_level = 1.
    ls_message-caller = 10.
    ls_message-called = 11.
    ls_message-msg = 'MAIN'.
    APPEND ls_message TO lt_msg.

    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 11.
    ls_message-called = 16.
    ls_message-msg = lcl_abap_trace=>c_create_method.
    APPEND ls_message TO lt_msg.

    APPEND LINES OF lt_msg TO mt_messages.

    CREATE OBJECT mo_index
      EXPORTING
        it_trace  = mt_messages
        ii_cycles = me.
  ENDMETHOD.                    "fixture

  METHOD lif_cycles~collect.
    INSERT is_cycle INTO TABLE mt_cycles.
  ENDMETHOD.                    "lif_cycles~collect

  METHOD lif_cycles~accepts.
    rv_flag = abap_true.
  ENDMETHOD.                    "lif_cycles~accepts

  METHOD detect_cycles.
    fixture( iv_cycles ).
    lcl_pattern=>new( it_xindex = mo_index->mt_xindex
                      ii_cycles = me )->detect_cycles( ).
  ENDMETHOD.                    "detect_cycles

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
  ENDMETHOD.                    "next_match

  METHOD detect.
    detect_cycles( ).
    cl_abap_unit_assert=>assert_equals( act = lines( mt_cycles )
                                        exp = 2
                                        msg = 'LCL_PATTERN DETECT_CYCLES( ) Error' ).
  ENDMETHOD.                    "detect

  METHOD detect_4.
    detect_cycles( 4 ).
    cl_abap_unit_assert=>assert_equals( act = lines( mt_cycles )
                                        exp = 2
                                        msg = 'LCL_PATTERN DETECT_CYCLES( ) Level 4 Error' ).
  ENDMETHOD.                    "detect_4

  METHOD fold.
    DATA lo_compactor TYPE REF TO lcl_uml_compactor.
    DATA lt_act TYPE tt_message.
    DATA lt_exp TYPE tt_message.
    DATA ls_message TYPE ts_message.

    detect_cycles( 3 ).
    CREATE OBJECT lo_compactor.
    lo_compactor->fold( EXPORTING it_trace = mt_messages
                        IMPORTING et_messages = lt_act ).
    ls_message-id = 'm'.
    ls_message-from_level = 1.
    ls_message-caller = 1.
    ls_message-called = 2.
    ls_message-msg = 'MAIN'.
    APPEND ls_message TO lt_exp.
    CLEAR ls_message.
    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 2.
    ls_message-called = 3.
    ls_message-msg = lcl_abap_trace=>c_create_method.
    APPEND ls_message TO lt_exp.
    CLEAR ls_message.
    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 2.
    ls_message-called = 4.
    ls_message-msg = 'CHANGE'.
    APPEND ls_message TO lt_exp.

    CLEAR ls_message.
    ls_message-id = 'm'.
    ls_message-from_level = 3.
    ls_message-caller = 4.
    ls_message-called = 6.
    ls_message-msg = 'UP'.
    APPEND ls_message TO lt_exp.

    CLEAR ls_message.
    ls_message-id = 'm'.
    ls_message-from_level = 1.
    ls_message-caller = 10.
    ls_message-called = 11.
    ls_message-msg = 'MAIN'.
    APPEND ls_message TO lt_exp.

    CLEAR ls_message.
    ls_message-id = 'm'.
    ls_message-from_level = 2.
    ls_message-caller = 11.
    ls_message-called = 16.
    ls_message-msg = lcl_abap_trace=>c_create_method.
    APPEND ls_message TO lt_exp.

    cl_abap_unit_assert=>assert_equals( act = lt_act
                                        exp = lt_exp
                                        msg = 'LCL_LOOP_COMPACTOR->FOLD( ) LOGIC Error' ).
  ENDMETHOD.                    "fold

ENDCLASS.                    "ltc_cycles IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_shrinkage DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_shrinkage DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    METHODS reconcile_loop_references FOR TESTING.
ENDCLASS.                    "ltc_shrinkage DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_shrinkage IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_shrinkage IMPLEMENTATION.

  METHOD reconcile_loop_references.
    DATA lt_cycles TYPE lcl_call_stack_compact=>tt_cycle.
    DATA lt_exp LIKE lt_cycles.
    DATA ls_cycle TYPE ts_cycle.

    DEFINE cycle_value.
      ls_cycle-start = &2.
      ls_cycle-end = &3.
      ls_cycle-last = &4.
      ls_cycle-times = &5.
      insert ls_cycle into table &1.
    END-OF-DEFINITION.

    cycle_value lt_cycles: 1  48 192 4,
                           3  24  46 2,
                          19  20  22 2,
                         193 194 200 4,
                         201 632 476264 1102,
                         217 218 220 2.
*      ( start = 1    end = 48   last = 192    times = 4 )    "   1 +  48 x 4 - 1 = 192
*      ( start = 3    end = 24   last = 46     times = 2 )    "   3 +  22 x 2 - 1 = 46
*      ( start = 19   end = 20   last = 22     times = 2 )    "  19 +   2 x 2 - 1 = 22
*      ( start = 193  end = 194  last = 200    times = 4 )    " 193 +   2 x 4 - 1 = 200
*      ( start = 201  end = 632  last = 476264 times = 1102 ) " 201 + 432 x 1102 - 1 = 476264
*      ( start = 217  end = 218  last = 220    times = 2 ) ). " 217 +   2 x 2 - 1 = 220

    lcl_shrinkage=>shrink_references( CHANGING ct_cycles = lt_cycles ).

    cycle_value lt_exp: 1 24 24 4,
                        3 22 22 2,
                       19 20 20 2,
                       25 26 26 4,
                       27 456 456 1102,
                       43 44 44 2.

    cl_abap_unit_assert=>assert_equals(
      act = lt_cycles
      exp = lt_exp
      msg = 'LCL_SHRINKAGE->SHRINK_REFERENCES( ) Error' ).
  ENDMETHOD.                    "reconcile_loop_references

ENDCLASS.                    "ltc_shrinkage IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_trace DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
    DATA mt_sat TYPE STANDARD TABLE OF ts_sat.

    METHODS setup.
    METHODS teardown.
    METHODS fill_trace.
    METHODS fixture_add_more.
    METHODS fixture_prepare_traceprog.
    METHODS fixture_prepare_tracetext.
    METHODS fixture_prepare_tracemeth.
    METHODS fixture_caller.

    METHODS add_message FOR TESTING.
    METHODS add_more FOR TESTING.
    METHODS skip_message FOR TESTING.
    METHODS filter_message FOR TESTING.
    METHODS save_caller FOR TESTING.
    METHODS test_output FOR TESTING.
    METHODS skip_record FOR TESTING.
ENDCLASS.                    "ltc_trace DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_trace IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_trace IMPLEMENTATION.

  METHOD setup.
    CLEAR mt_act_trace.
    CLEAR mt_sat.
    APPEND 'Y*' TO ms_cfg-pattern.
    ms_cfg-compact_trace = abap_true.
    ms_cfg-output_mode = lcl_diagram_text=>c_mode_aut.
    CREATE OBJECT: mo_trace,
                   mo_state EXPORTING io_atra = mo_trace->mo_atra.
  ENDMETHOD.                    "setup

  METHOD teardown.
    mo_state->restore( ).
    FREE mo_state.
    FREE mo_trace.
  ENDMETHOD.                    "teardown

  METHOD fill_trace.
    DATA lo_sequence TYPE REF TO lcl_sequence.
    CREATE OBJECT lo_sequence
      EXPORTING
        is_cfg = ms_cfg.
    mo_trace->fill( lo_sequence ).
    mt_act_trace = lo_sequence->mt_trace.
  ENDMETHOD.                    "fill_trace

  METHOD lif_collector~collect.
*   No filter / No conversion
    APPEND is_sat TO mt_sat.
  ENDMETHOD.                    "lif_collector~collect

  METHOD add_message.
    DATA ls_austab TYPE satr_austab_gesamt.
    ls_austab-id = 'm'.
    ls_austab-index = 1.
    ls_austab-ebene = 2.
    ls_austab-caller = 3.
    ls_austab-event = '>'.
    CLEAR mo_trace->mo_atra->it_austab_hier[].
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    mo_trace->fill( me ).
    cl_abap_unit_assert=>assert_equals( act = lines( mt_sat )
                                        exp = 1
                                        msg = 'LCL_TRACE ADD_MESSAGE( ) Error' ).
  ENDMETHOD.                    "add_message

  METHOD filter_message.
    DATA ls_prog TYPE satr_prog.
    DATA ls_austab TYPE satr_austab_gesamt.

    CLEAR mo_trace->mo_atra->it_traceprog[].
    ls_prog-cprog = 'Y_PROG'.
    ls_prog-object = c_type_prog.
    APPEND ls_prog TO mo_trace->mo_atra->it_traceprog.
    ls_prog-cprog = 'SAPPROG'.
    ls_prog-object = c_type_class.
    APPEND ls_prog TO mo_trace->mo_atra->it_traceprog.

    CLEAR mo_trace->mo_atra->it_austab_hier[].
    ls_austab-id = 'O'.
    ls_austab-index = 1.
    ls_austab-ebene = 0.
    ls_austab-caller = 0.
    ls_austab-event = '>'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'V'.
    ls_austab-subid = 'O'.
    ls_austab-index = 2.
    ls_austab-ebene = 1.
    ls_austab-caller = 1.
    ls_austab-event = '>'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'P'.
    ls_austab-index = 3.
    ls_austab-ebene = 1.
    ls_austab-caller = 1.
    ls_austab-event = '>'.
    ls_austab-progindex = 1.
    ls_austab-textindex = 1.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'm'.
    ls_austab-index = 4.
    ls_austab-ebene = 2.
    ls_austab-caller = 1.
    ls_austab-event = '>'.
    ls_austab-progindex = 1.
    ls_austab-methindex = 1.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'm'.
    ls_austab-subid = 'x'.
    ls_austab-index = 5.
    ls_austab-ebene = 2.
    ls_austab-caller = 1.
    ls_austab-event = '<'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.

    fill_trace( ).
    cl_abap_unit_assert=>assert_equals( act = lines( mt_act_trace )
                                        exp = 1
                                        msg = 'LCL_TRACE ADD_MESSAGE( ) Filter Error' ).
  ENDMETHOD.                    "filter_message

  METHOD skip_message.
    DATA ls_austab TYPE satr_austab_gesamt.

    CLEAR mo_trace->mo_atra->it_austab_hier[].
    ls_austab-id = 'm'.
    ls_austab-index = 1.
    ls_austab-ebene = 2.
    ls_austab-caller = 2.
    ls_austab-event = '<'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.

    fill_trace( ).
    cl_abap_unit_assert=>assert_initial( act = lines( mt_act_trace )
                                         msg = 'LCL_TRACE ADD_MESSAGE( ) Skip Error' ).
  ENDMETHOD.                    "skip_message

  METHOD save_caller.
    DATA ls_austab TYPE satr_austab_gesamt.

    CLEAR mo_trace->mo_atra->it_austab_hier[].
    ls_austab-id = 'm'.
    ls_austab-index = 1.
    ls_austab-ebene = 2.
    ls_austab-caller = 3.
    ls_austab-event = '>'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'm'.
    ls_austab-index = 1.
    ls_austab-ebene = 2.
    ls_austab-caller = 2.
    ls_austab-event = '<'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.

    fill_trace( ).
    cl_abap_unit_assert=>assert_equals( act = lines( mo_trace->mt_caller )
                                        exp = 1
                                        msg = 'LCL_TRACE ADD_MESSAGE( ) Save Caller' ).
  ENDMETHOD.                    "save_caller

  METHOD test_output.
    lcl_configuration=>gs_cfg-skip_dialog = abap_true.
    lcl_configuration=>gs_cfg-output_mode = lcl_diagram_text=>c_mode_aut.
    lcl_sequence=>to_diagram( lcl_configuration=>get( ) )->output( ).
  ENDMETHOD.                    "test_output

  METHOD fixture_prepare_traceprog.
    DATA ls_prog TYPE satr_prog.

    CLEAR mo_trace->mo_atra->it_traceprog[].
    CLEAR ls_prog.
    ls_prog-cprog = ltc_actors=>c_prog.
    ls_prog-object = c_type_prog.
    APPEND ls_prog TO mo_trace->mo_atra->it_traceprog.
    CLEAR ls_prog.
    ls_prog-cprog = ltc_actors=>c_prog.
    ls_prog-object = c_type_class.
    APPEND ls_prog TO mo_trace->mo_atra->it_traceprog.
    CLEAR ls_prog.
    ls_prog-cprog = 'SAPPROG'.
    ls_prog-object = c_type_class.
    APPEND ls_prog TO mo_trace->mo_atra->it_traceprog.
  ENDMETHOD.                    "fixture_prepare_traceprog

  METHOD fixture_prepare_tracetext.
    DATA ls_text LIKE LINE OF mo_trace->mo_atra->it_tracetext.

    CLEAR mo_trace->mo_atra->it_tracetext[].
    ls_text-tracetext = 'MAIN'.
    APPEND ls_text TO mo_trace->mo_atra->it_tracetext.
    ls_text-tracetext = lcl_abap_trace=>c_create_method.
    APPEND ls_text TO mo_trace->mo_atra->it_tracetext.
    ls_text-tracetext = 'CHANGE'.
    APPEND ls_text TO mo_trace->mo_atra->it_tracetext.
    ls_text-tracetext = ltc_actors=>c_ctrl.
    APPEND ls_text TO mo_trace->mo_atra->it_tracetext.
    ls_text-tracetext = ltc_actors=>c_model.
    APPEND ls_text TO mo_trace->mo_atra->it_tracetext.
    ls_text-tracetext = ltc_actors=>c_prog.
    APPEND ls_text TO mo_trace->mo_atra->it_tracetext.
  ENDMETHOD.                    "fixture_prepare_tracetext

  METHOD fixture_prepare_tracemeth.
    DATA ls_meth LIKE LINE OF mo_trace->mo_atra->it_tracemeth.

    CLEAR mo_trace->mo_atra->it_tracemeth[].
    ls_meth-methode = 'MAIN'.
    APPEND ls_meth TO mo_trace->mo_atra->it_tracemeth.
    ls_meth-methode = lcl_abap_trace=>c_create_method.
    APPEND ls_meth TO mo_trace->mo_atra->it_tracemeth.
    ls_meth-methode = 'CHANGE'.
    APPEND ls_meth TO mo_trace->mo_atra->it_tracemeth.
  ENDMETHOD.                    "fixture_prepare_tracemeth

  METHOD fixture_add_more.
    DATA ls_austab TYPE satr_austab_gesamt.
*   Fixture: Scenario from ltc_objects=>sat_fixture_events( )
    fixture_prepare_traceprog( ).
    fixture_prepare_tracetext( ).
    fixture_prepare_tracemeth( ).

    CLEAR mo_trace->mo_atra->it_austab_hier[].
    ls_austab-id = 'O'.
    ls_austab-index = 1.
    ls_austab-ebene = 0.
    ls_austab-caller = 0.
    ls_austab-event = '>'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'C'.
    ls_austab-subid = 'R'.
    ls_austab-index = 2.
    ls_austab-ebene = 1.
    ls_austab-caller = 1.
    ls_austab-event = '>'.
    ls_austab-textindex = 6.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'V'.
    ls_austab-subid = 'O'.
    ls_austab-index = 3.
    ls_austab-ebene = 2.
    ls_austab-caller = 2.
    ls_austab-event = '>'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'P'.
    ls_austab-index = 4.
    ls_austab-ebene = 2.
    ls_austab-caller = 2.
    ls_austab-event = '>'.
    ls_austab-progindex = 1.
    ls_austab-textindex = 1.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.

    CLEAR ls_austab.
    ls_austab-id = 'm'.
    ls_austab-index = 5.
    ls_austab-ebene = 3.
    ls_austab-caller = 4.
    ls_austab-event = '>'.
    ls_austab-progindex = 2.
    ls_austab-textindex = 4.
    ls_austab-methindex = 1.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'm'.
    ls_austab-subid = 'x'.
    ls_austab-index = 6.
    ls_austab-ebene = 4.
    ls_austab-caller = 5.
    ls_austab-event = '>'.
    ls_austab-progindex = 3.
    ls_austab-textindex = 5.
    ls_austab-methindex = 2.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'm'.
    ls_austab-subid = 'x'.
    ls_austab-index = 7.
    ls_austab-ebene = 4.
    ls_austab-caller = 6.
    ls_austab-event = '>'.
    ls_austab-progindex = 1.
    ls_austab-textindex = 4.
    ls_austab-methindex = 3.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.

    fill_trace( ).
  ENDMETHOD.                    "fixture_add_more

  METHOD add_more.
    DATA ls_message TYPE ts_message.
    DATA lt_exp TYPE tt_message.

    fixture_add_more( ).

    CLEAR ls_message.
    ls_message-id  = 'C'.
    ls_message-from_level = 1.
    ls_message-caller = 1.
    ls_message-called = 2.
    ls_message-msg = ltc_actors=>c_prog.
    APPEND ls_message TO lt_exp.

    CLEAR ls_message.
    ls_message-id  = 'm'.
    ls_message-from_level = 3.
    ls_message-caller = 2.
    ls_message-called = 3.
    ls_message-msg = 'MAIN'.
    APPEND ls_message TO lt_exp.

    CLEAR ls_message.
    ls_message-id  = c_id_skip.
    ls_message-from_level = 3.
    ls_message-caller = 3.
    ls_message-called = 4.
    ls_message-msg = lcl_abap_trace=>c_create_method.
    APPEND ls_message TO lt_exp.

    CLEAR ls_message.
    ls_message-id  = 'm'.
    ls_message-from_level = 4.
    ls_message-caller = 4.
    ls_message-called = 3.
    ls_message-msg = 'CHANGE'.
    APPEND ls_message TO lt_exp.

    cl_abap_unit_assert=>assert_equals( act = mt_act_trace
                                        exp = lt_exp
                                        msg = 'LCL_TRACE ADD_MESSAGE( ) More - Error' ).
  ENDMETHOD.                    "add_more

  METHOD fixture_caller.
    DATA ls_austab TYPE satr_austab_gesamt.

    fixture_prepare_traceprog( ).
    fixture_prepare_tracetext( ).
    fixture_prepare_tracemeth( ).

    CLEAR mo_trace->mo_atra->it_austab_hier[].
    ls_austab-id = 'O'.
    ls_austab-index = 1.
    ls_austab-ebene = 0.
    ls_austab-caller = 0.
    ls_austab-event = '>'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    CLEAR ls_austab.
    ls_austab-id = 'C'.
    ls_austab-subid = 'R'.
    ls_austab-index = 2.
    ls_austab-ebene = 1.
    ls_austab-caller = 1.
    ls_austab-event = '>'.
    ls_austab-textindex = 6.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.
    ls_austab-event = '<'.
    APPEND ls_austab TO mo_trace->mo_atra->it_austab_hier.

    fill_trace( ).
  ENDMETHOD.                    "fixture_caller

  METHOD skip_record.
    DATA lt_exp TYPE lcl_trace=>tt_seq_k1.
    DATA ls_seq TYPE ts_seq.

*   TO DO: check coverage/review fixture
    fixture_caller( ).

    CLEAR ls_seq.
    ls_seq-called-instance = 0.
    ls_seq-aus_tabix = 1.
    APPEND ls_seq TO lt_exp.
    CLEAR ls_seq.
    ls_seq-called-global = ltc_actors=>c_prog.
    ls_seq-called-type = c_type_prog.
    ls_seq-called-instance = 0.
    ls_seq-called-mod = ltc_actors=>c_prog.
    ls_seq-aus_tabix       = 2.
    APPEND ls_seq TO lt_exp.

    cl_abap_unit_assert=>assert_equals( act = mo_trace->mt_caller
                                        exp = lt_exp
                                        msg = 'LCL_TRACE SKIP_RECORD( ) Error' ).
  ENDMETHOD.                    "skip_record

ENDCLASS.                    "ltc_trace IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_filter_null DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
ENDCLASS.                    "ltc_filter_null DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_filter_null IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_filter_null IMPLEMENTATION.

  METHOD setup.
    DATA ls_cfg TYPE ts_diagram_config.
    mi_filter = lcl_filter=>new( ls_cfg ).
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mi_filter.
  ENDMETHOD.                    "teardown

  METHOD filter_empty.
    DATA ls_sat TYPE ts_sat.
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( ls_sat )
                                      msg = 'LCL_FILTER ACCEPTS( ) Empty - Null Filter Error' ).
  ENDMETHOD.                    "filter_empty

  METHOD filter_valid.
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( ltc_actors=>sat_fixture_record( ) )
                                      msg = 'LCL_FILTER ACCEPTS( ) Valid - Null Filter Error' ).
  ENDMETHOD.                    "filter_valid

ENDCLASS.                    "ltc_filter_null IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_filter_custom DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_filter_custom DEFINITION FOR TESTING
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mi_filter TYPE REF TO lif_trace_filter.

    METHODS setup.
    METHODS teardown.
    METHODS sample_sat RETURNING value(rs_sat) TYPE ts_sat.

    METHODS filter_empty FOR TESTING.
    METHODS filter_valid FOR TESTING.
    METHODS accepts FOR TESTING.
    METHODS system_events FOR TESTING.
    METHODS skip_constructor FOR TESTING.
ENDCLASS.                    "ltc_filter_custom DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_filter_custom IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_filter_custom IMPLEMENTATION.

  METHOD setup.
    DATA ls_cfg TYPE ts_diagram_config.

    APPEND 'Y*' TO ls_cfg-pattern.
    mi_filter = lcl_filter=>new( ls_cfg ).
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mi_filter.
  ENDMETHOD.                    "teardown

  METHOD filter_empty.
    DATA ls_sat TYPE ts_sat.

    cl_abap_unit_assert=>assert_false( act = mi_filter->accepts( ls_sat )
                                       msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Empty - Error' ).
  ENDMETHOD.                    "filter_empty

  METHOD filter_valid.
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( ltc_actors=>sat_fixture_record( ) )
                                      msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Valid - Error' ).
  ENDMETHOD.                    "filter_valid

  METHOD sample_sat.
    CLEAR rs_sat.
    rs_sat-id = 'L'.
    rs_sat-from_level = 8.
    rs_sat-aus_tabix = 21.
    rs_sat-caller-global = 'SAPMSSYD'.
    rs_sat-caller-type = 'PROG'.
    rs_sat-caller-mod = '%_IMODE_INIT'.
  ENDMETHOD.                    "sample_sat

  METHOD accepts.
    cl_abap_unit_assert=>assert_false( act = mi_filter->accepts( sample_sat( ) )
                                       msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Custom - Filter Error' ).
  ENDMETHOD.                    "accepts

  METHOD system_events.
    DATA ls_cfg TYPE ts_diagram_config.

    ls_cfg-system_events = abap_true.
    APPEND 'SAPM*' TO ls_cfg-pattern.
    mi_filter = lcl_filter=>new( ls_cfg ).
    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( sample_sat( ) )
                                      msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Custom - <system Event' ).
  ENDMETHOD.                    "system_events

  METHOD skip_constructor.
    DATA ls_sat TYPE ts_sat.
    DATA ls_prev TYPE ts_sat.
    DATA ls_cfg TYPE ts_diagram_config.

    ls_cfg-system_events = abap_true.
    APPEND 'SAPM*' TO ls_cfg-pattern.
    mi_filter = lcl_filter=>new( ls_cfg ).

    ls_sat = sample_sat( ).
    ls_prev = ls_sat.
    ls_prev-caller-mod = lcl_abap_trace=>c_create_method.
    mi_filter->accepts( ls_prev ).

    cl_abap_unit_assert=>assert_true( act = mi_filter->accepts( ls_sat )
                                      msg = 'LCL_FILTER_CUSTOM ACCEPTS( ) Custom - Filter Error' ).
  ENDMETHOD.                    "skip_constructor

ENDCLASS.                    "ltc_filter_custom IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltc_configuration DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_configuration DEFINITION FOR TESTING
  RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES lif_unit_test.
  PRIVATE SECTION.
    DATA mo_config TYPE REF TO lcl_configuration.

    METHODS setup.
    METHODS teardown.
    METHODS get_result RETURNING value(rv_result) TYPE char3.

    METHODS radiobutton_url FOR TESTING.
    METHODS radiobutton_txt FOR TESTING.
    METHODS radiobutton_exe FOR TESTING.
    METHODS from_radiobutton_txt FOR TESTING.
ENDCLASS.                    "ltc_configuration DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_configuration IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_configuration IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_config.
  ENDMETHOD.                    "setup

  METHOD teardown.
    FREE mo_config.
  ENDMETHOD.                    "teardown

  METHOD get_result.
    rv_result+0(1) = mo_config->mv_mode_url.
    rv_result+1(1) = mo_config->mv_mode_txt.
    rv_result+2(1) = mo_config->mv_mode_exe.
  ENDMETHOD.                    "get_result

  METHOD radiobutton_url.
    mo_config->gs_cfg-output_mode = lcl_diagram_text=>c_mode_url.
    mo_config->to_radiobutton( ).
    cl_abap_unit_assert=>assert_equals( act = get_result( )
                                        exp = 'X  '
                                        msg = 'LCL_CONFIGURATION->TO_RADIOBUTTON( ) - URL Error' ).
  ENDMETHOD.                    "radiobutton_url

  METHOD radiobutton_exe.
    mo_config->gs_cfg-output_mode = lcl_diagram_text=>c_mode_exe.
    mo_config->to_radiobutton( ).
    cl_abap_unit_assert=>assert_equals( act = get_result( )
                                        exp = '  X'
                                        msg = 'LCL_CONFIGURATION->TO_RADIOBUTTON( ) - EXE Error' ).
  ENDMETHOD.                    "radiobutton_exe

  METHOD radiobutton_txt.
    mo_config->gs_cfg-output_mode = lcl_diagram_text=>c_mode_txt.
    mo_config->to_radiobutton( ).
    cl_abap_unit_assert=>assert_equals( act = get_result( )
                                        exp = ' X '
                                        msg = 'LCL_CONFIGURATION->TO_RADIOBUTTON( ) - TXT Error' ).
  ENDMETHOD.                    "radiobutton_txt

  METHOD from_radiobutton_txt.
    mo_config->gs_cfg-output_mode = lcl_diagram_text=>c_mode_txt.
    mo_config->to_radiobutton( ).
    mo_config->from_radiobutton( ).
    cl_abap_unit_assert=>assert_equals( act = mo_config->gs_cfg-output_mode
                                        exp = lcl_diagram_text=>c_mode_txt
                                        msg = 'LCL_CONFIGURATION->FROM_RADIOBUTTON( ) - TXT Error' ).
  ENDMETHOD.                    "from_radiobutton_txt

ENDCLASS.                    "ltc_configuration IMPLEMENTATION

*-------------------------------------------------------------------------------------------------*
