*&---------------------------------------------------------------------*
"! UML Class PlantUML exporter (inspired by program UML_CLASS_DIAGRAM)
"! Installation of JNET is not required.
REPORT zz_uml_class_export.

*NAME     TEXT
*SO_DEVC  Package
*SO_ROOT  Object Name
*X_AGGR    Aggregations (<>--)
*X_ASOS    Associations (-->)
*X_ATTR    List Attributes
*X_CONS    Display Constants
*X_EXCEP  Exceptions (<<throws>>)
*X_FRND   Friends
*X_FUGR    Analyze Function Groups
*X_LOCL    Analyze Local Objects
*X_METH    List Methods
*X_PACKS  Package Member (~)
*X_PRIV    Private Member  (-)
*X_PROG    Analyze Program
*X_PROT    Protected Member (#)
*X_STRUCT  Structures  (<<data Type>>)
*X_USES    Dependency (<<uses>>)

*SYMBOL  TEXT
*007  Primary Selection Set
*008  Statements UML Scanner
*009  UML Representation Options
*SC1  Selection of Dev. Objects to Be Evaluated
*SC2  Options for Display of UML Class Diagrams
*SC7  Work Instructions for UML Scanner
*P01  PlantUML

TABLES sscrfields.                          " Selection Screen Fields

SELECTION-SCREEN FUNCTION KEY 1.                     "#EC CI_USE_WANTED
SELECTION-SCREEN FUNCTION KEY 2.                     "#EC CI_USE_WANTED

DATA gs_tadir TYPE tadir.
" ----------------------------------------------------------------------------------------------------------------------------------- *
" selection screen definition
SELECTION-SCREEN: BEGIN OF TABBED BLOCK mytab FOR 18 LINES,
                    TAB (40) button1 USER-COMMAND push1 DEFAULT SCREEN 510,
                    TAB (40) button2 USER-COMMAND push2 DEFAULT SCREEN 520,
                    TAB (40) button3 USER-COMMAND push3 DEFAULT SCREEN 530,
                  END OF BLOCK mytab.

" primary selection criteria
SELECTION-SCREEN BEGIN OF SCREEN 510 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE text-sc1.
SELECT-OPTIONS: so_root  FOR gs_tadir-obj_name,  " root objects
                so_devc  FOR gs_tadir-devclass.  " root objects dev-class
SELECTION-SCREEN END OF BLOCK s1.
SELECTION-SCREEN END OF SCREEN 510.

" UML scanner parameters
SELECTION-SCREEN BEGIN OF SCREEN 520 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-sc7.
PARAMETERS: x_locl   TYPE flag AS CHECKBOX DEFAULT 'X',
            x_prog   TYPE flag AS CHECKBOX DEFAULT 'X',
            x_fugr   TYPE flag AS CHECKBOX DEFAULT 'X',
            x_struct TYPE flag AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b0.
SELECTION-SCREEN END OF SCREEN 520.

" diagram option parameters
SELECTION-SCREEN BEGIN OF SCREEN 530 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-sc2.
PARAMETERS: x_priv  TYPE flag AS CHECKBOX DEFAULT 'X',
            x_prot  TYPE flag AS CHECKBOX DEFAULT 'X',
            x_packs TYPE flag AS CHECKBOX DEFAULT 'X',
            x_attr  TYPE flag AS CHECKBOX DEFAULT 'X',
            x_cons  TYPE flag AS CHECKBOX DEFAULT 'X',
            x_meth  TYPE flag AS CHECKBOX DEFAULT 'X',
            x_uses  TYPE flag AS CHECKBOX DEFAULT ' ',
            x_frnd  TYPE flag AS CHECKBOX DEFAULT 'X',
            x_excep TYPE flag AS CHECKBOX DEFAULT ' ',
            x_asos  TYPE flag AS CHECKBOX DEFAULT ' ',
            x_aggr  TYPE flag AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 530.

" ----------------------------------------------------------------------------------------------------------------------------------- *
" types/classes/interfaces

TYPES:  BEGIN OF ts_uml_config,
          show_aggregations TYPE flag, " show aggregations
          show_associations TYPE flag, " show associations
          show_uses         TYPE flag, " <<call>> show dependencies
          show_friends      TYPE flag, " show friends
          show_throws       TYPE flag, " show used exceptions
        END OF ts_uml_config.

TYPES:  BEGIN OF ts_scan_config,
          attributes TYPE flag,
          methods TYPE flag,
          constants TYPE flag,
          private_member TYPE flag,
          protected_member TYPE flag,
          packaged_member TYPE flag,

          scan_local_types TYPE flag,
          scan_programs TYPE flag,
          scan_function_groups TYPE flag,
          add_structures TYPE flag,
          scan_used_types TYPE flag,
        END OF ts_scan_config.


TYPES tv_scale TYPE perct.
CONSTANTS c_default_scale TYPE tv_scale VALUE '0.6'.
TYPES: BEGIN OF ts_diagram_config,
         local_path        TYPE string,
         java_jar          TYPE string,
         java_appl         TYPE string,
         server_url        TYPE string,
         output_mode       TYPE char01,
         skip_dialog       TYPE flag,
         scale             TYPE tv_scale,
         shadowing      TYPE flag,
         display_source TYPE flag,
         hpages         TYPE sytabix,
         vpages         TYPE sytabix,
         handwritten       TYPE flag,
       END OF ts_diagram_config.

*----------------------------------------------------------------------*
*       INTERFACE lif_unit_test
*----------------------------------------------------------------------*
INTERFACE lif_unit_test.
ENDINTERFACE.                    "lif_unit_test

"! Parameter Interface
*----------------------------------------------------------------------*
*       INTERFACE lif_parameter
*----------------------------------------------------------------------*
INTERFACE lif_parameter.
  METHODS get_display_config RETURNING value(rs_cfg) TYPE ts_uml_config.
  METHODS get_scan_config RETURNING value(rs_scan) TYPE ts_scan_config.
  METHODS get_scan_ranges CHANGING ct_so_packages TYPE cl_uml_class_scanner=>uml_range_packages
                                   ct_so_objects  TYPE cl_uml_class_scanner=>uml_range_types.
ENDINTERFACE.                    "lif_parameter

"! Parameter Class
*----------------------------------------------------------------------*
*       CLASS lcl_parameter DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_parameter DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_parameter.

    "! constructor
    "! @parameter iv_progname | program name
    METHODS constructor IMPORTING iv_progname TYPE progname.

  PRIVATE SECTION.
    TYPES tt_options TYPE RANGE OF text30.
    "! selection screen parameters
    DATA mt_parameters TYPE SORTED TABLE OF rsparamsl_255 WITH NON-UNIQUE KEY selname.

    METHODS get_boolean IMPORTING iv_parameter_name TYPE selname
                        RETURNING value(rv_value)   TYPE flag
                        RAISING   cx_dynamic_check.

    METHODS get_user_input IMPORTING iv_progname TYPE raldb_repo.

    "! get select option table parameter
    "! @parameter rt_so_value | selection option values
    METHODS get_parameter_so_tab IMPORTING iv_parameter_name  TYPE selname
                                 RETURNING value(rt_so_value) TYPE tt_options.
ENDCLASS.                    "lcl_parameter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_scanner DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_scanner DEFINITION INHERITING FROM cl_uml_class_scanner.
  PUBLIC SECTION.
    METHODS constructor IMPORTING ii_parameter TYPE REF TO lif_parameter.
    METHODS class_scan RETURNING value(rv_objects_selected) TYPE flag.
  PRIVATE SECTION.
    DATA mi_parameter TYPE REF TO lif_parameter.
ENDCLASS.                    "lcl_scanner DEFINITION

* Abstract UML code generator
CLASS lcl_uml DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING is_cfg TYPE ts_diagram_config
                      RETURNING value(ro_uml) TYPE REF TO lcl_uml.

    METHODS add IMPORTING iv_code TYPE string.
    METHODS get RETURNING value(rv_diagram) TYPE string.
  PROTECTED SECTION.
    DATA mv_diagram TYPE string.
    METHODS header IMPORTING is_cfg TYPE ts_diagram_config.
    METHODS footer.
ENDCLASS.                    "lcl_uml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_iterator DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_iterator DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
*   Convert and filter the trace for further processing
    TYPES ts_uml TYPE cl_uml_class_scanner=>uml_line.
    TYPES tt_uml TYPE STANDARD TABLE OF ts_uml WITH KEY name.

    METHODS constructor IMPORTING it_data  TYPE tt_uml
                                  iv_start TYPE sytabix DEFAULT 1
                                  iv_stop  TYPE sytabix OPTIONAL.
    METHODS next RETURNING value(rs_uml) TYPE ts_uml
                 RAISING   cx_dynamic_check.
    METHODS has_next RETURNING value(rv_flag) TYPE xsdboolean.
    METHODS skip IMPORTING iv_count TYPE i DEFAULT 1.
  PROTECTED SECTION.
    DATA mv_idx TYPE sytabix.
    DATA mv_size TYPE sytabix.
    DATA mt_data TYPE tt_uml.
ENDCLASS.                    "lcl_iterator DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_class DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_uml_class DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES ts_uml TYPE lcl_iterator=>ts_uml.

    METHODS constructor IMPORTING io_uml        TYPE REF TO lcl_uml
                                  is_uml_config TYPE ts_uml_config
                                  is_scan_config TYPE ts_scan_config.
    METHODS to_uml_text IMPORTING is_uml TYPE ts_uml.
  PRIVATE SECTION.
    METHODS plant_uml_class.

    METHODS uml_fields.
    METHODS uml_methods.
    METHODS uml_events.

    METHODS uml_add
      IMPORTING iv_abstract   TYPE xsdboolean DEFAULT abap_false
                iv_visibility TYPE char01
                iv_class      TYPE xsdboolean
                iv_name       TYPE csequence.
    METHODS uml_reduce IMPORTING it_data   TYPE STANDARD TABLE
                                 iv_sep    TYPE char3
                                 iv_suffix TYPE csequence OPTIONAL
                                 iv_active TYPE xsdboolean DEFAULT abap_true.
    METHODS begin_class RETURNING VALUE(rv_flag) TYPE flag.
    METHODS end_class.

    METHODS class_member.
    METHODS visibility IMPORTING iv_visibility TYPE char01.

    METHODS set_data IMPORTING is_uml TYPE ts_uml.

    METHODS get_class RETURNING value(rv_class) TYPE string.

    METHODS get_name IMPORTING iv_name        TYPE csequence
                     RETURNING value(rv_name) TYPE string.
    METHODS get_container RETURNING value(rv_name) TYPE string.

    METHODS escape IMPORTING iv_name TYPE csequence RETURNING value(rv_name) TYPE string.

    DATA mo_uml TYPE REF TO lcl_uml.
    DATA ms_uml TYPE ts_uml.
    DATA mv_name TYPE string.
    DATA ms_config TYPE ts_uml_config.
    DATA ms_scan_cfg TYPE ts_scan_config.
ENDCLASS.                    "lcl_uml_class DEFINITION

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
             title  TYPE string,
             name   TYPE string,
             ext    TYPE string,
             path   TYPE string,
             filter TYPE string,
           END OF ts_fullpath.
    DATA ms_file TYPE ts_fullpath.
ENDCLASS.                    "lcl_file_name DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_xmi TYPE char01 VALUE 'X',
      c_mode_txt TYPE char01 VALUE space,
      c_mode_png TYPE char01 VALUE 'P'.

    CLASS-METHODS download
      IMPORTING iv_data         TYPE xstring
                io_name         TYPE REF TO lcl_file_name
      RETURNING value(rv_subrc) TYPE sysubrc.
ENDCLASS.                    "lcl_file DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file_name_dummy DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name_dummy DEFINITION INHERITING FROM lcl_file_name FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS dialog REDEFINITION.
ENDCLASS.                    "lcl_file_name_dummy DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_plant_uml DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_plant_uml DEFINITION.
  PUBLIC SECTION.
    CONSTANTS c_plantuml_server TYPE string
      VALUE 'http://www.plantuml.com/plantuml/img/'  ##no_text.

    METHODS constructor IMPORTING iv_diagram TYPE string.
    METHODS to_url IMPORTING iv_base_url   TYPE string DEFAULT c_plantuml_server
                   RETURNING value(rv_url) TYPE string
                   RAISING   cx_dynamic_check.
    METHODS output IMPORTING is_cfg TYPE ts_diagram_config RAISING cx_dynamic_check.
  PROTECTED SECTION.
    TYPES tv_base64 TYPE c LENGTH 65.
    CONSTANTS:
      c_standard TYPE tv_base64 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='  ##no_text,
      c_plantuml TYPE tv_base64 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_0' ##no_text.
    DATA mv_diagram TYPE string.

    METHODS to_xstring IMPORTING iv_string         TYPE string
                       RETURNING value(rv_xstring) TYPE xstring
                       RAISING   cx_dynamic_check.
    METHODS source IMPORTING iv_display_source TYPE flag
                   RETURNING value(rv_source) TYPE string.

    METHODS png_file_name IMPORTING io_name  TYPE REF TO lcl_file_name
                                    is_cfg   TYPE ts_diagram_config
                          RETURNING value(rv_name) TYPE string.

    METHODS parameter_string IMPORTING io_name         TYPE REF TO lcl_file_name
                                       is_cfg TYPE ts_diagram_config
                             RETURNING value(rv_param) TYPE string.
    METHODS show_html IMPORTING iv_html TYPE string
                                iv_size TYPE string DEFAULT cl_abap_browser=>xlarge
                      RAISING   cx_dynamic_check.
    METHODS to_png IMPORTING io_name        TYPE REF TO lcl_file_name
                             is_cfg TYPE ts_diagram_config
                   RETURNING value(rv_name) TYPE string.

ENDCLASS.                    "lcl_plant_uml DEFINITION

"! Output interface
*----------------------------------------------------------------------*
*       INTERFACE lif_output DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_output.
  "! export data
  "! @parameter io_uml_class_scanner | UML class scanner instance
  METHODS output IMPORTING ii_parameter TYPE REF TO lif_parameter
                 RETURNING value(rv_flag) TYPE flag.
ENDINTERFACE.                    "lif_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xmi_output DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xmi_output DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_output.
  PRIVATE SECTION.
    CLASS-METHODS new_xmi IMPORTING io_scanner   TYPE REF TO cl_uml_class_scanner
                                    ii_parameter TYPE REF TO lif_parameter
                          RETURNING value(ro_xmi) TYPE REF TO cl_uml_class_decor_xmi.
ENDCLASS.                    "lcl_xmi_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_plantuml_output DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_plantuml_output DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_output.
ENDCLASS.                    "lcl_plantuml_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_null_output DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_null_output DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_output.
ENDCLASS.                    "lcl_xmi_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_class_diagram DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_class_diagram DEFINITION INHERITING FROM lcl_iterator
  CREATE PROTECTED FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS generate
      IMPORTING it_uml            TYPE tt_uml
                is_output_config  TYPE ts_diagram_config
                is_uml_config     TYPE ts_uml_config
                is_scan_config    TYPE ts_scan_config
      RETURNING value(rv_diagram) TYPE string.
  PRIVATE SECTION.
    METHODS to_uml_text IMPORTING is_uml_config TYPE ts_uml_config
                                  is_output_config TYPE ts_diagram_config
                                  is_scan_config TYPE ts_scan_config
                        RETURNING value(rv_diagram) TYPE string.
ENDCLASS.                    "lcl_class_diagram DEFINITION

"! export - app
*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS init.
    CLASS-METHODS user_command IMPORTING iv_ucomm       TYPE syucomm
                               RETURNING value(rv_flag) TYPE flag.
ENDCLASS.                    "lcl_app DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_configuration DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_configuration DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_aut TYPE char01 VALUE 'T',  " for ABAP Unit Test
      c_mode_url TYPE char01 VALUE 'U',
      c_mode_txt TYPE char01 VALUE space,
      c_mode_xmi TYPE char01 VALUE 'X',
      c_mode_exe TYPE char01 VALUE 'E'.

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
             scale             TYPE perct,
             handwritten       TYPE flag,
             shadowing      TYPE flag,
             display_source TYPE flag,
             hpages         TYPE i,
             vpages         TYPE i,
           END OF ts_param.
    METHODS get_attributes RETURNING value(rt_attr) TYPE sci_atttab.
    METHODS to_radiobutton.
    METHODS from_radiobutton.
    CLASS-DATA gs_cfg TYPE ts_param.
    DATA: mv_mode_url TYPE flag VALUE 'X',
          mv_mode_exe TYPE flag,
          mv_mode_txt TYPE flag.
    METHODS dialog.
    CLASS-METHODS get_java_path RETURNING value(rv_fullpath) TYPE string.
ENDCLASS.                    "lcl_configuration DEFINITION

*--------------------------------------------------------------------------------------------------------*

CLASS lcl_parameter IMPLEMENTATION.

  METHOD constructor.
    get_user_input( iv_progname ).
  ENDMETHOD.                    "constructor

  METHOD get_user_input.
    DATA lt_parameters TYPE TABLE OF rsparams ##needed.
    DATA lt_parameters_255 TYPE TABLE OF rsparamsl_255.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report         = iv_progname
      TABLES
        selection_table     = lt_parameters
        selection_table_255 = lt_parameters_255
      EXCEPTIONS
        not_found           = 1
        no_report           = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      CLEAR mt_parameters.
    ELSE.
      INSERT LINES OF lt_parameters_255 INTO TABLE mt_parameters.
    ENDIF.
  ENDMETHOD.                    "get_user_input

  METHOD lif_parameter~get_scan_config.
    rs_scan-attributes = get_boolean( 'X_ATTR' ).
    rs_scan-methods = get_boolean( 'X_METH' ).
    rs_scan-constants = get_boolean( 'X_CONS' ).
    rs_scan-private_member = get_boolean( 'X_PRIV' ).
    rs_scan-protected_member = get_boolean( 'X_PROT' ).
    rs_scan-packaged_member = get_boolean( 'X_PACKS' ).

    rs_scan-scan_local_types = get_boolean( 'X_LOCL' ).
    rs_scan-scan_programs = get_boolean( 'X_PROG' ).
    rs_scan-scan_function_groups = get_boolean( 'X_FUGR' ).
    rs_scan-add_structures = get_boolean( 'X_STRUCT' ).
    rs_scan-scan_used_types = get_boolean( 'X_USES' ).
  ENDMETHOD.                    "lif_parameter~get_scan_config

  METHOD lif_parameter~get_display_config.
    rs_cfg-show_aggregations = get_boolean( 'X_AGGR' ).
    rs_cfg-show_associations = get_boolean( 'X_ASOS' ).
    rs_cfg-show_uses         = get_boolean( 'X_USES' ).  " Display Dependencies
    rs_cfg-show_friends      = get_boolean( 'X_FRND' ).  " Display Friend Relationship
    rs_cfg-show_throws       = get_boolean( 'X_EXCEP' ).
  ENDMETHOD.                    "lif_parameter~get_config

  METHOD get_boolean.
    DATA ls_param LIKE LINE OF mt_parameters.

    READ TABLE mt_parameters INTO ls_param WITH KEY selname = iv_parameter_name.
    IF sy-subrc EQ 0.
      rv_value = ls_param-low.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_itab_error.
    ENDIF.
  ENDMETHOD.                    "get_boolean

  METHOD get_parameter_so_tab.
    DATA ls_param LIKE LINE OF mt_parameters.
    DATA ls_val LIKE LINE OF rt_so_value.

    LOOP AT mt_parameters INTO ls_param WHERE selname EQ iv_parameter_name AND NOT ( low IS INITIAL AND high IS INITIAL ).
      MOVE-CORRESPONDING ls_param TO ls_val.
      INSERT ls_val INTO TABLE rt_so_value.
    ENDLOOP.
  ENDMETHOD.                    "get_parameter_so_tab

  METHOD lif_parameter~get_scan_ranges.
    DATA ls_so_packages LIKE LINE OF ct_so_packages.

    ct_so_objects = get_parameter_so_tab( 'SO_ROOT' ).     " others
    ct_so_packages = get_parameter_so_tab( 'SO_DEVC' ).    " packages

    CHECK ct_so_packages IS INITIAL
      AND ct_so_objects IS NOT INITIAL.

    ls_so_packages-sign = 'I'.
    ls_so_packages-option = 'EQ'.

    SELECT DISTINCT devclass FROM tadir INTO ls_so_packages-low
      WHERE obj_name IN ct_so_objects.
      APPEND ls_so_packages TO ct_so_packages.
    ENDSELECT.
    CHECK sy-subrc NE 0.

    CLEAR ct_so_objects.
  ENDMETHOD.                    "lif_parameter~get_scan_ranges

ENDCLASS.                    "lcl_parameter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_scanner IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_scanner IMPLEMENTATION.

  METHOD constructor.
    DATA ls_scan TYPE ts_scan_config.
    super->constructor( ).
    mi_parameter = ii_parameter.
    ls_scan = mi_parameter->get_scan_config( ).
    set_scanner_configuration( scan_local_types = ls_scan-scan_local_types
                               scan_programs = ls_scan-scan_programs
                               scan_function_groups = ls_scan-scan_function_groups
                               add_structures = ls_scan-add_structures
                               scan_used_types = ls_scan-scan_used_types ).
  ENDMETHOD.                    "constructor

  METHOD class_scan.
    DATA lt_packages TYPE uml_range_packages.
    DATA lt_objects TYPE uml_range_types.

    rv_objects_selected = abap_false.
    cl_uml_cache=>get_singleton( )->clear_type_cache( ).

    mi_parameter->get_scan_ranges( CHANGING ct_so_packages = lt_packages
                                            ct_so_objects = lt_objects ).
    CHECK NOT ( lt_packages IS INITIAL AND lt_objects IS INITIAL ).
    rv_objects_selected = abap_true.

    execute( scan_packages = lt_packages
             scan_types = lt_objects ).
  ENDMETHOD.                    "lif_parameter~uml_class_scan

ENDCLASS.                    "lcl_scanner IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file IMPLEMENTATION
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
*       CLASS lcl_file_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name IMPLEMENTATION.

  METHOD new.
    CASE iv_mode.
      WHEN lcl_configuration=>c_mode_aut.
        CREATE OBJECT ro_file TYPE lcl_file_name_dummy
          EXPORTING
            iv_mode = iv_mode.
      WHEN OTHERS.
        CREATE OBJECT ro_file TYPE lcl_file_name
          EXPORTING
            iv_mode = iv_mode.
    ENDCASE.
  ENDMETHOD.                    "new

  METHOD constructor.
    CLEAR ms_file.
    CASE iv_mode.
      WHEN lcl_configuration=>c_mode_txt.
        ms_file-title = |Save UML text source|.
        ms_file-ext = |.txt|.
      WHEN lcl_configuration=>c_mode_xmi.
        ms_file-title    = |Export XMI file|.
        ms_file-ext      = |.xmi|.
        ms_file-filter   = |(*.xmi)\|*.xmi|.
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
    DATA lv_path TYPE string ##needed.

    CLEAR rv_user_action.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title      = ms_file-title           " Window Title
        default_extension = ms_file-ext             " Default Extension
        file_filter       = ms_file-filter
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
*       CLASS lcl_file_name_dummy IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_name_dummy IMPLEMENTATION.

  METHOD dialog.
    ms_file-path = |test.txt|.
    rv_user_action = cl_gui_frontend_services=>action_cancel.
  ENDMETHOD.                    "dialog

ENDCLASS.                    "lcl_file_name_dummy IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_plant_uml IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_plant_uml IMPLEMENTATION.

  METHOD constructor.
    mv_diagram = iv_diagram.
  ENDMETHOD.                    "constructor

  METHOD source.
    CLEAR rv_source.
    CHECK iv_display_source EQ abap_true.
    rv_source = |<p>{ mv_diagram }</p>|.
  ENDMETHOD.                    "source

  METHOD show_html.
    cl_abap_browser=>show_html( html_string = iv_html
                                size = iv_size
                                context_menu = abap_true ).
  ENDMETHOD.                    "show_html

  METHOD output.
    DATA lo_name TYPE REF TO lcl_file_name.

    CASE is_cfg-output_mode.
      WHEN lcl_configuration=>c_mode_url.
        show_html( |<img src="{ to_url( ) }"/>\n{ source( is_cfg-display_source ) }| ).

      WHEN lcl_configuration=>c_mode_exe.
        lo_name = lcl_file_name=>new( lcl_file=>c_mode_txt ).
        IF lcl_file=>download( iv_data = to_xstring( mv_diagram )
                               io_name = lo_name ) IS INITIAL.
          show_html( |<img src="{ to_png( io_name = lo_name
                                          is_cfg = is_cfg ) }"/>\n{ source( is_cfg-display_source ) }| ).
        ENDIF.

      WHEN OTHERS.
*       export data as PlantUML source
        lcl_file=>download( io_name = lcl_file_name=>new( is_cfg-output_mode )
                            iv_data = to_xstring( mv_diagram ) ).
    ENDCASE.
  ENDMETHOD.                    "output

  METHOD to_url.
    DATA lv_bin TYPE xstring.
*   for PlantUML Server: Convert to UTF-8, then deflate, then encode (base64 variant)
    cl_abap_gzip=>compress_binary(
      EXPORTING
        raw_in         = to_xstring( mv_diagram )   " UTF-8
        compress_level = 9
      IMPORTING
        gzip_out       = lv_bin ).

    rv_url = iv_base_url &&
             translate( val = cl_http_utility=>encode_x_base64( lv_bin )
                        from = c_standard
                        to =   c_plantuml ).
  ENDMETHOD.                    "to_url

  METHOD to_xstring.
    DATA lo_conv TYPE REF TO cl_abap_conv_out_ce.

    lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    lo_conv->convert( EXPORTING data = iv_string
                      IMPORTING buffer = rv_xstring ).
  ENDMETHOD.                    "to_xstring

  METHOD parameter_string.
    rv_param = |-jar { is_cfg-java_jar } -o { is_cfg-local_path } "{ io_name->get_fullpath( ) }"|.
  ENDMETHOD.                    "parameter_string

  METHOD png_file_name.
    TRY.
        rv_name = |{ is_cfg-local_path }{ io_name->get_prefix( ) }.png|.
      CATCH cx_dynamic_check.
        CLEAR rv_name.
    ENDTRY.
  ENDMETHOD.                    "png_file_name

  METHOD to_png.
    CLEAR rv_name.
    cl_gui_frontend_services=>execute(
      EXPORTING application = is_cfg-java_appl
                parameter = parameter_string( io_name = io_name
                                              is_cfg = is_cfg )
                synchronous = 'X'
      EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc EQ 0.
      rv_name = png_file_name( io_name = io_name
                               is_cfg = is_cfg ).
    ENDIF.
  ENDMETHOD.                    "to_png

ENDCLASS.                    "lcl_plant_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_class IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_uml_class IMPLEMENTATION.

  METHOD escape.
    IF iv_name CA '/'.
      rv_name = |"{ iv_name }"|.
    ELSE.
      rv_name = iv_name.
    ENDIF.
  ENDMETHOD.                    "escape

  METHOD constructor.
    super->constructor( ).
    mo_uml = io_uml.
    ms_config = is_uml_config.
    ms_scan_cfg = is_scan_config.
  ENDMETHOD.                    "constructor

  METHOD to_uml_text.
    CHECK is_uml-kind CA 'OFP'.
    set_data( is_uml ).
    plant_uml_class( ).
  ENDMETHOD.                    "to_uml_text

  METHOD set_data.
    ms_uml = is_uml.
    mv_name = get_name( is_uml-name ).
  ENDMETHOD.                    "set_data

  METHOD begin_class.
    DATA lv_name TYPE string.

    rv_flag = abap_false.
    lv_name = get_class( ).
    CHECK lv_name IS NOT INITIAL.
    mo_uml->add( |{ lv_name } \{\n| ).
    rv_flag = abap_true.
  ENDMETHOD.                    "begin_class

  METHOD end_class.
    mo_uml->add( |\}\n| ).
  ENDMETHOD.                    "end_class

  METHOD class_member.
    mo_uml->add( |\{static\}| ).
  ENDMETHOD.                    "class_member

  METHOD plant_uml_class.
    CHECK mv_name IS NOT INITIAL AND begin_class( ) EQ abap_true.
    uml_fields( ).
    uml_methods( ).
    uml_events( ).
    end_class( ).

    IF ms_uml-supertype IS NOT INITIAL.
      mo_uml->add( |{ escape( get_name( ms_uml-supertype ) ) } <\|-- { escape( mv_name ) }\n| ).
    ENDIF.

    uml_reduce( it_data = ms_uml-t_implementations
                iv_sep = '-->' ).
    uml_reduce( it_data = ms_uml-t_user
                iv_sep = '..o'
                iv_active = ms_config-show_uses ).
    uml_reduce( it_data = ms_uml-t_exceptions
                iv_sep = '..'
                iv_active = ms_config-show_throws ).
    uml_reduce( it_data = ms_uml-t_friends
                iv_sep = '..>'
                iv_suffix = | : friend |
                iv_active = ms_config-show_friends ).
  ENDMETHOD.                    "plant_uml_class

  METHOD get_name.
    "    CHECK substring_before( val = iv_name regex = '\\TYPE=' ) EQ space.
    rv_name = substring_after( val = iv_name regex = '\\(CLASS|INTERFACE)=' ).
    CHECK rv_name IS INITIAL.
    rv_name = substring_after( val = iv_name regex = '\\FUGR=' ).
    CHECK rv_name IS NOT INITIAL.
    rv_name = rv_name && | <<FUGR>>| .
  ENDMETHOD.                    "get_name

  METHOD get_container.
    rv_name = substring_after( val = ms_uml-container regex = '\\(CLASS-POOL|PROGRAM|FUNCTION-POOL)=' ).
  ENDMETHOD.                    "get_container

  METHOD get_class.
    DATA lv_prefix TYPE string.
    CLEAR rv_class.
    IF find( val = ms_uml-name sub = '\FUGR=' ) GE 0.
      IF ms_scan_cfg-scan_function_groups EQ abap_true.
        rv_class = |class { mv_name } << (F,#FF7700) FuGr >>|.
      ENDIF.
    ELSE.
    IF find( val = ms_uml-name sub = '\INTERFACE=' ) GE 0.
      lv_prefix = |interface|.
    ELSEIF ms_uml-is_abstract IS NOT INITIAL.
      lv_prefix = |abstract class|.
    ELSE.
      lv_prefix = |class|.
    ENDIF.
    rv_class = |{ lv_prefix } { mv_name }|.
    IF ms_uml-container IS NOT INITIAL AND ms_uml-is_local EQ abap_true.
      rv_class = |{ lv_prefix } "{ mv_name }\\n({ get_container( ) })" as { mv_name }|.
    ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_class

  METHOD visibility.
    CONSTANTS:
      c_vis_package   TYPE char01 VALUE 'I',
      c_vis_private   TYPE char01 VALUE 'P',
      c_vis_protected TYPE char01 VALUE 'O',
      c_vis_public    TYPE char01 VALUE 'U'.

    DATA lv_vis TYPE char01.

    CASE iv_visibility.
      WHEN c_vis_private.   lv_vis = '-'.
      WHEN c_vis_protected. lv_vis = '#'.
      WHEN c_vis_package.   lv_vis = '~'.
      WHEN c_vis_public.    lv_vis = '+'.
    ENDCASE.
    mo_uml->add( |{ lv_vis }| ).
  ENDMETHOD.                    "visibility

  METHOD uml_add.
    IF iv_abstract EQ abap_true.
      mo_uml->add( |\{abstract\}| ).
    ENDIF.
    visibility( iv_visibility ).
    IF iv_class EQ abap_true.
      class_member( ).
    ENDIF.
    mo_uml->add( |{ iv_name }\n| ).
  ENDMETHOD.                    "uml_add

  METHOD uml_reduce.
    FIELD-SYMBOLS <lv_line> TYPE csequence.
    DATA lv_prefix TYPE string.

    CHECK iv_active EQ abap_true.
    LOOP AT it_data ASSIGNING <lv_line>.
      AT FIRST.
        lv_prefix = escape( mv_name ) && ` ` && iv_sep.
      ENDAT.
      mo_uml->add( |{ lv_prefix } { escape( get_name( <lv_line> ) ) }{ iv_suffix }\n| ).
    ENDLOOP.
  ENDMETHOD.                    "uml_reduce

  METHOD uml_events.
    DATA ls_event LIKE LINE OF ms_uml-t_events.
*      mo_uml->add( |{ mv_name } ..o { get_name( ls_event-name ) }\n| ).
    LOOP AT ms_uml-t_events INTO ls_event.
      uml_add( iv_visibility = ls_event-visibility
               iv_class = ls_event-is_static
               iv_name = |{ ls_event-name }( )| ).
    ENDLOOP.
  ENDMETHOD.                    "uml_events

  METHOD uml_fields.
    DATA ls_field LIKE LINE OF ms_uml-t_attributes.
    CHECK ms_scan_cfg-attributes EQ abap_true.
    LOOP AT ms_uml-t_attributes INTO ls_field WHERE is_constant EQ space.
      uml_add( iv_visibility = ls_field-visibility
               iv_class = ls_field-is_class
               iv_name = ls_field-name ).
    ENDLOOP.
  ENDMETHOD.                    "uml_fields

  METHOD uml_methods.
    DATA ls_method LIKE LINE OF ms_uml-t_methods.
    CHECK ms_scan_cfg-methods EQ abap_true.
    LOOP AT ms_uml-t_methods INTO ls_method.
      uml_add( iv_abstract = ls_method-is_abstract
               iv_visibility = ls_method-visibility
               iv_class = ls_method-is_class
               iv_name = |{ ls_method-name }( )| ).
    ENDLOOP.
  ENDMETHOD.                    "uml_methods

ENDCLASS.                    "lcl_uml_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_class_diagram IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_class_diagram IMPLEMENTATION.

  METHOD generate.
    DATA lo_diagram TYPE REF TO lcl_class_diagram.

    CREATE OBJECT lo_diagram
      EXPORTING
        it_data = it_uml.

    rv_diagram = lo_diagram->to_uml_text( is_output_config = is_output_config
                                          is_uml_config = is_uml_config
                                          is_scan_config = is_scan_config ).
  ENDMETHOD.                    "generate

  METHOD to_uml_text.
    DATA lo_uml TYPE REF TO lcl_uml.
    DATA lo_class TYPE REF TO lcl_uml_class.

    lo_uml = lcl_uml=>new( is_output_config ).
    CREATE OBJECT lo_class
      EXPORTING
        io_uml        = lo_uml
        is_uml_config = is_uml_config
        is_scan_config = is_scan_config.
    WHILE has_next( ) EQ abap_true.
      lo_class->to_uml_text( next( ) ).
    ENDWHILE.

    rv_diagram = lo_uml->get( ).
  ENDMETHOD.                    "to_uml_text

ENDCLASS.                    "lcl_class_diagram IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_uml IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_uml.
    ro_uml->header( is_cfg ).
  ENDMETHOD.                    "new

  METHOD add.
    mv_diagram = mv_diagram && iv_code.
  ENDMETHOD.                    "add

  METHOD get.
    footer( ).
    rv_diagram = mv_diagram.
  ENDMETHOD.                    "get

  METHOD header.
    add( |@startuml\n| ).   " Header
    add( |scale { is_cfg-scale }\n| ).   " Reduce the size of the output image
    add( |page { is_cfg-hpages }x{ is_cfg-vpages }\n| ).  " split if n files
  ENDMETHOD.                    "header

  METHOD footer.
    add( |hide <<FUGR>> circle\n| ).
    add( |@enduml\n| ).
  ENDMETHOD.                    "footer

ENDCLASS.                    "lcl_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_iterator IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_iterator IMPLEMENTATION.

  METHOD constructor.
    CLEAR mv_idx.
    mt_data = it_data.    " never changed
    IF iv_stop IS INITIAL.
      mv_size = lines( mt_data ).
    ELSE.
      mv_size = iv_stop.
    ENDIF.
    skip( iv_start - 1 ).
  ENDMETHOD.                    "constructor

  METHOD next.
    skip( ).
    READ TABLE mt_data INDEX mv_idx INTO rs_uml.
    CHECK sy-subrc NE 0.
    RAISE EXCEPTION TYPE cx_sy_itab_error.
  ENDMETHOD.                    "next

  METHOD has_next.
    rv_flag = boolc( mv_idx < mv_size ).
  ENDMETHOD.                    "has_next

  METHOD skip.
    ADD iv_count TO mv_idx.
  ENDMETHOD.                    "skip

ENDCLASS.                    "lcl_iterator IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xmi_output IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_xmi_output IMPLEMENTATION.

  METHOD new_xmi.
    DATA ls_scan TYPE ts_scan_config.
    DATA ls_cfg TYPE ts_uml_config.

    CREATE OBJECT ro_xmi
      EXPORTING
        decorator = io_scanner.

    ls_scan = ii_parameter->get_scan_config( ).
    ro_xmi->set_decorator_configuration( attributes = ls_scan-attributes
                                         methods = ls_scan-methods
                                         constants = ls_scan-constants
                                         private_member = ls_scan-private_member
                                         protected_member = ls_scan-protected_member
                                         packaged_member = ls_scan-packaged_member ).
    ls_cfg = ii_parameter->get_display_config( ).
    ro_xmi->set_xmi_configuration( aggregations    = ls_cfg-show_aggregations
                                   associations    = ls_cfg-show_associations
                                   uses            = ls_cfg-show_uses
                                   friends         = ls_cfg-show_friends
                                   show_exceptions = ls_cfg-show_throws ).
  ENDMETHOD.                    "lif_parameter~new_xmi

  METHOD lif_output~output.
    DATA lr_data TYPE REF TO data.
    DATA lo_scanner TYPE REF TO lcl_scanner.
    DATA lo_xmi TYPE REF TO cl_uml_class_decor_xmi.
    FIELD-SYMBOLS <lv_data> TYPE xstring.

    rv_flag = abap_false.

    CREATE OBJECT lo_scanner
      EXPORTING
        ii_parameter = ii_parameter.

    CHECK lo_scanner->class_scan( ) EQ abap_true.
    lo_xmi = new_xmi( io_scanner = lo_scanner
                      ii_parameter = ii_parameter ).
    lo_xmi->get_diagram( CHANGING c_data = lr_data ).
    ASSIGN lr_data->* TO <lv_data> CASTING.
    CHECK <lv_data> IS ASSIGNED.

    lcl_file=>download( iv_data = <lv_data>
                        io_name = lcl_file_name=>new( lcl_file=>c_mode_xmi ) ).
    rv_flag = abap_true.
  ENDMETHOD.                    "lif_output~output

ENDCLASS.                    "lcl_xmi_output IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_plantuml_output IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_plantuml_output IMPLEMENTATION.

  METHOD lif_output~output.
    DATA ls_cfg TYPE ts_diagram_config.
    DATA lr_data TYPE REF TO data.
    DATA lo_scanner TYPE REF TO lcl_scanner.
    DATA lr_uml_tab TYPE REF TO cl_uml_class_scanner=>uml_tab.
    DATA lo_plant_uml TYPE REF TO lcl_plant_uml.

    rv_flag = abap_false.
    CREATE OBJECT lo_scanner
      EXPORTING
        ii_parameter = ii_parameter.

    CHECK lo_scanner->class_scan( ) EQ abap_true.
    "DATA lv_kind TYPE c VALUE cl_uml_class_scanner=>c_kind_uml_tab.
    lo_scanner->get_diagram( "IMPORTING e_kind = lv_kind
                             CHANGING c_data = lr_data ).
    TRY.
        lr_uml_tab ?= lr_data.
        CHECK lr_uml_tab IS BOUND.
        ls_cfg = lcl_configuration=>get( ).
        CREATE OBJECT lo_plant_uml
          EXPORTING iv_diagram = lcl_class_diagram=>generate( it_uml = lr_uml_tab->*
                                                              is_uml_config = ii_parameter->get_display_config( )
                                                              is_output_config = ls_cfg
                                                              is_scan_config = ii_parameter->get_scan_config( )  ).
        lo_plant_uml->output( ls_cfg ).
        rv_flag = abap_true.
      CATCH cx_dynamic_check.                           "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.                    "lif_output~output

ENDCLASS.                    "lcl_plantuml_output IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_null_output IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_null_output IMPLEMENTATION.

  METHOD lif_output~output.
    rv_flag = abap_false.
  ENDMETHOD.                    "lif_output~output

ENDCLASS.                    "lcl_null_output IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_configuration IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_configuration IMPLEMENTATION.

  METHOD class_constructor.
    gs_cfg-java_appl = get_java_path( ).
*                     PlantUML jar file and output path
    gs_cfg-local_path = `C:\Temp\Dokumente\UML\`.
    gs_cfg-java_jar = `C:\Temp\Dokumente\UML\plantuml.jar`.
*                     PlantUML server URL
    gs_cfg-server_url = `http://www.plantuml.com/plantuml/img/` ##no_text.
    gs_cfg-output_mode = c_mode_url.
    gs_cfg-skip_dialog = space.
    gs_cfg-scale = c_default_scale.
    gs_cfg-handwritten = abap_false.
    gs_cfg-shadowing = abap_false.
    gs_cfg-display_source = abap_true.
    gs_cfg-hpages = 1.
    gs_cfg-vpages = 1.
*   Windows: Local Java installation
    IF gs_cfg-java_appl IS INITIAL.
      gs_cfg-java_appl = `C:\Windows\System32\java`.
    ENDIF.
  ENDMETHOD.                    "class_constructor

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

  METHOD get_attributes.
    DATA attr LIKE LINE OF rt_attr.

    DEFINE fill_att.
      clear attr.
      get reference of &1 into attr-ref.
      attr-text = &2.
      attr-kind = &3.
      insert attr into table rt_attr.
    END-OF-DEFINITION.

    DEFINE fill_radio.
      clear attr.
      get reference of &1 into attr-ref.
      attr-text = &2.
      attr-kind = 'R'.
      attr-button_group = &3.
      insert attr into table rt_attr.
    END-OF-DEFINITION.
* Table Type has type 'T' - patterns SCI_PATTERN
*                     ' ' - ?? private attributes?
*                     'I' - ?? Integer?
    fill_att   gs_cfg-skip_dialog 'Remember my settings'(c00)     'C'.

    fill_att:   sy-index      'PlantUML Execution Mode'(c10) 'G'.   " Group
    fill_radio: mv_mode_url   'PlantUML web service'(c11)  'MOD',
                mv_mode_txt   'Save text file'(c12)        'MOD',
                mv_mode_exe   'Local PlantUML '(c13)       'MOD'.

    fill_att: ''              'PlantUML Settings'(c20)         'G',
              gs_cfg-scale       'Scale '(c21)                 'S',
              gs_cfg-hpages      'H-Pages '(c22)               'S',
              gs_cfg-vpages      'V-Pages '(c23)               'S'.

    fill_att: gs_cfg-server_url  'PlantUML Server'(c25)         'S',
              gs_cfg-local_path  'Local PlantUML path'(c26)     'S',
              gs_cfg-java_jar    'Local PlantUML jar file'(c27) ' ',
              gs_cfg-java_appl   'Local Java path'(c28)         'S'.  " Select-Options
    fill_att: gs_cfg-handwritten       'Handwritten '(c30)           'C',
              gs_cfg-shadowing         'Shadowing '(c31)             'C',
              gs_cfg-display_source    'Display source '(c32)        'C'.
  ENDMETHOD.                    "get_attributes

  METHOD to_radiobutton.
    mv_mode_url = boolc( gs_cfg-output_mode EQ c_mode_url ).
    mv_mode_exe = boolc( gs_cfg-output_mode EQ c_mode_exe ).
    mv_mode_txt = boolc( gs_cfg-output_mode EQ c_mode_txt ).
  ENDMETHOD.                    "to_radiobutton

  METHOD from_radiobutton.
    IF mv_mode_url EQ abap_true.
      gs_cfg-output_mode = c_mode_url.
    ELSEIF mv_mode_exe EQ abap_true.
      gs_cfg-output_mode = c_mode_exe.
    ELSEIF mv_mode_txt EQ abap_true.
      gs_cfg-output_mode = c_mode_txt.
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
    DATA lv_repid TYPE sychar30.
    lv_repid = sy-repid.
    to_radiobutton( ).
    CHECK gs_cfg-skip_dialog EQ abap_false.
    CHECK cl_ci_query_attributes=>generic(
        p_name       = lv_repid                    " unique screen ID
        p_title      = 'Class Diagram Parameters'            " Screen title
        p_attributes = get_attributes( )                     " Screen fields
        p_display    = abap_false                            " Edit / Display only
       ) EQ abap_false.   " Do not cancel
    from_radiobutton( ).
  ENDMETHOD.                    "dialog

ENDCLASS.                    "lcl_configuration IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD init.
    DATA ls_button TYPE smp_dyntxt.
    button1 = text-007.
    button2 = text-008.
    button3 = text-009.

*   pushbutton in the application toolbar
    ls_button-icon_id = '@4Y@'.        " ICON_BUSINAV_ENTITY
    ls_button-icon_text = text-p01.    " PlantUML
    ls_button-quickinfo = text-p01.
    ls_button-text = text-p01.
    sscrfields-functxt_01 = ls_button.

    CLEAR ls_button.
    ls_button-icon_id = '@BX@'.        " ICON_CONFIGURATION
    ls_button-icon_text = text-p02.    " PlantUML Configuration
    ls_button-quickinfo = text-p02.
    ls_button-text = text-p02.
    sscrfields-functxt_02 = ls_button.

  ENDMETHOD.                    "init

  METHOD user_command.
    DATA li_export TYPE REF TO lif_output.
    DATA lo_param TYPE REF TO lcl_parameter.

    CASE iv_ucomm.
      WHEN 'FC01'.
*       PlantUML Output
        CREATE OBJECT li_export TYPE lcl_plantuml_output.
      WHEN 'ONLI'.
        CREATE OBJECT li_export TYPE lcl_xmi_output.
      WHEN 'FC02'.
        lcl_configuration=>query( ).
        rv_flag = abap_true.
        RETURN.
      WHEN OTHERS.
        CREATE OBJECT li_export TYPE lcl_null_output.
    ENDCASE.

    CREATE OBJECT lo_param
      EXPORTING iv_progname = sy-repid.

    rv_flag = li_export->output( lo_param  ).
  ENDMETHOD.                    "execute

ENDCLASS.                    "lcl_app IMPLEMENTATION

" ----------------------------------------------------------------------------------------------------------------------------------- *
" program events

INITIALIZATION.
  lcl_app=>init( ).

AT SELECTION-SCREEN.
  IF lcl_app=>user_command( sscrfields-ucomm ) EQ abap_true.
    CLEAR sscrfields-ucomm.
  ENDIF.

START-OF-SELECTION.
  lcl_app=>user_command( sy-ucomm ).
