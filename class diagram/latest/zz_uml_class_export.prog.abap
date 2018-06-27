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
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE TEXT-sc1.
SELECT-OPTIONS: so_root  FOR gs_tadir-obj_name,  " root objects
                so_devc  FOR gs_tadir-devclass.  " root objects dev-class
SELECTION-SCREEN END OF BLOCK s1.
SELECTION-SCREEN END OF SCREEN 510.

" UML scanner parameters
SELECTION-SCREEN BEGIN OF SCREEN 520 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-sc7.
PARAMETERS: x_locl   TYPE flag AS CHECKBOX DEFAULT 'X',
            x_prog   TYPE flag AS CHECKBOX DEFAULT 'X',
            x_fugr   TYPE flag AS CHECKBOX DEFAULT 'X',
            x_struct TYPE flag AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b0.
SELECTION-SCREEN END OF SCREEN 520.

" diagram option parameters
SELECTION-SCREEN BEGIN OF SCREEN 530 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-sc2.
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

TYPES: BEGIN OF ts_uml_config,
         show_aggregations TYPE flag, " show aggregations
         show_associations TYPE flag, " show associations
         show_uses         TYPE flag, " <<call>> show dependencies
         show_friends      TYPE flag, " show friends
         show_throws       TYPE flag, " show used exceptions
       END OF ts_uml_config.

TYPES: BEGIN OF ts_scan_config,
         attributes           TYPE flag,
         methods              TYPE flag,
         constants            TYPE flag,
         private_member       TYPE flag,
         protected_member     TYPE flag,
         packaged_member      TYPE flag,

         scan_local_types     TYPE flag,
         scan_programs        TYPE flag,
         scan_function_groups TYPE flag,
         add_structures       TYPE flag,
         scan_used_types      TYPE flag,
       END OF ts_scan_config.


TYPES tv_scale TYPE perct.
CONSTANTS c_default_scale TYPE tv_scale VALUE '0.6'.
TYPES: BEGIN OF ts_diagram_config,
         local_path     TYPE string,
         java_jar       TYPE string,
         java_appl      TYPE string,
         server_url     TYPE string,
         output_mode    TYPE char01,
         skip_dialog    TYPE flag,
         scale          TYPE tv_scale,
         shadowing      TYPE flag,
         display_source TYPE flag,
         hpages         TYPE sytabix,
         vpages         TYPE sytabix,
       END OF ts_diagram_config.

*----------------------------------------------------------------------*
*       INTERFACE lif_unit_test
*----------------------------------------------------------------------*
INTERFACE lif_unit_test.
ENDINTERFACE.                    "lif_unit_test

*----------------------------------------------------------------------*
*       CLASS lcl_parameter DEFINITION
*----------------------------------------------------------------------*
"! Parameter Class
CLASS lcl_parameter DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    "! constructor
    "! @parameter iv_progname | program name
    METHODS constructor IMPORTING iv_progname TYPE progname DEFAULT sy-repid.

    METHODS get_display_config RETURNING VALUE(rs_cfg) TYPE ts_uml_config.
    METHODS get_scan_config RETURNING VALUE(rs_scan) TYPE ts_scan_config.
    METHODS get_scan_ranges EXPORTING et_so_packages TYPE cl_uml_class_scanner=>uml_range_packages
                                      et_so_objects  TYPE cl_uml_class_scanner=>uml_range_types.
  PRIVATE SECTION.
    TYPES tt_options TYPE RANGE OF text30.
    "! selection screen parameters
    DATA mt_parameters TYPE SORTED TABLE OF rsparamsl_255 WITH NON-UNIQUE KEY selname.

    METHODS get_boolean IMPORTING iv_parameter_name TYPE selname
                        RETURNING VALUE(rv_value)   TYPE flag
                        RAISING   cx_dynamic_check.

    METHODS get_user_input IMPORTING iv_progname TYPE raldb_repo.

    "! get select option table parameter
    "! @parameter rt_so_value | selection option values
    METHODS get_parameter_so_tab IMPORTING iv_parameter_name  TYPE selname
                                 RETURNING VALUE(rt_so_value) TYPE tt_options.
ENDCLASS.                    "lcl_parameter DEFINITION

CLASS lcl_scanner DEFINITION INHERITING FROM cl_uml_class_scanner FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_parameter TYPE REF TO lcl_parameter.
    METHODS class_scan RETURNING VALUE(rv_objects_selected) TYPE flag.
  PRIVATE SECTION.
    DATA mo_parameter TYPE REF TO lcl_parameter.
ENDCLASS.

* Abstract UML code generator
CLASS lcl_uml DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING is_config TYPE ts_diagram_config
                      RETURNING VALUE(ro_uml) TYPE REF TO lcl_uml.

    METHODS add IMPORTING iv_code TYPE string.
    METHODS get RETURNING VALUE(rv_diagram) TYPE string.
  PROTECTED SECTION.
    DATA mv_diagram TYPE string.
    METHODS header IMPORTING is_cfg TYPE ts_diagram_config.
    METHODS footer.
ENDCLASS.                    "lcl_uml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_uml_class DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_uml_class DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES ts_uml TYPE cl_uml_class_scanner=>uml_line.

    METHODS constructor IMPORTING io_uml       TYPE REF TO lcl_uml
                                  io_parameter TYPE REF TO lcl_parameter.
    METHODS to_uml_text IMPORTING is_uml TYPE ts_uml.
  PRIVATE SECTION.
    METHODS plant_uml_class.

    METHODS uml_fields.
    METHODS uml_methods.
    METHODS uml_events.    " not used yet

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

    METHODS visibility IMPORTING iv_visibility TYPE char01.

    METHODS set_data IMPORTING is_uml TYPE ts_uml.

    METHODS get_class RETURNING VALUE(rv_class) TYPE string.

    METHODS get_name IMPORTING iv_name        TYPE csequence
                     RETURNING VALUE(rv_name) TYPE string.
    METHODS get_container RETURNING VALUE(rv_name) TYPE string.

    METHODS escape IMPORTING iv_name TYPE csequence RETURNING VALUE(rv_name) TYPE string.

    DATA mo_uml TYPE REF TO lcl_uml.
    DATA ms_uml TYPE ts_uml.
    DATA mv_name TYPE string.
    DATA ms_uml_cfg TYPE ts_uml_config.
    DATA ms_scan_cfg TYPE ts_scan_config.
ENDCLASS.                    "lcl_uml_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_class_diagram DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_class_diagram DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
*   Convert and filter the trace for further processing
    TYPES tt_uml TYPE STANDARD TABLE OF cl_uml_class_scanner=>uml_line WITH KEY name.

    CLASS-METHODS generate
      IMPORTING it_uml         TYPE tt_uml
                io_parameter   TYPE REF TO lcl_parameter
                is_diagram_cfg TYPE ts_diagram_config
      RETURNING VALUE(rv_diagram) TYPE string.

  PRIVATE SECTION.
    DATA mo_uml TYPE REF TO lcl_uml.
    DATA mo_class TYPE REF TO lcl_uml_class.

    METHODS constructor IMPORTING is_diagram_cfg TYPE ts_diagram_config
                                  io_parameter TYPE REF TO lcl_parameter.

    METHODS to_uml_text IMPORTING it_uml            TYPE tt_uml
                        RETURNING VALUE(rv_diagram) TYPE string.
ENDCLASS.                    "lcl_class_diagram DEFINITION

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
             title  TYPE string,
             name   TYPE string,
             ext    TYPE string,
             path   TYPE string,
             filter TYPE string,
           END OF ts_fullpath.
    DATA ms_file TYPE ts_fullpath.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_file DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_xmi TYPE char01 VALUE 'X',
      c_mode_txt TYPE char01 VALUE space,
      c_mode_png TYPE char01 VALUE 'P'.

    CLASS-METHODS download
      IMPORTING iv_data         TYPE xstring
                io_name         TYPE REF TO lcl_file_name
      RETURNING VALUE(rv_subrc) TYPE sysubrc.
ENDCLASS.                    "lcl_file DEFINITION

CLASS lcl_file_name_dummy DEFINITION INHERITING FROM lcl_file_name FRIENDS lif_unit_test.
  PUBLIC SECTION.
    METHODS dialog REDEFINITION.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_plant_uml DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_plant_uml DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS c_plantuml_server TYPE string
      VALUE 'http://www.plantuml.com/plantuml/img/'  ##NO_TEXT.

    METHODS constructor IMPORTING iv_diagram TYPE string.
    METHODS to_url IMPORTING iv_base_url   TYPE string DEFAULT c_plantuml_server
                   RETURNING VALUE(rv_url) TYPE string
                   RAISING   cx_dynamic_check.
    METHODS output IMPORTING is_cfg TYPE ts_diagram_config RAISING cx_dynamic_check.
  PROTECTED SECTION.
    TYPES tv_base64 TYPE c LENGTH 65.
    CONSTANTS:
      c_standard TYPE tv_base64 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='  ##NO_TEXT,
      c_plantuml TYPE tv_base64 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_0' ##NO_TEXT.
    DATA mv_diagram TYPE string.

    METHODS to_xstring IMPORTING iv_string         TYPE string
                       RETURNING VALUE(rv_xstring) TYPE xstring
                       RAISING   cx_dynamic_check.
    METHODS source IMPORTING iv_display_source TYPE flag
                   RETURNING VALUE(rv_source)  TYPE string.

    METHODS png_file_name IMPORTING io_name        TYPE REF TO lcl_file_name
                                    is_cfg         TYPE ts_diagram_config
                          RETURNING VALUE(rv_name) TYPE string.

    METHODS parameter_string IMPORTING io_name         TYPE REF TO lcl_file_name
                                       is_cfg          TYPE ts_diagram_config
                             RETURNING VALUE(rv_param) TYPE string.
    METHODS show_html IMPORTING iv_html TYPE string
                                iv_size TYPE string DEFAULT cl_abap_browser=>xlarge
                      RAISING   cx_dynamic_check.
    METHODS to_png IMPORTING io_name        TYPE REF TO lcl_file_name
                             is_cfg         TYPE ts_diagram_config
                   RETURNING VALUE(rv_name) TYPE string.

ENDCLASS.                    "lcl_plant_uml DEFINITION

*----------------------------------------------------------------------*
*       INTERFACE lif_output DEFINITION
*----------------------------------------------------------------------*
"! Output interface
INTERFACE lif_output.
  "! export data
  "! @parameter io_uml_class_scanner | UML class scanner instance
  METHODS output IMPORTING io_parameter   TYPE REF TO lcl_parameter
                 RETURNING VALUE(rv_flag) TYPE flag.
ENDINTERFACE.                    "lif_output DEFINITION

CLASS lcl_xmi_output DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_output.
  PRIVATE SECTION.
    CLASS-METHODS new_xmi IMPORTING io_scanner    TYPE REF TO cl_uml_class_scanner
                                    io_parameter  TYPE REF TO lcl_parameter
                          RETURNING VALUE(ro_xmi) TYPE REF TO cl_uml_class_decor_xmi.
ENDCLASS.                    "lcl_xmi_output DEFINITION

CLASS lcl_plantuml_output DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_output.
ENDCLASS.                    "lcl_plantuml_output DEFINITION

CLASS lcl_null_output DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    INTERFACES lif_output.
ENDCLASS.                    "lcl_xmi_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_export DEFINITION
*----------------------------------------------------------------------*
"! export - application
CLASS lcl_export DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CLASS-METHODS init.
    CLASS-METHODS user_command IMPORTING iv_ucomm       TYPE syucomm
                               RETURNING VALUE(rv_flag) TYPE flag.
  PROTECTED SECTION.
    CLASS-METHODS output IMPORTING iv_ucomm       TYPE syucomm
                         RETURNING VALUE(rv_flag) TYPE flag.
ENDCLASS.                    "lcl_export DEFINITION

CLASS lcl_configuration DEFINITION CREATE PRIVATE FRIENDS lif_unit_test.
  PUBLIC SECTION.
    CONSTANTS:
      c_mode_aut TYPE char01 VALUE 'T',  " for ABAP Unit Test
      c_mode_url TYPE char01 VALUE 'U',
      c_mode_txt TYPE char01 VALUE space,
      c_mode_xmi TYPE char01 VALUE 'X',
      c_mode_exe TYPE char01 VALUE 'E'.

    CLASS-METHODS:
      get RETURNING VALUE(rs_cfg) TYPE ts_diagram_config,
      query,
      class_constructor.
  PRIVATE SECTION.
    CONSTANTS c_registry_java_base_key TYPE string VALUE 'SOFTWARE\JavaSoft\Java Runtime Environment'  ##NO_TEXT.
    TYPES: BEGIN OF ts_param,
             local_path     TYPE localfile,
             java_jar       TYPE localfile,
             java_appl      TYPE localfile,
             server_url     TYPE localfile,
             output_mode    TYPE char01,
             skip_dialog    TYPE flag,
             scale          TYPE perct,
             shadowing      TYPE flag,
             display_source TYPE flag,
             hpages         TYPE i,
             vpages         TYPE i,
           END OF ts_param.
    METHODS get_attributes RETURNING VALUE(rt_attr) TYPE sci_atttab.
    METHODS to_radiobutton.
    METHODS from_radiobutton.
    CLASS-DATA gs_cfg TYPE ts_param.
    DATA: mv_mode_url TYPE flag VALUE 'X',
          mv_mode_exe TYPE flag,
          mv_mode_txt TYPE flag.
    METHODS dialog.
    CLASS-METHODS get_java_path RETURNING VALUE(rv_fullpath) TYPE string.
    CLASS-METHODS get_registry_value IMPORTING iv_key TYPE string
                                               iv_value TYPE string
                                     EXPORTING ev_subrc TYPE sysubrc
                                     RETURNING VALUE(rv_value) TYPE string.
ENDCLASS.

*--------------------------------------------------------------------------------------------------------*

CLASS lcl_parameter IMPLEMENTATION.

  METHOD constructor.
    get_user_input( iv_progname ).
  ENDMETHOD.                    "constructor

  METHOD get_user_input.
    DATA lt_parameters TYPE TABLE OF rsparams ##needed.
    DATA lt_parameters_255 TYPE TABLE OF rsparamsl_255.

    CLEAR mt_parameters.

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

    CHECK sy-subrc EQ 0.
    INSERT LINES OF lt_parameters_255 INTO TABLE mt_parameters.
  ENDMETHOD.

  METHOD get_scan_config.
    rs_scan = VALUE #( attributes     = get_boolean( 'X_ATTR' )
                       methods        = get_boolean( 'X_METH' )
                       constants      = get_boolean( 'X_CONS' )
                       private_member   = get_boolean( 'X_PRIV' )
                       protected_member = get_boolean( 'X_PROT' )
                       packaged_member  = get_boolean( 'X_PACKS' )

                       scan_local_types = get_boolean( 'X_LOCL' )
                       scan_programs    = get_boolean( 'X_PROG' )
                       scan_function_groups = get_boolean( 'X_FUGR' )
                       add_structures   = get_boolean( 'X_STRUCT' )
                       scan_used_types  = get_boolean( 'X_USES' ) ).
  ENDMETHOD.

  METHOD get_display_config.
    rs_cfg = VALUE #( show_aggregations = get_boolean( 'X_AGGR' )
                      show_associations = get_boolean( 'X_ASOS' )
                      show_uses         = get_boolean( 'X_USES' )  " Display Dependencies
                      show_friends      = get_boolean( 'X_FRND' )  " Display Friend Relationship
                      show_throws       = get_boolean( 'X_EXCEP' ) ).
  ENDMETHOD.                    "lif_parameter~get_config

  METHOD get_boolean.
    rv_value = mt_parameters[ selname = iv_parameter_name ]-low.
  ENDMETHOD.                    "get_boolean

  METHOD get_parameter_so_tab.
    rt_so_value = VALUE #( FOR p IN mt_parameters
        WHERE ( selname EQ iv_parameter_name AND NOT ( low IS INITIAL AND high IS INITIAL ) )
                ( CORRESPONDING #( p ) ) ).
  ENDMETHOD.                    "get_parameter_so_tab

  METHOD get_scan_ranges.
    et_so_objects = get_parameter_so_tab( 'SO_ROOT' ).     " others
    et_so_packages = get_parameter_so_tab( 'SO_DEVC' ).    " packages

    CHECK et_so_packages IS INITIAL AND et_so_objects IS NOT INITIAL.

    SELECT DISTINCT devclass AS low, 'I' AS sign, 'EQ' AS option FROM tadir
      APPENDING CORRESPONDING FIELDS OF TABLE @et_so_packages   ##TOO_MANY_ITAB_FIELDS
      WHERE obj_name IN @et_so_objects.
    CHECK sy-subrc NE 0.

    CLEAR et_so_objects.
  ENDMETHOD.

ENDCLASS.                    "lcl_parameter IMPLEMENTATION

CLASS lcl_scanner IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_parameter = io_parameter.
    DATA(ls_scan) = mo_parameter->get_scan_config( ).
    set_scanner_configuration( scan_local_types = ls_scan-scan_local_types
                               scan_programs = ls_scan-scan_programs
                               scan_function_groups = ls_scan-scan_function_groups
                               add_structures = ls_scan-add_structures
                               scan_used_types = ls_scan-scan_used_types ).
  ENDMETHOD.

  METHOD class_scan.
    rv_objects_selected = abap_false.

    cl_uml_cache=>get_singleton( )->clear_type_cache( ).

    mo_parameter->get_scan_ranges( IMPORTING et_so_packages = DATA(lt_packages)
                                             et_so_objects = DATA(lt_objects) ).
    CHECK NOT ( lt_packages IS INITIAL AND lt_objects IS INITIAL ).
    rv_objects_selected = abap_true.

    execute( scan_packages = lt_packages
             scan_types = lt_objects ).
  ENDMETHOD.                    "lif_parameter~uml_class_scan

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_file IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_file IMPLEMENTATION.

  METHOD download.
    rv_subrc = 1.
    CHECK io_name->dialog( ) NE cl_gui_frontend_services=>action_cancel.

    rv_subrc = cl_uml_utilities=>save_xml_local( xml = iv_data
                                                 filename = io_name->get_fullpath( ) ).
  ENDMETHOD.

ENDCLASS.                    "lcl_file IMPLEMENTATION

CLASS lcl_file_name IMPLEMENTATION.

  METHOD new.
    CASE iv_mode.
      WHEN lcl_configuration=>c_mode_aut.
        ro_file = NEW lcl_file_name_dummy( iv_mode ).
      WHEN OTHERS.
        ro_file = NEW lcl_file_name( iv_mode ).
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.
    CASE iv_mode.
      WHEN lcl_configuration=>c_mode_txt.
        ms_file = VALUE #( title = |Save UML text source|
                           ext = |.txt| ).
      WHEN lcl_configuration=>c_mode_xmi.
        ms_file = VALUE #( title    = |Export XMI file|
                           ext      = |.xmi|
                           filter   = |(*.xmi)\|*.xmi| ).
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
  ENDMETHOD.

ENDCLASS.

CLASS lcl_file_name_dummy IMPLEMENTATION.

  METHOD dialog.
    ms_file-path = |test.txt|.
    rv_user_action = cl_gui_frontend_services=>action_cancel.
  ENDMETHOD.

ENDCLASS.

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
  ENDMETHOD.

  METHOD show_html.
    cl_abap_browser=>show_html( html_string = iv_html
                                size = iv_size
                                context_menu = abap_true ).
  ENDMETHOD.

  METHOD output.
    CASE is_cfg-output_mode.
      WHEN lcl_configuration=>c_mode_url.
        show_html( |<img src="{ to_url( ) }"/>\n{ source( is_cfg-display_source ) }| ).

      WHEN lcl_configuration=>c_mode_exe.
        DATA(lo_name) = lcl_file_name=>new( lcl_file=>c_mode_txt ).
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
    cl_abap_conv_out_ce=>create( encoding = 'UTF-8' )->convert( EXPORTING data = iv_string
                                                                IMPORTING buffer = rv_xstring ).
  ENDMETHOD.                    "to_xstring

  METHOD parameter_string.
    rv_param = |-jar { is_cfg-java_jar } -o { is_cfg-local_path } "{ io_name->get_fullpath( ) }"|.
  ENDMETHOD.

  METHOD png_file_name.
    TRY.
        rv_name = |{ is_cfg-local_path }{ io_name->get_prefix( ) }.png|.
      CATCH cx_dynamic_check.
        CLEAR rv_name.
    ENDTRY.
  ENDMETHOD.

  METHOD to_png.
    CLEAR rv_name.
    cl_gui_frontend_services=>execute(
      EXPORTING application = is_cfg-java_appl
                parameter = parameter_string( io_name = io_name
                                              is_cfg = is_cfg )
                synchronous = 'X'
      EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc EQ 0.
    rv_name = png_file_name( io_name = io_name
                             is_cfg = is_cfg ).
  ENDMETHOD.

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
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    mo_uml = io_uml.
    ms_uml_cfg = io_parameter->get_display_config( ).
    ms_scan_cfg = io_parameter->get_scan_config( ).
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

  METHOD plant_uml_class.
    CHECK mv_name IS NOT INITIAL AND begin_class( ).
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
                iv_active = ms_uml_cfg-show_uses ).
    uml_reduce( it_data = ms_uml-t_exceptions
                iv_sep = '..'
                iv_active = ms_uml_cfg-show_throws ).
    uml_reduce( it_data = ms_uml-t_friends
                iv_sep = '..>'
                iv_suffix = | : friend |
                iv_active = ms_uml_cfg-show_friends ).
  ENDMETHOD.                    "plant_uml_class

  METHOD get_name.
    rv_name = substring_after( val = iv_name regex = '\\(CLASS|INTERFACE)=' ).
    CHECK rv_name IS INITIAL.
    rv_name = substring_after( val = iv_name regex = '\\FUGR=' ).
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
      mo_uml->add( |\{static\}| ).
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
*      mo_uml->add( |{ mv_name } ..o { get_name( ls_event-name ) }\n| ).
    LOOP AT ms_uml-t_events INTO DATA(ls_event).
      uml_add( iv_visibility = ls_event-visibility
               iv_class = ls_event-is_static
               iv_name = |{ ls_event-name }( )| ).
    ENDLOOP.
  ENDMETHOD.                    "uml_events

  METHOD uml_fields.
    CHECK ms_scan_cfg-attributes EQ abap_true.
    LOOP AT ms_uml-t_attributes INTO DATA(ls_field) WHERE is_constant EQ space.
      uml_add( iv_visibility = ls_field-visibility
               iv_class = ls_field-is_class
               iv_name = ls_field-name ).
    ENDLOOP.
  ENDMETHOD.                    "uml_fields

  METHOD uml_methods.
    CHECK ms_scan_cfg-methods EQ abap_true.
    LOOP AT ms_uml-t_methods INTO DATA(ls_method).
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
    rv_diagram = NEW lcl_class_diagram( io_parameter = io_parameter
                                        is_diagram_cfg = is_diagram_cfg )->to_uml_text( it_uml ).
  ENDMETHOD.                    " generate

  METHOD constructor.
    mo_uml = lcl_uml=>new( is_diagram_cfg ).
    mo_class = NEW #( io_uml       = mo_uml
                      io_parameter = io_parameter ).
  ENDMETHOD.                    " constructor

  METHOD to_uml_text.
    LOOP AT it_uml INTO DATA(ls_next).
      mo_class->to_uml_text( ls_next ).
    ENDLOOP.

    rv_diagram = mo_uml->get( ).
  ENDMETHOD.                    " to_uml_text

ENDCLASS.                    "lcl_class_diagram IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uml IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_uml IMPLEMENTATION.

  METHOD new.
    ro_uml = NEW #( ).
    ro_uml->header( is_config ).
  ENDMETHOD.                    " new

  METHOD add.
    mv_diagram = mv_diagram && iv_code.
  ENDMETHOD.                    " add

  METHOD get.
    footer( ).
    rv_diagram = mv_diagram.
  ENDMETHOD.                    " get

  METHOD header.
    add( |@startuml\n| ).   " Header
    add( |scale { is_cfg-scale }\n| ).   " Reduce the size of the output image
    add( |page { is_cfg-hpages }x{ is_cfg-vpages }\n| ).  " split if n files
  ENDMETHOD.                    " header

  METHOD footer.
    add( |hide <<FUGR>> circle\n| ).
    add( |@enduml\n| ).
  ENDMETHOD.                    " footer

ENDCLASS.                    "lcl_uml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xmi_output IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_xmi_output IMPLEMENTATION.

  METHOD new_xmi.
    ro_xmi = NEW cl_uml_class_decor_xmi( io_scanner ).

    DATA(ls_scan) = io_parameter->get_scan_config( ).
    ro_xmi->set_decorator_configuration( attributes = ls_scan-attributes
                                         methods = ls_scan-methods
                                         constants = ls_scan-constants
                                         private_member = ls_scan-private_member
                                         protected_member = ls_scan-protected_member
                                         packaged_member = ls_scan-packaged_member ).
    DATA(ls_cfg) = io_parameter->get_display_config( ).
    ro_xmi->set_xmi_configuration( aggregations    = ls_cfg-show_aggregations
                                   associations    = ls_cfg-show_associations
                                   uses            = ls_cfg-show_uses
                                   friends         = ls_cfg-show_friends
                                   show_exceptions = ls_cfg-show_throws ).
  ENDMETHOD.

  METHOD lif_output~output.
    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <lv_data> TYPE xstring.

    rv_flag = abap_false.
    DATA(lo_scanner) = NEW lcl_scanner( io_parameter ).

    CHECK lo_scanner->class_scan( ).
    new_xmi( io_scanner = lo_scanner
             io_parameter = io_parameter )->get_diagram( CHANGING c_data = lr_data ).
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
    DATA lr_data TYPE REF TO data.
    DATA lr_uml_tab TYPE REF TO cl_uml_class_scanner=>uml_tab.

    rv_flag = abap_false.
    DATA(lo_scanner) = NEW lcl_scanner( io_parameter ).

    CHECK lo_scanner->class_scan( ).
    lo_scanner->get_diagram( CHANGING c_data = lr_data ).
    TRY.
        lr_uml_tab ?= lr_data.
        CHECK lr_uml_tab IS BOUND.
        DATA(ls_cfg) = lcl_configuration=>get( ).
        NEW lcl_plant_uml( lcl_class_diagram=>generate( it_uml = lr_uml_tab->*
                                                        io_parameter = io_parameter
                                                        is_diagram_cfg = ls_cfg )
                                                         )->output( ls_cfg ).
        rv_flag = abap_true.
      CATCH cx_dynamic_check.                           "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.                    "lif_output~output

ENDCLASS.                    "lcl_plantuml_output IMPLEMENTATION

CLASS lcl_null_output IMPLEMENTATION.

  METHOD lif_output~output.
    rv_flag = abap_false.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_configuration IMPLEMENTATION.

  METHOD class_constructor.
    gs_cfg = VALUE #( java_appl = get_java_path( )
                      local_path = `C:\Temp\Dokumente\UML\`                           " PlantUML jar file and output path
                      java_jar = `C:\Temp\Dokumente\UML\plantuml.jar`
                      server_url = `http://www.plantuml.com/plantuml/img/` ##NO_TEXT  " PlantUML server URL
                      output_mode = c_mode_url
                      skip_dialog = space
                      scale = c_default_scale
                      shadowing = abap_false
                      display_source = abap_true
                      hpages = 1
                      vpages = 1 ).
  ENDMETHOD.

  METHOD get_registry_value.
    cl_gui_frontend_services=>registry_get_value(
      EXPORTING
        root  = cl_gui_frontend_services=>hkey_local_machine
        key   = iv_key
        value = iv_value
      IMPORTING
        reg_value = rv_value
      EXCEPTIONS
        OTHERS               = 5 ).
    ev_subrc = sy-subrc.
  ENDMETHOD.

  METHOD get_java_path.
*   Windows: Local Java installation
    DATA lv_subrc TYPE sysubrc.

    rv_fullpath = `C:\Windows\System32\java`.   " Default
    DATA(lv_path) = get_registry_value( EXPORTING iv_key = c_registry_java_base_key
                                                  iv_value = 'CurrentVersion'
                                        IMPORTING ev_subrc = lv_subrc ).
    CHECK lv_subrc EQ 0.
    lv_path = get_registry_value( EXPORTING iv_key = |{ c_registry_java_base_key }\\{ lv_path }|
                                            iv_value = 'JavaHome'
                                  IMPORTING ev_subrc = lv_subrc ).
    CHECK lv_subrc EQ 0.
    rv_fullpath = |{ lv_path }\\bin\\java|.
  ENDMETHOD.

  METHOD get_attributes.
* Table Type has type 'T' - patterns SCI_PATTERN
*                     ' ' - ?? private attributes?
*                     'I' - ?? Integer?
    rt_attr = VALUE #(
     ( ref = REF #( gs_cfg-skip_dialog )    text = 'Remember my settings'(c00)    kind = 'C' )
     ( ref = REF #( sy-index )              text = 'PlantUML Execution Mode'(c10) kind = 'G' ) " Group
     ( ref = REF #( mv_mode_url )           text = 'PlantUML web service'(c11)    kind = 'R' button_group = 'MOD' )
     ( ref = REF #( mv_mode_txt )           text = 'Save text file'(c12)          kind = 'R' button_group = 'MOD' )
     ( ref = REF #( mv_mode_exe )           text = 'Local PlantUML '(c13)         kind = 'R' button_group = 'MOD' )
     ( ref = REF #( '' )                    text = 'PlantUML Settings'(c20)       kind = 'G' )
     ( ref = REF #( gs_cfg-scale )          text = 'Scale '(c21)                  kind = 'S' )
     ( ref = REF #( gs_cfg-hpages )         text = 'H-Pages '(c22)                kind = 'S' )
     ( ref = REF #( gs_cfg-vpages )         text = 'V-Pages '(c23)                kind = 'S' )
     ( ref = REF #( gs_cfg-server_url )     text = 'PlantUML Server'(c25)         kind = 'S' )
     ( ref = REF #( gs_cfg-local_path )     text = 'Local PlantUML path'(c26)     kind = 'S' )
     ( ref = REF #( gs_cfg-java_jar )       text = 'Local PlantUML jar file'(c27) kind = ' ' )
     ( ref = REF #( gs_cfg-java_appl )      text = 'Local Java path'(c28)         kind = 'S' ) " Select-Options
     ( ref = REF #( gs_cfg-shadowing )      text = 'Shadowing '(c31)              kind = 'C' )
     ( ref = REF #( gs_cfg-display_source ) text = 'Display source '(c32)         kind = 'C' )    ).
  ENDMETHOD.

  METHOD to_radiobutton.
    mv_mode_url = xsdbool( gs_cfg-output_mode EQ c_mode_url ).
    mv_mode_exe = xsdbool( gs_cfg-output_mode EQ c_mode_exe ).
    mv_mode_txt = xsdbool( gs_cfg-output_mode EQ c_mode_txt ).
  ENDMETHOD.

  METHOD from_radiobutton.
    IF mv_mode_url EQ abap_true.
      gs_cfg-output_mode = c_mode_url.
    ELSEIF mv_mode_exe EQ abap_true.
      gs_cfg-output_mode = c_mode_exe.
    ELSEIF mv_mode_txt EQ abap_true.
      gs_cfg-output_mode = c_mode_txt.
    ENDIF.
  ENDMETHOD.

  METHOD get.
    rs_cfg = CORRESPONDING #( gs_cfg ).
  ENDMETHOD.

  METHOD query.
    NEW lcl_configuration( )->dialog( ).
  ENDMETHOD.

  METHOD dialog.
    to_radiobutton( ).
    "CHECK gs_cfg-skip_dialog EQ abap_false.
    CHECK cl_ci_query_attributes=>generic(
        p_name       = CONV #( sy-repid )                    " unique screen ID
        p_title      = 'Class Diagram Parameters'            " Screen title
        p_attributes = get_attributes( )                     " Screen fields
        p_display    = abap_false                            " Edit / Display only
       ) EQ abap_false.   " Do not cancel
    from_radiobutton( ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_export IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_export IMPLEMENTATION.

  METHOD init.
    button1 = TEXT-007.
    button2 = TEXT-008.
    button3 = TEXT-009.
*   pushbutton in the application toolbar
    sscrfields-functxt_01 = VALUE smp_dyntxt( icon_id = '@4Y@'        " ICON_BUSINAV_ENTITY
                                              icon_text = TEXT-p01    " PlantUML
                                              quickinfo = TEXT-p01
                                              text = TEXT-p01 ).
    sscrfields-functxt_02 = VALUE smp_dyntxt( icon_id = '@BX@'        " ICON_CONFIGURATION
                                              icon_text = TEXT-p02    " PlantUML Configuration
                                              quickinfo = TEXT-p02
                                              text = TEXT-p02 ).
  ENDMETHOD.

  METHOD user_command.
    CASE iv_ucomm.
      WHEN 'FC02'.
        lcl_configuration=>query( ).
        rv_flag = abap_true.

      WHEN OTHERS.
        rv_flag = output( iv_ucomm ).
    ENDCASE.
  ENDMETHOD.

  METHOD output.
    DATA li_export TYPE REF TO lif_output.

    li_export = SWITCH #( iv_ucomm WHEN 'FC01' THEN NEW lcl_plantuml_output( )
                                   WHEN 'ONLI' THEN NEW lcl_xmi_output( )
                                   ELSE NEW lcl_null_output( ) ).
    rv_flag = li_export->output( NEW lcl_parameter( ) ).
  ENDMETHOD.

ENDCLASS.                    "lcl_export IMPLEMENTATION

" ----------------------------------------------------------------------------------------------------------------------------------- *
" program events

INITIALIZATION.
  lcl_export=>init( ).

AT SELECTION-SCREEN.
  IF lcl_export=>user_command( sscrfields-ucomm ).
    CLEAR sscrfields-ucomm.
  ENDIF.

START-OF-SELECTION.
  lcl_export=>user_command( sy-ucomm ).
