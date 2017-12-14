## Setup
- Implement include YY_SATRA_INCLUDE.
- create a source code plug-in to add the local classes in the include YY_SATRA_INCLUDE to class CL_ATRA_UML_HANDLING, e.g. as implicit enhancement at the end of include *LS_ABAP_TRACE_DATAD07*:

    ENHANCEMENT 1  YY_SATR_SEQUENCE.    "active version
      INCLUDE YY_SATRA_INCLUDE.
    ENDENHANCEMENT.

Add an implicit enhancement at the beginning of Method SHOW_SEQ_DIAGRAM( ) of CL_ATRA_UML_HANDLING to replace the logic:

    ENHANCEMENT 2  YY_SATR_SEQUENCE.    "active version
      TRY.
          lcl_sequence=>to_diagram( lcl_configuration=>query( ) )->output( ).
        CATCH cx_dynamic_check INTO DATA(gx_error).
          MESSAGE gx_error TYPE 'I' DISPLAY LIKE 'E'.  "#EC CI_USE_WANTED
      ENDTRY.
      RETURN.
    ENDENHANCEMENT.
