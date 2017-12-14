Class **CL_ATRA_UML_HANDLING** controls the generation of an UML sequence diagram from a measurement without aggregation in transaction SAT. Method SHOW_SEQ_DIAGRAM will call method DISPLAY_UML( ) of local class LCL_UML_HANDLING.

## Setup

Three steps are needed to override the standard behavior:
1. Create a new Include YY_SATRA_INCLUDE using the following code (Netweaver 7.4 SP05 required, a down port to 7.31 is at the end of this document):

2. Implement include YY_SATRA_INCLUDE.

Create a source code plug-in to add the local classes in the include YY_SATRA_INCLUDE to class CL_ATRA_UML_HANDLING, e.g. as implicit enhancement at the end of include **LS_ABAP_TRACE_DATAD07**:

    ENHANCEMENT 1  YY_SATR_SEQUENCE.    "active version
      INCLUDE YY_SATRA_INCLUDE.
    ENDENHANCEMENT.

3. Add an implicit enhancement at the beginning of Method **SHOW_SEQ_DIAGRAM( )** of class CL_ATRA_UML_HANDLING to replace the logic:

    ENHANCEMENT 2  YY_SATR_SEQUENCE.    "active version
      DATA lx_error TYPE REF TO cx_root.
      TRY.
          lcl_sequence=>to_diagram( lcl_configuration=>query( ) )->output( ).
        CATCH cx_dynamic_check INTO lx_error.
          MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.  "#EC CI_USE_WANTED
      ENDTRY.
      RETURN.
    ENDENHANCEMENT.
