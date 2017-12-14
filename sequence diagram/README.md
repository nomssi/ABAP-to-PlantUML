Class **CL_ATRA_UML_HANDLING** controls the generation of an UML sequence diagram from a measurement without aggregation in transaction SAT. Method SHOW_SEQ_DIAGRAM will call method DISPLAY_UML( ) of local class LCL_UML_HANDLING.

## Setup
Three steps are needed to override the standard behavior:

1. Create a new Include YY_SATRA_INCLUDE using the code corresponging to you Netweaver release 7.31, 7.40 or later

2. In class builder for class CL_ATRA_UML_HANDLING 

*Goto -> Local Definitions/Implementations -> Local Definitions/Implementations Ctrl+Shift+F6*. 

The definition/implementation of local class LCL_UML_HANDLING is displayed.
At the bottom of the source code in include **LS_ABAP_TRACE_DATAD07**, implement an implicit enhancement (source code plug-in) to insert the new include:

    ENHANCEMENT 1  YY_SATR_SEQUENCE.    "active version
      INCLUDE YY_SATRA_INCLUDE.
    ENDENHANCEMENT.

3. Add an implicit enhancement at the beginning of Method **SHOW_SEQ_DIAGRAM( )** of class CL_ATRA_UML_HANDLING:

The definition/implementation of local class LCL_UML_HANDLING is displayed. At the bottom of the source code in include **LS_ABAP_TRACE_DATAD07**, implement an implicit enhancement (source code plug-in) to insert the new include:

    ENHANCEMENT 2  YY_SATR_SEQUENCE.    "active version    
      DATA lx_error TYPE REF TO cx_dynamic_check.
      TRY.
          lcl_sequence=>to_diagram( lcl_configuration=>query( ) )->output( ).
        CATCH cx_dynamic_check INTO lx_error.
          MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.  "#EC CI_USE_WANTED
      ENDTRY.
      RETURN.
    ENDENHANCEMENT.

After activation, a popup appears while displaying a sequence diagram
![seq_export_params](https://github.com/nomssi/ABAP-to-PlantUML/blob/master/sequence%20diagram/seq_export_params.png)
