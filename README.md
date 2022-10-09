# ABAP-to-PlantUML Diagrams

[![Language: ABAP](https://img.shields.io/badge/Language-ABAP-blue.svg?style=flat)](https://www.sap.com/developer/topics/abap-platform.html)

Create expressive PlantUML Class and Sequence Diagrams from ABAP Code and ABAP Trace

Try PlantUML at http://plantuml.com/.


## Class Diagram from ABAP Code

https://blogs.sap.com/2017/04/27/plantuml-diagrams/

- This program generates UML class diagrams automatically from existing ABAP code.
- The diagrams are generated and displayed from the SAP GUI without any further settings.
- Local class LCL_PLANT_UML implements access to the PlantUML web service.

![export_class](https://github.com/nomssi/ABAP-to-PlantUML/blob/master/class%20diagram/uml_export.png)

## Sequence Diagram from ABAP Trace
https://wiki.scn.sap.com/wiki/display/Snippets/ABAP+Trace+to+PlantUML+Sequence+Diagram

https://blogs.sap.com/2022/10/09/abap-trace-to-plantuml-sequence-diagram/

Transaction SAT records the execution of an ABAP program and provides analysis tools for performance, program flow and memory consumption. Measurements can be imported from other SAP systems using either download/upload or RFC. Since SAP Netweaver 7.02, an UML sequence diagram can be generated if the measurement was recorded without aggregation.

 - Start transaction SAT -> Evaluate -> Double Click on a measurement; on the next screen press F8 (Display measurement as UML sequence diagram)
 
 ### Features
 - a secondary internal table index speeds up parsing
 - a custom filter displays calls from standard to custom code in addition to the standard filter logic
 - loop compaction produces smaller diagrams
 - the plain text output in PlantUML format is an editable representation of the sequence diagram.

## Future

ABAP Data Model to PlantUML, like 
https://github.com/Hywan/Database-to-PlantUML
