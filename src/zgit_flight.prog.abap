*&---------------------------------------------------------------------*
*& Report ZGIT_FLIGHT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgit_flight.

*----------------------------------------------------------------------*
*       CLASS Flight DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS flight DEFINITION.
***********************************************
*The Class will have 1 attribute - an internal table that replicates all the data found in SPFLI.
*All access to the data must be through object Methods so the attribute should be private.
*Create methods to access all the data in the internal table.
*Create a constructor to fill the internal table from the Database.

  PUBLIC SECTION.

    METHODS constructor. "Instance Constructor

    METHODS showalldata.

    METHODS showconniddata
      IMPORTING connidflight TYPE s_conn_id.

    METHODS numflightsto
      IMPORTING airportcode                          TYPE s_to_city
      RETURNING VALUE(numberofflightsfromthisaiport) TYPE i.

    METHODS getconnid
      IMPORTING departureairport    TYPE s_from_cit
                arrivaairport       TYPE s_to_city
      RETURNING VALUE(connidflight) TYPE s_conn_id..

    METHODS getflighttime
      IMPORTING connectionid                     TYPE s_conn_id
      RETURNING VALUE(numberofminutesflighttime) TYPE i.

    METHODS getallconnectionfacts
      IMPORTING connectionid                      TYPE s_conn_id
      RETURNING VALUE(wholeconnectioninformation) TYPE spfli.
***********************************************
  PRIVATE SECTION.
    TYPES: mytablespfli_type TYPE STANDARD TABLE OF spfli.
    DATA: mytablespfli TYPE mytablespfli_type.


ENDCLASS.                    "Flight DEFINITION

*----------------------------------------------------------------------*
*       CLASS Flight IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS flight IMPLEMENTATION.

  METHOD constructor. "Instance Constructor: Load the records from SPFLI into the internal table attribute.

    SELECT * FROM spfli INTO TABLE mytablespfli.
    IF sy-subrc <> 0.
      WRITE: 'There was a problem reading the SPFLI DB Table'.
    ENDIF.

  ENDMETHOD.

  METHOD showalldata. "write out to the screen all of the data
    DATA: wa TYPE spfli.

    LOOP AT mytablespfli INTO wa.
      WRITE: / wa-carrid, 5 wa-connid, 10 wa-countryfr, 14 wa-cityfrom, 36 wa-airpfrom, 40 wa-countryto,
      44 wa-cityto, 66 wa-airpto, 69 wa-fltime, 77 wa-deptime, 87 wa-arrtime, 97 wa-distance, 107 wa-distid,
      110 wa-fltype, 115 wa-period.
    ENDLOOP.

    ULINE.

  ENDMETHOD.

  METHOD showconniddata.
*take in a connection ID and write out the following:
*City From, Country From, City To, Country to, Flight Time (in minutes), distance in KM/miles
*If no connection ID exists write out “No record found matching connid:” followed by the connection ID value passed in.
    DATA: wa TYPE spfli.

    READ TABLE mytablespfli INTO wa WITH KEY connid = connidflight.
    IF sy-subrc = 0.
      WRITE: / wa-cityfrom, 22 wa-airpfrom, 27 wa-countryto, 49 wa-fltime, 54 wa-distance.
    ELSE.
      WRITE: 'No record found matching connid: ', connidflight.
    ENDIF.

  ENDMETHOD.

  METHOD numflightsto. "take in an airport code (FRA, SFC etc..) and
*    return the number of flights that travel to that airport.

    LOOP AT mytablespfli TRANSPORTING NO FIELDS WHERE airpto = airportcode.
      numberofflightsfromthisaiport = numberofflightsfromthisaiport + 1.
    ENDLOOP.

  ENDMETHOD.

* this method will take in 2 airport codes, 1 representing the departure airport
*    and 1 representing the arrival airport.
*    Return the connection ID of the flight that matches.
*    If the parameter combination has no matching records return 0.
  METHOD getconnid.
    DATA: wa TYPE spfli.

    connidflight = 0.
    READ TABLE mytablespfli INTO wa WITH KEY airpfrom = departureairport
                                             airpto   = arrivaairport.
    connidflight = wa-connid.
  ENDMETHOD.

* accepts a connection ID and returns the number of minutes in flight time.
  METHOD getflighttime.

    DATA: wa TYPE spfli.

    numberofminutesflighttime  = 0.
    READ TABLE mytablespfli INTO wa WITH KEY connid = connectionid.
    numberofminutesflighttime  = wa-fltime.

  ENDMETHOD.

* take in a connection ID and return a structure that contains all the information
* about that connection. This structure should correspond to a row in the table.
  METHOD getallconnectionfacts.

    CLEAR wholeconnectioninformation.
    READ TABLE mytablespfli INTO wholeconnectioninformation WITH KEY connid = connectionid.

  ENDMETHOD.

ENDCLASS.                    "Flight IMPLEMENTATION

START-OF-SELECTION.

  DATA: temp TYPE i,
        wa   TYPE spfli.

  DATA my_flight_class TYPE REF TO flight.
  CREATE OBJECT my_flight_class.

  my_flight_class->showalldata( ).

  my_flight_class->showconniddata( 1984 ).
  ULINE.

  temp = my_flight_class->numflightsto( 'JFK').
  WRITE temp.
  ULINE.

  temp = my_flight_class->getconnid( departureairport = 'JFK' arrivaairport = 'FRA').
  WRITE temp.
  ULINE.

  temp = my_flight_class->getflighttime( 3504 ).
  WRITE temp.
  ULINE.

  wa = my_flight_class->getallconnectionfacts( 3504 ).
  WRITE: / wa-carrid, 5 wa-connid, 10 wa-countryfr, 14 wa-cityfrom, 36 wa-airpfrom, 40 wa-countryto,
  44 wa-cityto, 66 wa-airpto, 69 wa-fltime, 77 wa-deptime, 87 wa-arrtime, 97 wa-distance, 107 wa-distid,
  110 wa-fltype, 115 wa-period.
