REPORT  zsmartform_compare.

*--------------------------------------------------------------------*
**** TYPE-POOLS
*--------------------------------------------------------------------*
TYPE-POOLS: slis.

DATA: v_filename_string TYPE string,
*** table to contain the parsed data
      g_t_xml_info      TYPE TABLE OF smum_xmltb INITIAL SIZE 0,
      g_t_xml_info_1    TYPE TABLE OF smum_xmltb INITIAL SIZE 0,
*** work area for internal table containing parsed data
      g_s_xml_info      LIKE LINE OF g_t_xml_info,
      g_s_xml_info_1    LIKE LINE OF g_t_xml_info,
*** table to contain the returned messages from parsing FM
      g_t_return        TYPE STANDARD TABLE OF bapiret2,
      g_t_return_1      TYPE STANDARD TABLE OF bapiret2,
*** handle string and xstring
      itab              TYPE STANDARD TABLE OF string,
      str               TYPE string.

*** ALV
TYPES: BEGIN OF gty_alv,
         name    TYPE string,
         string1 TYPE string,
         string2 TYPE string,
         color   TYPE lvc_t_scol,
       END OF gty_alv.
DATA: gt_alv  TYPE STANDARD TABLE OF gty_alv,
      gst_alv TYPE gty_alv.

*** Fieldcatalog and layout table
DATA: gt_fieldcat  TYPE TABLE OF slis_fieldcat_alv,
      gst_fieldcat TYPE slis_fieldcat_alv,
      gst_layout   TYPE slis_layout_alv,
      g_xmldata    TYPE xstring.

*** Selection Screen
PARAMETERS: p_file1 TYPE localfile OBLIGATORY,
            p_file2 TYPE localfile OBLIGATORY.

START-OF-SELECTION.

*** Read the first file
  CLEAR: v_filename_string.
  v_filename_string = p_file1.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = v_filename_string
      filetype                = 'ASC'
      has_field_separator     = 'X'
      dat_mode                = ''
    TABLES
      data_tab                = itab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

***  Convert to Xstring
  READ TABLE itab INTO str INDEX 1.
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = str
    IMPORTING
      buffer = g_xmldata
    EXCEPTIONS
      failed = 1
      OTHERS = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error in the XML file' TYPE 'E'.
  ENDIF.

*** convert to XML internal table
  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      xml_input = g_xmldata
    TABLES
      xml_table = g_t_xml_info
      return    = g_t_return
    EXCEPTIONS
      OTHERS    = 0.

*** get second file
  CLEAR: itab, v_filename_string.
  v_filename_string = p_file2.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = v_filename_string
      filetype                = 'ASC'
      has_field_separator     = 'X'
      dat_mode                = ''
    TABLES
      data_tab                = itab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*** convert to xstring
  CLEAR: str.
  READ TABLE itab INTO str INDEX 1.
  CLEAR: g_xmldata.
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = str
    IMPORTING
      buffer = g_xmldata
    EXCEPTIONS
      failed = 1
      OTHERS = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error in the XML file' TYPE 'E'.
  ENDIF.

*** convert XML data to internal table
  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      xml_input = g_xmldata
    TABLES
      xml_table = g_t_xml_info_1
      return    = g_t_return
    EXCEPTIONS
      OTHERS    = 0.

*** compare smartform header data
  PERFORM get_headers.

*** compare smartform page data
  PERFORM get_page_data.

*--------------------------------------------------------------------*
**** End of Selection
*--------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM build_fcat.   "Build Field Catalog
  PERFORM display_alv.  "Display ALV

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file1.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name = p_file1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file2.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name = p_file2.

*&---------------------------------------------------------------------*
*&      Form  build_fcat
*&---------------------------------------------------------------------*
*       Build Field Catalog
*----------------------------------------------------------------------*
FORM build_fcat.

  DATA: lw_index TYPE sytabix.

  "Change Name
  lw_index = lw_index + 1.
  PERFORM mask_field_catalog USING 'NAME' lw_index 'X'
                                   'Field Name'.

  "File 1
  lw_index = lw_index + 1.
  PERFORM mask_field_catalog USING 'STRING1' lw_index 'X'
                                   'Version 1'.

  "File 2
  lw_index = lw_index + 1.
  PERFORM mask_field_catalog USING 'STRING2' lw_index 'X'
                                   'Version 2'.

ENDFORM.                    "build_fcat

*&---------------------------------------------------------------------*
*&      Form  mask_field_catalog
*&---------------------------------------------------------------------*
*       Subroutine to prepeare the field catalog
*----------------------------------------------------------------------*
FORM mask_field_catalog USING p_fieldname p_colpos
                              p_no_zero   p_coltext.
  CLEAR gst_fieldcat.
  gst_fieldcat-fieldname = p_fieldname.
  gst_fieldcat-seltext_l = p_coltext.
  gst_fieldcat-col_pos   = p_colpos.
  gst_fieldcat-no_zero   = p_no_zero.
  APPEND gst_fieldcat TO gt_fieldcat.

ENDFORM.                    "mask_field_catalog

*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       display the ALV
*----------------------------------------------------------------------*
FORM display_alv.

  "Build Layout
  CLEAR gst_layout.
  gst_layout-colwidth_optimize = 'X'.
  gst_layout-coltab_fieldname = 'COLOR'.

  "Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gst_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    "display_alv

*&---------------------------------------------------------------------*
*&      Form  get_headers
*&---------------------------------------------------------------------*
*       get the heading data out
*----------------------------------------------------------------------*
FORM get_headers.

  DATA: lw_cname  TYPE char255,
        lw_tabix1 TYPE sytabix,
        lw_tabix2 TYPE sytabix.

  DATA: lst_xmlinfo  LIKE LINE OF g_t_xml_info,
        lst_xmlinfo1 LIKE LINE OF g_t_xml_info.


  DATA: lw_header_print TYPE char01. "flag to check if header is printed.

  "build range of headers.
  TYPES: BEGIN OF lty_headers,
           cname TYPE char255,
           type  TYPE char255,
         END OF lty_headers.
  DATA: lt_headers     TYPE STANDARD TABLE OF lty_headers,
        lst_headers    TYPE lty_headers,
        lw_fin_flag_1  TYPE char01,  "Finish Flag for Form 1
        lw_fin_flag_2  TYPE char01,  "Finish Flag for Form 2
        lw_num_2       TYPE sytabix,
        col            TYPE lvc_s_scol,
        coltab         TYPE lvc_t_scol,
        color          TYPE lvc_s_colo,
        lw_tabix_dup_1 TYPE sytabix,
        lw_loopc_1     TYPE syindex,
        lw_loopc_2     TYPE syindex.

  lst_headers-cname = 'SMARTFORM'.
  lst_headers-type  = 'Smartform'.
  APPEND lst_headers TO lt_headers.

  lst_headers-cname = 'HEADER'.
  lst_headers-type  = 'Header Data'.
  APPEND lst_headers TO lt_headers.

  lst_headers-cname = 'INTERFACE'.
  lst_headers-type  = 'Form Interface'.
  APPEND lst_headers TO lt_headers.

  lst_headers-cname = 'GTYPES'.
  lst_headers-type  = 'Types'.
  APPEND lst_headers TO lt_headers.

  lst_headers-cname = 'GDATA'.
  lst_headers-type  = 'Global Data'.
  APPEND lst_headers TO lt_headers.

  lst_headers-cname = 'GCODING'.
  lst_headers-type  = 'Initialization'.
  APPEND lst_headers TO lt_headers.

  lst_headers-cname = 'FCODING'.
  lst_headers-type  = 'Form Routines'.
  APPEND lst_headers TO lt_headers.

  lst_headers-cname = 'VARHEADER'.
  lst_headers-type  = 'Output Options'.
  APPEND lst_headers TO lt_headers.

  lst_headers-cname = 'PAGETREE'.
  lst_headers-type  = 'Start of Pages'.
  APPEND lst_headers TO lt_headers.

  CLEAR: lst_headers.

***  set ALV line color
  color-col = '3'.
  color-int = '0'.
  color-inv = '0'.

  "get number of lines
  CLEAR: lw_num_2.
  DESCRIBE TABLE g_t_xml_info_1 LINES lw_num_2.

  "compare headers
  CLEAR: lw_header_print.
  LOOP AT lt_headers INTO lst_headers.

    IF lst_headers-cname EQ 'PAGETREE'.
      EXIT.
    ENDIF.

    CLEAR: lw_tabix1, lw_tabix2, lw_fin_flag_1, lw_fin_flag_2.

    "check if headers of version 1 is fully compared or read the data
    READ TABLE g_t_xml_info INTO g_s_xml_info
         WITH KEY cname = lst_headers-cname.
    IF sy-subrc EQ 0.
      lw_tabix1 = sy-tabix.
    ELSE.
      lw_fin_flag_1 = 'X'.
    ENDIF.

    "check if headers of version 1 is fully compared or read the data
    READ TABLE g_t_xml_info_1 INTO g_s_xml_info_1
         WITH KEY cname = lst_headers-cname.
    IF sy-subrc EQ 0.
      lw_tabix2 = sy-tabix.
    ELSE.
      lw_fin_flag_2 = 'X'.
    ENDIF.

***    loop on version 1
    CLEAR: lw_loopc_1.
    lw_loopc_1 = 1.
    LOOP AT g_t_xml_info INTO g_s_xml_info FROM lw_tabix1.

      lw_tabix_dup_1 = sy-tabix.

      READ TABLE lt_headers WITH KEY cname = g_s_xml_info-cname TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0 AND lw_loopc_1 GT 1
          AND g_s_xml_info-type IS INITIAL.
        lw_fin_flag_1 = 'X'.
        EXIT.
      ENDIF.

*** if version 1 is fully compared
      IF lw_fin_flag_1 = 'X'.
        lw_tabix2 = lw_tabix2 + 1.
        EXIT.
      ENDIF.
*** if version 2 is fully compared
      IF lw_fin_flag_2 = 'X'.
        lw_tabix1 = lw_tabix1 + 1.
        EXIT.
      ENDIF.

*** read versoin 2
      READ TABLE g_t_xml_info_1 INTO g_s_xml_info_1 INDEX lw_tabix2.
      IF sy-subrc EQ 0.

        "increment for next cycle
        lw_tabix2 = lw_tabix2 + 1.

        "check if table expty or finished
        IF lw_tabix2 GT lw_num_2.
          lw_fin_flag_2 = 'X'.
        ENDIF.

        "check if new window reached
        READ TABLE lt_headers WITH KEY cname = g_s_xml_info_1-cname TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0 AND lw_loopc_1 GT 1
            AND g_s_xml_info_1-type IS INITIAL.
          lw_fin_flag_2 = 'X'.
          EXIT.
        ENDIF.

        "match the data in the 2 forms.
        IF g_s_xml_info_1-cvalue NE g_s_xml_info-cvalue.

          IF lw_header_print IS INITIAL.

            col-fname = 'STRING1'.
            col-color = color.
            APPEND col TO coltab.
            CLEAR col.

            col-fname = 'STRING2'.
            col-color = color.
            APPEND col TO coltab.
            CLEAR col.

            gst_alv-color  = coltab.

            gst_alv-string1 = 'Window: '.
            gst_alv-string2 = lst_headers-type.
            APPEND gst_alv TO gt_alv.
            CLEAR: gst_alv.

            lw_header_print = 'X'.

          ENDIF.

          "if data is different, then add the difference in the alv
          gst_alv-name    = g_s_xml_info-cname.
          gst_alv-string1 = g_s_xml_info-cvalue.
          gst_alv-string2 = g_s_xml_info_1-cvalue.
          APPEND gst_alv TO gt_alv.
          CLEAR: gst_alv.

        ENDIF.

      ENDIF.

      lw_loopc_1 = lw_loopc_1 + 1.

    ENDLOOP.

    "if form 1 is not finished
    IF lw_fin_flag_1 IS INITIAL.
      LOOP AT g_t_xml_info INTO g_s_xml_info FROM lw_tabix_dup_1.

        READ TABLE lt_headers WITH KEY cname = g_s_xml_info-cname TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          EXIT.
        ENDIF.

        IF lw_header_print IS INITIAL.

          col-fname = 'STRING1'.
          col-color = color.
          APPEND col TO coltab.
          CLEAR col.

          col-fname = 'STRING2'.
          col-color = color.
          APPEND col TO coltab.
          CLEAR col.

          gst_alv-color  = coltab.

          gst_alv-string1 = 'Window: '.
          gst_alv-string2 = lst_headers-type.
          APPEND gst_alv TO gt_alv.
          CLEAR: gst_alv.

          lw_header_print = 'X'.

        ENDIF.

        gst_alv-name    = g_s_xml_info-cname.
        gst_alv-string1 = g_s_xml_info-cvalue.
        gst_alv-string2 = ''.
        APPEND gst_alv TO gt_alv.
        CLEAR: gst_alv.
      ENDLOOP.
    ENDIF.

    "if form 2 is not finished
    IF lw_fin_flag_2 IS INITIAL.
      LOOP AT g_t_xml_info_1 INTO g_s_xml_info_1 FROM lw_tabix2.

        READ TABLE lt_headers WITH KEY cname = g_s_xml_info_1-cname TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          EXIT.
        ENDIF.

        IF lw_header_print IS INITIAL.

          col-fname = 'STRING1'.
          col-color = color.
          APPEND col TO coltab.
          CLEAR col.

          col-fname = 'STRING2'.
          col-color = color.
          APPEND col TO coltab.
          CLEAR col.

          gst_alv-color  = coltab.

          gst_alv-string1 = 'Window: '.
          gst_alv-string2 = lst_headers-type.
          APPEND gst_alv TO gt_alv.
          CLEAR: gst_alv.

          lw_header_print = 'X'.

        ENDIF.

        gst_alv-name    = g_s_xml_info_1-cname.
        gst_alv-string1 = ''.
        gst_alv-string2 = g_s_xml_info_1-cvalue.
        APPEND gst_alv TO gt_alv.
        CLEAR: gst_alv.
      ENDLOOP.
    ENDIF.

    CLEAR: lw_header_print.

  ENDLOOP.

ENDFORM.                    "get_headers

*&---------------------------------------------------------------------*
*&      Form  get_page_data
*&---------------------------------------------------------------------*
*       Get data of pages
*----------------------------------------------------------------------*
FORM get_page_data.

  DATA: lw_tabix1        TYPE sytabix,  "tabix for file 1
        lw_tabix2        TYPE sytabix,  "Tabix for file 2
        lw_tabix_2_start TYPE sytabix,  "Tabix for file 2. pending records
        lw_tabix_2_end   TYPE sytabix,  "Tabix for file 2. pending records
        lw_tab1          TYPE sytabix, "Tab 1 size
        lw_tab2          TYPE sytabix. "Tab 2 size

  TYPES: BEGIN OF lty_xml.
          INCLUDE TYPE smum_xmltb.
  TYPES:  read TYPE char01,     "read flag
          END OF lty_xml.
  DATA: lt_xml_1        TYPE STANDARD TABLE OF lty_xml,
        lt_xml_2        TYPE STANDARD TABLE OF lty_xml,
        lst_xml_1       TYPE lty_xml,
        lst_xml_2       TYPE lty_xml,
        lw_header_print TYPE char01,
        col             TYPE lvc_s_scol,
        coltab          TYPE lvc_t_scol,
        color           TYPE lvc_s_colo.

***  set color
  color-col = '3'.
  color-int = '0'.
  color-inv = '0'.

  "delete header data
  READ TABLE g_t_xml_info WITH KEY cname = 'PAGETREE'
       TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    lw_tabix1 = sy-tabix.
    DELETE g_t_xml_info FROM 1 TO lw_tabix1.
  ENDIF.

  READ TABLE g_t_xml_info_1 WITH KEY cname = 'PAGETREE'
       TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    lw_tabix2 = sy-tabix.
    DELETE g_t_xml_info_1 FROM 1 TO lw_tabix2.
  ENDIF.

***  copare the 2 versions
  CLEAR: lw_tabix1, lw_tabix2.
  DO.

    CLEAR: lt_xml_1, lt_xml_2.

    "exit if no more data
    IF g_t_xml_info IS INITIAL
      OR g_t_xml_info_1 IS INITIAL.
      EXIT.
    ENDIF.

    CLEAR: lw_tabix1, lw_tabix2.

    "build node of version 1
    LOOP AT g_t_xml_info INTO g_s_xml_info.
      IF sy-tabix NE 1
        AND g_s_xml_info-cname EQ 'NODE'.
        lw_tabix1 = sy-tabix.
        EXIT.
      ENDIF.

      lst_xml_1 = g_s_xml_info.
      APPEND lst_xml_1 TO lt_xml_1.
    ENDLOOP.

    "get tab 1 size
    CLEAR: lw_tab1.
    DESCRIBE TABLE lt_xml_1 LINES lw_tab1.
    IF lw_tabix1 IS INITIAL.
      CLEAR: g_t_xml_info.
    ELSE.
      lw_tabix1 = lw_tabix1 - 1.
      DELETE g_t_xml_info FROM 1 TO lw_tabix1.
    ENDIF.

    "get window name from version 1
    CLEAR: lst_xml_1.
    READ TABLE lt_xml_1 INTO lst_xml_1 WITH KEY cname = 'INAME'.

    READ TABLE g_t_xml_info_1 INTO g_s_xml_info_1
         WITH KEY cname  = lst_xml_1-cname
                  cvalue = lst_xml_1-cvalue.
    IF sy-subrc EQ 0.

      lw_tabix_2_start = sy-tabix.
      lw_tabix_2_start = lw_tabix_2_start - 1.

      "get the window in version 2
      DO.
        READ TABLE g_t_xml_info_1 INTO g_s_xml_info_1
             INDEX lw_tabix_2_start.
        IF g_s_xml_info_1-cname EQ 'NODE'.
          EXIT.
        ELSE.
          lw_tabix_2_start  = lw_tabix_2_start - 1.
        ENDIF.

        IF lw_tabix_2_start EQ 0.
          lw_tabix_2_start = 1.
          EXIT.
        ENDIF.
      ENDDO.

    ELSE.

      lw_tabix_2_start = 1.

    ENDIF.

    "build node of version 2
    LOOP AT g_t_xml_info_1 INTO g_s_xml_info_1 FROM lw_tabix_2_start.
      IF sy-tabix NE 1
        AND g_s_xml_info_1-cname EQ 'NODE'.
        lw_tabix2 = sy-tabix.
        EXIT.
      ENDIF.

      lst_xml_2 = g_s_xml_info_1.
      APPEND lst_xml_2 TO lt_xml_2.
    ENDLOOP.

    "get tab 2 size
    CLEAR: lw_tab2.
    DESCRIBE TABLE lt_xml_2 LINES lw_tab2.

    IF lw_tabix2 IS INITIAL.
      CLEAR g_t_xml_info_1.
    ELSE.
      lw_tabix2 = lw_tabix2 - 1.
      DELETE g_t_xml_info_1 FROM lw_tabix_2_start TO lw_tabix2.
    ENDIF.


    CLEAR: lw_tabix1, lw_tabix2.

    "check the window names
    CLEAR: lst_xml_1.
    READ TABLE lt_xml_1 INTO lst_xml_1 WITH KEY cname = 'INAME'.

    "check the window names
    CLEAR: lst_xml_2.
    READ TABLE lt_xml_2 INTO lst_xml_2 WITH KEY cname = 'INAME'.

    IF lst_xml_1-cvalue EQ lst_xml_2-cvalue.

      "compare using loop .. since both are same.

      "window name for display
      col-fname = 'STRING1'.
      col-color = color.
      APPEND col TO coltab.
      CLEAR col.

      col-fname = 'STRING2'.
      col-color = color.
      APPEND col TO coltab.
      CLEAR col.

      gst_alv-color  = coltab.

      gst_alv-string1 = 'Window: '.
      gst_alv-string2 = lst_xml_2-cvalue.
      "end window name for display

      CLEAR: lw_tabix2.
      lw_tabix2 = 1.

      "compare diff
      LOOP AT lt_xml_1 INTO lst_xml_1.

        IF lw_tabix2 LE lw_tab2.

          READ TABLE lt_xml_2 INTO lst_xml_2 INDEX lw_tabix2.
          IF sy-subrc EQ 0
            AND lst_xml_1-cvalue NE lst_xml_2-cvalue .

            "print window name.
            IF lw_header_print IS INITIAL.
              APPEND gst_alv TO gt_alv.
              lw_header_print = 'X'.
            ENDIF.

            "if cname is not item .. there would be some value other diff ..
            "check for that
            IF lst_xml_1-cname NE 'item'.

              CLEAR: lst_xml_2.
              READ TABLE lt_xml_2 INTO lst_xml_2 WITH KEY cname = lst_xml_1-cname.
              IF lst_xml_1-cvalue EQ lst_xml_2-cvalue.
                CONTINUE.
              ENDIF.

            ENDIF.

            CLEAR: gst_alv.
            gst_alv-name = lst_xml_1-cname.
            gst_alv-string1 = lst_xml_1-cvalue.
            gst_alv-string2 = lst_xml_2-cvalue.
            APPEND gst_alv TO gt_alv.

          ENDIF.

        ENDIF.

        lw_tabix2 = lw_tabix2 + 1.

      ENDLOOP.

      "print extra records
      IF lw_tabix2 LE lw_tab2.

        "print window name.
        IF lw_header_print IS INITIAL.
          APPEND gst_alv TO gt_alv.
          lw_header_print = 'X'.
        ENDIF.

        LOOP AT lt_xml_2 INTO lst_xml_2 FROM lw_tabix2.

          CLEAR: gst_alv.
          gst_alv-name = lst_xml_2-cname.
          gst_alv-string1 = ''.
          gst_alv-string2 = lst_xml_2-cvalue.
          APPEND gst_alv TO gt_alv.

        ENDLOOP.

      ENDIF.

    ELSE.

      "show both the windows individually
      "print file 1

      "get window name
      CLEAR: lst_xml_1.
      READ TABLE lt_xml_1 INTO lst_xml_1 WITH KEY cname = 'INAME'.

      "window name for display
      col-fname = 'STRING1'.
      col-color = color.
      APPEND col TO coltab.
      CLEAR col.

      col-fname = 'STRING2'.
      col-color = color.
      APPEND col TO coltab.
      CLEAR col.

      gst_alv-color  = coltab.

      gst_alv-string1 = 'Window: '.
      gst_alv-string2 = lst_xml_1-cvalue.
      APPEND gst_alv TO gt_alv.
      "end window name for display

      LOOP AT lt_xml_1 INTO lst_xml_1.

        CLEAR: gst_alv.
        gst_alv-name = lst_xml_1-cname.
        gst_alv-string1 = lst_xml_1-cvalue.
        gst_alv-string2 = ''.
        APPEND gst_alv TO gt_alv.

      ENDLOOP.

      "print file 2
      "get window name
      CLEAR: lst_xml_2.
      READ TABLE lt_xml_2 INTO lst_xml_2 WITH KEY cname = 'INAME'.

      "window name for display
      col-fname = 'STRING1'.
      col-color = color.
      APPEND col TO coltab.
      CLEAR col.

      col-fname = 'STRING2'.
      col-color = color.
      APPEND col TO coltab.
      CLEAR col.

      gst_alv-color  = coltab.

      gst_alv-string1 = 'Window: '.
      gst_alv-string2 = lst_xml_2-cvalue.
      APPEND gst_alv TO gt_alv.
      "end window name for display

      LOOP AT lt_xml_2 INTO lst_xml_2.

        CLEAR: gst_alv.
        gst_alv-name = lst_xml_2-cname.
        gst_alv-string1 = ''.
        gst_alv-string2 = lst_xml_2-cvalue.
        APPEND gst_alv TO gt_alv.

      ENDLOOP.

    ENDIF.


    CLEAR: lw_header_print.

  ENDDO.

  "add data of file 1
  LOOP AT g_t_xml_info INTO g_s_xml_info.

    CLEAR: gst_alv.
    gst_alv-name = g_s_xml_info-cname.
    gst_alv-string1 = g_s_xml_info-cvalue.
    gst_alv-string2 = ''.
    APPEND gst_alv TO gt_alv.

  ENDLOOP.

  "add data of file 2
  LOOP AT g_t_xml_info_1 INTO g_s_xml_info_1.

    CLEAR: gst_alv.
    gst_alv-name = g_s_xml_info_1-cname.
    gst_alv-string1 = ''.
    gst_alv-string2 = g_s_xml_info_1-cvalue..
    APPEND gst_alv TO gt_alv.

  ENDLOOP.

ENDFORM.                    "get_page_data
