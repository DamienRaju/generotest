------------------------------------------------------------------------------------------------------------------------------------
#+ Server Maintenance
#+
#+ Author: Damien Raju
#+
#+ Allows user to maintain server details
#+
#+ MENU ACTIONS
#+
#+ CHANGES
#+
IMPORT os
SCHEMA xactdev
&include "sy_lib.inc"
--	DECLARATION OF CONSTANT VARIABLES
	CONSTANT	m_prog  			= "dx101_server_mast",
				m_ver_no    		= "Ver 1.00"
--==================================================================================================================================
 -- Variables with type specified
	DEFINE  	m_prog_ver          STRING, 		-- Store Program Name & Version No.
				m_key_val_to_find	STRING,
				m_rec_is_displayed  SMALLINT,
				m_ok_to_update		BOOLEAN,

				m_cr                INTEGER

	-- Define all records and ARRAY
    DEFINE  	mr_dx01s			RECORD LIKE dx01s_server_mast.*,
				ma_dx01d			DYNAMIC ARRAY OF RECORD LIKE dx01d_db_mast.*,
				mab_dx01d			DYNAMIC ARRAY OF RECORD LIKE dx01d_db_mast.*

--==================================================================================================================================
MAIN 
	DEFINE l_err_src 	    	STRING

	LET m_prog_ver = m_prog||" - "||m_ver_no
	LET l_err_src = m_prog_ver || " > MAIN"

	LET g_schema_name	= ARG_VAL(1)
	LET g_user_name		= ARG_VAL(2)

	CALL lf_prog_init(	{prog_name}     	m_prog,
						{form_name}     	m_prog,
						{ver_no}        	m_ver_no,
						{child}         	TRUE,
						{test_prog_access}	TRUE,
						{err_src}			l_err_src	)

	CALL sb_module_init(l_err_src)
	
	CALL main_menu( l_err_src )

END MAIN

--==================================================================================================================================
FUNCTION main_menu( p_err_src )

	DEFINE 	p_err_src 			STRING

    LET p_err_src = p_err_src, " > Menu"

	MENU
		BEFORE MENU 

			CALL sb_setup_menu_actions(m_rec_is_displayed, "other", p_err_src )

		ON ACTION CREATE
			INITIALIZE mr_dx01s.* TO NULL
			CALL cl_clear_form_and_recs( p_err_src )
			CALL w_main_input_rec( "create", p_err_src ) RETURNING m_rec_is_displayed
			CALL sb_setup_menu_actions(m_rec_is_displayed, "other", p_err_src )

        ON ACTION find
			INITIALIZE mr_dx01s.* TO NULL

			LET m_key_val_to_find  = lf_lookup(	{tbl_name}          "dx01s_server_mast",
												{cols}              "server",
												{col_titles}        "Server",
												{fld_att}           "V15",
												{flds_to_search}    "server",
												{flds_to_return}	"1",
												{input_string}      NULL,
												{sub_qry}           NULL,
												{ord_by}            "server",
												{win_title}         "Server Lookup",
												{err_src}			p_err_src )

			IF ( m_key_val_to_find IS NULL ) THEN
				CALL cl_clear_form_and_recs( p_err_src )
				LET m_rec_is_displayed = FALSE
				
			ELSE
				LET m_rec_is_displayed = bld_and_disp_dx01s_rec ( m_key_val_to_find, p_err_src )
				LET m_rec_is_displayed = bld_and_display_dx01d ( m_key_val_to_find, p_err_src )	
				
			END IF
			CALL sb_setup_menu_actions(m_rec_is_displayed, "other", p_err_src )

		ON ACTION next
   			CALL lf_next_prev_rec( "next", "dx01s_server_mast", "server", m_key_val_to_find, NULL, p_err_src )  RETURNING m_rec_is_displayed, m_key_val_to_find
            
            IF ( m_rec_is_displayed ) THEN--Tests whether rec was found
				LET m_rec_is_displayed = bld_and_disp_dx01s_rec ( m_key_val_to_find, p_err_src )
				LET m_rec_is_displayed = bld_and_display_dx01d ( m_key_val_to_find, p_err_src )
            END IF                            
        
            CALL sb_setup_menu_actions(m_rec_is_displayed, "next", p_err_src )
                    
		ON ACTION prev
    		CALL lf_next_prev_rec ( "prev", "dx01s_server_mast", "server", m_key_val_to_find, NULL, p_err_src) RETURNING m_rec_is_displayed, m_key_val_to_find
            
   			IF ( m_rec_is_displayed ) THEN--Tests whether rec was found
				LET m_rec_is_displayed = bld_and_disp_dx01s_rec ( m_key_val_to_find, p_err_src ) 
				LET m_rec_is_displayed = bld_and_display_dx01d ( m_key_val_to_find, p_err_src )
			END IF

            CALL sb_setup_menu_actions(m_rec_is_displayed, "prev", p_err_src )
            
            IF ( m_rec_is_displayed ) THEN 
                NEXT OPTION "prev"
			ELSE
				NEXT OPTION "next"
			END IF		

		ON ACTION amend
			CALL w_main_input_rec( "amend", p_err_src ) RETURNING m_rec_is_displayed

		ON ACTION DELETE
			CALL delete_dx01s( p_err_src )

		ON ACTION EXIT
            IF ( lf_exit_program( TRUE, p_err_src ) ) THEN
                EXIT MENU
				
            END IF
            NEXT OPTION "exit"

    END MENU

	CALL lf_exit_program( FALSE, p_err_src )

END FUNCTION
{==================================================================================================================================}
FUNCTION CLEAR_DATA_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ CLEAR FORM 
#+
#+ BUSINESS RULE:
#+ Clear the form and all modular variables that need to be set to NULL
#+
#+ @code CALL cl_clear_form_and_recs( p_err_src )
#+
#+ @param p_err_src Error Source Tracking
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION cl_clear_form_and_recs( p_err_src )

	DEFINE  p_err_src 	STRING

	LET  p_err_src =  p_err_src || " > cl_clear_form_and_recs"

	CLEAR FORM

	INITIALIZE mr_dx01s TO NULL
	LET m_rec_is_displayed = FALSE

END FUNCTION
{==================================================================================================================================}
{													END CLEAR FUNCTIONS															   }
{==================================================================================================================================}

{==================================================================================================================================}
{													BUILD FUNCTIONS																   } 
{==================================================================================================================================}
FUNCTION BUILD_FUNCTIONS()
END FUNCTION
#+==================================================================================================================================
#+ DISPLAY TO FORM
#+
#+ BUSINESS RULE:
#+ Displays the dx01s record dependant on the Key value being passed through
#+
#+ @code CALL bld_and_disp_dx01s_rec( p_key_doc_no, p_err_src )
#+
#+ @param p_key_val_to_find Value used to determin select
#+ @param p_err_src 			Error Source Tracking
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION bld_and_disp_dx01s_rec( p_key_val_to_find, p_err_src )

	DEFINE  p_key_val_to_find	LIKE dx01s_server_mast.server,
            p_err_src			STRING,
			l_sql				STRING

	LET  p_err_src =  p_err_src || " > bld_and_disp_dx01s_rec "		

	LET l_sql = "SELECT * FROM dx01s_server_mast WHERE server = '" || p_key_val_to_find ||"'"

	TRY
	--Insert Parent Record

		PREPARE sel_dx01s_stment FROM l_sql
		EXECUTE sel_dx01s_stment INTO mr_dx01s.*
		
		DISPLAY BY NAME mr_dx01s.*
		RETURN TRUE 

	CATCH
		LET p_err_src = lf_sql_error ( p_err_src, mr_dx01s.server )
		CALL fgl_winmessage( %"Error", p_err_src, "stop" )
		RETURN FALSE

	END TRY

END FUNCTION
#+==================================================================================================================================
#+ DISPLAY TO FORM
#+
#+ BUSINESS RULE:
#+ Displays the dx01s record dependant on the Key value being passed through
#+
#+ @code CALL bld_and_display_dx01d( p_key_val_to_find, p_err_src )
#+
#+ @param p_key_val_to_find 	Value used to determin select
#+ @param p_err_src 			Error Source Tracking
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION bld_and_display_dx01d( p_key_val_to_find, p_err_src )

	DEFINE  p_key_val_to_find	LIKE dx01d_db_mast.server,
            p_err_src			STRING,
			
			l_cnt				INTEGER,
			l_sql				STRING

	LET  p_err_src =  p_err_src || " > bld_and_display_dx01d "		

	LET l_sql = "SELECT * FROM dx01d_db_mast WHERE server = '" || p_key_val_to_find ||"'"

	TRY
	--Insert Parent Record
		CALL ma_dx01d.CLEAR()
		CALL mab_dx01d.CLEAR()
		
		LET l_cnt = 1
		
		PREPARE sel_dx01d_stment FROM l_sql
		DECLARE curs_dx01d CURSOR FOR sel_dx01d_stment

		FOREACH curs_dx01d INTO ma_dx01d[l_cnt].*
			LET l_cnt = l_cnt + 1
		END FOREACH
		CALL ma_dx01d.deleteelement(l_cnt)
		
		FOR l_cnt = 1 TO ma_dx01d.getLength()
			LET mab_dx01d[l_cnt].* = ma_dx01d[l_cnt].* 
		END FOR 
			
		DISPLAY ARRAY ma_dx01d TO tbl_db.*
			BEFORE DISPLAY
				EXIT DISPLAY
		END DISPLAY

		RETURN TRUE 

	CATCH
		LET p_err_src = lf_sql_error ( p_err_src, p_key_val_to_find )
		CALL fgl_winmessage( %"Error", p_err_src, "stop" )
		RETURN FALSE

	END TRY

END FUNCTION
{==================================================================================================================================}
{													END BUILD FUNCTIONS														       }
{==================================================================================================================================}

--##################################################################################################################################
{==================================================================================================================================}
{													INPUT FUNCTIONS																   }
{==================================================================================================================================}
FUNCTION INPUT_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ INPUT [mr_dx01s] RECORD
#+
#+ Enter additional detailed description
#+
#+ @code CALL w_main_input ( p_action_selected, p_err_src ) RETURNING TRUE/FALSE
#+
#+ @param p_action_selected e.g. "create" or "amend", etc.
#+ @param p_err_src Error Source from calling statement
#+
#+ @return TRUE Input was VALID and record created or updated
#+
#+ @return FALSE Input was INVALID and record NOT created or updated
#+
#+ CHANGES
#+
FUNCTION w_main_input_rec( p_action_selected, p_err_src )

    DEFINE  p_action_selected 	VARCHAR(10),
			p_err_src 			STRING,
			
			l_dlog				UI.DIALOG,
			
			cont_dlog	   	BOOLEAN,
            l_accept_dlog   BOOLEAN,
            l_del_msg       STRING

	LET p_err_src = p_err_src || " > w_main_input_rec"

	LET m_ok_to_update = TRUE 
	LET l_dlog = ui.dialog.getcurrent()

	--IF( action_selected = "create" )THEN
		--LET mr_dx01s.unit_qty		= 0
		--LET mr_dx01s.levy_cost		= 0
	--END IF

	DIALOG ATTRIBUTES ( FIELD ORDER FORM, UNBUFFERED )

		INPUT BY NAME mr_dx01s.* ATTRIBUTE ( WITHOUT DEFAULTS )
			BEFORE INPUT

				IF ( p_action_selected = "amend" ) THEN
					CALL DIALOG.setFieldActive( "server",		FALSE )
				END IF

			AFTER FIELD server
                CALL w_main_is_field_valid ( "server", p_action_selected, p_err_src )

			AFTER FIELD port
                CALL w_main_is_field_valid ( "port", p_action_selected, p_err_src )

			AFTER FIELD ip
                CALL w_main_is_field_valid ( "ip", p_action_selected, p_err_src )	

		END INPUT

		----------------------------------------------------------------------------------------
        DISPLAY ARRAY ma_dx01d TO tbl_db.*
        
            BEFORE DISPLAY
                CALL DIALOG.setactionhidden( "up_arw",      TRUE )
                CALL DIALOG.setactionhidden( "down_arw",    TRUE )
            --If array is blank create new row 
                IF ( ma_dx01d.getLength() = 0 ) THEN
                    LET m_cr = 1
                    CALL ma_dx01d.insertElement(m_cr)
                END IF 

			----------------------------------------------------------------------------------------
            BEFORE ROW
                LET m_cr = ARR_CURR()
                IF  ( ma_dx01d[m_cr].db IS NULL ) THEN
                    CALL DIALOG.setActionActive( "delete_row", FALSE )
                ELSE 
                    CALL DIALOG.setActionActive( "delete_row", TRUE  )
                END IF 

			----------------------------------------------------------------------------------------
			AFTER ROW
                LET m_cr = ARR_CURR()
            --remove a blank row from the table
                IF ( ma_dx01d[m_cr].db IS NULL ) THEN
                    CALL ma_dx01d.deleteElement(m_cr)
                END IF 
            --on accept exit dialog 
				IF ( l_accept_dlog ) THEN
					EXIT DIALOG
				END IF
                
            ------------------------------------------------------------------------------------
            ON ACTION edit_row
                LET m_cr = ARR_CURR()
                IF w_input_db_line( p_err_src ) THEN
                --If  current focus is not in the last row push focus to the next row if multi bank account is ticked
                    IF ( ( m_cr < ma_dx01d.getLength() ) OR ( m_cr = ma_dx01d.getLength() AND ma_dx01d[m_cr].db IS NOT NULL ) ) THEN
                    --put user on blank row below
                        IF ( m_cr = ma_dx01d.getLength() ) THEN 
                            CALL ma_dx01d.insertelement( m_cr + 1)
                        END IF 
                        LET m_cr = m_cr + 1
                        CALL fgl_set_arr_curr(m_cr)
                    END IF 
                END IF
                
            ------------------------------------------------------------------------------------
            ON ACTION delete_row
                LET m_cr = ARR_CURR()
                IF m_cr <= 0 THEN
                    CONTINUE DIALOG
                END IF

                IF ( ma_dx01d[m_cr].db IS NULL ) THEN
                    CALL fgl_winmessage( %"Invalid Input", %"Blank Row cannot be deleted", "exclamation" )
                    CONTINUE DIALOG
                END IF
            
                IF ( ma_dx01d[m_cr].db IS NULL ) AND (( m_cr = 1 ) OR ( m_cr = ma_dx01d.getlength() )) THEN					
                    CONTINUE DIALOG
                END IF

            --Setup the prompt message for delete
                IF ( ma_dx01d[m_cr].db IS NOT NULL ) THEN
                    LET l_del_msg = %"Are you sure you want to delete row item '" || ma_dx01d[m_cr].db || "'?"
                ELSE
                    LET l_del_msg = %"Are you sure you want to delete this blank line?"
                END IF
            
                IF ( fgl_winbutton( %"Delete Row", l_del_msg, "No", %"Yes|No", "question", 0 ) = %"Yes" ) THEN                        
                --remove row from the array
                    CALL ma_dx01d.deleteElement(m_cr)
                END IF
                
            --if only one line item was in the array, and was deleted, then a new blank row is created.
                IF ma_dx01d.getlength() = 0 THEN
                    CALL ma_dx01d.insertelement( 1 )
                END IF

            ------------------------------------------------------------------------------------
            ON ACTION up_arw
                IF arr_curr() = 1 THEN
                    CALL DIALOG.nextfield( "db" )
                ELSE
                    CALL fgl_set_arr_curr( arr_curr()-1 )
                END IF

            ON ACTION down_arw
            --If  current focus is not in the last row push focus to the next row if multi bank account is ticked
                IF ( ( m_cr < ma_dx01d.getLength() ) OR ( m_cr = ma_dx01d.getLength() AND ma_dx01d[m_cr].db IS NOT NULL ) ) THEN
                --put user on blank row below
                    IF ( m_cr = ma_dx01d.getLength() ) THEN 
                        CALL ma_dx01d.insertelement( m_cr + 1)
                    END IF 
                    LET m_cr = m_cr + 1
                    CALL fgl_set_arr_curr(m_cr)
                END IF 

        END DISPLAY

		ON ACTION ACCEPT
			LET m_ok_to_update = FALSE
			CALL w_main_is_field_valid( "all" , p_action_selected, p_err_src )
			
			IF ( m_ok_to_update = TRUE ) THEN
				EXIT DIALOG
			ELSE
				CONTINUE DIALOG
			END IF
			
		-------------------------------------------------------------------------------------------
        ON ACTION CANCEL
			IF ( sy_winbutton( %"Cancel Inputs", %"Cancel All Inputs?", %"No", %"Yes|No", "question", 0 ) = %"No" ) THEN
				CONTINUE DIALOG
			END IF
			
			MESSAGE %"Canceled by user"

			LET m_ok_to_update = FALSE

			IF ( mr_dx01s.server IS NOT NULL ) AND ( p_action_selected = "amend" ) THEN
				CALL bld_and_disp_dx01s_rec( mr_dx01s.server, p_err_src ) RETURNING g_not_used
				CALL bld_and_display_dx01d ( mr_dx01s.server, p_err_src ) RETURNING g_not_used
			ELSE
				CALL cl_clear_form_and_recs( p_err_src )

				CALL l_dlog.setactionactive("amend",			FALSE)
				CALL l_dlog.setactionactive("delete",			FALSE)
			END IF
			
			RETURN FALSE

		EXIT DIALOG
			
	END DIALOG

	IF ( m_ok_to_update = TRUE ) THEN

		IF ( upd_dx01s ( p_action_selected, p_err_src ) ) THEN
			RETURN TRUE
		END IF

	END IF

END FUNCTION

FUNCTION w_input_db_line( p_err_src )

	DEFINE	p_err_src           STRING,
			l_accept_dlog       BOOLEAN

	DEFINE	lr_dx01d           RECORD LIKE dx01d_db_mast.*

	LET p_err_src = p_err_src , " > w_input_db_line"

--Backup the current record 
	LET lr_dx01d.* = ma_dx01d[m_cr].*

--User Input
	DIALOG ATTRIBUTES( FIELD ORDER FORM, UNBUFFERED )

		INPUT ma_dx01d[m_cr].* FROM tbl_db[scr_line(){m_cr}].* ATTRIBUTES( WITHOUT DEFAULTS = TRUE )
		-----------------------------------------------------------------------------------------------
			BEFORE INPUT

		-----------------------------------------------------------------------------------------------
			AFTER FIELD db
                CALL w_main_is_dx01d_field_valid( "db", p_err_src ) RETURNING g_not_used
                
		-----------------------------------------------------------------------------------------------
			AFTER INPUT
                IF ( w_main_is_dx01d_field_valid( "all", p_err_src ) = TRUE ) THEN 
                    LET l_accept_dlog = TRUE
                    EXIT DIALOG
                END IF 
		
		END INPUT
	-----------------------------------------------------------------------------------------------
		ON ACTION ACCEPT
			LET l_accept_dlog = TRUE
			ACCEPT DIALOG

	-----------------------------------------------------------------------------------------------
		ON ACTION CANCEL
			LET l_accept_dlog = FALSE
			LET ma_dx01d[m_cr].* = lr_dx01d.*
			EXIT DIALOG
	
	END DIALOG

	RETURN l_accept_dlog

END FUNCTION
--==================================================================================================================================
#+ TEST & VALIDATE MAIN_CHG RECORD INPUT
#+
#+ BUSINESS RULE:
#+ Test and validate Main Record data AFTER INPUT.
#+ If field_name = "ALL" then test all fields and prompt if Details Correct
#+
#+ @code CALL w_main_is_field_valid ( p_field_name, p_action_selected, p_err_src )
#+
#+ @param p_field_name
#+ @param p_action_selected e.g. "create" or "amend", etc.
#+ @param p_err_src Error Source from calling statement
#+
#+ @return NONE
#+
#+ CHANGES
FUNCTION w_main_is_field_valid( p_field_name, p_action_selected, p_err_src )

	DEFINE  p_field_name  			STRING,
			p_action_selected		VARCHAR(10),
			p_err_src 				STRING,
			
			l_dlog					ui.dialog

	LET l_dlog = ui.dialog.getcurrent()
	LET p_err_src = p_err_src , " > w_main_is_field_valid"

	IF ( p_field_name = "server" ) OR ( p_field_name = "all" ) THEN

		IF ( mr_dx01s.server IS NULL ) THEN
			CALL fgl_winmessage( %"Invalid Input", %"Please enter a Server.", "exclamation" )
			CALL l_dlog.nextField( "server" )
			RETURN 

		END IF

		IF ( lf_row_exists("dx01s_server_mast", "server = '" || mr_dx01s.server || "'", p_err_src) ) THEN
			IF ( p_action_selected <> "amend" ) THEN
				CALL fgl_winmessage( %"Invalid Input", %"This server already exists", "exclamation" )
				CALL l_dlog.nextField( "server" )
				RETURN
			END IF
		END IF

		IF ( lf_invalid_chars_in_string ( mr_dx01s.server, "C" ) ) THEN 
		    CALL l_dlog.nextfield ( "server" )
            RETURN 
        END IF

	END IF

	IF ( p_field_name = "port" ) OR ( p_field_name = "all" ) THEN
		
		IF ( mr_dx01s.port IS NULL ) THEN
			CALL fgl_winmessage( %"Invalid Input", %"The port number cannot be blank.", "exclamation")
			CALL l_dlog.nextField( "port" )
			RETURN
			
		END IF
		
		IF ( lf_invalid_chars_in_string ( mr_dx01s.port, "N" ) ) THEN 
			CALL l_dlog.nextField( "port" )
			RETURN
			
		END IF

		IF ( mr_dx01s.port < 0 ) THEN
			CALL fgl_winmessage( %"Invalid Input", %"Port number cannot be less than zero.", "exclamation" )
			CALL l_dlog.nextField( "port" )
			RETURN
			
		END IF
		
	END IF

	IF ( p_field_name = "ip" ) OR ( p_field_name = "all" ) THEN
	
		IF ( mr_dx01s.ip IS NULL ) THEN
			CALL fgl_winmessage( %"Invalid Input", %"The IP cannot be blank.", "exclamation")
			CALL l_dlog.nextField( "ip" )
			RETURN
			
		END IF

	END IF

	IF ( p_field_name = "all" ) THEN
        
		IF ( fgl_winbutton( %"Accept Details", %"Are Details Correct", %"No", %"Yes|No", "question", 0) = %"Yes" )  THEN
            LET m_ok_to_update = TRUE		
            RETURN

		ELSE
			CALL l_dlog.nextField( "server" )
			
		END IF
        
	END IF

	RETURN

END FUNCTION

--==================================================================================================================================
#+ TEST & VALIDATE MAIN_CHG RECORD INPUT
#+
#+ BUSINESS RULE:
#+ Test and validate Main Record data AFTER INPUT.
#+ If field_name = "ALL" then test all fields and prompt if Details Correct
#+
#+ @code CALL w_main_is_dx01d_field_valid ( p_field_name, p_action_selected, p_err_src )
#+
#+ @param p_field_name
#+ @param p_action_selected e.g. "create" or "amend", etc.
#+ @param p_err_src Error Source from calling statement
#+
#+ @return NONE
#+
#+ CHANGES
FUNCTION w_main_is_dx01d_field_valid( p_field_name, p_err_src )

	DEFINE  p_field_name  			STRING,
			p_err_src 				STRING,
			
			l_dlog					ui.DIALOG,
			l_cnt					INTEGER

	LET l_dlog = ui.dialog.getcurrent()
	LET p_err_src = p_err_src , " > w_main_is_dx01d_field_valid"

	IF ( p_field_name = "db" ) OR ( p_field_name = "all" ) THEN
	
		IF ( ma_dx01d[m_cr].db IS NULL ) THEN
			CALL fgl_winmessage( %"Invalid Input", %"The DB cannot be blank.", "exclamation")
			CALL l_dlog.nextField( "db" )
			RETURN FALSE
			
		END IF

		FOR l_cnt = 1 TO ma_dx01d.getLength()
			IF ( l_cnt <> m_cr ) THEN
				IF ( ma_dx01d[l_cnt].db = ma_dx01d[m_cr].db ) THEN
					CALL sy_winmessage("Invalid Input","DB is already in the list!\nPlease Enter in a different DB","exclamation")
					CALL l_dlog.nextfield( "db" )
					CALL ui.interface.refresh()
					RETURN FALSE
				END IF 
			END IF 
		END FOR

	END IF

	RETURN TRUE

END FUNCTION

{==================================================================================================================================}
{													END INPUT FUNCTIONS														       }
{==================================================================================================================================}

--##################################################################################################################################
{==================================================================================================================================}
{										    		UPDATE FUNCTION																   }
{==================================================================================================================================}
FUNCTION UPDATE_FUNCTION()
END FUNCTION
{==================================================================================================================================}
#+ UPDATE dx01s
#+
#+ BUSINESS RULE:
#+ Update or insert into dx01s dependant on action selected
#+
#+ @code CALL upd_dx01s ( p_action_selected, p_err_src )
#+
#+ @param p_action_selected Value of ACTION selected e.g. CREATE or FIND or NEXT or etc.
#+ @param p_err_src Error Source from calling statement
#+
#+ @return TRUE  if success
#+ @return FALSE if fail
#+
#+ CHANGES
#+
FUNCTION upd_dx01s( p_action_selected, p_err_src )

	DEFINE  p_action_selected     	VARCHAR(8),
			p_err_src 				STRING,
			
			l_upd_sql				STRING

	LET  p_err_src =  p_err_src || " > upd_dx01s "
	
	CASE p_action_selected

		WHEN ( "amend" )

			LET l_upd_sql = "UPDATE "								||
									"dx01s_server_mast "			||
								"SET "								||
									"port = '"						|| mr_dx01s.port		||"', "||
									"ip = '"						|| mr_dx01s.ip			||"' "||
								"WHERE "							||
									"server = '"					|| mr_dx01s.server		||"'"
												
			EXECUTE IMMEDIATE 	l_upd_sql
			
		WHEN ( "create" )
		-- Assign Any Program Generated Values here before Insert
			TRY
				BEGIN WORK				
				--Insert Parent Record				
                INSERT INTO dx01s_server_mast VALUES ( mr_dx01s.* )		     						
                COMMIT WORK

            CATCH            
                LET p_err_src = lf_sql_error ( p_err_src, mr_dx01s.server )                				
				ROLLBACK WORK				
				CALL sy_winmessage( %"Error", p_err_src, "stop" )
                RETURN FALSE
            END TRY 
			
	END CASE

	IF ( upd_dx01d( p_err_src ) = FALSE ) THEN
		RETURN FALSE
	END IF

	RETURN TRUE

END FUNCTION 

----------------------------------------------------------------------------------------------------
#+ UPDATE RECORD AFTER INPUT
#+
#+
#+ BUSINESS RULE: #+
#+ Writes data inputed to the database or updates existing data depending on
#+ action selected
#+
#+ @code    CALL upd_dx01d ( perr_src ) RETURNING TRUE/FALSE 	
#+
#+ @param   p_err_src #+
#+
#+ @return  TRUE		Update was successful
#+ @return  FALSE		Update was NOT successful
#+
#+ CHANGES
#+
FUNCTION upd_dx01d( p_err_src )
	
	DEFINE  p_err_src	STRING,
	
            l_cnt       INTEGER,
			lr_dx01d	RECORD LIKE dx01d_db_mast.*

    LET p_err_src = p_err_src, " > upd_dx01d"

	TRY
    
        BEGIN WORK

    --Row Reversal for DBs on update
        FOR l_cnt = 1 TO mab_dx01d.getLength()
			DELETE FROM dx01d_db_mast WHERE server = mr_dx01s.server AND db = mab_dx01d[l_cnt].db
		END FOR 
        
    --Row Update for DBs on update
        FOR l_cnt = 1 TO ma_dx01d.getLength()
		
			IF ( ma_dx01d[l_cnt].db IS NULL ) THEN
				CONTINUE FOR
			END IF

			LET lr_dx01d.server	= mr_dx01s.server
			LET lr_dx01d.db 	= ma_dx01d[l_cnt].db
			
			INSERT INTO dx01d_db_mast VALUES (lr_dx01d.*)

		END FOR

    --Backup the dx01d rows
        CALL mab_dx01d.clear()
        FOR l_cnt = 1 TO ma_dx01d.getLength()
            LET mab_dx01d[l_cnt].* = ma_dx01d[l_cnt].*
        END FOR 
        
		COMMIT WORK

		RETURN TRUE
        
    CATCH
        LET p_err_src = lf_sql_error(p_err_src, "Server Database Details")
		ROLLBACK WORK
		CALL sy_winmessage( %"Error", p_err_src, "stop" )
		RETURN FALSE
    END TRY
	
END FUNCTION
{==================================================================================================================================}
{										    		END UPDATE FUNCTION															   }
{==================================================================================================================================}

--##################################################################################################################################
{==================================================================================================================================}
{													DELETE FUNCTION																   }
{==================================================================================================================================}
FUNCTION DELETE_FUNCTION()
END FUNCTION 
{==================================================================================================================================}
#+ DELETE RECORD
#+
#+ BUSINESS RULE:
#+ Deletes a parent record. If the delete fails the transaction is closed.
#+
#+ @code CALL delete_dx01s( p_err_src )
#+
#+ @param p_err_src Error Source from calling statement
#+
#+ @return  TRUE
#+
#+ CHANGES
#+
FUNCTION delete_dx01s( p_err_src )

	DEFINE 	p_err_src   	STRING,
	
			l_sql_dx01s		STRING,
			l_sql_dx01d		STRING

	LET  p_err_src =  p_err_src || " > delete_dx01s "
	
	LET l_sql_dx01s = "DELETE FROM dx01s_server_mast WHERE server = '" || mr_dx01s.server ||"'"
	LET l_sql_dx01d = "DELETE FROM dx01d_db_mast WHERE server = '" || mr_dx01s.server ||"'"

	IF ( sy_winbutton( %"Confirm", %"Are you sure you want to delete this server?", %"No", %"Yes|No", "question", 0 ) = %"Yes" ) THEN
		TRY
	
			PREPARE del_stment_dx01s FROM l_sql_dx01s
			EXECUTE del_stment_dx01s

			PREPARE del_stment_dx01d FROM l_sql_dx01d
			EXECUTE del_stment_dx01d
			
			CALL cl_clear_form_and_recs( p_err_src )
			CALL sb_setup_menu_actions(m_rec_is_displayed, "other", p_err_src )
			
		CATCH
			LET p_err_src = lf_sql_error ( p_err_src, mr_dx01s.server )
			CALL fgl_winmessage( %"Error", p_err_src, "stop" )
			
		END TRY
	END IF
	
 END FUNCTION
--##################################################################################################################################
{==================================================================================================================================}
{										    		GENERAL FUNCTIONS															   }
{==================================================================================================================================}
FUNCTION GENERAL_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ SETUP ACTIONS BUTTONS
#+
#+ BUSINESS RULE:
#+ Set the BUTTON state to Visable or Greyed depending on condition of m_rec_is_displayed
#+
#+ @code CALL sb_setup_menu_actions ( p_rec_is_displayed, p_action_dislayed, p_err_src )
#+
#+ @param p_rec_is_displayed TRUE = There is a valid record displayed -or- FALSE = There is no record displayed
#+ @param p_action_dislayed Value of ACTION selected e.g. CREATE or FIND or NEXT or etc.
#+ @param p_err_src Error Source from calling statement
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION sb_setup_menu_actions(p_rec_is_displayed, p_action_dislayed, p_err_src )

	DEFINE  p_rec_is_displayed		SMALLINT,
			p_action_dislayed 		VARCHAR(8),
			p_err_src 				STRING,
			
			l_dlog					ui.Dialog
		
	LET l_dlog    = ui.dialog.getcurrent()	
	LET p_err_src = p_err_src || " > sb_setup_menu_actions"
	
	
    CALL l_dlog.setactionactive("create", TRUE)
	CALL l_dlog.setactionactive("find",   TRUE)
	CALL l_dlog.setactionactive("next",   TRUE)
	CALL l_dlog.setactionactive("prev",   TRUE)
	CALL l_dlog.setactionactive("amend",  TRUE)
	CALL l_dlog.setactionactive("delete", TRUE)
	
	IF ( NOT p_rec_is_displayed ) THEN -- when a record is displayed on the form
		CASE
			WHEN ( p_action_dislayed = "other" )
				CALL l_dlog.setactionactive("amend",  FALSE)
				CALL l_dlog.setactionactive("delete", FALSE)
            
				
			WHEN ( p_action_dislayed = "next" OR p_action_dislayed = "prev" )
												 				
				CASE ( lf_row_exists ( "dx01s_server_mast", NULL, p_err_src ) ) -- test if it works  test if any records in database										
					WHEN ( TRUE )    											-- reached the last(NEXT) or first (PREV) record in non-empty file
						CASE p_action_dislayed 													
							WHEN "next"						
								CALL l_dlog.setactionactive("next",FALSE)								
								
							WHEN "prev"						
								CALL l_dlog.setactionactive("prev", FALSE)
						END CASE				
					
					WHEN ( FALSE )     	--If condition empty file
						CALL l_dlog.setactionactive("next",   FALSE)
						CALL l_dlog.setactionactive("prev",   FALSE)
						CALL l_dlog.setactionactive("amend",  FALSE)
						CALL l_dlog.setactionactive("delete", FALSE)
	
				END CASE
		END CASE	

	END IF
END FUNCTION
{==================================================================================================================================}
#+ MODULE INIT
#+
#+ BUSINESS RULE: #+ 
#+ Setup any module specific tasks not done in lf_progr_init
#+
#+
#+ @code    CALL module_init ( p_err_src )
#+
#+ @param   p_err_src
#+
#+ @return  NONE
#+
#+ CHANGES
#+	
FUNCTION sb_module_init( p_err_src )

	DEFINE	p_err_src				STRING

	LET p_err_src = p_err_src || " > module_init"


END FUNCTION
{==================================================================================================================================}
{										    		END GENERAL FUNCTION														   }
{==================================================================================================================================}