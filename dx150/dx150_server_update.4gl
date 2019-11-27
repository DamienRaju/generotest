----------------------------------------------------------------------------------------------------
#+ MULTI-SERVER UPDATE PROCEDURE
#+
#+ Author: Damien Raju
#+
#+ Executes the server update for each selected server.
#+
#+ MENU ACTIONS
#+
#+ ACTION _chg Enter Name
#+
#+  	BUSINESS RULES: 
#+  	Enter busines rules and logic for this ACTION 
#+
#+ CHANGES
#+
IMPORT os

&include "sy_lib.inc"

SCHEMA xactdev

    CONSTANT	m_prog  			= "dx150_server_update",
                m_ver_no    		= "Ver 1.00"

-- Variables with type specified 
	DEFINE  m_prog_ver  			STRING, 		-- Store Program Name & Version No.
			m_sep   				STRING, 		-- Dir separator Win = \\  Linux = /
			m_path_etc				STRING,
	
			m_email_subject			STRING,

			m_prog_str				STRING,
			m_update_prog_str		STRING,
			m_path_env_script		STRING,
			m_etc_direct_path		STRING,
			m_data_dict_direct_path	STRING,
			m_menu_direct_path		STRING,
			m_pics_direct_path		STRING,
			m_code_250_direct_path	STRING,
			m_code_310_direct_path	STRING

--Server Input Record
	DEFINE	mr_update				RECORD
										upd_code			VARCHAR(1),
										upd_file_spec		VARCHAR(1),
										file_name			VARCHAR(20),
										upd_data_dict		VARCHAR(1),
										upd_menu			VARCHAR(1),
										upd_etc				VARCHAR(1),
										upd_pics			VARCHAR(1)
									END RECORD
									
	DEFINE	mr_update_type			RECORD
										man_or_auto			VARCHAR(10),
										backup_server_db	VARCHAR(10),
										backup_code			VARCHAR(10),
										check_in_use		VARCHAR(10)
									END RECORD

--Arrays for Server Updates
	DEFINE	ma_server_list			DYNAMIC ARRAY OF RECORD
										execute_update	VARCHAR(1),
										server			LIKE dx01s_server_mast.server,
										port			LIKE dx01s_server_mast.port,
										ip				LIKE dx01s_server_mast.ip,
										complete		STRING
									END RECORD,

			ma_db_list				DYNAMIC ARRAY OF RECORD LIKE dx01d_db_mast.*			
----------------------------------------------------------------------------------------------------
MAIN

	DEFINE  l_err_src 		STRING

	LET m_prog_ver = m_prog || " - " || m_ver_no
	
	LET l_err_src = m_prog_ver || " > MAIN"

	LET g_schema_name	= ARG_VAL(1)
	LET g_user_name		= ARG_VAL(2)

	--Initialise ComboBoxs
    CALL ui.combobox.setdefaultinitializer ( "sb_bld_cbox_build" )

	CALL lf_prog_init(	{prog_name}     	m_prog,
						{form_name}     	m_prog,
						{ver_no}        	m_ver_no,
						{child}         	TRUE,
						{test_prog_access}	TRUE,
						{err_src}			l_err_src	)
	
	CALL sb_module_init( l_err_src )

	CALL sb_build_dir( l_err_src )
	
	CALL main_menu( l_err_src )

	CALL lf_exit_program( FALSE, l_err_src )
	
END MAIN


{==================================================================================================================================}

{==================================================================================================================================}
#+ Main Menu
#+
#+ BUSINESS RULE: 
#+ Menu
#+
#+ @code    CALL main_menu( p_err_src )
#+
#+ @param NONE
#+
#+ @return NONE
#+
#+ CHANGES
#+
#+
FUNCTION main_menu( p_err_src )

	DEFINE	p_err_src			    STRING,
	
			l_cnt				    INTEGER
        
	LET p_err_src = p_err_src || " > main_menu"
	
	MENU

	ON ACTION update
		IF ( w_main_input_hd( p_err_src ) ) THEN
		--User has accepted input already.
		ELSE
			RETURN
		END IF

	--run server updates
		FOR l_cnt = 1 TO ma_server_list.getLength()

			IF ( ma_server_list[l_cnt].execute_update <> "Y" ) THEN
				CONTINUE FOR
			END IF

			LET m_email_subject = ma_server_list[l_cnt].server || ": Manual Update"

		  --Build string display
			IF ( m_prog_str IS NULL ) THEN
				LET m_prog_str = ma_server_list[l_cnt].server || "\n\nUpdating Server...\n\n"
			ELSE
				LET m_prog_str = m_prog_str || ma_server_list[l_cnt].server || "\n\nUpdating Server...\n\n"
			END IF
			DISPLAY m_prog_str TO update_process
			CALL ui.Interface.refresh()
				
		--Display completed icon for current server.
			LET ma_server_list[l_cnt].complete	= "refresh"
			DISPLAY ARRAY ma_server_list TO tbl_company_list.*
				BEFORE DISPLAY
					EXIT DISPLAY
			END DISPLAY

		--run updates
			IF ( upd_servers( l_cnt, p_err_src )) THEN
			
				LET m_prog_str = m_prog_str || ma_server_list[l_cnt].server || "\n\n\nServer Update Complete!"
				DISPLAY m_prog_str TO update_process
				CALL ui.Interface.refresh()
			
			--update successful
				CALL lf_email_file(	{prompt_user}	FALSE,
									{bcc_user}		TRUE,
									{file_type}		"body",
									{filename}		NULL,
									{email_subject}	m_email_subject || " - Completed",
									{email_address}	"dev@xacterp.co.za",
									{email_body}	"Manual Server Update has Completed! \n\n"|| m_update_prog_str,
									{p_err_src}		p_err_src)

			--Display completed icon for current db.
				LET ma_server_list[l_cnt].complete	= "accept"
				DISPLAY ARRAY ma_server_list TO tbl_company_list.*
					BEFORE DISPLAY
						EXIT DISPLAY
				END DISPLAY

			ELSE

				LET m_prog_str = m_prog_str || ma_server_list[l_cnt].server || "\n\n\nServer Update Skipped!"
				DISPLAY m_prog_str TO update_process
				CALL ui.Interface.refresh()
				
				--update unsuccessful
				CALL lf_email_file(	{prompt_user}	FALSE,
									{bcc_user}		TRUE,
									{file_type}		"body",
									{filename}		NULL,
									{email_subject}	m_email_subject || " - Skipped",
									{email_address}	"dev@xacterp.co.za",
									{email_body}	"Manual Server Update has skipped! \n\n"|| m_update_prog_str,
									{p_err_src}		p_err_src)
			
			END IF

				
		END FOR

	ON ACTION EXIT
		IF ( lf_exit_program( TRUE, p_err_src ) ) THEN
			EXIT MENU
		END IF
		
	END MENU

END FUNCTION


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													BUILD FUNCTIONS
}
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION BUILD_FUNCTIONS()
END FUNCTION	
{==================================================================================================================================}
{==================================================================================================================================}
{
												END OF BUILD FUNCTIONS
}
--##################################################################################################################################
{==================================================================================================================================}

{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													HEADER FUNCTIONS
}
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION HEADER_INPUT_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ INPUT [mr_parent_chg] HEADER RECORD 
#+
#+ BUSINESS RULE: 
#+ Allows user to select when to schedule dayend.
#+
#+ @code    CALL w_main_input_hd ( p_err_src ) RETURNING TRUE/FALSE 
#+
#+ @param   p_err_src 	Error Source from calling statement
#+
#+ @return  TRUE		Input was VALID, dayend must be run
#+ @return  FALSE		Input was INVALID and dayend must NOT be run
#+
#+ CHANGES 
#+
FUNCTION w_main_input_hd( p_err_src )

    DEFINE	p_err_src 			    STRING,
			
			l_run_update		    BOOLEAN,
			l_cnt				    INTEGER,
			l_server_selected		BOOLEAN,
			l_up_arw			    BOOLEAN

            	
	LET p_err_src 			= p_err_src || " > w_main_input_hd"
	LET w_cur 				= ui.Window.getCurrent()
    LET f_cur 				= w_cur.getForm()

	LET l_run_update = FALSE

    DIALOG ATTRIBUTES ( FIELD ORDER FORM, UNBUFFERED )

		--------------------------------------------------------------------------------------------
        INPUT BY NAME mr_update.* ATTRIBUTE ( WITHOUT DEFAULTS )

			BEFORE INPUT
			--Build defualts
				LET mr_update.upd_code 			= "N"
				LET mr_update.upd_file_spec 	= "N"
				LET mr_update.file_name 		= NULL
				LET mr_update.upd_data_dict		= "N"
				LET mr_update.upd_menu 			= "N"
				LET mr_update.upd_etc 			= "N"
				LET mr_update.upd_pics 			= "N"

LET mr_update_type.man_or_auto  = "now"

				

				CALL DIALOG.setFieldActive( "upd_file_spec",	FALSE )
				CALL f_cur.setFieldHidden(  "upd_file_spec", 	TRUE  )
				CALL f_cur.setElementHidden( "label7", 			TRUE  )
				
				CALL DIALOG.setFieldActive( "file_name",		FALSE )
				CALL f_cur.setFieldHidden(  "file_name",		TRUE  )
				CALL f_cur.setElementHidden("label9",			TRUE  )

				CALL sb_build_company_list( p_err_src )
				
				--LET mr_update_type.man_or_auto = NULL
				LET mr_update_type.backup_server_db = "Y"
				LET mr_update_type.backup_code		= "Y"
				LET mr_update_type.check_in_use 	= "Y"

				LET m_prog_str = NULL
				DISPLAY " " TO update_process
			----------------------------------------------------------------------------------------
            ON CHANGE upd_code
                CALL w_update_is_field_valid( "upd_code", p_err_src ) RETURNING g_not_used
            ----------------------------------------------------------------------------------------
            ON CHANGE upd_file_spec
                CALL w_update_is_field_valid( "upd_file_spec", p_err_src ) RETURNING g_not_used
			----------------------------------------------------------------------------------------
            AFTER FIELD file_name
                CALL w_update_is_field_valid( "file_name", p_err_src ) RETURNING g_not_used

			AFTER INPUT
				CALL w_update_is_field_valid( "all", p_err_src ) RETURNING g_not_used
                            
        END INPUT
		
        --------------------------------------------------------------------------------------------
        INPUT ARRAY ma_server_list FROM tbl_company_list.* ATTRIBUTES( AUTO APPEND = FALSE, APPEND ROW = FALSE, DELETE ROW = FALSE, INSERT ROW = FALSE )
            ----------------------------------------------------------------------------------------
            BEFORE INPUT                
                CALL DIALOG.setActionHidden( "up_arw", TRUE )
                CALL DIALOG.setFieldActive( "server", FALSE )

            ----------------------------------------------------------------------------------------
            BEFORE ROW
                LET l_up_arw = FALSE

            ----------------------------------------------------------------------------------------
            AFTER ROW
            
                IF NOT( l_up_arw ) AND ( ARR_CURR() = ma_server_list.getLength() ) THEN
                    NEXT FIELD man_or_auto
                END IF
            
            ----------------------------------------------------------------------------------------
            AFTER INPUT
			--Make sure a server is selected
                LET l_server_selected = FALSE
                FOR l_cnt = 1 TO ma_server_list.getLength()
                    IF ( ma_server_list[l_cnt].execute_update = "Y" ) THEN
                        LET l_server_selected = TRUE
                        EXIT FOR
                    END IF
                END FOR

                IF NOT( l_server_selected ) THEN
                    CALL fgl_winmessage( %"Invalid Data", %"Please select at least 1 server for the update procedure.", "exclamation" )
                    NEXT FIELD CURRENT
                END IF

            ----------------------------------------------------------------------------------------
            ON ACTION up_arw
                LET l_up_arw = TRUE
                CALL fgl_set_arr_curr( ARR_CURR() - 1 )
                
        END INPUT
        
        --------------------------------------------------------------------------------------------
        INPUT BY NAME mr_update_type.* ATTRIBUTE ( WITHOUT DEFAULTS )              
                
            ON CHANGE man_or_auto
                CALL w_update_type_is_field_valid( "man_or_auto", p_err_src )		RETURNING g_not_used

			ON CHANGE backup_server_db
                CALL w_update_type_is_field_valid( "backup_server_db", p_err_src )	RETURNING g_not_used
				
			ON CHANGE backup_code
                CALL w_update_type_is_field_valid( "backup_code", p_err_src )		RETURNING g_not_used

			ON CHANGE check_in_use
                CALL w_update_type_is_field_valid( "check_in_use", p_err_src )		RETURNING g_not_used
                            
        END INPUT

        --------------------------------------------------------------------------------------------	
        AFTER DIALOG
			IF ( w_update_type_is_field_valid( "all", p_err_src ) ) THEN
				LET l_run_update = TRUE
				EXIT DIALOG
			END IF
            NEXT FIELD CURRENT

        --------------------------------------------------------------------------------------------	
        ON ACTION ACCEPT
            ACCEPT DIALOG
			
        --------------------------------------------------------------------------------------------     
        ON ACTION CANCEL 
            IF ( lf_cancel_input() ) THEN
                LET l_run_update = FALSE
                EXIT DIALOG
            END IF

    END DIALOG
    
	RETURN l_run_update

END FUNCTION

{==================================================================================================================================}
#+ TEST & VALIDATE HEADER RECORD INPUT
#+
#+ BUSINESS RULE: 
#+ Test and validate update Record data AFTER INPUT. 
#+ If field_name = "ALL" then test all fields and prompt if Details Correct 
#+
#+ @code    CALL w_update_is_field_valid ( p_field_name, p_err_src )  	
#+
#+ @param   p_field_name	Name of field to validate
#+ @param   p_err_src 		Error Source from calling statement 
#+
#+ @return  NONE
#+
#+ CHANGES
#+
FUNCTION w_update_is_field_valid( p_field_name, p_err_src )

 
	DEFINE  p_field_name  	STRING,
			p_err_src 		STRING,
			
			l_dlog			ui.dialog

	LET l_dlog			= ui.dialog.getcurrent()
	LET p_err_src		= p_err_src || " > w_update_is_field_valid"

	------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) OR ( p_field_name = "upd_code" ) THEN

		IF ( mr_update.upd_code = "Y" ) THEN
			CALL l_dlog.setFieldActive( "upd_file_spec",	TRUE  )
			CALL f_cur.setFieldHidden(  "upd_file_spec", 	FALSE )
			CALL f_cur.setElementHidden( "label7", 			FALSE )
				
			CALL l_dlog.setFieldActive(  "file_name", 		FALSE )
			CALL f_cur.setFieldHidden( "file_name",			FALSE )
			CALL f_cur.setElementHidden("label9", 			FALSE )
		ELSE
			CALL l_dlog.setFieldActive( "upd_file_spec",	FALSE )
			CALL f_cur.setFieldHidden(  "upd_file_spec", 	TRUE  )
			CALL f_cur.setElementHidden("label7", 			TRUE  )
				
			CALL l_dlog.setFieldActive(  "file_name", 		FALSE )
			CALL f_cur.setFieldHidden( "file_name", 		TRUE  )
			CALL f_cur.setElementHidden("label9", 			TRUE  )

			LET mr_update.upd_file_spec	= "N"
			LET mr_update.file_name 	= NULL
		END IF

	END IF
	
	------------------------------------------------------------------------------------------------
	IF ( p_field_name = "upd_file_spec" ) THEN

		IF ( mr_update.upd_file_spec = "Y" ) THEN
			CALL l_dlog.setFieldActive( "file_name", TRUE  )
		ELSE
			CALL l_dlog.setFieldActive( "file_name", FALSE )
			LET mr_update.file_name = NULL
		END IF

	END IF

	------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) OR ( p_field_name = "file_name" ) THEN

		IF ( mr_update.upd_file_spec = "Y" ) THEN
		
			IF ( mr_update.file_name IS NULL )THEN
				CALL fgl_winmessage( %"Warning", %"File Name cannot be blank", "exclamation" )
				CALL l_dlog.nextField( "file_name" )
				RETURN FALSE
			END IF
			
		ELSE
			LET mr_update.file_name = NULL
		END IF

	END IF

	------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) THEN

		IF 	( mr_update.upd_code 		= "N" )	AND
			( mr_update.upd_data_dict	= "N" )	AND
			( mr_update.upd_menu 		= "N" )	AND
			( mr_update.upd_etc 		= "N" )	AND
			( mr_update.upd_pics 		= "N" )
		THEN
			CALL fgl_winmessage( %"Warning", %"Please select at least 1 update", "exclamation" )
			CALL l_dlog.nextField( "upd_code" )
			RETURN FALSE
		END IF

	END IF
	
	RETURN TRUE
	
END FUNCTION	

{==================================================================================================================================}
#+ TEST & VALIDATE HEADER RECORD INPUT
#+
#+ BUSINESS RULE: 
#+ Test and validate Main Record data AFTER INPUT. 
#+ If field_name = "ALL" then test all fields and prompt if Details Correct 
#+
#+ @code    CALL w_update_type_is_field_valid ( p_field_name, p_err_src)  	
#+
#+ @param   p_field_name		Name of field to validate
#+ @param   p_err_src 			Error Source from calling statement 
#+
#+ @return  NONE
#+
#+ CHANGES
#+
FUNCTION w_update_type_is_field_valid( p_field_name, p_err_src )

 
	DEFINE  p_field_name  	STRING,
			p_err_src 		STRING,
			
			l_dlog			ui.dialog

	LET l_dlog				= ui.dialog.getcurrent()
	LET p_err_src			= p_err_src || " > w_update_type_is_field_valid"

	------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) OR ( p_field_name = "man_or_auto" ) THEN

		IF ( mr_update_type.man_or_auto IS NULL ) THEN
			CALL fgl_winmessage( %"Error", %"Please choose when the Update will be run.", "exclamation" )
			CALL l_dlog.nextField( "man_or_auto" )
			RETURN FALSE
		END IF

	END IF
	------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) OR ( p_field_name = "backup_server_db" ) THEN

		IF ( mr_update_type.backup_server_db IS NULL ) THEN
			CALL fgl_winmessage( %"Error", %"Please choose if you want to backup the server DB.", "exclamation" )
			CALL l_dlog.nextField( "backup_server_db" )
			RETURN FALSE
		END IF

	END IF
	------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) OR ( p_field_name = "backup_code" ) THEN

		IF ( mr_update_type.backup_code IS NULL ) THEN
			CALL fgl_winmessage( %"Error", %"Please choose if you want to backup the code.", "exclamation" )
			CALL l_dlog.nextField( "backup_code" )
			RETURN FALSE
		END IF

	END IF
	------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) OR ( p_field_name = "check_in_use" ) THEN

		IF ( mr_update_type.check_in_use IS NULL ) THEN
			CALL fgl_winmessage( %"Error", %"Please choose if you want to check for in use logs.", "exclamation" )
			CALL l_dlog.nextField( "check_in_use" )
			RETURN FALSE
		END IF
	--Prompt if DB changes are selected and user is not selecting to do in use checks
		IF ( mr_update.upd_data_dict = "Y" ) AND ( mr_update_type.check_in_use = "N" ) THEN
			IF ( fgl_winbutton( %"Confirmation?", %"DB changes are scheduled, are you sure you don't want to check for in use logs before updating?", %"Check for in use logs", %"Yes|Check for in use logs","question", 0 ) = %"Check for in use logs" ) THEN
				LET mr_update_type.check_in_use = "Y"
			END IF
		END IF

	END IF
	------------------------------------------------------------------------------------------------
	RETURN TRUE
	
END FUNCTION	


{==================================================================================================================================}
{==================================================================================================================================}
{
												END OF HEADER INPUT FUNCTIONS
}
--##################################################################################################################################
{==================================================================================================================================}


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													UPDATE FUNCTIONS
}
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION UPDATE_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ UPDATE SERVERS AFTER INPUT
#+
#+ BUSINESS RULE: 
#+ Updates servers from here.
#+
#+ @code    CALL upd_servers( p_cnt, p_err_src ) RETURNING TRUE/FALSE 	
#+
#+ @param   p_cnt 		Server array counter
#+ @param   p_err_src 	Error Source from calling statement 
#+
#+ @return  TRUE		Update was successful
#+ @return  FALSE		Update was NOT successful
#+
#+ CHANGES
#+
FUNCTION upd_servers( p_cnt, p_err_src )
	
	DEFINE	p_cnt				INTEGER,
			p_err_src 			STRING
			
	LET p_err_src     = p_err_src || " > upd_servers"

	LET m_update_prog_str	= NULL
	
--Email for Start of Server Updates
	CALL lf_email_file(	{prompt_user}	FALSE,
						{bcc_user}		TRUE,
						{file_type}		"body",
						{filename}		NULL,
						{email_subject}	m_email_subject || " - Started",
						{email_address}	"dev@xacterp.co.za",
						{email_body}	"Manual Server Update has Started!",
						{err_src}		p_err_src)

--Build list of DBs for server
	CALL sb_build_db_list( ma_server_list[p_cnt].server, p_err_src )

	WHILE ( 1=1 )
	--Check if there are any in use logs, if not then skip server update
		IF ( mr_update_type.check_in_use = "Y" ) THEN
			IF ( sb_check_in_use( p_cnt, p_err_src ) = TRUE ) THEN
				--Do Nothing
			ELSE
				EXIT WHILE
			END IF
		END IF

	--Create a backup of the DBs, if it fails then skip server update
		IF ( mr_update_type.backup_server_db = "Y" ) THEN
			IF ( sb_db_backup( p_cnt, p_err_src ) = TRUE ) THEN
				--Do Nothing
			ELSE
				EXIT WHILE
			END IF
		END IF

	--Create a backup of the code, if it fails then skip server update
		IF ( mr_update_type.backup_code = "Y" ) THEN
			IF ( sb_backup_code( p_cnt, p_err_src ) = TRUE ) THEN
				--Do Nothing
			ELSE
				EXIT WHILE
			END IF
		END IF

	--Update data dict
		IF ( mr_update.upd_data_dict = "Y" ) THEN
			CALL upd_data_dict( p_cnt, p_err_src )

		--Update the DB, if it fails then skip server update
			IF ( sb_menu_or_db_updates( "db", p_cnt, p_err_src ) = TRUE ) THEN
				--Do Nothing
			ELSE
				CALL sb_display_update_progress( p_cnt, p_err_src )
				EXIT WHILE
			END IF
			
			CALL sb_display_update_progress( p_cnt, p_err_src )
		END IF

	--Update the menu
		IF ( mr_update.upd_menu = "Y" ) THEN
			CALL upd_menu( p_cnt, p_err_src )

		--Update the Menu, if it fails then skip server update
			IF ( sb_menu_or_db_updates( "menu", p_cnt, p_err_src ) = TRUE ) THEN
				--Do Nothing
			ELSE
				CALL sb_display_update_progress( p_cnt, p_err_src )
				EXIT WHILE
			END IF
			
			CALL sb_display_update_progress( p_cnt, p_err_src )
		END IF

	--Update etc
		IF ( mr_update.upd_etc = "Y" ) THEN
			CALL upd_etc( p_cnt, p_err_src )
			CALL sb_display_update_progress( p_cnt, p_err_src )
		END IF

	--Update pics
		IF ( mr_update.upd_pics = "Y" ) THEN
			CALL upd_pics( p_cnt, p_err_src )
			CALL sb_display_update_progress( p_cnt, p_err_src )
		END IF

	--Update code
		IF ( mr_update.upd_code = "Y" ) THEN
			CALL upd_code( p_cnt, p_err_src )
			CALL sb_display_update_progress( p_cnt, p_err_src )
		END IF

		EXIT WHILE

	END WHILE
	
	RETURN TRUE
	
END FUNCTION
{==================================================================================================================================}
#+ UPDATE ETC
#+
#+ BUSINESS RULE: 
#+ Updates servers with the ETC files.
#+
#+ @code    CALL upd_etc( p_cnt, p_err_src )
#+
#+ @param   p_cnt 		Server array counter
#+ @param   p_err_src 	Error Source from calling statement 
#+
#+ CHANGES
#+
FUNCTION upd_etc( p_cnt, p_err_src )

	DEFINE	p_cnt			INTEGER,
			p_err_src 		STRING,

			l_etc_cmd		STRING

	LET p_err_src     = p_err_src || " > upd_etc"

--echo ETC UPDATE
	RUN "sudo -u xactii echo UPDATING ETC >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

--Move etc files to server
	LET l_etc_cmd =	". " || m_path_env_script || ";"||
					"sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete --exclude co_list.conf "|| m_etc_direct_path ||"* xactii@"|| ma_server_list[p_cnt].ip||":/opt/xact/xact_ii/etc/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"
	
	RUN l_etc_cmd
	
END FUNCTION
{==================================================================================================================================}
#+ UPDATE DATA DICTIONARY
#+
#+ BUSINESS RULE: 
#+ Updates servers with the Data Dictionary files.
#+
#+ @code    CALL upd_data_dict( p_cnt, p_err_src )
#+
#+ @param   p_cnt 		Server array counter
#+ @param   p_err_src 	Error Source from calling statement 
#+
#+ CHANGES
#+
FUNCTION upd_data_dict( p_cnt, p_err_src )

	DEFINE	p_cnt				INTEGER,
			p_err_src 			STRING,

			l_data_dict_cmd		STRING

	LET p_err_src     = p_err_src || " > upd_data_dict"

--echo DATA DICT UPDATE
	RUN "sudo -u xactii echo UPDATING DATA DICT >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

--Move data dict files to server
	LET l_data_dict_cmd =	". " || m_path_env_script || ";"||
							"sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete "|| m_data_dict_direct_path ||"* xactii@"|| ma_server_list[p_cnt].ip||":/opt/xact/xact_ii/data_dict/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"
	
	RUN l_data_dict_cmd
	
END FUNCTION
{==================================================================================================================================}
#+ UPDATE MENU
#+
#+ BUSINESS RULE: 
#+ Updates servers with the Menu files.
#+
#+ @code    CALL upd_menu( p_cnt, p_err_src )
#+
#+ @param   p_cnt 		Server array counter
#+ @param   p_err_src 	Error Source from calling statement 
#+
#+ CHANGES
#+
FUNCTION upd_menu( p_cnt, p_err_src )

	DEFINE	p_cnt			INTEGER,
			p_err_src 		STRING,

			l_menu_cmd		STRING

	LET p_err_src     = p_err_src || " > upd_menu"

--echo MENU UPDATE
	RUN "sudo -u xactii echo UPDATING MENU >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

--Move menu files to server
	LET l_menu_cmd =	". " || m_path_env_script || ";"||
							"sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete "|| m_menu_direct_path ||"* xactii@"|| ma_server_list[p_cnt].ip||":/opt/xact/xact_ii/menu_structure/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"
	RUN l_menu_cmd
	
END FUNCTION
{==================================================================================================================================}
#+ UPDATE PICTURES
#+
#+ BUSINESS RULE: 
#+ Updates servers with the Pictures files.
#+
#+ @code    CALL upd_pics( p_cnt, p_err_src )
#+
#+ @param   p_cnt 		Server array counter
#+ @param   p_err_src 	Error Source from calling statement 
#+
#+ CHANGES
#+
FUNCTION upd_pics( p_cnt, p_err_src )

	DEFINE	p_cnt			INTEGER,
			p_err_src 		STRING,

			l_pics_cmd		STRING

	LET p_err_src     = p_err_src || " > upd_pics"

--echo PICS UPDATE
	RUN "sudo -u xactii echo UPDATING PICS >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

--Move pic files to server
	LET l_pics_cmd =	". " || m_path_env_script || ";"||
							"sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete "|| m_pics_direct_path ||"* xactii@"|| ma_server_list[p_cnt].ip||":/opt/xact/pics/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

	DISPLAY l_pics_cmd

	RUN l_pics_cmd
	
END FUNCTION
{==================================================================================================================================}
#+ UPDATE CODE
#+
#+ BUSINESS RULE: 
#+ Updates servers with the Code compiles.
#+
#+ @code    CALL upd_code( p_cnt, p_err_src )
#+
#+ @param   p_cnt 		Server array counter
#+ @param   p_err_src 	Error Source from calling statement 
#+
#+ CHANGES
#+
FUNCTION upd_code( p_cnt, p_err_src )

	DEFINE	p_cnt				INTEGER,
			p_err_src 			STRING,

			l_code_cmd_250		STRING,
			l_code_cmd_310		STRING

	LET p_err_src     = p_err_src || " > upd_code"

--echo CODE UPDATE
	RUN "sudo -u xactii echo UPDATING CODE >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

--Check if code updates are file specific
	IF ( mr_update.upd_file_spec = "Y" ) THEN
	--Make sure there is a file name
		IF ( mr_update.file_name IS NOT NULL ) THEN 
			LET l_code_cmd_250 =	". " || m_path_env_script || ";"||
									"sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete "|| m_code_250_direct_path || mr_update.file_name ||"* xactii@"|| ma_server_list[p_cnt].ip ||":/opt/xact/xact_ii/bin/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

			LET l_code_cmd_310 =	". " || m_path_env_script || ";"||
									"sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete "|| m_code_310_direct_path || mr_update.file_name ||"* xactii@"|| ma_server_list[p_cnt].ip ||":/opt/xact/xact_ii_310/bin/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

			RUN l_code_cmd_250
			RUN l_code_cmd_310
		END IF
	ELSE
		LET l_code_cmd_250 =	". " || m_path_env_script || ";"||
								"sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete "|| m_code_250_direct_path ||" xactii@"|| ma_server_list[p_cnt].ip ||":/opt/xact/xact_ii/bin/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

		LET l_code_cmd_310 =	". " || m_path_env_script || ";"||
								"sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete "|| m_code_310_direct_path ||" xactii@"|| ma_server_list[p_cnt].ip ||":/opt/xact/xact_ii_310/bin/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt;"

		RUN l_code_cmd_250
		RUN l_code_cmd_310
	END IF
	
END FUNCTION
{==================================================================================================================================}
{==================================================================================================================================}
{
												END UPDATE FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}

--##################################################################################################################################
{
												BACKUPS AND UPDATE FUNCTIONS
}
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION BACKUP_AND_UPDATE_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
{==================================================================================================================================}
#+ DATABASE & MENU UPDATES
#+
#+ BUSINESS RULE:  
#+ Updates either the menu or db 
#+
#+ @code    CALL sb_menu_or_db_updates( p_menu_or_db, p_cnt, p_err_src ) RETURNING boolean_value
#+
#+
#+ @param   p_meu_or_db 	Update either Menu or DB
#+ @param   p_cnt 			Server array counter
#+ @param   p_err_src       Error Source from calling statement
#+
#+ @return  boolean_value   TRUE/1 - update success, FALSE/0 - update failure
#+
#+ CHANGES
#+
FUNCTION sb_menu_or_db_updates( p_menu_or_db, p_cnt, p_err_src )

	DEFINE	p_menu_or_db	STRING,
			p_cnt			INTEGER,
			p_err_src		STRING,

			l_cnt			INTEGER,
			l_check			INTEGER

	LET p_err_src = p_err_src || " > sb_menu_or_db_updates"

	CALL sy_pop_up_message ( "Update Script Running!", "OPEN" )

	--echo the environment script
	RUN "echo . /opt/xact/scripts/env_vars_310.sh > /u/xact_misc_data/dx150_scripts/update_" || ma_server_list[p_cnt].server || ".sh"

	FOR l_cnt = 1 TO ma_db_list.getLength()
	--Run dbaccess commands for either the DB changes or Menu changes
		IF ( p_menu_or_db = "db" ) THEN
			RUN "echo dbaccess " || ma_db_list[l_cnt].db || " /opt/xact/xact_ii/data_dict/z_SQL_Updates.sql >> /u/xact_misc_data/dx150_scripts/update_" || ma_server_list[p_cnt].server || ".sh"
		END IF

		IF ( p_menu_or_db = "menu" ) THEN
			RUN "echo dbaccess " || ma_db_list[l_cnt].db || " /opt/xact/xact_ii/menu_structure/load_menu_structure.sql >> /u/xact_misc_data/dx150_scripts/update_" || ma_server_list[p_cnt].server || ".sh"
		END IF	
	END FOR

	--Move script to relevant server
	RUN "sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete /u/xact_misc_data/dx150_scripts/update_" || ma_server_list[p_cnt].server || ".sh xactii@"|| ma_server_list[p_cnt].ip ||":/u/tmp/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt 2>&1"

	--make update.sh executable
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'chmod +x /u/tmp/update_" || ma_server_list[p_cnt].server || ".sh'"
	
	--Run update.sh script
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'sudo -u informix /u/tmp/update_" || ma_server_list[p_cnt].server || ".sh' >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt 2>&1" RETURNING l_check

	--check if update was successful then delete g_path_tmp_files 
	IF ( l_check <> 0 ) THEN

		--email if updates failed
		CALL lf_email_file(	{prompt_user}	FALSE,
							{bcc_user}		TRUE,
							{file_type}		"body",
							{filename}		NULL,
							{email_subject}	ma_server_list[p_cnt].server || ": DB/Menu Updates",
							{email_address}	"dev@xacterp.co.za",
							{email_body}	"Update script Failed on " || ma_server_list[p_cnt].server,
							{err_src}		p_err_src)
		CALL fgl_winmessage( %"Update", %"Update script Failed! ", "exclamation" )
		CALL sy_pop_up_message ( "Update script Finished", "CLOSE" )
		RETURN FALSE

	ELSE
		--remove update.sh from g_path_tmp_files
		RUN "rm /u/xact_misc_data/dx150_scripts/update_" || ma_server_list[p_cnt].server || ".sh"
		RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'rm /u/tmp/update_" || ma_server_list[p_cnt].server || ".sh'"
	END IF 

	CALL sy_pop_up_message ( "Update script Finished", "CLOSE" )

	RETURN TRUE

END FUNCTION
{==================================================================================================================================}
#+ DATABASE BACKUP
#+
#+ BUSINESS RULE:  
#+ Makes a Level 0 backup of the entire company database
#+
#+ @code    CALL sb_db_backup( p_cnt, p_err_src ) RETURNING boolean_value
#+
#+
#+ @param   p_cnt 			Server array counter
#+ @param   p_err_src       Error Source from calling statement
#+
#+ @return  boolean_value   TRUE/1 - backup success, FALSE/0 - backup failure
#+
#+ CHANGES
#+
FUNCTION sb_db_backup( p_cnt, p_err_src )

	DEFINE	p_cnt			INTEGER,
			p_err_src		STRING,

			l_hostname		STRING,
			l_mth			STRING,
			l_day			STRING,
			l_check			INTEGER,

			l_date_time 	STRING,					--GET TODAYS DATE

			l_ch			base.Channel

	LET p_err_src = p_err_src || " > sb_db_backup"

	--Fetch the Hostname of the current server
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'hostname' > /u/xact_misc_data/dx150_scripts/host.tmp"

	LET	 l_ch = base.Channel.create()
	CALL l_ch.openFile( "/u/xact_misc_data/dx150_scripts/host.tmp", "r" )

	LET  l_hostname = l_ch.readLine()

	--Set Month FORMAT MM
	IF ( MONTH(TODAY) < 10 ) THEN 
		LET l_mth =	"0" || MONTH(TODAY)
	ELSE 
		LET l_mth =	MONTH(TODAY)
	END IF 

	--Set Day Format DD
	IF ( DAY(TODAY) < 10 ) THEN 
		LET l_day =	"0" || DAY(TODAY)
	ELSE 
		LET l_day =	DAY(TODAY)
	END IF 

	CALL sy_pop_up_message ( "Backup Running!", "OPEN" )

	--echo the environment script
	RUN "echo . /opt/xact/scripts/env_vars_310.sh > /u/xact_misc_data/dx150_scripts/informix_db_" || ma_server_list[p_cnt].server || "_backup.sh"

	--echo Create Backup Before Month end update
	RUN "echo ontape -s -L 0 >> /u/xact_misc_data/dx150_scripts/informix_db_" || ma_server_list[p_cnt].server || "_backup.sh"

	--echo rename backup
	LET l_date_time = TODAY USING "YYYYmmdd" 
	RUN "echo mv /u/backups/db/" || l_hostname || "_0_L0 /opt/xact/xact_ii/data_dict/backups/"  ||l_hostname||"_0_L0." || l_date_time || "_" || CURRENT HOUR TO HOUR || CURRENT MINUTE TO MINUTE || "_DB_" || ma_server_list[p_cnt].server || ".xz" || " >> /u/xact_misc_data/dx150_scripts/informix_db_" || ma_server_list[p_cnt].server || "_backup.sh"

	--Move script to relevant server
	RUN "sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete /u/xact_misc_data/xsrce_db/informix_db_" || ma_server_list[p_cnt].server || "_backup.sh xactii@"|| ma_server_list[p_cnt].ip ||":/opt/xact/xact_ii/data_dict/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt 2>&1"

	--make informix_backup.sh executable
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'chmod +x /opt/xact/xact_ii/data_dict/informix_db_" || ma_server_list[p_cnt].server || "_backup.sh'"
	--
	--Run informix_backup.sh script
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'sudo -u informix /opt/xact/xact_ii/data_dict/informix_db_" || ma_server_list[p_cnt].server || "_backup.sh' >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt 2>&1" RETURNING l_check

	--check if backup was successful then delete g_path_tmp_files 
	IF ( l_check <> 0 ) THEN

		--email support when month end backup failed
		CALL lf_email_file(	{prompt_user}	FALSE,
							{bcc_user}		TRUE,
							{file_type}		"body",
							{filename}		NULL,
							{email_subject}	ma_server_list[p_cnt].server || ": DB Change backup",
							{email_address}	"dev@xacterp.co.za",
							{email_body}	"DB Change backup Failed on " || ma_server_list[p_cnt].server,
							{err_src}		p_err_src)
		CALL fgl_winmessage( %"Backup", %"DB change backup Failed! ", "exclamation" )
		CALL sy_pop_up_message ( "Backup Finished", "CLOSE" )
		RETURN FALSE

	ELSE
		--remove informix_backup.sh from g_path_tmp_files
		RUN "rm /u/xact_misc_data/dx150_scripts/informix_db_" || ma_server_list[p_cnt].server || "_backup.sh"
		RUN "rm /u/xact_misc_data/dx150_scripts/host.tmp"
		RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'rm /opt/xact/xact_ii/data_dict/informix_db_" || ma_server_list[p_cnt].server || "_backup.sh'"
	END IF 

	CALL sy_pop_up_message ( "Backup Finished", "CLOSE" )

	RETURN TRUE

END FUNCTION
{==================================================================================================================================}
#+ CODE BACKUP
#+
#+ BUSINESS RULE:  
#+ Makes a backup of the xactii folder
#+
#+ @code    CALL sb_backup_code( p_cnt, p_err_src ) RETURNING boolean_value
#+
#+
#+ @param   p_cnt 			Server array counter
#+ @param   p_err_src       Error Source from calling statement
#+
#+ @return  boolean_value   TRUE/1 - backup success, FALSE/0 - backup failure
#+
#+ CHANGES
#+
FUNCTION sb_backup_code( p_cnt, p_err_src )

	DEFINE	p_cnt			INTEGER,
			p_err_src		STRING,

			l_hostname		STRING,
			l_mth			STRING,
			l_day			STRING,
			l_check			INTEGER,

			l_date_time 	STRING,					--GET TODAYS DATE

			l_ch			base.Channel

	LET p_err_src = p_err_src || " > sb_backup_code"

	--Fetch the Hostname of the current server
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'hostname' > /u/xact_misc_data/dx150_scripts/host.tmp"

	LET	 l_ch = base.Channel.create()
	CALL l_ch.openFile( "/u/xact_misc_data/dx150_scripts/host.tmp", "r" )

	LET  l_hostname = l_ch.readLine()

	--Set Month FORMAT MM
	IF ( MONTH(TODAY) < 10 ) THEN 
		LET l_mth =	"0" || MONTH(TODAY)
	ELSE 
		LET l_mth =	MONTH(TODAY)
	END IF 

	--Set Day Format DD
	IF ( DAY(TODAY) < 10 ) THEN 
		LET l_day =	"0" || DAY(TODAY)
	ELSE 
		LET l_day =	DAY(TODAY)
	END IF 

	CALL sy_pop_up_message ( "Code Backup Running!", "OPEN" )

	--echo the environment script
	RUN "echo . /opt/xact/scripts/env_vars_310.sh > /u/xact_misc_data/dx150_scripts/"|| ma_server_list[p_cnt].server || "_code_backup.sh"

	--echo Create Backup Before Month end update
	RUN "echo rm /u/tmp/code_* >> /u/xact_misc_data/dx150_scripts/"|| ma_server_list[p_cnt].server || "_code_backup.sh"
	
	--echo Create Backup Before Month end update
	RUN "echo cd /u/tmp >> /u/xact_misc_data/dx150_scripts/"|| ma_server_list[p_cnt].server || "_code_backup.sh"

	--echo rename backup
	LET l_date_time = TODAY USING "YYYYmmdd" 

	--echo Create Backup Before Month end update
	RUN "echo tar cJPf code_backup_"||l_hostname||"." || l_date_time || "_" || CURRENT HOUR TO HOUR || CURRENT MINUTE TO MINUTE || "_" || ma_server_list[p_cnt].server || "_xact_ii.tar.xz /opt/xact/xact_ii >> /u/xact_misc_data/dx150_scripts/"|| ma_server_list[p_cnt].server || "_code_backup.sh"
	
	--Move script to relevant server
	RUN "sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete /u/xact_misc_data/xsrce_db/"|| ma_server_list[p_cnt].server || "_code_backup.sh xactii@"|| ma_server_list[p_cnt].ip ||":/opt/xact/ >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt 2>&1"

	--make code_backup.sh executable
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'chmod +x /opt/xact/"|| ma_server_list[p_cnt].server || "_code_backup.sh'"

	--Run code_backup.sh script
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'sudo -u informix /opt/xact/"|| ma_server_list[p_cnt].server || "_code_backup.sh' >> /u/xact_misc_data/dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt 2>&1" RETURNING l_check

	--check if backup was successful then delete g_path_tmp_files 
	IF ( l_check <> 0 ) THEN

		--email support when month end backup failed
		CALL lf_email_file(	{prompt_user}	FALSE,
							{bcc_user}		TRUE,
							{file_type}		"body",
							{filename}		NULL,
							{email_subject}	ma_server_list[p_cnt].server || ": Code backup",
							{email_address}	"dev@xacterp.co.za",
							{email_body}	"Code backup Failed on " || ma_server_list[p_cnt].server,
							{err_src}		p_err_src)
		CALL fgl_winmessage( %"Code Backup", %"Code backup Failed! ", "exclamation" )
		CALL sy_pop_up_message ( "Code Backup Finished", "CLOSE" )
		RETURN FALSE

	ELSE
		--remove informix_backup.sh from g_path_tmp_files
		RUN "rm /u/xact_misc_data/dx150_scripts/"|| ma_server_list[p_cnt].server || "_code_backup.sh"
		RUN "rm /u/xact_misc_data/dx150_scripts/host.tmp"
		RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'rm /opt/xact/"|| ma_server_list[p_cnt].server || "_code_backup.sh'"
	END IF 

	CALL sy_pop_up_message ( "Code Backup Finished", "CLOSE" )

	RETURN TRUE

END FUNCTION
{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
												GENERAL FUNCTIONS
}
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION GENERAL_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ DISPLAY UPDATE PROGRESS
#+
#+ BUSINESS RULE:
#+ This funciton displays update messages to the update screen
#+
#+ @code    CALL sb_display_update_progress( p_cnt, p_err_src )	#+
#+
#+ @param   p_cnt 			Server array counter
#+ @param   p_err_src		Err Src Trackng	#+
#+
#+ CHANGES
#+
FUNCTION sb_display_update_progress( p_cnt, p_err_src )

    DEFINE	p_cnt	        	INTEGER,
			p_err_src	        STRING,

			l_ch				base.channel,
			l_file				STRING,
			l_str_for_txt		STRING,
			l_file_txt			STRING

	LET p_err_src = p_err_src || " > sb_display_update_progress"

	LET l_str_for_txt = NULL
			 
	LET l_ch	= base.channel.create()

--Get the file to read
	LET l_file  = fgl_getEnv( "XACT_MISC_DATA_PATH" ) || m_sep || "dx150_scripts/Xactii" || ma_server_list[p_cnt].server || "Rsync.txt"

	IF ( NOT os.Path.exists(l_file) ) THEN
		RETURN
	END IF

	LET l_str_for_txt = NULL
	CALL l_ch.openfile( l_file, "r" )

--Build the file into a string variable to display
	WHILE l_ch.read( l_file_txt )
		IF ( l_str_for_txt IS NULL ) THEN
			LET l_str_for_txt = l_file_txt
		ELSE
			IF ( l_file_txt IS NULL ) THEN
				LET l_str_for_txt = l_str_for_txt || "\n"
			ELSE
				LET l_str_for_txt = l_str_for_txt || "\n" || l_file_txt
			END IF
		END IF
	END WHILE

	IF ( m_update_prog_str IS NULL ) THEN
		LET m_update_prog_str = l_str_for_txt
	ELSE
		LET m_update_prog_str = m_update_prog_str || "\n\n" || l_str_for_txt
	END IF
	
	LET m_prog_str = m_prog_str || "\n\n\n" || l_str_for_txt

--Display string
	DISPLAY m_prog_str TO update_process
	CALL ui.Interface.refresh()
	
	CALL l_ch.close()

--Remove file so the text in the string won't duplicate
	RUN "rm -f " || l_file
	
END FUNCTION
{==================================================================================================================================}
#+ BUILD SERVER LIST
#+
#+ BUSINESS RULE: 
#+ Build the array with servers to use.
#+
#+ @code    CALL sb_build_company_list( p_err_src )
#+
#+ @param   p_err_src       Error tracking string.
#+
#+ @return  NONE 
#+
#+ CHANGES:
#+
FUNCTION sb_build_company_list( p_err_src )

	DEFINE  p_err_src			STRING,

			l_cnt				INTEGER,
			lr_dx01s			RECORD LIKE dx01s_server_mast.*

	LET p_err_src = p_err_src, " > sb_build_company_list"

	CALL ma_server_list.clear()
	LET l_cnt = 1
	
	PREPARE get_server FROM "SELECT * FROM dx01s_server_mast ORDER BY server ASC"
	DECLARE curs_server CURSOR FOR get_server

	FOREACH curs_server INTO lr_dx01s.*

	--DevTested will only see the TRAINIX server
		IF ( gr_sy02.access_grp = "Z1" AND lr_dx01s.server = "TRAINIX" ) OR ( gr_sy02.access_grp = "Z0" ) THEN 
			LET ma_server_list[l_cnt].execute_update	= "N"
			LET ma_server_list[l_cnt].server			= lr_dx01s.server
			LET ma_server_list[l_cnt].port				= lr_dx01s.port
			LET ma_server_list[l_cnt].ip				= lr_dx01s.ip
			LET ma_server_list[l_cnt].complete			= "cancel"

			LET l_cnt = l_cnt + 1
		END IF

	END FOREACH

END FUNCTION

{==================================================================================================================================}
#+ BUILD DB LIST
#+
#+ BUSINESS RULE: 
#+ Build the array with DBs to use.
#+
#+ @code    CALL sb_build_db_list( p_err_src )
#+
#+ @param   p_err_src       Error tracking string.
#+
#+ @return  NONE 
#+
#+ CHANGES:
#+
FUNCTION sb_build_db_list( p_server, p_err_src )

	DEFINE  p_server 			LIKE dx01d_db_mast.server,
			p_err_src			STRING,

			l_cnt				INTEGER,
			lr_dx01d			RECORD LIKE dx01d_db_mast.*

	LET p_err_src = p_err_src, " > sb_build_db_list"

	CALL ma_db_list.clear()
	LET l_cnt = 1
	
	PREPARE get_db FROM "SELECT * FROM dx01d_db_mast WHERE server = '"||p_server||"' ORDER BY db ASC"
	DECLARE curs_db CURSOR FOR get_db

	FOREACH curs_db INTO lr_dx01d.*

		LET ma_db_list[l_cnt].server	= lr_dx01d.server
		LET ma_db_list[l_cnt].db		= lr_dx01d.db

		LET l_cnt = l_cnt + 1

	END FOREACH

	IF ( p_server = 'TRAINIX' ) THEN
		LET m_etc_direct_path		=	"/u/projects/xsrce/xact_ii_dev_tested/1.00/etc/"
		LET m_data_dict_direct_path	=	"/u/projects/xsrce/xact_ii_dev_tested/1.00/data_dict/"
		LET m_menu_direct_path		=	"/u/xact_misc_data/dev_tested_db/menu_structure/"
		LET m_pics_direct_path		=	"/u/projects/xsrce/xact_ii_dev_tested/1.00/pics/"
		LET m_code_250_direct_path	=	"/u/projects/xsrce/xact_ii_dev_tested/1.00/bin/"
		LET m_code_310_direct_path	=	"/u/projects/xsrce/xact_ii_dev_tested_310/1.00/bin/"
	ELSE
		LET m_etc_direct_path		=	"/u/projects/xsrce/xact_ii/1.00/etc/"
		LET m_data_dict_direct_path	=	"/u/projects/xsrce/xact_ii/1.00/data_dict/"
		LET m_menu_direct_path		=	"/u/xact_misc_data/xsrce_db/menu_structure/"
		LET m_pics_direct_path		=	"/u/projects/xsrce/xact_ii/1.00/pics/"
		LET m_code_250_direct_path	=	"/u/projects/xsrce/xact_ii/1.00/bin/"
		LET m_code_310_direct_path	=	"/u/projects/xsrce/xact_ii_310/1.00/bin/"
	END IF

END FUNCTION

{==================================================================================================================================}
#+ CHECK IN USE
#+
#+ BUSINESS RULE: 
#+ Check for in use logs on the DBs
#+
#+ @code    CALL sb_check_in_use( p_cnt, p_err_src )
#+
#+ @param   p_cnt 			Server array counter
#+ @param   p_err_src       Error tracking string.
#+
#+ @return  NONE 
#+
#+ CHANGES:
#+
FUNCTION sb_check_in_use( p_cnt, p_err_src )

	DEFINE	p_cnt			INTEGER,
			p_err_src		STRING,

			l_check			INTEGER,
			l_cnt			INTEGER,
			l_ch			base.channel,
			l_file			STRING,
			l_str_for_txt	STRING,
			l_file_txt		STRING

	LET p_err_src = p_err_src || " > sb_check_in_use"

	LET l_file			= NULL
	LET l_file_txt		= NULL
	LET l_str_for_txt	= NULL

	CALL sy_pop_up_message ( "CHECKING FOR IN USE LOGS!", "OPEN" )

	--echo the environment script
	RUN "echo . /opt/xact/scripts/env_vars_310.sh > /u/xact_misc_data/dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".sh"

	--echo run dbaccess command
	RUN "echo dbaccess "|| ma_db_list[1].db ||" /opt/xact/xact_ii/data_dict/in_use_check_" || ma_server_list[p_cnt].server || ".sql >> /u/xact_misc_data/dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".sh"
	
	FOR l_cnt = 1 TO ma_db_list.getLength()
		--echo Create SQL file for checking the in use log
		RUN "echo 'SELECT * FROM "|| ma_db_list[l_cnt].db ||":sy37_in_use_log;' >> /u/xact_misc_data/dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".sql"
	END FOR

	--Move dbaccess script to relevant server
	RUN "sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete /u/xact_misc_data/xsrce_db/in_use_check_" || ma_server_list[p_cnt].server || ".sh xactii@"|| ma_server_list[p_cnt].ip ||":/opt/xact/xact_ii/data_dict/  >> /u/xact_misc_data/dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".log 2>&1"

	--Move SQL script to relevant server
	RUN "sudo -u xactii rsync -avzr -e 'ssh -p"|| ma_server_list[p_cnt].port||"' --delete /u/xact_misc_data/xsrce_db/in_use_check_" || ma_server_list[p_cnt].server || ".sql xactii@"|| ma_server_list[p_cnt].ip ||":/opt/xact/xact_ii/data_dict/ >> /u/xact_misc_data/dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".log 2>&1"

	--make in_use_check.sh executable
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'chmod +x /opt/xact/xact_ii/data_dict/in_use_check_" || ma_server_list[p_cnt].server || ".sh'"

	--Run in_use_check.sh script
	RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'sudo -u informix /opt/xact/xact_ii/data_dict/in_use_check_" || ma_server_list[p_cnt].server || ".sh' >> /u/xact_misc_data/dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".log 2>&1" RETURNING l_check

--Get log file to read
	LET l_file  = fgl_getEnv( "XACT_MISC_DATA_PATH" ) || m_sep || "dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".log"

	LET l_ch	= base.channel.create()
	
	CALL l_ch.openfile( l_file, "r" )

--Build log file into a string
	WHILE l_ch.read( l_file_txt )
		IF ( l_str_for_txt IS NULL ) THEN
			LET l_str_for_txt = l_file_txt
		ELSE
			IF ( l_file_txt IS NULL ) THEN
				LET l_str_for_txt = l_str_for_txt || "\n"
			ELSE
				LET l_str_for_txt = l_str_for_txt || "\n" || l_file_txt
			END IF
		END IF
	END WHILE

	CALL l_ch.close()
	
--check if in use was found was successful then delete g_path_tmp_files 
	IF ( l_str_for_txt MATCHES "*retrieved*" ) THEN

		--email support when month end backup failed
		CALL lf_email_file(	{prompt_user}	FALSE,
							{bcc_user}		TRUE,
							{file_type}		"body",
							{filename}		NULL,
							{email_subject}	ma_server_list[p_cnt].server || ": In use check",
							{email_address}	"dev@xacterp.co.za",
							{email_body}	"There is an in use record on " || ma_server_list[p_cnt].server,
							{err_src}		p_err_src)
		CALL fgl_winmessage( %"In Use", %"In use check Failed! ", "exclamation" )
		CALL sy_pop_up_message ( "In Use Check Finished", "CLOSE" )
		RETURN FALSE

	ELSE
		--remove in_use_check.sh from g_path_tmp_files
		RUN "rm /u/xact_misc_data/dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".sh"
		RUN "rm /u/xact_misc_data/dx150_scripts/in_use_check_" || ma_server_list[p_cnt].server || ".sql"
		RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'rm /opt/xact/xact_ii/data_dict/in_use_check_" || ma_server_list[p_cnt].server || ".sh'"
		RUN "sudo -u xactii ssh -p "||ma_server_list[p_cnt].port||" xactii@"||ma_server_list[p_cnt].ip||" 'rm /opt/xact/xact_ii/data_dict/in_use_check_" || ma_server_list[p_cnt].server || ".sql'"
	END IF 

	CALL sy_pop_up_message ( "In Use Check Finished", "CLOSE" )

	RETURN TRUE

END FUNCTION
{==================================================================================================================================}
#+ BUILD DIRECTORIES
#+
#+ BUSINESS RULE: 
#+ Build file directories
#+
#+ @code    CALL sb_build_dir( p_err_src )
#+
#+ @param   p_err_src       Error tracking string.
#+
#+ @return  NONE 
#+
#+ CHANGES:
#+
FUNCTION sb_build_dir( p_err_src )

	DEFINE	p_err_src		STRING

	LET p_err_src = p_err_src || " > sb_build_dir"
	
	CASE (gr_sy02.access_grp)

		WHEN "Z0"
			LET m_etc_direct_path		=	"/u/projects/xsrce/xact_ii/1.00/etc/"
			LET m_data_dict_direct_path	=	"/u/projects/xsrce/xact_ii/1.00/data_dict/"
			LET m_menu_direct_path		=	"/u/xact_misc_data/xsrce_db/menu_structure/"
			LET m_pics_direct_path		=	"/u/projects/xsrce/xact_ii/1.00/pics/"
			LET m_code_250_direct_path	=	"/u/projects/xsrce/xact_ii/1.00/bin/"
			LET m_code_310_direct_path	=	"/u/projects/xsrce/xact_ii_310/1.00/bin/"
			
		WHEN "Z1"
			LET m_etc_direct_path		=	"/u/projects/xsrce/xact_ii_dev_tested/1.00/etc/"
			LET m_data_dict_direct_path	=	"/u/projects/xsrce/xact_ii_dev_tested/1.00/data_dict/"
			LET m_menu_direct_path		=	"/u/xact_misc_data/dev_tested_db/menu_structure/"
			LET m_pics_direct_path		=	"/u/projects/xsrce/xact_ii_dev_tested/1.00/pics/"
			LET m_code_250_direct_path	=	"/u/projects/xsrce/xact_ii_dev_tested/1.00/bin/"
			LET m_code_310_direct_path	=	"/u/projects/xsrce/xact_ii_dev_tested_310/1.00/bin/"
			
		WHEN "Z2"
		
	END CASE

END FUNCTION
{==================================================================================================================================}
{==================================================================================================================================}
{
										END OF GENERAL FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}

{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
												INITIALISATION FUNCTIONS
}
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION INIT_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ BUILD COMBO BOX
#+
#+ BUSINESS RULE: 
#+ Build Combo Box
#+
#+ @code    CALL sb_bld_cbox_build(p_cbox)
#+
#+ @param   p_cbox       Combobox
#+
#+ @return  NONE 
#+
#+ CHANGES:
#+
FUNCTION sb_bld_cbox_build(p_cbox)
		
    DEFINE	p_cbox        	ui.combobox,
	
			l_cbox_tag    	CHAR(20),
			l_cnt			INTEGER,
			l_err_src 		STRING

	TRY			
		LET	l_err_src 	= l_err_src || " > sb_cbox_build"		
		LET l_cbox_tag 	= p_cbox.gettag()
		LET l_cnt		= 1
            
        --------------------------------------------------
			CASE (l_cbox_tag) 
				--------------------------------------------------
					WHEN "man_or_auto"
				 
						CALL p_cbox.clear()
						CALL p_cbox.additem("now","Manual")

				--------------------------------------------------
					WHEN "backup_server_db"
				 
						CALL p_cbox.clear()
						CALL p_cbox.additem("Y","Yes")
						CALL p_cbox.additem("N","No")

				--------------------------------------------------
					WHEN "backup_code"
				 
						CALL p_cbox.clear()
						CALL p_cbox.additem("Y","Yes")
						CALL p_cbox.additem("N","No")

				--------------------------------------------------
					WHEN "check_in_use"
				 
						CALL p_cbox.clear()
						CALL p_cbox.additem("Y","Yes")
						CALL p_cbox.additem("N","No")
						
				--------------------------------------------------
			END CASE 

    CATCH 
        LET l_cbox_tag	= lf_sql_error( l_cbox_tag, " > sb_bld_cbox_build" )
		CALL fgl_winmessage( %"Error cbox_b.", l_cbox_tag , "stop" )
    END TRY 
    
END FUNCTION 
{==================================================================================================================================}
#+ MODULE INIT
#+
#+ BUSINESS RULE:  
#+ Setup any module specific tasks not done in lf_progr_init
#+
#+
#+ @code    CALL sb_module_init( p_err_src )
#+
#+ @param   p_err_src 	Error Source from calling statement 
#+
#+ @return  NONE
#+
#+ CHANGES
#+
FUNCTION sb_module_init( p_err_src )

    DEFINE	p_err_src			STRING

	LET p_err_src = p_err_src || " > sb_module_init"
	
--Set up Operating Specific Variables
	LET m_sep = os.path.separator()

--Fetch Environment Variables
	LET m_path_env_script	= fgl_getEnv( "XACT_ENV_SCRIPT" )
	LET m_path_etc			= fgl_getEnv( "XACT_ETC" )
	
--Build up the list companies into a list.
	CALL sb_build_company_list( p_err_src )
END FUNCTION


{==================================================================================================================================}
{==================================================================================================================================}
{
										END OF INITIALISATION FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}