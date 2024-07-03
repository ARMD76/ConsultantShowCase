create or replace PACKAGE       XXCUS_AP_ISUPPS_UTILS_PKG AUTHID CURRENT_USER AS
   /***************************************************************************************
    # Modulo        : XXCUS_AP_ISUPPS_UTILS_PKG
    # Autor         : armando.mendez
    # Versión       : 1.0
    # Fecha         : 30/07/2020
    # Descripción   : Paquete para asistir al portal de proveedores
    #
    #
    # Ejecutado Por : usuario responsable/ portal de proveedores
    #
    # Ejecuciones   : usuarios
    #
    # Modificado Por           Fecha          Descripción
    # -------------------------------------------------------------------------------------
    # Armando Méndez           30-JUL-2020    Funcion de seteo de unidad operativa para extraccion de monto
    # Armando Méndez           10-AUG-2020    Procedimiento de actualizacion masiva de Bill to location
    ***************************************************************************************/

                                   
  FUNCTION Get_rcv_txn_qty_amt(p_org_id              IN NUMBER,
                              p_rcv_transaction_id    IN NUMBER,
                              p_matching_basis        IN VARCHAR2,
                              p_returned_item         IN VARCHAR2)
                              RETURN NUMBER;
                              
  PROCEDURE updateTerms ( errbuf          OUT VARCHAR2,
                                   retcode         OUT NUMBER,
                                   pPath           IN  VARCHAR2,
                                   pFileName       IN  VARCHAR2,
                                   pTipo           IN  VARCHAR2);
                                  
  PROCEDURE updateBillToLoc ( errbuf          OUT VARCHAR2,
                                   retcode         OUT NUMBER,
                                   pPath           IN  VARCHAR2,
                                   pFileName       IN  VARCHAR2,
                                   pTipo           IN  VARCHAR2);
                                   
  PROCEDURE deactivateSupplier ( errbuf          OUT VARCHAR2,
                                   retcode         OUT NUMBER,
                                   pPath           IN  VARCHAR2,
                                   pFileName       IN  VARCHAR2,
                                   pTipo           IN  VARCHAR2);
                                   
  PROCEDURE updateAmountTaxFlag ( errbuf          OUT VARCHAR2,
                                   retcode         OUT NUMBER,
                                   pTipo           IN  VARCHAR2);
                                   
                              
END XXCUS_AP_ISUPPS_UTILS_PKG;
/
create or replace PACKAGE BODY       XXCUS_AP_ISUPPS_UTILS_PKG AS
   g_retcode    NUMBER;
    /***************************************************************************************
    # Modulo        : XXCUS_AP_ISUPPS_UTILS_PKG
    # Autor         : armando.mendez
    # Versión       : 1.0
    # Fecha         : 30/07/2020
    # Descripción   : Paquete para asistir el portal de proveedores
    #
    #
    # Ejecutado Por : usuario responsable/ portal de proveedores
    #
    # Ejecuciones   : usuarios
    #
    # Modificado Por           Fecha          Descripción
    # -------------------------------------------------------------------------------------
    # Armando Méndez           30-JUL-2020    Funcion de seteo de unidad operativa para extraccion de monto
    # Armando Méndez           10-AUG-2020    Procedimiento de actualizacion masiva del bill to location
    ***************************************************************************************/
   PROCEDURE writeLog (p_msg        IN VARCHAR2,
                       p_split_in   IN PLS_INTEGER DEFAULT 32767) IS
      l_to       NUMBER;
      l_substr   VARCHAR2 (32767);
   BEGIN
      FOR i IN 1 .. CEIL (LENGTH (p_msg) / p_split_in)
      LOOP
         IF apps.fnd_global.conc_request_id != -1
         THEN
            fnd_file.put_line (
               which   => fnd_file.LOG,
               buff    => SUBSTR (p_msg, p_split_in * (i - 1) + 1, p_split_in));
         ELSE
            DBMS_OUTPUT.put_line (
               SUBSTR (p_msg, p_split_in * (i - 1) + 1, p_split_in));
         END IF;
      END LOOP;
   EXCEPTION
      WHEN OTHERS
      THEN
         fnd_file.put_line (
            which   => fnd_file.LOG,
            buff    => 'writeLog::No se pudo escribir el msg:');
         fnd_file.put_line (which   => fnd_file.LOG,
                            buff    => 'write_log::P_MSG:' || p_msg);
         fnd_file.put_line (which   => fnd_file.LOG,
                            buff    => 'write_log::P_SPLIT_IN:' || P_SPLIT_IN);
         fnd_file.put_line (which   => fnd_file.LOG,
                            buff    => 'write_log::L_TO:' || L_TO);
         fnd_file.put_line (which   => fnd_file.LOG,
                            buff    => 'write_log::L_SUBSTR:' || L_SUBSTR);
         fnd_file.put_line (which => fnd_file.LOG, buff => 'write_log::Error:');
         fnd_file.put_line (which   => fnd_file.LOG,
                            buff    => 'write_log::' || SQLERRM);
         fnd_file.put_line (
            which   => fnd_file.LOG,
            buff    => 'write_log::' || DBMS_UTILITY.format_error_backtrace);
         fnd_file.put_line (
            which   => fnd_file.LOG,
            buff    => 'write_log::' || DBMS_UTILITY.format_error_stack);
         fnd_file.put_line (
            which   => fnd_file.LOG,
            buff    => 'write_log::' || DBMS_UTILITY.format_call_stack);
   END writeLog;

   PROCEDURE writeOut (pMessage  IN VARCHAR2) IS
   BEGIN
      FND_FILE.PUT_LINE(FND_FILE.OUTPUT,pMessage);
      DBMS_OUTPUT.ENABLE(20000);
      DBMS_OUTPUT.PUT_LINE(pMessage);
   END;

   PROCEDURE log_trace
   IS
   BEGIN
      writeLog (p_msg => DBMS_UTILITY.format_error_backtrace);
      writeLog (p_msg => DBMS_UTILITY.format_error_stack);
      writeLog (p_msg => DBMS_UTILITY.format_call_stack);
      g_retcode := 1;
   END log_trace;
   --
   --
   FUNCTION ChangeLocation (pExternalTable  IN  VARCHAR2,
                            pPath           IN  VARCHAR2,
                            pFileName       IN  VARCHAR2
                            ) RETURN BOOLEAN IS

      vlDirectoryName         all_directories.directory_name%TYPE;
      vlReturn                BOOLEAN;

      CURSOR CDirectory IS
         SELECT directory_name
         FROM   all_directories
         WHERE  directory_path = pPath;

   BEGIN
      OPEN CDirectory;
      FETCH CDirectory INTO vlDirectoryName;
      IF CDirectory%NOTFOUND THEN
         writeOut('Directorio '||pPath||' Invalido');
         writeLog('Directorio '||pPath||' Invalido');
         vlReturn := FALSE;
      ELSE
         EXECUTE IMMEDIATE 'ALTER TABLE '||pExternalTable||' LOCATION ('||vlDirectoryName||':'||''''||pFileName||''')';
         vlReturn := TRUE;
      END IF;
      CLOSE CDirectory;

      RETURN vlReturn;
   END;
   --
   --
   
   PROCEDURE updateTerms ( errbuf          OUT VARCHAR2,
                                   retcode         OUT NUMBER,
                                   pPath           IN  VARCHAR2,
                                   pFileName       IN  VARCHAR2,
                                   pTipo           IN  VARCHAR2) IS

      CURSOR CSuppTerms IS
         SELECT
				REPLACE(rfc_proveedor, CHR(13), NULL) rfc_proveedor,
				REPLACE(site_proveedor, CHR(13), NULL) site_proveedor,
				REPLACE(grupo_pago, CHR(13), NULL) grupo_pago,
				REPLACE(terminos_pago, CHR(13), NULL) terminos_pago
		  FROM
				xxCUS.CUS_ap_updated_terms;
        
        
      vlError                    BOOLEAN;
      vlEstatus                  VARCHAR2(1);
      vlMensaje                  VARCHAR2(4000);
      vlMessageError             VARCHAR2(4000);

      vlCountOK                  NUMBER := 0;
      vlCountError               NUMBER := 0;
      
      lr_vendor_site_rec          apps.ap_vendor_pub_pkg.r_vendor_site_rec_type;
      lr_existing_vendor_site_rec apps.ap_supplier_sites_all%ROWTYPE;
      
      p_api_version               NUMBER;
      p_init_msg_list             VARCHAR2(200);
      p_commit                    VARCHAR2(200);
      p_validation_level          NUMBER;
      p_calling_prog              VARCHAR2(200);
      

      x_msg_count                NUMBER;
      x_term_id                  NUMBER;
      x_msg_data                 VARCHAR2(4000);
      x_return_status            VARCHAR2(4000);
      x_vendor_site_id        NUMBER;
      x_per_party_id             NUMBER;
      x_rel_party_id             NUMBER;
      x_rel_id                   NUMBER;
      x_org_contact_id           NUMBER;
      x_party_site_id            NUMBER;

   BEGIN
      IF pTipo = 'P' THEN
         writeOut('************************');
         writeOut('* Tipo de Carga Previa *');
         writeOut('************************');
         writeOut(' ');
      ELSE
         writeOut('****************************');
         writeOut('* Tipo de Carga Definitiva *');
         writeOut('****************************');
         writeOut(' ');
      END IF;

      IF NOT ChangeLocation (pExternalTable  => 'xxCUS.CUS_ap_updated_terms',
                             pPath           => pPath,
                             pFileName       => pFileName) THEN
         retcode := 2;
         RETURN;
      END IF;

      writeOut (' ');
      writeOut ('======= Interface de Contactos =======');

      FOR RSuppTerms IN CSuppTerms LOOP
         vlError := FALSE;

         writeOut (' ');
         writeOut ('Actualizando Termino de Pago - Proveedor '||RSuppTerms.rfc_proveedor);
         
         BEGIN
            select max(term_id) 
            into x_term_id
            from apps.ap_terms_tl 
            where TRANSLATE(upper(trim(chr(160) from trim(name))), 'ÁÉÍÓÚÑáéíóúñ','AEIOUNaeioun') = 
            TRANSLATE(upper(trim(chr(160) from trim(RSuppTerms.terminos_pago))), 'ÁÉÍÓÚÑáéíóúñ','AEIOUNaeioun');
         EXCEPTION
            WHEN OTHERS THEN
               vlError := TRUE;
               vlMessageError := 'No existe termino de pago ';
         END;
         
         IF NOT vlError THEN
             BEGIN
                SELECT assa.*
                INTO lr_existing_vendor_site_rec
                FROM apps.ap_supplier_sites_all assa,
                apps.ap_suppliers ass
                WHERE assa.vendor_id = ass.vendor_id
                AND upper(trim(chr(160) from trim(ass.segment1))) = upper(trim(chr(160) from trim(RSuppTerms.rfc_proveedor)))
                AND upper(trim(chr(160) from trim(assa.vendor_site_code))) = upper(trim(chr(160) from trim(RSuppTerms.site_proveedor)))
                AND assa.org_id in (select organization_id from apps.hr_all_organization_units 
                                    where upper(trim(chr(160) from trim(name))) in 
                                    ('UO_CUS')
                                    );
                
                SELECT assa.vendor_site_id
                INTO x_vendor_site_id
                FROM apps.ap_supplier_sites_all assa,
                apps.ap_suppliers ass
                WHERE assa.vendor_id = ass.vendor_id
                AND upper(trim(chr(160) from trim(ass.segment1))) = upper(trim(chr(160) from trim(RSuppTerms.rfc_proveedor)))
                AND upper(trim(chr(160) from trim(assa.vendor_site_code))) = upper(trim(chr(160) from trim(RSuppTerms.site_proveedor)))
                AND assa.org_id in (select organization_id from apps.hr_all_organization_units 
                                    where upper(trim(chr(160) from trim(name)))in 
                                    ('UO_CUS')
                                    );
             EXCEPTION
                WHEN OTHERS THEN
                   vlError := TRUE;
                   vlMessageError := 'No existe proveedor y/o site de proveedor';
             END;
         END IF;

         IF NOT vlError THEN
              p_api_version      := 1.0;
              p_init_msg_list    := apps.fnd_api.g_true;
              p_commit           := apps.fnd_api.g_false;
              p_validation_level := apps.fnd_api.g_valid_level_full;
              p_calling_prog     := 'XXCUS';
              -- Assign Vendor Site Details
              lr_vendor_site_rec.vendor_site_id   := lr_existing_vendor_site_rec.vendor_site_id;
              lr_vendor_site_rec.last_update_date := SYSDATE;
              --lr_vendor_site_rec.last_updated_by  := 1119;
              lr_vendor_site_rec.vendor_id        := lr_existing_vendor_site_rec.vendor_id;
              lr_vendor_site_rec.org_id           := lr_existing_vendor_site_rec.org_id;
              --lr_vendor_site_rec.inactive_date    := SYSDATE;
              lr_vendor_site_rec.terms_id          := x_term_id;
              
              DBMS_APPLICATION_INFO.set_client_info (lr_existing_vendor_site_rec.org_id);
              apps.mo_global.set_policy_context ('S', lr_existing_vendor_site_rec.org_id);
             
              apps.AP_VENDOR_PUB_PKG.UPDATE_VENDOR_SITE(p_api_version      => p_api_version,
                                                   p_init_msg_list    => p_init_msg_list,
                                                   p_commit           => p_commit,
                                                   p_validation_level => p_validation_level,
                                                   x_return_status    => x_return_status,
                                                   x_msg_count        => x_msg_count,
                                                   x_msg_data         => x_msg_data,
                                                   p_vendor_site_rec  => lr_vendor_site_rec,
                                                   p_vendor_site_id   => x_vendor_site_id,
                                                   p_calling_prog     => p_calling_prog);
                     
            x_msg_data := SUBSTR(x_msg_data,1,250);
            IF x_return_status = 'S'
            THEN
               vlCountOK := vlCountOK + 1;
               writeOut ('  => Se actualizo el termino de pago para el proveedor:     ' ||RSuppTerms.rfc_proveedor||' '||'su nuevo termino de pago es:   '||RSuppTerms.terminos_pago);
            ELSE
               vlCountError := vlCountError + 1;
               writeOut ('  => Error al actualizar el termino de pago Para El Proveedor : '||RSuppTerms.rfc_proveedor);
               FOR j IN 1..apps.fnd_msg_pub.count_msg LOOP
                  writeOut('    '||apps.fnd_msg_pub.get(p_msg_index => j, p_encoded => 'F'));
               END LOOP;
            END IF;
         END IF;

         IF vlError THEN
            vlCountError := vlCountError + 1;
            writeOut('  = Error al actualizar el termino de pago para el proveedor '||RSuppTerms.rfc_proveedor||' '||vlMessageError);
         END IF;

      END LOOP;

      writeOut('  ');
      writeOut('===================================');
      writeOut('ReCUStros exitosos  '||vlCountOK);
      writeOut('ReCUStros con error '||vlCountError);
      writeOut('===================================');
      writeOut('  ');

      IF pTipo = 'P' THEN
         ROLLBACK;
      ELSE
         COMMIT;
      END IF;
   END updateTerms;
   
    PROCEDURE updateBillToLoc ( errbuf          OUT VARCHAR2,
                                   retcode         OUT NUMBER,
                                   pPath           IN  VARCHAR2,
                                   pFileName       IN  VARCHAR2,
                                   pTipo           IN  VARCHAR2) IS

      CURSOR CSuppBillTo IS
         SELECT
				REPLACE(rfc_proveedor, CHR(13), NULL) rfc_proveedor,
				REPLACE(site_proveedor, CHR(13), NULL) site_proveedor,
				REPLACE(organizacion, CHR(13), NULL) organizacion,
				REPLACE(bill_to_loc, CHR(13), NULL) bill_to_loc
		  FROM
				xxCUS.CUS_ap_updated_bill_location;
        
        
      vlError                    BOOLEAN;
      vlEstatus                  VARCHAR2(1);
      vlMensaje                  VARCHAR2(4000);
      vlMessageError             VARCHAR2(4000);

      vlCountOK                  NUMBER := 0;
      vlCountError               NUMBER := 0;
      
      lr_vendor_site_rec          apps.ap_vendor_pub_pkg.r_vendor_site_rec_type;
      lr_existing_vendor_site_rec apps.ap_supplier_sites_all%ROWTYPE;
      
      p_api_version               NUMBER;
      p_init_msg_list             VARCHAR2(200);
      p_commit                    VARCHAR2(200);
      p_validation_level          NUMBER;
      p_calling_prog              VARCHAR2(200);
      

      x_msg_count                NUMBER;
      x_location_id                  NUMBER;
      x_msg_data                 VARCHAR2(4000);
      x_return_status            VARCHAR2(4000);
      x_vendor_site_id        NUMBER;
      x_per_party_id             NUMBER;
      x_rel_party_id             NUMBER;
      x_rel_id                   NUMBER;
      x_org_contact_id           NUMBER;
      x_party_site_id            NUMBER;

   BEGIN
      IF pTipo = 'P' THEN
         writeOut('************************');
         writeOut('* Tipo de Carga Previa *');
         writeOut('************************');
         writeOut(' ');
      ELSE
         writeOut('****************************');
         writeOut('* Tipo de Carga Definitiva *');
         writeOut('****************************');
         writeOut(' ');
      END IF;

      IF NOT ChangeLocation (pExternalTable  => 'xxCUS.CUS_ap_updated_bill_location',
                             pPath           => pPath,
                             pFileName       => pFileName) THEN
         retcode := 2;
         RETURN;
      END IF;

      writeOut (' ');
      writeOut ('======= Interface de Sites =======');
      
      EXECUTE IMMEDIATE ('ALTER SESSION SET NLS_LANGUAGE = ''American''');

      FOR RSuppBillTo IN CSuppBillTo LOOP
         vlError := FALSE;

         writeOut (' ');
         writeOut ('Actualizando Bill to Location - Proveedor '||RSuppBillTo.rfc_proveedor);
         
         BEGIN
            select max(location_id)
            into x_location_id
            from hr_locations hrl 
            where TRANSLATE(upper(trim(chr(160) from trim(location_code))), 'ÁÉÍÓÚÑáéíóúñ','AEIOUNaeioun') = 
            TRANSLATE(upper(trim(chr(160) from trim(RSuppBillTo.bill_to_loc))), 'ÁÉÍÓÚÑáéíóúñ','AEIOUNaeioun');
         EXCEPTION
            WHEN OTHERS THEN
               vlError := TRUE;
               vlMessageError := 'No existe location code ';
         END;
         
         IF NOT vlError THEN
             BEGIN
                SELECT assa.*
                INTO lr_existing_vendor_site_rec
                FROM apps.ap_supplier_sites_all assa,
                apps.ap_suppliers ass
                WHERE assa.vendor_id = ass.vendor_id
                AND upper(trim(chr(160) from trim(ass.segment1))) = upper(trim(chr(160) from trim(RSuppBillTo.rfc_proveedor)))
                AND upper(trim(chr(160) from trim(assa.vendor_site_code))) = upper(trim(chr(160) from trim(RSuppBillTo.site_proveedor)))
                AND assa.org_id in (select organization_id from apps.hr_all_organization_units 
                                    where upper(trim(chr(160) from trim(name))) = 
                                    (RSuppBillTo.organizacion));
             EXCEPTION
                WHEN OTHERS THEN
                   vlError := TRUE;
                   vlMessageError := 'No existe proveedor y/o site de proveedor';
             END;
         END IF;

         IF NOT vlError THEN
              p_api_version      := 1.0;
              p_init_msg_list    := apps.fnd_api.g_true;
              p_commit           := apps.fnd_api.g_false;
              p_validation_level := apps.fnd_api.g_valid_level_full;
              p_calling_prog     := 'XXCUS';
              -- Assign Vendor Site Details
              lr_vendor_site_rec.vendor_site_id   := lr_existing_vendor_site_rec.vendor_site_id;
              lr_vendor_site_rec.last_update_date := SYSDATE;
              --lr_vendor_site_rec.last_updated_by  := 1119;
              lr_vendor_site_rec.vendor_id        := lr_existing_vendor_site_rec.vendor_id;
              lr_vendor_site_rec.org_id           := lr_existing_vendor_site_rec.org_id;
              --lr_vendor_site_rec.inactive_date    := SYSDATE;
              lr_vendor_site_rec.bill_to_location_id          := x_location_id;
              
              DBMS_APPLICATION_INFO.set_client_info (lr_existing_vendor_site_rec.org_id);
              apps.mo_global.set_policy_context ('S', lr_existing_vendor_site_rec.org_id);
             
              apps.AP_VENDOR_PUB_PKG.UPDATE_VENDOR_SITE(p_api_version      => p_api_version,
                                                   p_init_msg_list    => p_init_msg_list,
                                                   p_commit           => p_commit,
                                                   p_validation_level => p_validation_level,
                                                   x_return_status    => x_return_status,
                                                   x_msg_count        => x_msg_count,
                                                   x_msg_data         => x_msg_data,
                                                   p_vendor_site_rec  => lr_vendor_site_rec,
                                                   p_vendor_site_id   => lr_existing_vendor_site_rec.vendor_site_id,
                                                   p_calling_prog     => p_calling_prog);
                     
            x_msg_data := SUBSTR(x_msg_data,1,250);
            IF x_return_status = 'S'
            THEN
               vlCountOK := vlCountOK + 1;
               writeOut ('  => Se actualizo el bill to location para el proveedor:     ' ||RSuppBillTo.rfc_proveedor||' '||'su nuevo bill to location es:   '||RSuppBillTo.bill_to_loc);
            ELSE
               vlCountError := vlCountError + 1;
               writeOut ('  => Error al actualizar el bill to location Para El Proveedor : '||RSuppBillTo.rfc_proveedor);
               FOR j IN 1..apps.fnd_msg_pub.count_msg LOOP
                  writeOut('    '||apps.fnd_msg_pub.get(p_msg_index => j, p_encoded => 'F'));
               END LOOP;
            END IF;
         END IF;

         IF vlError THEN
            vlCountError := vlCountError + 1;
            writeOut('  = Error al actualizar el bill to location para el proveedor '||RSuppBillTo.rfc_proveedor||' '||vlMessageError);
         END IF;

      END LOOP;

      writeOut('  ');
      writeOut('===================================');
      writeOut('ReCUStros exitosos  '||vlCountOK);
      writeOut('ReCUStros con error '||vlCountError);
      writeOut('===================================');
      writeOut('  ');

      IF pTipo = 'P' THEN
         ROLLBACK;
      ELSE
         COMMIT;
      END IF;
   END updateBillToLoc;
   
    PROCEDURE deactivateSupplier ( errbuf          OUT VARCHAR2,
                                   retcode         OUT NUMBER,
                                   pPath           IN  VARCHAR2,
                                   pFileName       IN  VARCHAR2,
                                   pTipo           IN  VARCHAR2) IS

      CURSOR CSuppDeact IS
         SELECT
				REPLACE(rfc_proveedor, CHR(13), NULL) rfc_proveedor,
				REPLACE(site_proveedor, CHR(13), NULL) site_proveedor,
				REPLACE(organizacion, CHR(13), NULL) organizacion,
				REPLACE(end_date, CHR(13), NULL) end_date
		  FROM
				xxCUS.CUS_ap_deactivate_supplier
        WHERE site_proveedor is null;
        
        CURSOR CSiteDeact IS
         SELECT
				REPLACE(rfc_proveedor, CHR(13), NULL) rfc_proveedor,
				REPLACE(site_proveedor, CHR(13), NULL) site_proveedor,
				REPLACE(organizacion, CHR(13), NULL) organizacion,
				REPLACE(end_date, CHR(13), NULL) end_date
		  FROM
				xxCUS.CUS_ap_deactivate_supplier
        WHERE site_proveedor is not null;
        
        
      vlError                    BOOLEAN;
      vlEstatus                  VARCHAR2(1);
      vlMensaje                  VARCHAR2(4000);
      vlMessageError             VARCHAR2(4000);

      vlCountOK                  NUMBER := 0;
      vlCountError               NUMBER := 0;
      
      lr_vendor_site_rec          apps.ap_vendor_pub_pkg.r_vendor_site_rec_type;
      lrec_vendor_rec             apps.ap_vendor_pub_pkg.r_vendor_rec_type;
      lr_existing_vendor_site_rec apps.ap_supplier_sites_all%ROWTYPE;
      Ln_Vendor_Id              NUMBER;
      
      p_api_version               NUMBER;
      p_init_msg_list             VARCHAR2(200);
      p_commit                    VARCHAR2(200);
      p_validation_level          NUMBER;
      p_calling_prog              VARCHAR2(200);
      

      x_msg_count                NUMBER;
      x_location_id                  NUMBER;
      x_msg_data                 VARCHAR2(4000);
      x_return_status            VARCHAR2(4000);
      x_vendor_site_id        NUMBER;
      x_per_party_id             NUMBER;
      x_rel_party_id             NUMBER;
      x_rel_id                   NUMBER;
      x_org_contact_id           NUMBER;
      x_party_site_id            NUMBER;

   BEGIN
      IF pTipo = 'P' THEN
         writeOut('************************');
         writeOut('* Tipo de Carga Previa *');
         writeOut('************************');
         writeOut(' ');
      ELSE
         writeOut('****************************');
         writeOut('* Tipo de Carga Definitiva *');
         writeOut('****************************');
         writeOut(' ');
      END IF;

      IF NOT ChangeLocation (pExternalTable  => 'xxCUS.CUS_ap_deactivate_supplier',
                             pPath           => pPath,
                             pFileName       => pFileName) THEN
         retcode := 2;
         RETURN;
      END IF;
      
      EXECUTE IMMEDIATE ('ALTER SESSION SET NLS_LANGUAGE = ''American''');
      
            writeOut (' ');
      writeOut ('======= Interface de Proveedor =======');
      
      

      FOR RSuppDeact IN CSuppDeact LOOP
         vlError := FALSE;

         writeOut (' ');
         writeOut ('Inactivando Proveedor - Proveedor '||RSuppDeact.rfc_proveedor);
         
         IF NOT vlError THEN
             BEGIN
                SELECT ass.vendor_id
                INTO Ln_Vendor_Id
                FROM apps.ap_suppliers ass
                WHERE upper(trim(chr(160) from trim(ass.segment1))) = upper(trim(chr(160) from trim(RSuppDeact.rfc_proveedor)));
                
             EXCEPTION
                WHEN OTHERS THEN
                   vlError := TRUE;
                   vlMessageError := 'No existe proveedor';
             END;
         END IF;

         IF NOT vlError THEN
              p_api_version      := 1.0;
              p_init_msg_list    := apps.fnd_api.g_true;
              p_commit           := apps.fnd_api.g_false;
              p_validation_level := apps.fnd_api.g_valid_level_full;
              p_calling_prog     := 'XXCUS';
              -- Assign Vendor Site Details
              Lrec_Vendor_Rec.END_DATE_ACTIVE     := to_date(RSuppDeact.end_date,'dd/mm/yyyy');
              
              apps.ap_vendor_pub_pkg.update_vendor_public( p_api_version => p_api_version,--
                                                      x_return_status => x_return_status,                 --
                                                      x_msg_count => x_msg_count,                         --
                                                      x_msg_data => x_msg_data,                           --
                                                      p_vendor_rec => Lrec_Vendor_Rec,                     --
                                                      p_Vendor_Id => Ln_Vendor_Id);
                     
            x_msg_data := SUBSTR(x_msg_data,1,250);
            IF x_return_status = 'S'
            THEN
               vlCountOK := vlCountOK + 1;
               writeOut ('  => Se aplico la fecha de inactivacion al proveedor:  ' ||RSuppDeact.rfc_proveedor||' '||'fecha de inactivacion es:   '||RSuppDeact.end_date);
            ELSE
               vlCountError := vlCountError + 1;
               writeOut ('  => Error al actualizar la fecha de inactivacion Para El Proveedor : '||RSuppDeact.rfc_proveedor);
               FOR j IN 1..apps.fnd_msg_pub.count_msg LOOP
                  writeOut('    '||apps.fnd_msg_pub.get(p_msg_index => j, p_encoded => 'F'));
               END LOOP;
            END IF;
         END IF;

         IF vlError THEN
            vlCountError := vlCountError + 1;
            writeOut('  = Error al actualizar la fecha de inactivacion para el proveedor '||RSuppDeact.rfc_proveedor||' '||vlMessageError);
         END IF;

      END LOOP;

      writeOut (' ');
      writeOut ('======= Interface de Sites =======');
      
      

      FOR RSiteDeact IN CSiteDeact LOOP
         vlError := FALSE;

         writeOut (' ');
         writeOut ('Inactivando site - Proveedor '||RSiteDeact.rfc_proveedor);
         
         IF NOT vlError THEN
             BEGIN
                SELECT assa.*
                INTO lr_existing_vendor_site_rec
                FROM apps.ap_supplier_sites_all assa,
                apps.ap_suppliers ass
                WHERE assa.vendor_id = ass.vendor_id
                AND upper(trim(chr(160) from trim(ass.segment1))) = upper(trim(chr(160) from trim(RSiteDeact.rfc_proveedor)))
                AND upper(trim(chr(160) from trim(assa.vendor_site_code))) = upper(trim(chr(160) from trim(RSiteDeact.site_proveedor)))
                AND assa.org_id in (select organization_id from apps.hr_all_organization_units 
                                    where upper(trim(chr(160) from trim(name))) = 
                                    (RSiteDeact.organizacion));
             EXCEPTION
                WHEN OTHERS THEN
                   vlError := TRUE;
                   vlMessageError := 'No existe proveedor y/o site de proveedor';
             END;
         END IF;

         IF NOT vlError THEN
              p_api_version      := 1.0;
              p_init_msg_list    := apps.fnd_api.g_true;
              p_commit           := apps.fnd_api.g_false;
              p_validation_level := apps.fnd_api.g_valid_level_full;
              p_calling_prog     := 'XXCUS';
              -- Assign Vendor Site Details
              lr_vendor_site_rec.vendor_site_id   := lr_existing_vendor_site_rec.vendor_site_id;
              lr_vendor_site_rec.last_update_date := SYSDATE;
              --lr_vendor_site_rec.last_updated_by  := 1119;
              lr_vendor_site_rec.vendor_id        := lr_existing_vendor_site_rec.vendor_id;
              lr_vendor_site_rec.org_id           := lr_existing_vendor_site_rec.org_id;
              lr_vendor_site_rec.inactive_date    := to_date(RSiteDeact.end_date,'dd/mm/yyyy');
              --lr_vendor_site_rec.bill_to_location_id          := x_location_id;
              
              DBMS_APPLICATION_INFO.set_client_info (lr_existing_vendor_site_rec.org_id);
              apps.mo_global.set_policy_context ('S', lr_existing_vendor_site_rec.org_id);
             
              apps.AP_VENDOR_PUB_PKG.UPDATE_VENDOR_SITE(p_api_version      => p_api_version,
                                                   p_init_msg_list    => p_init_msg_list,
                                                   p_commit           => p_commit,
                                                   p_validation_level => p_validation_level,
                                                   x_return_status    => x_return_status,
                                                   x_msg_count        => x_msg_count,
                                                   x_msg_data         => x_msg_data,
                                                   p_vendor_site_rec  => lr_vendor_site_rec,
                                                   p_vendor_site_id   => lr_existing_vendor_site_rec.vendor_site_id,
                                                   p_calling_prog     => p_calling_prog);
                     
            x_msg_data := SUBSTR(x_msg_data,1,250);
            IF x_return_status = 'S'
            THEN
               vlCountOK := vlCountOK + 1;
               writeOut ('  => La fecha de inactivacion para el proveedor:     ' ||RSiteDeact.rfc_proveedor||' Site: ' ||RSiteDeact.site_proveedor||' '||'su fecha de inacticacion es:   '||RSiteDeact.end_date);
            ELSE
               vlCountError := vlCountError + 1;
               writeOut ('  => Error al actualizar la fecha de inactivacion Para El Proveedor : '||RSiteDeact.rfc_proveedor||' Site: ' ||RSiteDeact.site_proveedor);
               FOR j IN 1..apps.fnd_msg_pub.count_msg LOOP
                  writeOut('    '||apps.fnd_msg_pub.get(p_msg_index => j, p_encoded => 'F'));
               END LOOP;
            END IF;
         END IF;

         IF vlError THEN
            vlCountError := vlCountError + 1;
            writeOut('  = Error al actualizar la fecha de inactivacion para el proveedor '||RSiteDeact.rfc_proveedor||' Site: ' ||RSiteDeact.site_proveedor||' '||vlMessageError);
         END IF;

      END LOOP;

      writeOut('  ');
      writeOut('===================================');
      writeOut('ReCUStros exitosos  '||vlCountOK);
      writeOut('ReCUStros con error '||vlCountError);
      writeOut('===================================');
      writeOut('  ');

      IF pTipo = 'P' THEN
         ROLLBACK;
      ELSE
         COMMIT;
      END IF;
   END deactivateSupplier;

      PROCEDURE updateAmountTaxFlag ( errbuf          OUT VARCHAR2,
                                   retcode         OUT NUMBER,
                                   pTipo           IN  VARCHAR2) IS

      CURSOR CSuppAmtTx IS
        select ass.vendor_id, 
        ass.segment1 rfc_proveedor, 
        assa.vendor_site_id ,
        assa.VENDOR_SITE_CODE,
        assa.VAT_CODE , 
        assa.AMOUNT_INCLUDES_TAX_FLAG,
        assa.org_id
        from apps.ap_supplier_sites_all assa, 
        apps.ap_suppliers ass 
        where assa.vendor_id = ass.vendor_id 
        AND assa.PAY_GROUP_LOOKUP_CODE != 'FIDEICOMISO'
        AND ass.vendor_type_lookup_code = ('PROVEEDOR NACIONAL')
        AND assa.VAT_CODE is not null
        AND nvl(assa.AMOUNT_INCLUDES_TAX_FLAG,'') != 'Y';
        
        
      vlError                    BOOLEAN;
      vlEstatus                  VARCHAR2(1);
      vlMensaje                  VARCHAR2(4000);
      vlMessageError             VARCHAR2(4000);

      vlCountOK                  NUMBER := 0;
      vlCountError               NUMBER := 0;
      
      lr_vendor_site_rec          apps.ap_vendor_pub_pkg.r_vendor_site_rec_type;
      lr_existing_vendor_site_rec apps.ap_supplier_sites_all%ROWTYPE;
      
      p_api_version               NUMBER;
      p_init_msg_list             VARCHAR2(200);
      p_commit                    VARCHAR2(200);
      p_validation_level          NUMBER;
      p_calling_prog              VARCHAR2(200);
      

      x_msg_count                NUMBER;
      x_term_id                  NUMBER;
      x_msg_data                 VARCHAR2(4000);
      x_return_status            VARCHAR2(4000);
      x_vendor_site_id        NUMBER;
      x_per_party_id             NUMBER;
      x_rel_party_id             NUMBER;
      x_rel_id                   NUMBER;
      x_org_contact_id           NUMBER;
      x_party_site_id            NUMBER;

   BEGIN
      IF pTipo = 'P' THEN
         writeOut('************************');
         writeOut('* Tipo de Carga Previa *');
         writeOut('************************');
         writeOut(' ');
      ELSE
         writeOut('****************************');
         writeOut('* Tipo de Carga Definitiva *');
         writeOut('****************************');
         writeOut(' ');
      END IF;


      writeOut (' ');
      writeOut ('======= Interface de Contactos =======');

      FOR RSuppAmtTx IN CSuppAmtTx LOOP
         vlError := FALSE;

         writeOut (' ');
         writeOut ('Actualizando Uso de Impuesto en Monto - Proveedor '||RSuppAmtTx.rfc_proveedor);
         
              p_api_version      := 1.0;
              p_init_msg_list    := apps.fnd_api.g_true;
              p_commit           := apps.fnd_api.g_false;
              p_validation_level := apps.fnd_api.g_valid_level_full;
              p_calling_prog     := 'XXCUS';
              -- Assign Vendor Site Details
              lr_vendor_site_rec.vendor_site_id   := RSuppAmtTx.vendor_site_id;
              lr_vendor_site_rec.last_update_date := SYSDATE;
              --lr_vendor_site_rec.last_updated_by  := 1119;
              lr_vendor_site_rec.vendor_id        := RSuppAmtTx.vendor_id;
              lr_vendor_site_rec.org_id           := RSuppAmtTx.org_id;
              --lr_vendor_site_rec.inactive_date    := SYSDATE;
              lr_vendor_site_rec.AMOUNT_INCLUDES_TAX_FLAG          := RSuppAmtTx.AMOUNT_INCLUDES_TAX_FLAG;
              
              DBMS_APPLICATION_INFO.set_client_info (lr_existing_vendor_site_rec.org_id);
              apps.mo_global.set_policy_context ('S', lr_existing_vendor_site_rec.org_id);
             
              apps.AP_VENDOR_PUB_PKG.UPDATE_VENDOR_SITE(p_api_version      => p_api_version,
                                                   p_init_msg_list    => p_init_msg_list,
                                                   p_commit           => p_commit,
                                                   p_validation_level => p_validation_level,
                                                   x_return_status    => x_return_status,
                                                   x_msg_count        => x_msg_count,
                                                   x_msg_data         => x_msg_data,
                                                   p_vendor_site_rec  => lr_vendor_site_rec,
                                                   p_vendor_site_id   => RSuppAmtTx.vendor_site_id,
                                                   p_calling_prog     => p_calling_prog);
                     
            x_msg_data := SUBSTR(x_msg_data,1,250);
            IF x_return_status = 'S'
            THEN
               vlCountOK := vlCountOK + 1;
               writeOut ('  => Se actualizo el uso de impuesto en monto para el proveedor:     ' ||RSuppAmtTx.rfc_proveedor);
            ELSE
               vlCountError := vlCountError + 1;
               writeOut ('  => Error al actualizar el uso de impuesto en monto Para El Proveedor : '||RSuppAmtTx.rfc_proveedor);
               FOR j IN 1..apps.fnd_msg_pub.count_msg LOOP
                  writeOut('    '||apps.fnd_msg_pub.get(p_msg_index => j, p_encoded => 'F'));
               END LOOP;
            END IF;

      END LOOP;

      writeOut('  ');
      writeOut('===================================');
      writeOut('ReCUStros exitosos  '||vlCountOK);
      writeOut('ReCUStros con error '||vlCountError);
      writeOut('===================================');
      writeOut('  ');

      IF pTipo = 'P' THEN
         ROLLBACK;
      ELSE
         COMMIT;
      END IF;
   END updateAmountTaxFlag;

  
   FUNCTION Get_rcv_txn_qty_amt(p_org_id              IN NUMBER,
                              p_rcv_transaction_id    IN NUMBER,
                              p_matching_basis        IN VARCHAR2,
                              p_returned_item         IN VARCHAR2)
   RETURN NUMBER IS
   BEGIN
   DBMS_APPLICATION_INFO.set_client_info(p_org_id);
   apps.mo_global.set_policy_context ('S', p_org_id);
   
   
   RETURN apps.AP_MATCHING_UTILS_PKG.Get_rcv_txn_qty_amt( p_rcv_transaction_id, p_matching_basis, p_returned_item);
   
   END Get_rcv_txn_qty_amt;
 END;
 /