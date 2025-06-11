# language: es
# src/test/resources/features/configValidation.feature
Característica: Validación de configuración de archivo json en ambiente de Produccion
  Como QA Automatizador
  Quiero validar que el archivo appsettings.Prod.json tiene la configuración correcta
  Para asegurar que la aplicación funcionará adecuadamente

  Escenario: Depuración del contenido JSON
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.Prod.json"
    Entonces imprimo el contenido del JSON para depuración

  Escenario: Verificar datos mas relevantes del archivo
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.Prod.json"
    Entonces el JSON debe contener el campo "Apis.UrlClients" con valor "http://api.seguros.local/clientes"
    Y el JSON debe contener el campo "Apis.UrlProductos" con valor "http://api.confuturo.cl/productos"
    Y el JSON debe contener el campo "Apis.UrlPayment" con valor "http://api.seguros.local/payment-service/"

  Escenario: Verificar que el documento no contiene enlaces a rutas equivocadas
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.Prod.json"
    Entonces el archivo Json NO debe contener la palabra "qa"
    Y el archivo Json NO debe contener la palabra "development"
