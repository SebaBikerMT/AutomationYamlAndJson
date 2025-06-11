# language: es
# src/test/resources/features/configValidation.feature
Característica: Validación de configuración de archivo json en ambiente de QA
  Como QA Automatizador
  Quiero validar que el archivo appsettings.QA.json tiene la configuración correcta
  Para asegurar que la aplicación funcionará adecuadamente

  Escenario: Depuración del contenido JSON
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.QA.json"
    Entonces imprimo el contenido del JSON para depuración

  Escenario: Verificar datos mas relevantes del archivo
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.QA.json"
    Entonces el JSON debe contener el campo "Apis.UrlClients" con valor "http://api-clientes.qa.seguros.local/clientes/"
    Y el JSON debe contener el campo "Apis.UrlProductos" con valor "http://api-productos.qa.seguros.local/"
    Y el JSON debe contener el campo "Apis.UrlPayment" con valor "http://api.qa.seguros.local/payment-service/"
    Y el JSON debe contener el campo "Keycloak.Url" con valor "http://auth-qa.confuturo.cl/auth"
    Y el JSON debe contener el campo "Keycloak.Realm" con valor "confuturo-sso-qa"


  Escenario: Verificar que el documento no contiene enlaces a rutas equivocadas
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.QA.json"
    Entonces el JSON debe contener el campo "TestMode" con valor booleano "true"
    Y el archivo Json NO debe contener la palabra "prod"
    Y el archivo Json NO debe contener la palabra "development"

