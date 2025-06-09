# language: es
# src/test/resources/features/configValidation.feature
Característica: Validación de configuración del Backend.BackOffice.Zendesk
  Como desarrollador
  Quiero validar que el archivo appsettings.QA.json tiene la configuración correcta
  Para asegurar que la aplicación funcionará adecuadamente

  Escenario: Depuración del contenido JSON
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.QA.json"
    Entonces imprimo el contenido del JSON para depuración

  Escenario: Verificar la Url del Cliente
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.QA.json"
    Entonces el JSON debe contener el campo "Apis.UrlClients" con valor "http://api-clientes.qa.seguros.local/clientes/"

  Escenario: Verificar el modo de prueba
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.QA.json"
    Entonces el JSON debe contener el campo "TestMode" con valor booleano "true"
    Y el archivo NO debe contener la palabra "prod"
    Y el archivo NO debe contener la palabra "qa"

