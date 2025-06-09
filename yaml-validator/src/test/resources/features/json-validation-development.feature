# language: es
# src/test/resources/features/configValidation.feature
Característica: Validación de configuración del Backend.BackOffice.Zendesk
  Como desarrollador
  Quiero validar que el archivo appsettings.Development.json tiene la configuración correcta
  Para asegurar que la aplicación funcionará adecuadamente

  Escenario: Depuración del contenido JSON
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.Development.json"
    Entonces imprimo el contenido del JSON para depuración

  Escenario: Verificar datos mas relevantes del archivo
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.Development.json"
    Entonces el JSON debe contener el campo "Apis.Client.Url" con valor "http://api-clientes.qa.seguros.local/clientes/"
    Y el JSON debe contener el campo "Apis.Product.Url" con valor "http://api-productos.qa.seguros.local/"
    Y el JSON debe contener el campo "Apis.Payment.Url" con valor "http://api.qa.seguros.local/payment-service/"
    Y el JSON debe contener el campo "Apis.Pdf.Url" con valor "https://pdf-service-42nqpimoca-uc.a.run.app/"
    Y el JSON debe contener el campo "Apis.Notifications.Url" con valor "http://api-notificaciones.qa.seguros.local/notificaciones/"

  Escenario: Verificar que el documento no contiene enlaces a rutas equivocadas
    Dado que tengo acceso al repositorio JSON "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo JSON "src/Backend.BackOffice.Zendesk/appsettings.Development.json"
    Entonces el archivo NO debe contener la palabra "192.168.68"
