# language: es

Característica: Validación de configuración de archivo yaml en ambiente de 
  Como desarrollador
  Quiero validar que el archivo qa-values-gke.yaml tiene la configuración correcta
  Para evitar errores en el despliegue en producción

  Escenario: El archivo YAML de QA debe tener solo valores que hagan referencia a QA
    Dado que tengo acceso al repositorio "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo "helm/qa-values-gke.yaml"
    Entonces el archivo debe contener el campo "ASPNETCORE_ENVIRONMENT" con valor "QA"
    Y el archivo debe contener el campo "Apis__Client__Url" con valor "http://cliente-servicio-api.qa-clientes.svc.cluster.local/clientes/"
    Y el archivo debe contener el campo "Apis__Product__Url" con valor "http://productos-services.qa-servicios-globales.svc.cluster.local/productos/"
    Y el archivo debe contener el campo "Apis__Payment__Url" con valor "http://payment-service-api.qa-payment-services.svc.cluster.local/payment-service/"
    Y el archivo debe contener el campo "Apis__Zendesk__Url" con valor "https://atencionconfuturo1616160263.zendesk.com/"
    Y el archivo debe contener el campo "Apis__Pdf__Url" con valor "http://pdf-service.qa-pdf-service.svc.cluster.local/pdf-service/"
    Y el archivo debe contener el campo "Apis__Kta__Url" con valor "http://192.168.68.19/vwnetx_confuturo_zendesk/"
    Y el valor de Apis__Kta__User debe ser "admin"
    Y el archivo debe contener el campo "Apis__Notifications__Url" con valor "http://api-notificaciones.qa-notificaciones.svc.cluster.local/notificaciones/"
    Y el archivo debe contener el campo "Apis__HumanResources__Url" con valor "http://rrhh-service.qa-rrhh-service.svc.cluster.local/rrhh/"
    Y el archivo debe contener el campo "UrlVisualTime" con valor "http://testapps292.seguros.local/vtimenet/visualtime/bucvtpipe.aspx"
    Y el archivo debe contener el campo "vault.hashicorp.com/role" con valor "qa"
    
  Escenario: El archivo YAML debe tener la configuración de servicios correcta
    Dado que tengo acceso al repositorio "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo "helm/qa-values-gke.yaml"
    Entonces el archivo debe contener el campo "serviceAccount.create" con valor true