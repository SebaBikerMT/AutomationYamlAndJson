# language: es

Característica: Validación de archivo YAML de valores para Kubernetes
  Como desarrollador
  Quiero validar que el archivo prod-values-gke.yaml tiene la configuración correcta
  Para evitar errores en el despliegue en producción

  Escenario: El archivo YAML de produccion debe tener solo valores que hagan referencia a produccion
    Dado que tengo acceso al repositorio "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo "helm/prod-values-gke.yaml"
    Entonces el archivo debe contener el campo "ASPNETCORE_ENVIRONMENT" con valor "Production"
    Y el archivo debe contener el campo "Apis__Client__Url" con valor "http://cliente-servicio-api.prod-servicios-globales.svc.cluster.local/clientes/"
    Y el archivo debe contener el campo "Apis__Product__Url" con valor "http://productos-services.prod-servicios-globales.svc.cluster.local/productos/"
    Y el archivo debe contener el campo "Apis__Payment__Url" con valor "http://payment-service-api.prod-payment-services.svc.cluster.local/payment-service/"
    Y el archivo debe contener el campo "Apis__Zendesk__Url" con valor "https://atencionconfuturo.zendesk.com/"
    Y el archivo debe contener el campo "Apis__Pdf__Url " con valor "http://pdf-service.prod-pdf-service.svc.cluster.local/pdf-service/"
    Y el archivo debe contener el campo "Apis__Kta__Url " con valor "http://192.168.83.90/vwnetx_Confuturo_Zendesk/"
    Y el valor de Apis__Kta__User debe ser "admin"
    Y el archivo debe contener el campo "Apis__Notifications__Url" con valor "http://api-notificaciones.prod-notificaciones.svc.cluster.local/notificacion/"
    Y el archivo debe contener el campo "Apis__HumanResources__Url" con valor "http://rrhh-service.prod-rrhh-service.svc.cluster.local/rrhh/"
    Y el archivo debe contener el campo "UrlVisualTime " con valor "http://prodapps292.seguros.local/vtimenet/visualtime/bucvtpipe.aspx"
    Y el archivo debe contener el campo "vault.hashicorp.com/role" con valor "prod"
    
  Escenario: El archivo YAML debe tener la configuración de servicios correcta
    Dado que tengo acceso al repositorio "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager"
    Cuando leo el archivo "helm/prod-values-gke.yaml"
    Entonces el archivo debe contener el campo "serviceAccount.create" con valor true
    Y el archivo debe contener el campo "TestMode" con valor false

    