package com.confuturo.steps;

import io.cucumber.java.After;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import io.cucumber.java.en.Then;
import org.gitlab4j.api.GitLabApi;
import org.gitlab4j.api.GitLabApiException;
import org.gitlab4j.api.models.Project;
import org.gitlab4j.api.models.RepositoryFile;
import org.json.JSONException;
import org.json.JSONObject;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import io.github.cdimascio.dotenv.Dotenv;

public class JsonValidationSteps {
    
    private GitLabApi gitLabApi;
    private String projectPath;
    private String rawJsonContent;  // Contenido crudo del JSON
    private JSONObject jsonContent;
    private List<String> validationErrors = new ArrayList<>(); // Lista para registrar errores
    private String cleanedJsonContent; // Contenido JSON sin comentarios
    /* 
    @Before
    public void beforeScenario(io.cucumber.java.Scenario scenario) {
        System.out.println("\n===== INICIANDO ESCENARIO =====");
        System.out.println("Nombre: " + scenario.getName());
        validationErrors.clear(); // Limpiar errores de ejecuciones anteriores
    }
    
    @After
    public void afterScenario(io.cucumber.java.Scenario scenario) {
        // Si hay errores de validación, marcar el escenario como fallido
        if (!validationErrors.isEmpty()) {
            System.out.println("\n===== ERRORES DE VALIDACIÓN ENCONTRADOS =====");
            for (String error : validationErrors) {
                System.err.println("- " + error);
                scenario.log("Error de validación: " + error); // Agregar al log de Cucumber
                // Agregar el error al escenario para que aparezca en los informes
                scenario.attach(error.getBytes(), "text/plain", "Error de validación");
            }
            // Forzar que el escenario se marque como fallido
            throw new AssertionError("Se encontraron " + validationErrors.size() + " errores de validación. Consulta el detalle en los logs.");
        }
        
        // Log final para asegurar que se muestre
        if (scenario.isFailed()) {
            System.out.println("\n===== ESCENARIO FALLIDO =====");
            System.out.println("Nombre: " + scenario.getName());
            System.out.println("Error: " + scenario.getId());
        } else {
            System.out.println("\n===== ESCENARIO EXITOSO =====");
            System.out.println("Nombre: " + scenario.getName());
        }
    }
    */
    // Método para probar la conexión con GitLab
    public boolean testGitLabConnection() {
        try {
            // Obtener el usuario actual para verificar que el token funciona
            gitLabApi.getUserApi().getCurrentUser();
            System.out.println("Conexión a GitLab establecida correctamente");
            return true;
        } catch (GitLabApiException e) {
            System.err.println("Error al conectar con GitLab: " + e.getMessage());
            return false;
        }
    }

    @Given("que tengo acceso al repositorio JSON {string}")
    public void queAccesoAlRepositorioJson(String repositoryPath) {
        System.out.println("Conectando al repositorio: " + repositoryPath);
        
        // Iniciar con token vacío para forzar la carga desde .env
        String gitLabToken = null;
        
        // Intentar cargar desde .env
        try {
            System.out.println("Buscando token en archivo .env...");
            
            // Intentar diferentes rutas para el archivo .env
            String[] possiblePaths = {
                ".env",
                "../.env",
                "src/test/resources/.env",
                System.getProperty("user.dir") + "/.env",
                System.getProperty("user.dir") + "/src/test/resources/.env"
            };
            
            for (String path : possiblePaths) {
                java.nio.file.Path envPath = java.nio.file.Paths.get(path);
                System.out.println("Comprobando archivo .env en: " + envPath.toAbsolutePath());
                
                if (java.nio.file.Files.exists(envPath)) {
                    System.out.println("Archivo .env encontrado en: " + envPath.toAbsolutePath());
                    
                    // Leer el archivo manualmente
                    List<String> lines = java.nio.file.Files.readAllLines(envPath);
                    for (String line : lines) {
                        if (line.trim().startsWith("GITLAB_TOKEN=")) {
                            gitLabToken = line.trim().substring("GITLAB_TOKEN=".length());
                            // Quitar comillas si las hay
                            if (gitLabToken.startsWith("\"") && gitLabToken.endsWith("\"")) {
                                gitLabToken = gitLabToken.substring(1, gitLabToken.length() - 1);
                            }
                            System.out.println("Token encontrado en archivo .env: " + path);
                            break;
                        }
                    }
                    
                    if (gitLabToken != null && !gitLabToken.isEmpty()) {
                        break; // Encontrado, salir del bucle
                    }
                }
            }
            
            // Si no se encontró en archivos, intentar con la biblioteca dotenv
            if (gitLabToken == null || gitLabToken.isEmpty()) {
                Dotenv dotenv = Dotenv.configure().ignoreIfMissing().load();
                gitLabToken = dotenv.get("GITLAB_TOKEN");
                if (gitLabToken != null && !gitLabToken.isEmpty()) {
                    System.out.println("Token encontrado usando dotenv");
                }
            }
        } catch (Exception e) {
            System.err.println("Error al cargar .env: " + e.getMessage());
        }
        
        // Si aún no hay token, usar el valor predeterminado como último recurso
        if (gitLabToken == null || gitLabToken.isEmpty()) {
            gitLabToken = "glpat-8c973AsqeMvs1Hbqis8p"; // Valor de respaldo
            System.out.println("Usando token predeterminado como respaldo");
        }
        
        if (gitLabToken == null || gitLabToken.isEmpty()) {
            throw new IllegalStateException("GITLAB_TOKEN no está configurado");
        }
        
        gitLabApi = new GitLabApi("https://gitlab.com", gitLabToken);
        projectPath = repositoryPath;
        
        // Verifica la conexión inmediatamente
        if (!testGitLabConnection()) {
            throw new IllegalStateException("No se pudo establecer conexión con GitLab. Verifica tu token.");
        }
        
        System.out.println("Conexión establecida correctamente al repositorio: " + repositoryPath);
    }
    
    @When("leo el archivo JSON {string}")
    public void leoElArchivoJson(String filePath) throws GitLabApiException {
        try {
            System.out.println("Intentando leer archivo JSON: " + filePath + " del proyecto: " + projectPath);
            
            // Obtener el proyecto primero
            Project project = gitLabApi.getProjectApi().getProject(projectPath);
            System.out.println("Proyecto encontrado con ID: " + project.getId());
            
            // Intentar con diferentes ramas
            String[] branchesToTry = {"develop", "master", "main"};
            RepositoryFile file = null;
            String usedBranch = null;
            
            for (String branch : branchesToTry) {
                try {
                    System.out.println("Intentando obtener archivo de la rama: " + branch);
                    file = gitLabApi.getRepositoryFileApi().getFile(project.getId(), filePath, branch);
                    usedBranch = branch;
                    System.out.println("Archivo encontrado en la rama: " + branch);
                    break;
                } catch (GitLabApiException e) {
                    System.out.println("No se encontró el archivo en la rama " + branch + ": " + e.getMessage());
                }
            }
            
            if (file == null) {
                throw new GitLabApiException("No se pudo encontrar el archivo en ninguna de las ramas intentadas");
            }
            
            // Decodificar el contenido
            rawJsonContent = new String(
                    java.util.Base64.getDecoder().decode(file.getContent()),
                    StandardCharsets.UTF_8);
            
            // Guardar una copia del archivo para verificación
            try {
                java.io.FileWriter writer = new java.io.FileWriter("gitlab-json-content.txt");
                writer.write(rawJsonContent);
                writer.close();
                System.out.println("Contenido JSON guardado en 'gitlab-json-content.txt'");
            } catch (Exception e) {
                System.err.println("Error al guardar contenido: " + e.getMessage());
            }
            
            // Usar el método mejorado para parsear JSON
            parseJsonContent();
            
        } catch (Exception e) {
            System.err.println("Error al leer archivo JSON: " + e.getMessage());
            throw e;
        }
    }

    @Then("el archivo NO debe contener la palabra {string}")
    public void elArchivoNoDebeContenerLaPalabra(String palabra) {
    System.out.println("\n===== VERIFICANDO QUE NO EXISTA LA PALABRA EXACTA =====");
    System.out.println("Palabra EXACTA que NO debe existir: '" + palabra + "'");
    System.out.println("Nota: Solo busca la palabra completa, no como parte de otras palabras");
    
    if (cleanedJsonContent == null || cleanedJsonContent.isEmpty()) {
        String errorMsg = "El contenido del archivo está vacío o no se ha cargado";
        System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
        validationErrors.add(errorMsg);
        return;
    }
    
    // Crear patrón regex para buscar ÚNICAMENTE la palabra completa y exacta
    // \b = límite de palabra (word boundary)
    // Pattern.quote() = escapa caracteres especiales para búsqueda literal
    // Esto significa que "prod" NO coincidirá con "producto", "producir", etc.
    String patron = "\\b" + java.util.regex.Pattern.quote(palabra) + "\\b";
    java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(patron, java.util.regex.Pattern.CASE_INSENSITIVE);
    java.util.regex.Matcher matcher = pattern.matcher(cleanedJsonContent);
    
    boolean encontradaPalabraExacta = matcher.find();
    System.out.println("Patrón de búsqueda (palabra exacta): " + patron);
    System.out.println("¿Palabra EXACTA encontrada? " + encontradaPalabraExacta);
    
    if (encontradaPalabraExacta) {
        // Mostrar contexto donde se encontró la palabra exacta
        List<String> ocurrencias = new ArrayList<>();
        matcher.reset(); // Reiniciar matcher para encontrar todas las ocurrencias
        while (matcher.find() && ocurrencias.size() < 5) { // Máximo 5 ejemplos
            int inicio = Math.max(0, matcher.start() - 30);
            int fin = Math.min(cleanedJsonContent.length(), matcher.end() + 30);
            String contexto = cleanedJsonContent.substring(inicio, fin).replaceAll("\\s+", " ");
            ocurrencias.add("..." + contexto + "...");
        }
        
        String errorMsg = "La palabra EXACTA '" + palabra + "' fue encontrada en el archivo pero NO debería estar presente. " +
                         "Contexto(s): " + String.join(" | ", ocurrencias);
        System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
        validationErrors.add(errorMsg);
    } else {
        System.out.println("✓ Correcto: La palabra EXACTA '" + palabra + "' NO fue encontrada");
        
        // Información adicional: verificar si existe como substring para debugging
        if (cleanedJsonContent.toLowerCase().contains(palabra.toLowerCase())) {
            System.out.println("ℹ Nota informativa: La secuencia '" + palabra + "' SÍ aparece como parte de otras palabras, pero NO como palabra independiente");
            
            // Mostrar algunos ejemplos de dónde aparece como substring
            String contenidoLower = cleanedJsonContent.toLowerCase();
            String palabraLower = palabra.toLowerCase();
            int index = contenidoLower.indexOf(palabraLower);
            int ejemplos = 0;
            while (index >= 0 && ejemplos < 3) {
                int inicioContexto = Math.max(0, index - 15);
                int finContexto = Math.min(cleanedJsonContent.length(), index + palabra.length() + 15);
                String contextoEjemplo = cleanedJsonContent.substring(inicioContexto, finContexto).replaceAll("\\s+", " ");
                System.out.println("  Ejemplo " + (ejemplos + 1) + ": ..." + contextoEjemplo + "...");
                index = contenidoLower.indexOf(palabraLower, index + 1);
                ejemplos++;
            }
        } else {
            System.out.println("ℹ Confirmado: La secuencia '" + palabra + "' no aparece en absoluto en el archivo");
        }
    }
}
    
    // Método mejorado para parsear JSON que maneja comentarios
    private void parseJsonContent() {
    try {
        // Mostrar el contenido original para depuración
        System.out.println("=== CONTENIDO JSON ORIGINAL ===");
        System.out.println(rawJsonContent);
        System.out.println("=== FIN DEL CONTENIDO ORIGINAL ===");
        
        // Eliminar comentarios línea por línea
        StringBuilder cleanedBuilder = new StringBuilder();
        String[] lines = rawJsonContent.split("\n");
        
        for (String line : lines) {
            String processedLine = line;
            
            // Buscar // pero excluir :// de las URLs
            int commentIndex = -1;
            int searchFrom = 0;
            
            while (searchFrom < processedLine.length()) {
                int index = processedLine.indexOf("//", searchFrom);
                if (index == -1) break;
                
                // Verificar si es parte de :// (URL)
                if (index > 0 && processedLine.charAt(index - 1) == ':') {
                    searchFrom = index + 2;
                    continue;
                }
                
                // Es un comentario real
                commentIndex = index;
                break;
            }
            
            // Si encontramos un comentario real, eliminar desde ahí
            if (commentIndex >= 0) {
                processedLine = processedLine.substring(0, commentIndex);
            }
            
            // Agregar la línea limpia
            if (!processedLine.trim().isEmpty()) {
                cleanedBuilder.append(processedLine).append("\n");
            }
        }
        
        // GUARDAR EL CONTENIDO LIMPIO EN LA VARIABLE DE INSTANCIA
        cleanedJsonContent = cleanedBuilder.toString();
        
        // Guardar el JSON limpio para inspección
        try {
            java.io.FileWriter writer = new java.io.FileWriter("cleaned-json-content.txt");
            writer.write(cleanedJsonContent);
            writer.close();
            System.out.println("JSON limpio guardado en 'cleaned-json-content.txt'");
        } catch (Exception e) {
            System.err.println("Error al guardar JSON limpio: " + e.getMessage());
        }
        
        // Parsear utilizando Jackson en lugar de org.json para mayor tolerancia
        try {
            com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
            mapper.configure(com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
            mapper.configure(com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_COMMENTS, true);
            mapper.configure(com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_SINGLE_QUOTES, true);
            
            // Convertir el Jackson JsonNode a org.json JSONObject
            com.fasterxml.jackson.databind.JsonNode jacksonNode = mapper.readTree(cleanedJsonContent);
            String standardJson = jacksonNode.toString();
            jsonContent = new JSONObject(standardJson);
            
            System.out.println("JSON parseado correctamente utilizando Jackson");
        } catch (Exception jacksonException) {
            System.err.println("Error al parsear con Jackson: " + jacksonException.getMessage());
            
            // Intentar parsear directamente con org.json como fallback
            try {
                jsonContent = new JSONObject(cleanedJsonContent);
                System.out.println("JSON parseado correctamente utilizando org.json directamente");
            } catch (JSONException orgJsonException) {
                System.err.println("Error al parsear con org.json: " + orgJsonException.getMessage());
                jsonContent = new JSONObject();
                throw new RuntimeException("No se pudo parsear el JSON utilizando ningún método");
            }
        }
        
        // Imprimir claves de primer nivel para depuración
        System.out.println("=== CLAVES DE PRIMER NIVEL EN EL JSON ===");
        jsonContent.keys().forEachRemaining(key -> System.out.println("- " + key));
        System.out.println("======================================");
        
    } catch (Exception e) {
        System.err.println("Error general al parsear JSON: " + e.getMessage());
        e.printStackTrace();
        jsonContent = new JSONObject(); // Inicializar vacío para evitar NullPointerException
        cleanedJsonContent = ""; // Inicializar también el contenido limpio
    }
}
    
    // Método mejorado para eliminar comentarios de JSON
    private String removeComments(String json) {
        StringBuilder result = new StringBuilder();
        String[] lines = json.split("\n");
        boolean inMultilineComment = false;
        
        for (String line : lines) {
            if (inMultilineComment) {
                // Verificar si el comentario multilínea termina en esta línea
                int endIndex = line.indexOf("*/");
                if (endIndex >= 0) {
                    line = line.substring(endIndex + 2);
                    inMultilineComment = false;
                } else {
                    // Aún dentro del comentario multilínea, omitir esta línea
                    continue;
                }
            }
            
            // Buscar comentarios de una línea (//...)
            int singleLineCommentIndex = line.indexOf("//");
            
            // Buscar inicio de comentario multilínea (/*...)
            int multiLineCommentStart = line.indexOf("/*");
            
            if (singleLineCommentIndex >= 0 && (multiLineCommentStart == -1 || singleLineCommentIndex < multiLineCommentStart)) {
                // Hay un comentario de una línea antes que cualquier comentario multilínea
                line = line.substring(0, singleLineCommentIndex);
            } else if (multiLineCommentStart >= 0) {
                // Hay un inicio de comentario multilínea
                int multiLineCommentEnd = line.indexOf("*/", multiLineCommentStart + 2);
                if (multiLineCommentEnd >= 0) {
                    // El comentario multilínea termina en la misma línea
                    String beforeComment = line.substring(0, multiLineCommentStart);
                    String afterComment = line.substring(multiLineCommentEnd + 2);
                    line = beforeComment + afterComment;
                    // Verificar si hay más comentarios en la misma línea (recursivo)
                    line = removeComments(line);
                } else {
                    // El comentario multilínea continúa en la siguiente línea
                    line = line.substring(0, multiLineCommentStart);
                    inMultilineComment = true;
                }
            }
            
            // Agregar la línea procesada si no está vacía
            String trimmedLine = line.trim();
            if (!trimmedLine.isEmpty()) {
                result.append(trimmedLine).append("\n");
            }
        }
        
        // Agregar comas faltantes entre objetos si es necesario
        String processedJson = result.toString();
        
        // Limpiar cualquier coma final antes de cerrar un objeto o array
        processedJson = processedJson.replaceAll(",\\s*}", "}");
        processedJson = processedJson.replaceAll(",\\s*]", "]");
        
        return processedJson;
    }
    
    @Then("imprimo el contenido del JSON para depuración")
    public void imprimoElContenidoDelJsonParaDepuracion() {
        System.out.println("=== CONTENIDO JSON RECUPERADO ===");
        System.out.println(rawJsonContent);
        System.out.println("=== FIN DEL CONTENIDO JSON ===");
        
        // También imprimir las claves de primer nivel para verificar la estructura
        if (jsonContent != null) {
            System.out.println("=== CLAVES DE PRIMER NIVEL ===");
            jsonContent.keys().forEachRemaining(key -> System.out.println("- " + key));
            System.out.println("=== FIN DE CLAVES DE PRIMER NIVEL ===");
        } else {
            System.out.println("El objeto jsonContent es nulo, no se pudo parsear el JSON");
        }
    }
    
    @Then("el JSON debe contener el campo {string} con valor {string}")
    public void elJsonDebeContenerCampoConValorTexto(String fieldPath, String expectedValue) {
        Object actualValue = getFieldValue(fieldPath);
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: '" + expectedValue + "'");
        System.out.println("Valor actual: '" + actualValue + "'");
        
        // Verificación sin lanzar excepción
        if (actualValue == null || !expectedValue.equals(String.valueOf(actualValue))) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualValue + "' pero debería ser '" + expectedValue + "'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @Then("el JSON debe contener el campo {string} con valor numérico {int}")
    public void elJsonDebeContenerCampoConValorNumerico(String fieldPath, int expectedValue) {
        Object actualValue = getFieldValue(fieldPath);
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: " + expectedValue);
        System.out.println("Valor actual: " + actualValue);
        
        // Verificación sin lanzar excepción
        try {
            if (actualValue == null) {
                String errorMsg = "El campo " + fieldPath + " es nulo pero debería ser " + expectedValue;
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
                return;
            }
            
            int actualNumeric = Integer.parseInt(String.valueOf(actualValue));
            if (actualNumeric != expectedValue) {
                String errorMsg = "El campo " + fieldPath + " tiene valor " + actualNumeric + " pero debería ser " + expectedValue;
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg); // Registrar el error
            }
        } catch (NumberFormatException e) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualValue + "' que no es un número válido";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @Then("el JSON debe contener el campo {string} con valor booleano {string}")
    public void elJsonDebeContenerCampoConValorBooleano(String fieldPath, String expectedBoolStr) {
        Object actualValue = getFieldValue(fieldPath);
        boolean expectedBool = "true".equalsIgnoreCase(expectedBoolStr);
        
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: " + expectedBool);
        System.out.println("Valor actual: " + actualValue);
        
        // Verificación sin lanzar excepción
        if (actualValue == null) {
            String errorMsg = "El campo " + fieldPath + " es nulo pero debería ser '" + expectedBool + "'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
            return;
        }
        
        boolean actualBool = false;
        if (actualValue instanceof Boolean) {
            actualBool = (Boolean) actualValue;
        } else {
            actualBool = Boolean.parseBoolean(actualValue.toString());
        }
        
        if (actualBool != expectedBool) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualBool + "' pero debería ser '" + expectedBool + "'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @Then("el JSON debe contener un objeto {string} con el campo {string} de valor {string}")
    public void elJsonDebeContenerObjetoConCampoDeValor(String objectPath, String fieldName, String expectedValue) {
        try {
            // Obtener el objeto anidado
            Object obj = getFieldValue(objectPath);
            if (obj == null) {
                String errorMsg = "El campo '" + objectPath + "' no existe en el JSON";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
                return;
            }
            
            if (!(obj instanceof JSONObject)) {
                String errorMsg = "El campo '" + objectPath + "' no es un objeto JSON válido";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
                return;
            }
            
            JSONObject jsonObj = (JSONObject) obj;
            if (!jsonObj.has(fieldName)) {
                String errorMsg = "El objeto '" + objectPath + "' no contiene el campo '" + fieldName + "'";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
                return;
            }
            
            String actualValue = jsonObj.getString(fieldName);
            System.out.println("Campo: " + objectPath + "." + fieldName);
            System.out.println("Valor esperado: '" + expectedValue + "'");
            System.out.println("Valor actual: '" + actualValue + "'");
            
            if (!expectedValue.equals(actualValue)) {
                String errorMsg = "El campo " + objectPath + "." + fieldName + " tiene valor '" + actualValue + "' pero debería ser '" + expectedValue + "'";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
            }
        } catch (JSONException e) {
            String errorMsg = "Error al acceder al campo " + objectPath + "." + fieldName + ": " + e.getMessage();
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
        }
    }
    
    @Then("el valor de la conexión {string} debe contener {string}")
    public void elValorDeLaConexionDebeContener(String connectionName, String expectedSubstring) {
        try {
            // Navegar a la estructura específica para conexiones
            if (jsonContent == null) {
                String errorMsg = "El objeto JSON es nulo, no se puede verificar la conexión";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
                return;   
            }
            
            if (!jsonContent.has("ConnectionStrings")) {
                String errorMsg = "El JSON no contiene la sección 'ConnectionStrings'";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
                return;
            }
            
            JSONObject connections = jsonContent.getJSONObject("ConnectionStrings");
            if (!connections.has(connectionName)) {
                String errorMsg = "No existe la conexión '" + connectionName + "' en 'ConnectionStrings'";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
                return;
            }
            
            String connectionString = connections.getString(connectionName);
            System.out.println("Conexión: " + connectionName);
            System.out.println("Debe contener: '" + expectedSubstring + "'");
            System.out.println("Valor actual: '" + connectionString + "'");
            
            if (!connectionString.contains(expectedSubstring)) {
                String errorMsg = "La conexión '" + connectionName + "' no contiene el valor '" + expectedSubstring + "'";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg);
            }
        } catch (JSONException e) {
            String errorMsg = "Error al verificar la conexión " + connectionName + ": " + e.getMessage();
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
        }
    }
    
    private Object getFieldValue(String fieldPath) {
        try {
            if (jsonContent == null) {
                System.err.println("El objeto JSON es nulo, no se puede obtener el valor del campo");
                return null;
            }
            
            // Manejo de rutas anidadas (usando puntos)
            if (fieldPath.contains(".")) {
                String[] parts = fieldPath.split("\\.");
                Object current = jsonContent;
                
                // Navegar a través de los objetos anidados
                for (int i = 0; i < parts.length; i++) {
                    String part = parts[i];
                    if (current instanceof JSONObject) {
                        JSONObject jsonObj = (JSONObject) current;
                        if (!jsonObj.has(part)) {
                            System.err.println("Campo '" + part + "' no encontrado en el JSON (ruta: " + fieldPath + ")");
                            return null;
                        }
                        
                        if (i == parts.length - 1) {
                            // Es el último elemento de la ruta
                            return jsonObj.get(part);
                        } else {
                            // Continuar la navegación
                            current = jsonObj.get(part);
                        }
                    } else {
                        System.err.println("No se puede navegar más allá en la ruta: " + fieldPath + " (detenido en: " + part + ")");
                        return null;
                    }
                }
                
                return current;
            } else {
                // Campo simple de primer nivel
                if (!jsonContent.has(fieldPath)) {
                    System.err.println("Campo '" + fieldPath + "' no encontrado en el JSON");
                    return null;
                }
                
                return jsonContent.get(fieldPath);
            }
        } catch (JSONException e) {
            System.err.println("Error al obtener valor del campo '" + fieldPath + "': " + e.getMessage());
            return null;
        }
    }

    // Agregar estos métodos a tu JsonValidationSteps.java existente:

@Then("el archivo debe contener la palabra {string}")
public void elArchivoDebeContenerLaPalabra(String palabra) {
    System.out.println("\n===== VERIFICANDO EXISTENCIA DE PALABRA =====");
    System.out.println("Palabra a buscar: '" + palabra + "'");
    
    if (rawJsonContent == null || rawJsonContent.isEmpty()) {
        String errorMsg = "El contenido del archivo está vacío o no se ha cargado";
        System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
        validationErrors.add(errorMsg);
        return;
    }
    
    boolean encontrada = rawJsonContent.contains(palabra);
    System.out.println("¿Palabra encontrada? " + encontrada);
    
    if (!encontrada) {
        String errorMsg = "La palabra '" + palabra + "' no fue encontrada en el archivo";
        System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
        validationErrors.add(errorMsg);
    } else {
        System.out.println("✓ Palabra encontrada exitosamente");
    }
}



@Then("el archivo debe contener al menos una de las palabras: {string}")
public void elArchivoDebeContenerAlMenosUnaDeLasPalabras(String palabrasStr) {
    System.out.println("\n===== VERIFICANDO EXISTENCIA DE AL MENOS UNA PALABRA =====");
    
    String[] palabras = palabrasStr.split(",");
    System.out.println("Palabras a buscar: " + String.join(", ", palabras));
    
    if (rawJsonContent == null || rawJsonContent.isEmpty()) {
        String errorMsg = "El contenido del archivo está vacío o no se ha cargado";
        System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
        validationErrors.add(errorMsg);
        return;
    }
    
    boolean algunaEncontrada = false;
    List<String> palabrasEncontradas = new ArrayList<>();
    
    for (String palabra : palabras) {
        palabra = palabra.trim();
        if (rawJsonContent.contains(palabra)) {
            algunaEncontrada = true;
            palabrasEncontradas.add(palabra);
        }
    }
    
    System.out.println("Palabras encontradas: " + 
        (palabrasEncontradas.isEmpty() ? "ninguna" : String.join(", ", palabrasEncontradas)));
    
    if (!algunaEncontrada) {
        String errorMsg = "Ninguna de las palabras [" + palabrasStr + "] fue encontrada en el archivo";
        System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
        validationErrors.add(errorMsg);
    } else {
        System.out.println("✓ Al menos una palabra fue encontrada exitosamente");
    }
}

@Then("el archivo debe contener todas las palabras: {string}")
public void elArchivoDebeContenerTodasLasPalabras(String palabrasStr) {
    try {
        System.out.println("\n===== VERIFICANDO EXISTENCIA DE TODAS LAS PALABRAS =====");
        
        if (palabrasStr == null || palabrasStr.trim().isEmpty()) {
            String errorMsg = "La lista de palabras a buscar está vacía";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
            return;
        }
        
        String[] palabras = palabrasStr.split(",");
        System.out.println("Palabras que deben existir: " + String.join(", ", palabras));
        
        if (rawJsonContent == null || rawJsonContent.isEmpty()) {
            String errorMsg = "El contenido del archivo está vacío o no se ha cargado";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
            return;
        }
        
        List<String> palabrasEncontradas = new ArrayList<>();
        List<String> palabrasNoEncontradas = new ArrayList<>();
        
        for (String palabra : palabras) {
            palabra = palabra.trim();
            if (rawJsonContent.contains(palabra)) {
                palabrasEncontradas.add(palabra);
            } else {
                palabrasNoEncontradas.add(palabra);
            }
        }
        
        System.out.println("Palabras encontradas: " + 
            (palabrasEncontradas.isEmpty() ? "ninguna" : String.join(", ", palabrasEncontradas)));
        System.out.println("Palabras NO encontradas: " + 
            (palabrasNoEncontradas.isEmpty() ? "ninguna" : String.join(", ", palabrasNoEncontradas)));
        
        if (!palabrasNoEncontradas.isEmpty()) {
            String errorMsg = "Las siguientes palabras no fueron encontradas: [" + 
                            String.join(", ", palabrasNoEncontradas) + "]";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
        } else {
            System.out.println("✓ Todas las palabras fueron encontradas exitosamente");
        }
    } catch (Exception e) {
        String errorMsg = "Error al verificar las palabras '" + palabrasStr + "': " + e.getMessage();
        System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
        validationErrors.add(errorMsg);
    }
}

// Hook para ejecutar después de cada escenario y validar todos los errores acumulados
@After
public void validarTodosLosErrores() {
    if (!validationErrors.isEmpty()) {
        System.err.println("\n===== RESUMEN DE ERRORES DE VALIDACIÓN JSON =====");
        for (int i = 0; i < validationErrors.size(); i++) {
            System.err.println((i + 1) + ". " + validationErrors.get(i));
        }
        System.err.println("================================================\n");
        
        String errorMessage = "Se encontraron " + validationErrors.size() + 
                            " errores de validación: " + String.join("; ", validationErrors);
        
        // Limpiar errores para el próximo escenario
        validationErrors.clear();
        
        throw new AssertionError(errorMessage);
    }
}
}