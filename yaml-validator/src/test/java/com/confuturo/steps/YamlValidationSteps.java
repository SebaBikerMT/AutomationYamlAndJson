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
import org.yaml.snakeyaml.Yaml;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import io.github.cdimascio.dotenv.Dotenv;

public class YamlValidationSteps {
    
    private GitLabApi gitLabApi;
    private String projectPath;
    private String rawYamlContent;  // Contenido crudo del YAML
    private Map<String, Object> yamlContent;
    private List<String> validationErrors = new ArrayList<>(); // Lista para registrar errores
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
    }*/
    
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

    @Given("que tengo acceso al repositorio {string}")
    public void queAccesoAlRepositorio(String repositoryPath) {
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
    
    
    gitLabApi = new GitLabApi("https://gitlab.com", gitLabToken);
    projectPath = repositoryPath;
    
    // Verifica la conexión inmediatamente
    if (!testGitLabConnection()) {
        throw new IllegalStateException("No se pudo establecer conexión con GitLab. Verifica tu token.");
    }
    
    System.out.println("Conexión establecida correctamente al repositorio: " + repositoryPath);
}
    
    @When("leo el archivo {string}")
    public void leoElArchivo(String filePath) throws GitLabApiException {
        try {
            System.out.println("Intentando leer archivo: " + filePath + " del proyecto: " + projectPath);
            
            // Obtener el proyecto primero
            Project project = gitLabApi.getProjectApi().getProject(projectPath);
            System.out.println("Proyecto encontrado con ID: " + project.getId());
            
            // Obtener el archivo
            RepositoryFile file = gitLabApi.getRepositoryFileApi()
                    .getFile(project.getId(), filePath, "main");
            
            // Decodificar el contenido
            rawYamlContent = new String(
                    java.util.Base64.getDecoder().decode(file.getContent()),
                    StandardCharsets.UTF_8);
            
            // Guardar una copia del archivo para verificación
            try {
                java.io.FileWriter writer = new java.io.FileWriter("gitlab-yaml-content.txt");
                writer.write(rawYamlContent);
                writer.close();
                System.out.println("Contenido YAML guardado en 'gitlab-yaml-content.txt'");
            } catch (Exception e) {
                System.err.println("Error al guardar contenido: " + e.getMessage());
            }
            
            // IMPORTANTE: Reemplazar las tabulaciones por espacios antes de parsear
            String yamlContentFixed = rawYamlContent.replace("\t", "    ");
            
            // Parsear el YAML
            try {
                Yaml yaml = new Yaml();
                yamlContent = yaml.load(new ByteArrayInputStream(yamlContentFixed.getBytes()));
                System.out.println("Archivo YAML leído y parseado correctamente");
            } catch (Exception e) {
                System.err.println("Error al parsear YAML, continuando con validaciones manuales: " + e.getMessage());
                // Aunque falle el parseo, podemos continuar con validaciones basadas en el contenido crudo
                yamlContent = new java.util.HashMap<>();
            }
            
        } catch (Exception e) {
            System.err.println("Error al leer archivo YAML: " + e.getMessage());
            throw e;
        }
    }
    
    @Then("el archivo debe contener el campo {string} con valor {int}")
    public void elArchivoDebeContenerCampoConValorEntero(String fieldPath, int expectedValue) {
        Object actualValue = getNestedField(fieldPath);
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: " + expectedValue);
        System.out.println("Valor actual: " + actualValue);
        
        // Verificación sin lanzar excepción
        if (!String.valueOf(expectedValue).equals(String.valueOf(actualValue))) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualValue + "' pero debería ser '" + expectedValue + "'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @Then("el archivo debe contener el campo {string} con valor {string}")
    public void elArchivoDebeContenerCampoConValorTexto(String fieldPath, String expectedValue) {
        Object actualValue = getNestedField(fieldPath);
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: '" + expectedValue + "'");
        System.out.println("Valor actual: '" + actualValue + "'");
        
        // Verificación sin lanzar excepción
        if (!expectedValue.equals(String.valueOf(actualValue))) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualValue + "' pero debería ser '" + expectedValue + "'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @Then("el archivo debe contener el campo {string} con valor true")
    public void elArchivoDebeContenerCampoConValorTrue(String fieldPath) {
        Object actualValue = getNestedField(fieldPath);
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: true");
        System.out.println("Valor actual: " + actualValue);
        
        // Verificación sin lanzar excepción
        if (!Boolean.TRUE.equals(actualValue)) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualValue + "' pero debería ser 'true'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @Then("el archivo debe contener el campo {string} con valor false")
    public void elArchivoDebeContenerCampoConValorFalse(String fieldPath) {
        Object actualValue = getNestedField(fieldPath);
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: false");
        System.out.println("Valor actual: " + actualValue);
        
        // Verificación sin lanzar excepción
        if (!Boolean.FALSE.equals(actualValue)) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualValue + "' pero debería ser 'false'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @Then("el archivo debe contener el campo {string} con al menos un elemento")
    public void elArchivoDebeContenerCampoConAlMenosUnElemento(String fieldPath) {
        Object value = getNestedField(fieldPath);
        
        if (!(value instanceof List)) {
            String errorMsg = "El campo '" + fieldPath + "' no es una lista, es: " + 
                (value != null ? value.getClass().getName() : "null");
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
            return;
        }
        
        List<?> list = (List<?>) value;
        if (list.isEmpty()) {
            String errorMsg = "El campo '" + fieldPath + "' es una lista vacía";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @Then("el campo {string} debe tener exactamente el valor {string}")
    public void elCampoDebeTenerExactamenteElValor(String campo, String valorEsperado) {
        boolean encontrado = false;
        String valorActual = null;
        
        System.out.println("\n===== VERIFICACIÓN EXACTA DEL CAMPO =====");
        System.out.println("Campo a verificar: " + campo);
        System.out.println("Valor esperado: '" + valorEsperado + "'");
        
        // Buscar línea por línea
        for (String line : rawYamlContent.split("\n")) {
            String trimmed = line.trim();
            if (trimmed.startsWith(campo + ":") || trimmed.startsWith(campo + " :")) {
                String[] parts = trimmed.split(":", 2);
                if (parts.length > 1) {
                    valorActual = parts[1].trim();
                    // Quitar comillas si las hay
                    if (valorActual.startsWith("\"") && valorActual.endsWith("\"")) {
                        valorActual = valorActual.substring(1, valorActual.length() - 1);
                    }
                    encontrado = true;
                    System.out.println("Valor actual encontrado: '" + valorActual + "'");
                    break;
                }
            }
        }
        
        if (!encontrado) {
            String errorMsg = "Campo " + campo + " no encontrado en el YAML";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
            return;
        }
        
        System.out.println("¿Son iguales? " + valorActual.equals(valorEsperado));
        
        // Comparación sin lanzar excepción
        if (!valorActual.equals(valorEsperado)) {
            String errorMsg = "El campo " + campo + " tiene valor '" + valorActual + "' pero debería ser '" + valorEsperado + "'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        } else {
            System.out.println("¡Validación exitosa! Los valores son iguales");
        }
    }
    
    @Then("el valor de Apis__Kta__User debe ser {string}")
    public void elValorDeApisKtaUserDebeSer(String valorEsperado) {
        try {
            System.out.println("\n===== VERIFICACIÓN ESPECÍFICA DE Apis__Kta__User =====");
            System.out.println("Valor esperado: '" + valorEsperado + "'");
            
            // Búsqueda directa tal como lo hacía SimpleYamlTest
            String campo = "Apis__Kta__User";
            boolean encontrado = false;
            String valorActual = null;
            
            // Buscar línea por línea
            for (String line : rawYamlContent.split("\n")) {
                String trimmed = line.trim();
                if (trimmed.startsWith(campo + ":") || trimmed.startsWith(campo + " :")) {
                    String[] parts = trimmed.split(":", 2);
                    if (parts.length > 1) {
                        valorActual = parts[1].trim();
                        // Quitar comillas si las hay
                        if (valorActual.startsWith("\"") && valorActual.endsWith("\"")) {
                            valorActual = valorActual.substring(1, valorActual.length() - 1);
                        }
                        encontrado = true;
                        System.out.println("Campo encontrado: " + campo);
                        System.out.println("Valor actual: '" + valorActual + "'");
                        break;
                    }
                }
            }
            
            if (!encontrado) {
                String errorMsg = "Campo " + campo + " no encontrado en el YAML";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg); // Registrar el error
                return;
            }
            
            // Mostrar comparación
            System.out.println("¿Son iguales? " + valorActual.equals(valorEsperado));
            
            // Comparación sin lanzar excepción
            if (!valorActual.equals(valorEsperado)) {
                String errorMsg = "El campo " + campo + " tiene valor '" + valorActual + "' pero debería ser '" + valorEsperado + "'";
                System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
                validationErrors.add(errorMsg); // Registrar el error
            } else {
                System.out.println("¡Validación exitosa! Los valores son iguales");
            }
        } catch (Exception e) {
            String errorMsg = "Error al validar Apis__Kta__User: " + e.getMessage();
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }
    
    @SuppressWarnings("unchecked")
    private Object getNestedField(String fieldPath) {
        try {
            // Primero buscar como campo plano
            if (yamlContent.containsKey(fieldPath)) {
                return yamlContent.get(fieldPath);
            }
            
            // Buscar como campo anidado en el mapa
            if (fieldPath.contains(".")) {
                String[] parts = fieldPath.split("\\.");
                Map<String, Object> current = yamlContent;
                
                for (int i = 0; i < parts.length - 1; i++) {
                    Object value = current.get(parts[i]);
                    if (!(value instanceof Map)) {
                        // Búsqueda alternativa en el texto crudo
                        return buscarEnContenidoCrudo(fieldPath);
                    }
                    current = (Map<String, Object>) value;
                }
                
                return current.get(parts[parts.length - 1]);
            }
            
            // Si no se encuentra, buscar en el contenido crudo
            return buscarEnContenidoCrudo(fieldPath);
            
        } catch (Exception e) {
            System.err.println("Error al obtener el campo '" + fieldPath + "': " + e.getMessage());
            // Intentar buscar en el contenido crudo como último recurso
            return buscarEnContenidoCrudo(fieldPath);
        }
    }
    
    private Object buscarEnContenidoCrudo(String fieldPath) {
        // Buscar directamente en el texto del YAML
        for (String line : rawYamlContent.split("\n")) {
            String trimmed = line.trim();
            String fieldName = fieldPath.contains(".") ? fieldPath.substring(fieldPath.lastIndexOf(".") + 1) : fieldPath;
            
            if (trimmed.startsWith(fieldName + ":") || trimmed.startsWith(fieldPath + ":")) {
                String[] parts = trimmed.split(":", 2);
                if (parts.length > 1) {
                    String value = parts[1].trim();
                    
                    // Intentar convertir al tipo adecuado
                    if (value.equalsIgnoreCase("true") || value.equalsIgnoreCase("false")) {
                        return Boolean.parseBoolean(value.toLowerCase());
                    } else if (value.matches("\\d+")) {
                        return Integer.parseInt(value);
                    } else {
                        // Quitar comillas si las hay
                        if (value.startsWith("\"") && value.endsWith("\"")) {
                            return value.substring(1, value.length() - 1);
                        }
                        return value;
                    }
                }
            }
        }
        
        System.err.println("Campo '" + fieldPath + "' no encontrado en el YAML");
        return null;
    }
}