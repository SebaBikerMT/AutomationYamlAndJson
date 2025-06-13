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
    private String rawYamlContent; // Contenido crudo del YAML
    private Map<String, Object> yamlContent;
    private List<String> validationErrors = new ArrayList<>(); // Lista para registrar errores
    private String cleanedYamlContent; // Contenido YAML sin comentarios

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
            throw new AssertionError("Se encontraron " + validationErrors.size()
                    + " errores de validación. Consulta el detalle en los logs.");
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

            // NUEVO: Usar el método mejorado para parsear YAML (similar al JSON)
            parseYamlContent();

        } catch (Exception e) {
            System.err.println("Error al leer archivo YAML: " + e.getMessage());
            throw e;
        }
    }

    private void parseYamlContent() {
        try {
            // Mostrar el contenido original para depuración
            System.out.println("=== CONTENIDO YAML ORIGINAL ===");
            System.out.println("Longitud: " + rawYamlContent.length() + " caracteres");
            System.out.println("Primeras 300 caracteres:");
            System.out.println(rawYamlContent.substring(0, Math.min(300, rawYamlContent.length())));
            System.out.println("=== FIN DEL CONTENIDO ORIGINAL ===");

            // Limpiar comentarios usando el método especializado
            cleanedYamlContent = removeYamlComments(rawYamlContent);

            // Guardar el YAML limpio para inspección
            try {
                java.io.FileWriter writer = new java.io.FileWriter("cleaned-yaml-content.txt");
                writer.write(cleanedYamlContent);
                writer.close();
                System.out.println("YAML limpio guardado en 'cleaned-yaml-content.txt'");
                System.out.println("Longitud después de limpieza: " + cleanedYamlContent.length() + " caracteres");
            } catch (Exception e) {
                System.err.println("Error al guardar YAML limpio: " + e.getMessage());
            }

            // IMPORTANTE: Reemplazar las tabulaciones por espacios antes de parsear
            String yamlContentFixed = cleanedYamlContent.replace("\t", "    ");

            // Parsear el YAML limpio
            try {
                Yaml yaml = new Yaml();
                yamlContent = yaml.load(new ByteArrayInputStream(yamlContentFixed.getBytes()));
                System.out.println("YAML limpio parseado correctamente");
            } catch (Exception e) {
                System.err.println(
                        "Error al parsear YAML limpio, continuando con validaciones manuales: " + e.getMessage());
                // Aunque falle el parseo, podemos continuar con validaciones basadas en el
                // contenido limpio
                yamlContent = new java.util.HashMap<>();
            }

        } catch (Exception e) {
            System.err.println("Error general al parsear YAML: " + e.getMessage());
            e.printStackTrace();
            yamlContent = new java.util.HashMap<>(); // Inicializar vacío para evitar NullPointerException
            cleanedYamlContent = rawYamlContent; // Si falla la limpieza, usar el original
        }
    }

    // NUEVO MÉTODO: Limpiar comentarios de YAML (equivalente al removeComments de
    // JSON)
    private String removeYamlComments(String yaml) {
        StringBuilder result = new StringBuilder();
        String[] lines = yaml.split("\n");

        System.out.println("=== PROCESO DE LIMPIEZA DE COMENTARIOS YAML ===");
        int lineasOriginales = lines.length;
        int lineasProcesadas = 0;
        int comentariosEliminados = 0;

        for (String line : lines) {
            String originalLine = line;
            String processedLine = line;

            // Buscar # pero excluir casos especiales donde # no es un comentario
            int commentIndex = findYamlCommentStart(processedLine);

            // Si encontramos un comentario real, eliminar desde ahí
            if (commentIndex >= 0) {
                String beforeComment = processedLine.substring(0, commentIndex);
                // IMPORTANTE: Preservar la indentación original pero quitar el comentario
                processedLine = beforeComment.replaceAll("\\s+$", ""); // Quitar espacios al final
                comentariosEliminados++;

                // Debug: mostrar las primeras 10 líneas procesadas
                if (comentariosEliminados <= 10) {
                    System.out.println("Línea " + (lineasProcesadas + 1) + ":");
                    System.out.println("  Original: [" + originalLine + "]");
                    System.out.println("  Limpia:   [" + processedLine + "]");
                }
            }

            // CAMBIO CRÍTICO: Agregar TODAS las líneas, incluso las vacías, para preservar
            // estructura YAML
            result.append(processedLine).append("\n");

            lineasProcesadas++;
        }

        System.out.println("Líneas originales: " + lineasOriginales);
        System.out.println("Líneas procesadas: " + lineasProcesadas);
        System.out.println("Comentarios eliminados: " + comentariosEliminados);
        System.out.println("=== FIN DEL PROCESO DE LIMPIEZA ===");

        return result.toString();
    }

    // MÉTODO AUXILIAR: Encontrar el inicio de comentarios # en YAML
    private int findYamlCommentStart(String line) {
        boolean inDoubleQuotes = false;
        boolean inSingleQuotes = false;
        char prevChar = ' ';

        for (int i = 0; i < line.length(); i++) {
            char currentChar = line.charAt(i);

            // Manejar comillas dobles (pero solo si no están escapadas)
            if (currentChar == '"' && prevChar != '\\') {
                inDoubleQuotes = !inDoubleQuotes;
            }
            // Manejar comillas simples (pero solo si no están escapadas)
            else if (currentChar == '\'' && prevChar != '\\') {
                inSingleQuotes = !inSingleQuotes;
            }
            // Si encontramos # y no estamos dentro de comillas, es un comentario
            else if (currentChar == '#' && !inDoubleQuotes && !inSingleQuotes) {
                // Verificación adicional: asegurarse de que no sea parte de una URL
                // Buscar hacia atrás para ver si hay "://" cerca
                boolean isPartOfUrl = false;
                if (i >= 2) {
                    String before = line.substring(Math.max(0, i - 10), i);
                    if (before.contains("://") || before.contains("http") || before.contains("https")) {
                        // Podría ser parte de una URL, verificar más cuidadosamente
                        String urlPattern = "https?://[^\\s#]*";
                        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(urlPattern);
                        java.util.regex.Matcher matcher = pattern.matcher(line);
                        while (matcher.find()) {
                            if (i >= matcher.start() && i < matcher.end()) {
                                isPartOfUrl = true;
                                break;
                            }
                        }
                    }
                }

                if (!isPartOfUrl) {
                    return i;
                }
            }

            prevChar = currentChar;
        }

        return -1; // No se encontró comentario
    }

    @Then("el archivo debe contener el campo {string} con valor {int}")
    public void elArchivoDebeContenerCampoConValorEntero(String fieldPath, int expectedValue) {
        Object actualValue = getNestedField(fieldPath);
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: " + expectedValue);
        System.out.println("Valor actual: " + actualValue);

        // Verificación sin lanzar excepción
        if (!String.valueOf(expectedValue).equals(String.valueOf(actualValue))) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualValue + "' pero debería ser '"
                    + expectedValue + "'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg); // Registrar el error
        }
    }

    @Then("el archivo debe contener el campo {string} con valor {string}")
    public void elArchivoDebeContenerCampoConValorTexto(String fieldPath, String expectedValue) {
        System.out.println("\n===== VERIFICANDO CAMPO CON VALOR TEXTO =====");
        System.out.println("Campo: " + fieldPath);
        System.out.println("Valor esperado: '" + expectedValue + "'");

        // Primero intentar obtener del YAML parseado
        Object actualValue = getNestedField(fieldPath);
        System.out.println("Valor del YAML parseado: '" + actualValue + "'");

        // Si no se pudo obtener del parseado, buscar en el contenido limpio
        // directamente
        if (actualValue == null && cleanedYamlContent != null) {
            System.out.println("Buscando directamente en el contenido limpio...");

            // Crear patrón para buscar el campo en YAML: "fieldPath: valor"
            String patron = fieldPath + ":\\s*[\"']?([^\"'\\n]+)[\"']?";
            java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(patron);
            java.util.regex.Matcher matcher = pattern.matcher(cleanedYamlContent);

            if (matcher.find()) {
                actualValue = matcher.group(1).trim();
                System.out.println("Valor encontrado con regex: '" + actualValue + "'");
            }
        }

        System.out.println("Valor final obtenido: '" + actualValue + "'");

        // Verificación sin lanzar excepción
        if (actualValue == null || !expectedValue.equals(String.valueOf(actualValue))) {
            String errorMsg = "El campo " + fieldPath + " tiene valor '" + actualValue + "' pero debería ser '"
                    + expectedValue + "'";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
        } else {
            System.out.println("✓ Campo validado correctamente");
        }
    }

    @Then("el archivo Yaml NO debe contener la palabra {string}")
    public void elArchivoYamlNoDebeContenerLaPalabra(String palabra) {
        imprimirSeparador("Verificando que NO exista la palabra exacta");
        System.out.println("🎯 Palabra EXACTA que NO debe existir: '" + palabra + "'");
        System.out.println("📝 Nota: Búsqueda realizada en contenido SIN comentarios");

        // CAMBIO: Usar cleanedYamlContent en lugar de rawYamlContent
        String contenidoParaBuscar = (cleanedYamlContent != null && !cleanedYamlContent.isEmpty())
                ? cleanedYamlContent
                : rawYamlContent;

        if (contenidoParaBuscar == null || contenidoParaBuscar.isEmpty()) {
            String errorMsg = "El contenido del archivo está vacío o no se ha cargado";
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
            return;
        }

        System.out.println("Longitud del contenido a buscar: " + contenidoParaBuscar.length());
        System.out.println("Usando contenido: "
                + (contenidoParaBuscar == cleanedYamlContent ? "LIMPIO (sin comentarios)" : "ORIGINAL"));

        // VERIFICACIÓN SIMPLE PRIMERO para debug
        boolean contieneSimple = contenidoParaBuscar.toLowerCase().contains(palabra.toLowerCase());
        System.out.println("¿Contiene '" + palabra + "' con búsqueda simple? " + contieneSimple);

        if (contieneSimple) {
            System.out.println("Ejemplos de dónde aparece (máximo 3):");
            String contenidoLower = contenidoParaBuscar.toLowerCase();
            String palabraLower = palabra.toLowerCase();
            int index = contenidoLower.indexOf(palabraLower);
            int contador = 0;
            while (index >= 0 && contador < 3) {
                int inicio = Math.max(0, index - 30);
                int fin = Math.min(contenidoParaBuscar.length(), index + palabra.length() + 30);
                String contexto = contenidoParaBuscar.substring(inicio, fin);
                System.out.println("  " + (contador + 1) + ": ..." + contexto + "...");
                index = contenidoLower.indexOf(palabraLower, index + 1);
                contador++;
            }
        }

        // Patrones CORREGIDOS que detectan 'prod' como término independiente en
        // contextos específicos
        List<String> patronesYaml = java.util.Arrays.asList(
                // 1. Valor después de dos puntos (key: valor) - palabra completa
                ":\\s*[\"']?" + java.util.regex.Pattern.quote(palabra) + "[\"']?\\s*(?=\\s|$|#)",

                // 2. En URLs como "api.prod-servicios" o "service.prod.domain" (con
                // separadores)
                "\\." + java.util.regex.Pattern.quote(palabra) + "(?=[-\\.])",

                // 3. Al inicio de subdominios como "prod-servicios" (con separadores)
                "(?<=[:/\\.])" + java.util.regex.Pattern.quote(palabra) + "(?=[-\\.])",

                // 4. CORREGIDO: En hostnames SOLO con números/guiones, NO letras (evita
                // "productos")
                "(?<=[:/\\.])" + java.util.regex.Pattern.quote(palabra) + "(?=[0-9\\-])",

                // 5. Como parte de path "/prod/" o "/prod-algo/" (con separadores)
                "/" + java.util.regex.Pattern.quote(palabra) + "(?=[-/]|$)",

                // 6. NUEVO: Palabra completa al inicio de línea o después de espacios
                "(?:^|\\s)" + java.util.regex.Pattern.quote(palabra) + "(?=\\s|$|:|#)",

                // 7. NUEVO: En nombres de archivos o configuraciones como "prod.yaml",
                // "prod-config"
                "(?<=[\\s/\\\\])" + java.util.regex.Pattern.quote(palabra) + "(?=[-_\\.]\\w)",

                // 8. NUEVO: Como ambiente o variable "PROD", "prod_env", etc.
                "\\b" + java.util.regex.Pattern.quote(palabra.toUpperCase()) + "\\b",

                // 9. NUEVO: Palabra exacta rodeada de límites de palabra (fallback general)
                "\\b" + java.util.regex.Pattern.quote(palabra) + "\\b(?![a-zA-Z])");

        boolean encontradaPalabraExacta = false;
        List<String> ocurrenciasEncontradas = new ArrayList<>();
        java.util.Set<String> contextosDuplicados = new java.util.HashSet<>();

        System.out.println("Aplicando " + patronesYaml.size() + " patrones de búsqueda...");

        for (int i = 0; i < patronesYaml.size(); i++) {
            String patronYaml = patronesYaml.get(i);
            java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(patronYaml,
                    java.util.regex.Pattern.CASE_INSENSITIVE);
            java.util.regex.Matcher matcher = pattern.matcher(contenidoParaBuscar);

            int coincidenciasPatron = 0;
            while (matcher.find()) {
                encontradaPalabraExacta = true;
                coincidenciasPatron++;

                int inicio = Math.max(0, matcher.start() - 30);
                int fin = Math.min(contenidoParaBuscar.length(), matcher.end() + 30);
                String contexto = contenidoParaBuscar.substring(inicio, fin).replaceAll("\\s+", " ");

                if (!contextosDuplicados.contains(contexto) && ocurrenciasEncontradas.size() < 10) {
                    contextosDuplicados.add(contexto);
                    ocurrenciasEncontradas.add("Patrón " + (i + 1) + ": ..." + contexto + "...");
                }
            }

            if (coincidenciasPatron > 0) {
                System.out.println(
                        "Patrón " + (i + 1) + " (" + patronYaml + "): " + coincidenciasPatron + " coincidencias");
            }
        }

        System.out.println("¿Palabra EXACTA encontrada? " + encontradaPalabraExacta);
        System.out.println("Total de contextos únicos: " + ocurrenciasEncontradas.size());

        if (encontradaPalabraExacta) {
            String errorMsg = "La palabra EXACTA '" + palabra
                    + "' fue encontrada en el archivo pero NO debería estar presente. " +
                    "Total ocurrencias: " + ocurrenciasEncontradas.size() + ". " +
                    "Contexto(s): " + String.join(" | ", ocurrenciasEncontradas);
            System.err.println("¡ERROR DE VALIDACIÓN! " + errorMsg);
            validationErrors.add(errorMsg);
        } else {
            System.out.println(
                    "✓ Correcto: La palabra EXACTA '" + palabra + "' NO fue encontrada como término independiente");

            if (contieneSimple) {
                System.out.println("ℹ Nota: La secuencia '" + palabra
                        + "' SÍ aparece en el archivo, pero NO como término independiente según los patrones definidos");
            } else {
                System.out.println("ℹ Confirmado: La secuencia '" + palabra + "' no aparece en absoluto en el archivo");
            }
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
            String errorMsg = "El campo " + campo + " tiene valor '" + valorActual + "' pero debería ser '"
                    + valorEsperado + "'";
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
                String errorMsg = "El campo " + campo + " tiene valor '" + valorActual + "' pero debería ser '"
                        + valorEsperado + "'";
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
            String fieldName = fieldPath.contains(".") ? fieldPath.substring(fieldPath.lastIndexOf(".") + 1)
                    : fieldPath;

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

    /**
     * Imprime un separador visual en la consola
     */
    private void imprimirSeparador(String titulo) {
        System.out.println("\n" + "=".repeat(60));
        System.out.println("🔍 " + titulo.toUpperCase());
        System.out.println("=".repeat(60));
    }

    /**
     * Imprime un subseparador para secciones menores
     */
    private void imprimirSubSeparador(String titulo) {
        System.out.println("\n" + "-".repeat(40));
        System.out.println("📋 " + titulo);
        System.out.println("-".repeat(40));
    }

    /**
     * Formatea el contexto de manera más legible
     */
    private String formatearContexto(String contexto, int maxLength) {
        if (contexto.length() <= maxLength) {
            return contexto;
        }

        // Truncar y agregar puntos suspensivos
        return contexto.substring(0, maxLength - 3) + "...";
    }
}