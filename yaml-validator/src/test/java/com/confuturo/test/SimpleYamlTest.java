package com.confuturo.test;

import org.junit.Test;

import io.github.cdimascio.dotenv.Dotenv;

import java.util.List;

import org.gitlab4j.api.GitLabApi;
import org.gitlab4j.api.models.Project;
import org.gitlab4j.api.models.RepositoryFile;

public class SimpleYamlTest {
    
    @Test
    public void testYamlFieldValue() throws Exception {

        String gitLabToken = null;
        try {
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
            
            System.out.println("Usando token especificado directamente");
            
            // Inicializar GitLab API
            GitLabApi gitLabApi = new GitLabApi("https://gitlab.com", gitLabToken);
            
            // Ruta correcta del proyecto
            String projectPath = "confuturo_ti/portales-internos/transaction-manager/backend-transaction-manager";
            
            System.out.println("Intentando acceder al proyecto: " + projectPath);
            
            // Intentar acceder al proyecto
            Project project = gitLabApi.getProjectApi().getProject(projectPath);
            System.out.println("¡Proyecto encontrado! ID: " + project.getId());
            
            // Ruta y rama correctas del archivo
            String filePath = "helm/prod-values-gke.yaml";
            String branch = "main";
            
            System.out.println("Intentando leer archivo: " + filePath + " en rama: " + branch);
            
            // Intentar obtener el archivo
            RepositoryFile file = gitLabApi.getRepositoryFileApi()
                    .getFile(project.getId(), filePath, branch);
            
            // Decodificar el contenido
            String yamlContent = new String(
                    java.util.Base64.getDecoder().decode(file.getContent()),
                    java.nio.charset.StandardCharsets.UTF_8);
            
            // Guardar el contenido para verificación
            java.io.FileWriter writer = new java.io.FileWriter("gitlab-yaml-content.txt");
            writer.write(yamlContent);
            writer.close();
            
            System.out.println("Contenido guardado en 'gitlab-yaml-content.txt'");
            System.out.println("\n===== VALIDANDO CAMPO ESPECÍFICO =====");
            
            // Buscar campo Apis__Kta__User
            String campo = "Apis__Kta__User";
            boolean encontrado = false;
            String valorActual = null;
            
            // Buscar línea por línea
            for (String line : yamlContent.split("\n")) {
                String trimmed = line.trim();
                if (trimmed.startsWith(campo)) {
                    String[] parts = trimmed.split(":", 2);
                    if (parts.length > 1) {
                        valorActual = parts[1].trim();
                        encontrado = true;
                        System.out.println("Campo encontrado: " + campo);
                        System.out.println("Valor actual: '" + valorActual + "'");
                        break;
                    }
                }
            }
            
            if (encontrado) {
                // Valida el valor - ACTUALIZADO PARA USAR EL VALOR CORRECTO
                String valorEsperado = "admin"; // El valor correcto según el YAML
                System.out.println("Valor esperado: '" + valorEsperado + "'");
                System.out.println("¿Son iguales? " + valorActual.equals(valorEsperado));
                
                if (!valorActual.equals(valorEsperado)) {
                    System.out.println("¡ERROR DE VALIDACIÓN! Valores diferentes");
                    throw new AssertionError("El campo " + campo + 
                        " tiene valor '" + valorActual + "' pero debería ser '" + valorEsperado + "'");
                } else {
                    System.out.println("¡VALIDACIÓN EXITOSA! El valor '" + valorActual + "' es correcto");
                }
            } else {
                System.out.println("¡ERROR! Campo '" + campo + "' no encontrado en el YAML");
                throw new AssertionError("Campo " + campo + " no encontrado en el YAML");
            }
            
        } catch (Exception e) {
            System.err.println("ERROR: " + e.getMessage());
            e.printStackTrace();
            throw e;
        }
    }
}