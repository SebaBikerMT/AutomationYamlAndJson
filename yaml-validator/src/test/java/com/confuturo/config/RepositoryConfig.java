package com.confuturo.config;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Clase para manejar la configuración de repositorios desde archivo de texto
 */
public class RepositoryConfig {
    
    private static final String DEFAULT_CONFIG_FILE = "src/test/resources/repositories.txt";
    private static List<String> repositories = new ArrayList<>();
    private static boolean isLoaded = false;
    
    /**
     * Carga la lista de repositorios desde el archivo de configuración
     */
    public static void loadRepositories() {
        loadRepositories(DEFAULT_CONFIG_FILE);
    }
    
    /**
     * Carga la lista de repositorios desde un archivo específico
     * @param configFilePath Ruta al archivo de configuración
     */
    public static void loadRepositories(String configFilePath) {
        repositories.clear();
        
        try {
            Path path = Paths.get(configFilePath);
            
            if (!Files.exists(path)) {
                System.err.println("⚠️ Archivo de configuración no encontrado: " + configFilePath);
                System.err.println("Creando archivo de ejemplo...");
                createExampleFile(path);
                return;
            }
            
            System.out.println("📂 Cargando repositorios desde: " + path.toAbsolutePath());
            
            List<String> lines = Files.readAllLines(path);
            int lineNumber = 0;
            
            for (String line : lines) {
                lineNumber++;
                
                // Ignorar líneas vacías y comentarios
                String trimmedLine = line.trim();
                if (trimmedLine.isEmpty() || trimmedLine.startsWith("#")) {
                    continue;
                }
                
                // Procesar múltiples repositorios separados por punto y coma
                String[] repos = trimmedLine.split(";");
                for (String repo : repos) {
                    String cleanRepo = repo.trim();
                    if (!cleanRepo.isEmpty()) {
                        repositories.add(cleanRepo);
                        System.out.println("✅ Repositorio agregado: " + cleanRepo);
                    }
                }
            }
            
            isLoaded = true;
            System.out.println("📊 Total de repositorios cargados: " + repositories.size());
            
            if (repositories.isEmpty()) {
                System.err.println("⚠️ No se encontraron repositorios válidos en el archivo");
            }
            
        } catch (IOException e) {
            System.err.println("❌ Error al leer archivo de configuración: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Crea un archivo de ejemplo con repositorios de muestra
     */
    private static void createExampleFile(Path path) {
        try {
            // Crear directorio padre si no existe
            Files.createDirectories(path.getParent());
            
            List<String> exampleContent = List.of(
                "# Archivo de configuración de repositorios",
                "# Formato: un repositorio por línea o varios separados por punto y coma (;)",
                "# Las líneas que empiecen con # son comentarios y serán ignoradas",
                "",
                "# Ejemplos:",
                "# usuario/proyecto1",
                "# usuario/proyecto2; usuario/proyecto3",
                "# grupo/subgrupo/proyecto4",
                "",
                "# Agrega tus repositorios aquí:",
                "ejemplo/repositorio1",
                "ejemplo/repositorio2; ejemplo/repositorio3"
            );
            
            Files.write(path, exampleContent);
            System.out.println("📝 Archivo de ejemplo creado en: " + path.toAbsolutePath());
            System.out.println("Por favor, edita el archivo y agrega tus repositorios reales");
            
        } catch (IOException e) {
            System.err.println("❌ Error al crear archivo de ejemplo: " + e.getMessage());
        }
    }
    
    /**
     * Obtiene la lista de repositorios cargados
     * @return Lista de repositorios
     */
    public static List<String> getRepositories() {
        if (!isLoaded) {
            loadRepositories();
        }
        return new ArrayList<>(repositories);
    }
    
    /**
     * Obtiene un repositorio específico por índice
     * @param index Índice del repositorio
     * @return Repositorio en el índice especificado
     */
    public static String getRepository(int index) {
        List<String> repos = getRepositories();
        if (index >= 0 && index < repos.size()) {
            return repos.get(index);
        }
        throw new IndexOutOfBoundsException("Índice de repositorio fuera de rango: " + index + " (total: " + repos.size() + ")");
    }
    
    /**
     * Obtiene el número total de repositorios
     * @return Cantidad de repositorios
     */
    public static int getTotalRepositories() {
        return getRepositories().size();
    }
    
    /**
     * Verifica si hay repositorios cargados
     * @return true si hay repositorios, false en caso contrario
     */
    public static boolean hasRepositories() {
        return !getRepositories().isEmpty();
    }
    
    /**
     * Recarga los repositorios desde el archivo
     */
    public static void reload() {
        isLoaded = false;
        loadRepositories();
    }
    
    /**
     * Establece manualmente la lista de repositorios (útil para testing)
     * @param repositoryList Lista de repositorios
     */
    public static void setRepositories(List<String> repositoryList) {
        repositories.clear();
        repositories.addAll(repositoryList);
        isLoaded = true;
    }
    
    /**
     * Limpia la configuración cargada
     */
    public static void clear() {
        repositories.clear();
        isLoaded = false;
    }
}