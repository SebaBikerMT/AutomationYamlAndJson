package com.confuturo.utils;

import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlReader {
    
    public static Map<String, Object> readYaml(InputStream inputStream) {
        Yaml yaml = new Yaml();
        return yaml.load(inputStream);
    }
}