package com.confuturo.runners;

import io.cucumber.junit.Cucumber;
import io.cucumber.junit.CucumberOptions;
import org.junit.runner.RunWith;

@RunWith(Cucumber.class)
@CucumberOptions(
    features = "src/test/resources/features",
    glue = "com.confuturo.steps",
    plugin = {
        "pretty",
        "json:target/cucumber-json/cucumber-report.json"
    },
    dryRun = false,
    monochrome = true
)
public class CucumberRunner {
    
    
}