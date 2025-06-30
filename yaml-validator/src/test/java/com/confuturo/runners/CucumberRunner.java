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
        "html:target/cucumber-reports/cucumber-html-report.html",
        "json:target/cucumber-reports/cucumber.json",
        "junit:target/cucumber-reports/cucumber.xml",
        "rerun:target/rerun.txt",
        "json:target/cucumber-json/cucumber-json.report"
    },
    monochrome = true,
    dryRun = false,
    tags = ""
)

public class CucumberRunner {
}