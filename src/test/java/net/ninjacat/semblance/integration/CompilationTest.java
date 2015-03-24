package net.ninjacat.semblance.integration;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import static net.ninjacat.semblance.utils.Values.number;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class CompilationTest {

    @Test
    public void testShouldCompileProgram() throws Exception {
        final RootContext context = new RootContext();

        final File tempFile = File.createTempFile("tmp", "smbl");
        try (final InputStream input = getClass().getResourceAsStream("/test_program.smbl");
             final OutputStream output = new FileOutputStream(tempFile)) {
            context.compile(input, output);
        }

        final Interpreter interpreter = new Interpreter();
        final LispValue value = interpreter.runCompiledFile(tempFile.getAbsolutePath());

        assertThat(value, is(number(3)));
    }
}
