package net.ninjacat.semblance.repl;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;

/**
 * Emulated console using System.out and System.input
 */
@SuppressWarnings("UseOfSystemOutOrSystemErr")
public class EmulatedConsole implements IoConsole {
    private final BufferedReader reader;
    private final PrintWriter writer;

    /**
     * Creates a new instance of emulated console
     */
    public EmulatedConsole() {
        reader = new BufferedReader(new InputStreamReader(System.in));
        writer = new PrintWriter(System.out);
    }

    @Override
    public String readLine() {
        try {
            return reader.readLine();
        } catch (final Exception ignored) {
            return null;
        }
    }

    @SuppressWarnings("OverloadedVarargsMethod")
    @Override
    public void printf(final String format, final Object... args) {
        writer.printf(format, args);
    }

    @Override
    public PrintWriter writer() {
        return writer;
    }

    @Override
    public void flush() {
        writer.flush();
    }
}
