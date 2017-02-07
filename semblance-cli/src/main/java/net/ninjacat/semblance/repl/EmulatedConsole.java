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
    private final PrintWriter errWriter;

    /**
     * Creates a new instance of emulated console
     */
    public EmulatedConsole() {
        reader = new BufferedReader(new InputStreamReader(System.in));
        writer = new PrintWriter(System.out);
        errWriter = new PrintWriter(System.err);
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
    public void errPrintf(final String format, final Object... args) {
        errWriter.printf(format, args);
    }

    @Override
    public void flush() {
        writer.flush();
        errWriter.flush();
    }
}
