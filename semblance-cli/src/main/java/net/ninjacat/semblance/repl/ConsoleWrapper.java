package net.ninjacat.semblance.repl;

import javax.annotation.Nonnull;
import java.io.Console;

/**
 * Implementation of {@link IoConsole} which wraps {@link Console}
 */
public class ConsoleWrapper implements IoConsole {

    @Nonnull
    private final Console console;

    /**
     * Creates new instance and wraps java.io.Console
     *
     * @param console Instance of {@link Console}
     */
    public ConsoleWrapper(@Nonnull final Console console) {
        this.console = console;
    }

    @Override
    public String readLine() {
        return console.readLine();
    }

    @SuppressWarnings("OverloadedVarargsMethod")
    @Override
    public void printf(final String format, final Object... args) {
        console.printf(format, args);
    }

    @Override
    public void errPrintf(final String format, final Object... args) {
        printf(format, args);
    }

    @Override
    public void flush() {
        console.flush();
    }
}
