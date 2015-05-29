package net.ninjacat.semblance.repl;

import java.io.PrintWriter;

/**
 * Console abstraction
 */
public interface IoConsole {
    /**
     * Reads a line from console
     *
     * @return Line that has been read
     */
    String readLine();

    /**
     * Prints text with formatting
     *
     * @param format Format string
     * @param args   Format arguments
     */
    void printf(String format, Object... args);

    /**
     * @return {@link PrintWriter} to be used for output
     */
    PrintWriter writer();

    /**
     * Flushes output
     */
    void flush();
}
