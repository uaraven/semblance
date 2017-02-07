package net.ninjacat.semblance.repl;

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
     * Prints text with formatting as a error
     *
     * @param format Format string
     * @param args   Format arguments
     */
    void errPrintf(String format, Object... args);

    /**
     * Flushes output
     */
    void flush();
}
