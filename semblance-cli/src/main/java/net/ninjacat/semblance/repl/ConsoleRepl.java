package net.ninjacat.semblance.repl;

import javax.annotation.Nonnull;

/**
 * Implementation of REPL using {@link java.io.Console}
 */
@SuppressWarnings("UseOfSystemOutOrSystemErr")
public class ConsoleRepl extends Repl {
    private final IoConsole console;

    /**
     * Creates a new instance of ConsoleRepl
     *
     * @param console Console operations provider
     */
    public ConsoleRepl(@Nonnull final IoConsole console) {
        this.console = console;
    }

    @Override
    public String read() {
        final StringBuilder ln = new StringBuilder();
        do {
            if (ln.length() > 0) {
                // continuation
                ln.append(" ");
            }
            ln.append(console.readLine());
            final boolean unevenParens = !ReplUtils.isMatchingParentheses(ln);
            if (unevenParens) {
                print("... ");
            } else {
                return ln.toString();
            }
        } while (true);
    }

    @Override
    public void print(final String line) {
        console.flush();
        console.printf(line);
        console.flush();
    }

    @Override
    public void printError(final String message) {
        console.flush();
        console.errPrintf("%n%s%n", message);
        console.flush();
    }

}
