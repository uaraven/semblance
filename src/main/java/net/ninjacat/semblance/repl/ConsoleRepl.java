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
            final boolean unevenParens = !isMatchingParentheses(ln);
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
        console.writer().print(line);
        console.flush();
    }

    @Override
    public void printError(final String message) {
        console.flush();
        console.printf("%n%s%n", message);
        console.flush();
    }

    private boolean isMatchingParentheses(final CharSequence ln) {
        int openParens = 0;
        int closeParens = 0;
        int openBrackets = 0;
        int closeBrackets = 0;
        for (int i = 0; i < ln.length(); i++) {
            final char c = ln.charAt(i);
            switch (c) {
                case '(':
                    openParens += 1;
                    break;
                case ')':
                    closeParens += 1;
                    break;
                case '[':
                    openBrackets += 1;
                    break;
                case ']':
                    closeBrackets += 1;
                    break;
            }
        }
        return openParens == closeParens && openBrackets == closeBrackets;
    }
}
