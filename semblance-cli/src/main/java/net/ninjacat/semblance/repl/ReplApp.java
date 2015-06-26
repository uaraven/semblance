package net.ninjacat.semblance.repl;

/**
 * Application which runs REPL
 */
public final class ReplApp {

    private ReplApp() {
    }

    /**
     * Main method for REPL
     *
     * @param args arguments
     */
    public static void main(final String[] args) {

        final Repl repl = new ConsoleRepl(System.console() != null
                ? new ConsoleWrapper(System.console()) : new EmulatedConsole());

        repl.print("Semblance REPL console\n");
        repl.print("Version: " + repl.getVersion() + "\n");
        repl.print("Use (quit) to terminate");
        repl.repl();
    }
}
