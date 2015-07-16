package net.ninjacat.semblance.repl;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.smooth.utils.Option;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import java.io.File;
import java.io.IOException;

/**
 * Application which runs Semblance CLI
 */
public final class SemblanceApp {

    private SemblanceApp() {
    }

    /**
     * Main method for CLI
     *
     * @param args arguments
     */
    public static void main(final String[] args) {
        final IoConsole console = System.console() != null
                ? new ConsoleWrapper(System.console()) : new EmulatedConsole();

        final Option<CommandLine> commandLine = parseCommandLine(console, args);

        if (!commandLine.isPresent() || commandLine.get().getArgList().isEmpty()) {
            runRepl(console);
        } else if (commandLine.isPresent()) {
            compileFiles(commandLine.get(), console);
        }
    }

    private static void compileFiles(final CommandLine commandLine, final IoConsole console) {
        final Interpreter semblance = new Interpreter();

        for (final String fileName : commandLine.getArgs()) {
            try {
                final File file = new File(fileName);
                semblance.compile(file, file.getParent());
                console.printf("Compiled %s", fileName);
            } catch (final IOException e) {
                console.printf("Failed to read file %s", fileName);
            } catch (final ParsingException e) {
                console.printf("Compilation error in %s", fileName);
                console.printf(e.toString());
            }
        }

        console.flush();
    }

    private static Option<CommandLine> parseCommandLine(final IoConsole console, final String[] args) {
        final Options options = new Options();
        try {
            return Option.of(new DefaultParser().parse(options, args));
        } catch (final ParseException e) {
            console.printf(e.toString());
            return Option.absent();
        }
    }

    private static void runRepl(final IoConsole console) {
        final Repl repl = new ConsoleRepl(console);

        repl.print("Semblance REPL console\n");
        repl.print("Version: " + repl.getVersion() + "\n");
        repl.print("Use (quit) to terminate");
        repl.repl();
    }
}
