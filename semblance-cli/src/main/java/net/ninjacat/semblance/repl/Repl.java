package net.ninjacat.semblance.repl;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.Vector;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.utils.Either;

import java.util.Optional;
import java.util.stream.Collectors;

import static net.ninjacat.semblance.evaluator.SourceUtils.readProgram;
import static net.ninjacat.semblance.java.Lambdas.methodAsFunction;
import static net.ninjacat.semblance.utils.Values.*;

/**
 * REPL implementation for Semblance
 */
@SuppressWarnings("ClassNamingConvention")
public abstract class Repl {

    private final Context replContext;
    private final RootContext rootContext;

    /**
     * Creates a new instance of Repl
     */
    protected Repl() {
        rootContext = new RootContext();
        final LispValue quitRepl = new SpecialForm("quit") {
            @Override
            public LispValue apply(final Context context, final LispCollection parameters) {
                print("\n(Bye)\n");
                System.exit(0);
                return NilCollection.INSTANCE;
            }
        };
        rootContext.bind(symbol("quit"), quitRepl);
        rootContext.bind(symbol("print"), methodAsFunction(this::printIt));
        rootContext.bind(symbol("println"), methodAsFunction(this::printLnIt));
        replContext = LocalContext.namedChildContext(symbol("repl"), rootContext);
    }

    /**
     * Reads string from standard input (usually)
     *
     * @return expression entered by user
     */
    public abstract String read();

    /**
     * Prints evaluation result to console
     *
     * @param line value to print
     */
    public abstract void print(String line);

    /**
     * Prints error message to console
     *
     * @param message Error message
     */
    public abstract void printError(String message);

    /**
     * @return Version of underlying engine
     */
    public String getVersion() {
        final Optional<LispValue> version = rootContext.findSymbol(symbol("--VERSION--"));
        if (version.isPresent()) {
            final Vector versionVector = asVector(version.get());
            final StringBuilder ver = new StringBuilder();
            for (final LispValue item : versionVector) {
                ver.append(cleanString(item)).append(".");
            }
            ver.setLength(ver.length() - 1);
            return ver.toString();
        } else {
            return "Unknown";
        }
    }

    /**
     * Performs evaluation of user input
     *
     * @param input expression entered by user
     * @return {@link Either} exception or evaluated and stringisized result
     */
    public Either<Exception, String> eval(final String input) {
        try {
            final LispValue value = RootContext.evaluateInContext(readProgram(input), replContext);
            replContext.bind(symbol("$$"), value);
            return Either.second(value.repr());
        } catch (final Exception ex) {
            return Either.first(ex);
        }
    }

    /**
     * Performs REPL.
     */
    @SuppressWarnings({"MethodNamesDifferingOnlyByCase", "ThrowableResultOfMethodCallIgnored"})
    public void repl() {
        String input;
        do {
            print(System.lineSeparator() + "> ");
            input = read();
            if (input != null) {
                final Either<Exception, String> result = eval(input);
                if (result.hasFirst()) {
                    printError(result.getFirst().getMessage());
                } else {
                    print(result.getSecond());
                }
            }
        } while (input != null);
    }

    private LispValue printIt(final Context context, final LispCollection params) {
        final String output = params.stream().map(LispValue::printIt).collect(Collectors.joining(" "));
        print(output);
        return string(output);
    }

    private LispValue printLnIt(final Context context, final LispCollection params) {
        final String output = params.stream().map(LispValue::printIt).collect(Collectors.joining(" "));
        print(output + "\n");
        return string(output);
    }

}