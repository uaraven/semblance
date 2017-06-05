package net.ninjacat.semblance.java;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.SList;

import java.util.function.Function;

import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Factory to generate Java {@link Function} from Semblance functions.
 */
public class FunctionFactory {

    private final Interpreter interpreter;

    /**
     * Creates new FunctionFactory instance bound to given Semblance {@link Interpreter}
     *
     * @param interpreter Semblance interpreter
     */
    public FunctionFactory(final Interpreter interpreter) {
        this.interpreter = interpreter;
    }

    /**
     * Creates Java function that will delegate its execution to Semblance function.
     * <p>
     * If interpreter does not have binding for the name function then {@link net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException} will
     * be thrown when the returned function is called.
     *
     * @param name Name of the Semblance function to wrap into Java function.
     * @return {@link Function}
     */
    public Function<SList, LispValue> getFunction(final String name) {
        final SymbolAtom nameSymbol = symbol(name);
        return (params) -> this.interpreter.callFunction(nameSymbol, params);
    }

}
