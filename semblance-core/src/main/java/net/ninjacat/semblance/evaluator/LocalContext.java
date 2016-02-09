package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.smooth.utils.Option;

/**
 * Implementation of context for use everywhere where local context is required: blocks, functions, macros, etc.
 *
 * @author oleksiivoronin, date: 15-03-07.
 */
public class LocalContext extends BaseContext {

    protected LocalContext(final SymbolAtom name, final Context parent) {
        super(name, parent, Option.<UndefinedFunctionStrategy>absent());
    }

    /**
     * Creates new nameless context.
     *
     * @param parent Parent context.
     * @return New local context.
     */
    public static Context namelessChildContext(final Context parent) {
        return new LocalContext(Constants.NONE, parent);
    }

    /**
     * Creates named context.
     *
     * @param name   Name of the new context.
     * @param parent Parent context.
     * @return New local context.
     */
    public static Context namedChildContext(final SymbolAtom name, final Context parent) {
        return new LocalContext(name, parent);
    }
}
