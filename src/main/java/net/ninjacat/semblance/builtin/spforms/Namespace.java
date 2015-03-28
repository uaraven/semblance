package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.smooth.utils.Option;

import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Namespace declaration
 * <p/>
 * (namespace name &rest body)
 */
@CreatesContext
public class Namespace extends SpecialForm {

    /**
     * Creates new instance of namespace.
     */
    public Namespace() {
        super("namespace", "name", "&rest", "body");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final SymbolAtom name = asSymbol(parameters.head());
        final LispCollection body = parameters.tail();
        final Context localContext = LocalContext.namedChildContext(name.repr(), context);

        final LispValue result = localContext.evaluateBlock(body);

        final Option<net.ninjacat.semblance.evaluator.Namespace> namespace = localContext.getNamespace(Constants.NONE);

        if (namespace.isPresent()) {
            context.addNamespace(namespace.get().rename(name));
        }

        return result;
    }
}
