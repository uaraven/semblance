package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import java.util.Optional;

import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Namespace declaration
 * <p/>
 * (namespace name &rest body)
 */
@CreatesContext
public class Namespace extends SpecialForm {

    private static final long serialVersionUID = -6132619739845968897L;

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
        final Context localContext = LocalContext.namedChildContext(name, context);

        final LispValue result = localContext.evaluateBlock(body);

        final Optional<net.ninjacat.semblance.evaluator.Namespace> namespace = localContext.getNamespace(Constants.NONE);

        namespace.ifPresent(namespace1 -> context.addNamespace(namespace1.rename(name)));

        return result;
    }
}
