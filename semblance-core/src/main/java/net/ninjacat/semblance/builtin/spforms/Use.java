package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.ParameterException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.semblance.evaluator.Namespace;

import java.util.Optional;

import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Use namespace
 */
@SuppressWarnings("ClassNamingConvention")
@CreatesContext
public class Use extends SpecialForm {

    private static final long serialVersionUID = 1479924863559681324L;

    /**
     * Creates a new instance
     */
    public Use() {
        super("use", "namespace", "body*");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final SymbolAtom namespaceName = asSymbol(parameters.head());

        final Optional<Namespace> namespace = context.findNamespace(namespaceName);
        if (namespace.isPresent()) {
            final Context localContext = LocalContext.namelessChildContext(context);
            localContext.setBindings(namespace.get().getBindings());
            return localContext.evaluateBlock(parameters.tail());
        } else {
            throw new ParameterException("Existing namespace name was expected", parameters.getSourceInfo());
        }
    }
}
