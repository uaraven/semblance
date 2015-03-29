package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.semblance.utils.Values;

/**
 * Named block
 */
public class Block extends SpecialForm {

    /**
     * Creates new instance of named block
     */
    public Block() {
        super("block", "name", "&rest", "forms");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final SymbolAtom name = Values.asSymbol(parameters.head());
        final Context localContext = LocalContext.namedChildContext(name, context);

        return localContext.evaluateBlock(parameters.tail());
    }
}
